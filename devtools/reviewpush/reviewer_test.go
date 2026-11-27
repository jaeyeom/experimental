package reviewpush

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestParseReviewResult(t *testing.T) {
	tests := []struct {
		name    string
		raw     string
		want    ReviewResult
		wantErr string
	}{
		{
			name: "valid push",
			raw:  `{"status":"PUSH","summary":"looks good","multi_commits":false,"push_target":"abc123","blocked_reason":null}`,
			want: ReviewResult{
				Status:     StatusPush,
				Summary:    "looks good",
				PushTarget: strPtr("abc123"),
			},
		},
		{
			name: "valid multi-commit push",
			raw:  `{"status":"PUSH","summary":"batch ok","multi_commits":true,"push_target":"def456","blocked_reason":null}`,
			want: ReviewResult{
				Status:       StatusPush,
				Summary:      "batch ok",
				MultiCommits: true,
				PushTarget:   strPtr("def456"),
			},
		},
		{
			name: "valid blocked",
			raw:  `{"status":"BLOCKED","summary":"secret found","multi_commits":false,"push_target":null,"blocked_reason":"leaked API key"}`,
			want: ReviewResult{
				Status:        StatusBlocked,
				Summary:       "secret found",
				BlockedReason: strPtr("leaked API key"),
			},
		},
		{
			name: "minimal push without optional fields",
			raw:  `{"status":"PUSH","summary":"ok"}`,
			want: ReviewResult{
				Status:  StatusPush,
				Summary: "ok",
			},
		},
		{
			name:    "empty output",
			raw:     "   \n",
			wantErr: "reviewer output is empty",
		},
		{
			name:    "json null",
			raw:     "null",
			wantErr: "reviewer output must be a JSON object",
		},
		{
			name:    "json array",
			raw:     `["PUSH"]`,
			wantErr: "reviewer output must be a JSON object",
		},
		{
			name:    "malformed json",
			raw:     `{"status":"PUSH",`,
			wantErr: "parse reviewer output",
		},
		{
			name:    "trailing data after object",
			raw:     `{"status":"PUSH","summary":"ok"}{"extra":1}`,
			wantErr: "trailing data",
		},
		{
			name:    "unknown field rejected",
			raw:     `{"status":"PUSH","summary":"ok","extra":"nope"}`,
			wantErr: "unknown field",
		},
		{
			name:    "missing status",
			raw:     `{"summary":"ok"}`,
			wantErr: `missing required field "status"`,
		},
		{
			name:    "missing summary",
			raw:     `{"status":"PUSH"}`,
			wantErr: `missing required field "summary"`,
		},
		{
			name:    "invalid status",
			raw:     `{"status":"MAYBE","summary":"unsure"}`,
			wantErr: "unexpected reviewer status",
		},
		{
			name:    "push with blocked reason rejected",
			raw:     `{"status":"PUSH","summary":"ok","blocked_reason":"wait no"}`,
			wantErr: "PUSH verdict must not include a blocked_reason",
		},
		{
			name:    "push with empty push_target rejected",
			raw:     `{"status":"PUSH","summary":"ok","push_target":"  "}`,
			wantErr: "push_target must be null or a non-empty commit ref",
		},
		{
			name:    "multi_commits without push_target rejected",
			raw:     `{"status":"PUSH","summary":"ok","multi_commits":true,"push_target":null}`,
			wantErr: "multi_commits=true requires a non-empty push_target",
		},
		{
			name:    "blocked with push_target rejected",
			raw:     `{"status":"BLOCKED","summary":"bad","push_target":"abc123","blocked_reason":"broken"}`,
			wantErr: "BLOCKED verdict must not include a push_target",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ParseReviewResult([]byte(tt.raw))
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want substring %q", err, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if !reflect.DeepEqual(*got, tt.want) {
				t.Fatalf("got %+v, want %+v", *got, tt.want)
			}
		})
	}
}

func TestSplitCommand(t *testing.T) {
	tests := []struct {
		name    string
		raw     string
		want    []string
		wantErr string
	}{
		{
			name: "simple binary",
			raw:  "my-reviewer",
			want: []string{"my-reviewer"},
		},
		{
			name: "flags and quoted args",
			raw:  `/usr/local/bin/rev --mode "read only" --tag 'v1' --escaped\ arg`,
			want: []string{"/usr/local/bin/rev", "--mode", "read only", "--tag", "v1", "--escaped arg"},
		},
		{
			name:    "empty",
			raw:     "   ",
			wantErr: "must not be empty",
		},
		{
			name:    "unclosed quote",
			raw:     `my-reviewer --flag "unclosed`,
			wantErr: "unclosed quote",
		},
		{
			name:    "trailing backslash",
			raw:     `my-reviewer \`,
			wantErr: "trailing backslash",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := SplitCommand(tt.raw)
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want substring %q", err, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("got %#v, want %#v", got, tt.want)
			}
		})
	}
}

func TestCommandReviewer(t *testing.T) {
	t.Run("passes metadata env, stdin prompt, cwd, and parses PUSH", func(t *testing.T) {
		repoDir := t.TempDir()
		envCapture := filepath.Join(t.TempDir(), "captured.txt")
		script := writeTestCodexScript(t, `#!/bin/sh
prompt="$(cat)"
{
  printf 'PWD=%s\n' "$(pwd)"
  printf 'BASE=%s\n' "$REVIEWPUSH_BASE_BRANCH"
  printf 'REMOTE=%s\n' "$REVIEWPUSH_REMOTE_REF"
  printf 'EXISTS=%s\n' "$REVIEWPUSH_REMOTE_REF_EXISTS"
  printf 'ITER=%s\n' "$REVIEWPUSH_ITERATION"
  printf 'AHEAD=%s\n' "$REVIEWPUSH_AHEAD_COUNT"
  printf 'TARGET=%s\n' "$REVIEWPUSH_TARGET_COMMIT"
  printf 'SCHEMA_EXISTS=%s\n' "$(test -f "$REVIEWPUSH_SCHEMA_FILE" && echo yes || echo no)"
  printf 'PROMPT_HAS_TARGET=%s\n' "$(printf '%s' "$prompt" | grep -q 'Target commit: commit-1' && echo yes || echo no)"
} > "`+envCapture+`"
echo 'noise on stderr' >&2
printf '%s\n' '{"status":"PUSH","summary":"clean","multi_commits":false,"push_target":"commit-1","blocked_reason":null}'
`)
		var stderr bytes.Buffer
		rev := &CommandReviewer{
			Command:      []string{script},
			TimeoutSecs:  5,
			Stderr:       &stderr,
			StreamStderr: false,
		}
		res, err := rev.RunPass(context.Background(), &ReviewPassInput{
			RepoDir:         repoDir,
			BaseBranch:      "main",
			RemoteRef:       "origin/main",
			RemoteRefExists: true,
			Iteration:       1,
			AheadCount:      2,
			TargetCommit:    "commit-1",
		})
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if res.Status != StatusPush || res.PushTarget == nil || *res.PushTarget != "commit-1" {
			t.Fatalf("unexpected result: %+v", res)
		}
		if stderr.Len() != 0 {
			t.Fatalf("expected stderr suppressed on success, got %q", stderr.String())
		}
		rawCap, err := os.ReadFile(envCapture)
		if err != nil {
			t.Fatal(err)
		}
		captured := string(rawCap)
		resolvedRepo, _ := filepath.EvalSymlinks(repoDir)
		for _, want := range []string{
			"PWD=" + resolvedRepo,
			"BASE=main",
			"REMOTE=origin/main",
			"EXISTS=true",
			"ITER=1",
			"AHEAD=2",
			"TARGET=commit-1",
			"SCHEMA_EXISTS=yes",
			"PROMPT_HAS_TARGET=yes",
		} {
			if !strings.Contains(captured, want) {
				t.Fatalf("captured env/stdin missing %q:\n%s", want, captured)
			}
		}
	})

	t.Run("rejects non-zero exit and emits captured stderr", func(t *testing.T) {
		script := writeTestCodexScript(t, `#!/bin/sh
echo 'custom reviewer exploded' >&2
exit 3
`)
		var stderr bytes.Buffer
		rev := &CommandReviewer{
			Command:      []string{script},
			TimeoutSecs:  5,
			Stderr:       &stderr,
			StreamStderr: false,
		}
		_, err := rev.RunPass(context.Background(), &ReviewPassInput{
			RepoDir:      t.TempDir(),
			BaseBranch:   "main",
			RemoteRef:    "origin/main",
			TargetCommit: "commit-1",
		})
		if err == nil || !strings.Contains(err.Error(), "reviewer execution failed") {
			t.Fatalf("err = %v, want reviewer execution failed", err)
		}
		if !strings.Contains(stderr.String(), "custom reviewer exploded") {
			t.Fatalf("stderr = %q, want captured stderr", stderr.String())
		}
	})

	t.Run("rejects malformed stdout verdict", func(t *testing.T) {
		script := writeTestCodexScript(t, `#!/bin/sh
echo 'PUSH'
`)
		rev := &CommandReviewer{
			Command:     []string{script},
			TimeoutSecs: 5,
		}
		_, err := rev.RunPass(context.Background(), &ReviewPassInput{
			RepoDir:      t.TempDir(),
			BaseBranch:   "main",
			RemoteRef:    "origin/main",
			TargetCommit: "commit-1",
		})
		if err == nil || !strings.Contains(err.Error(), "JSON object") {
			t.Fatalf("err = %v, want JSON object validation error", err)
		}
	})

	t.Run("times out slow reviewer", func(t *testing.T) {
		script := writeTestCodexScript(t, `#!/bin/sh
sleep 3
`)
		rev := &CommandReviewer{
			Command:     []string{script},
			TimeoutSecs: 1,
		}
		_, err := rev.RunPass(context.Background(), &ReviewPassInput{
			RepoDir:      t.TempDir(),
			BaseBranch:   "main",
			RemoteRef:    "origin/main",
			TargetCommit: "commit-1",
		})
		if err == nil || !strings.Contains(err.Error(), "timed out") {
			t.Fatalf("err = %v, want timeout error", err)
		}
	})
}
