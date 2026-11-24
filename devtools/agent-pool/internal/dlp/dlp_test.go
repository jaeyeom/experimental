package dlp

import (
	"strings"
	"testing"
)

func TestScanCleanDeliverable(t *testing.T) {
	t.Parallel()

	scanner := NewScanner()
	payload := []byte(`{
  "summary": "Reviewed commit a1b2c3d4e5f60718293a4b5c6d7e8f9012345678 in devtools/agent-pool/internal/protocol/types.go",
  "findings": [
    {
      "file": "devtools/agent-pool/internal/protocol/types.go",
      "line": 42,
      "comment": "Consider adding a doc comment: func Compute(x int) int { return x + 1 }"
    }
  ]
}`)

	res := scanner.Scan(payload, HostContext{
		WorktreePath:  "/home/alice/.local/state/agent-pool/worktrees/job_42",
		Username:      "alice",
		HomeDir:       "/home/alice",
		Hostname:      "workstation-01",
		PersonalEmail: "alice@example.org",
		ExtraDenied:   []string{"internal-corp-secret-host"},
	})

	if res.Blocked {
		t.Fatalf("expected clean deliverable not to be blocked, got findings: %+v", res.Findings)
	}
	if len(res.Findings) != 0 {
		t.Fatalf("expected zero findings, got %+v", res.Findings)
	}
	if string(res.Normalized) != string(payload) {
		t.Fatalf("expected normalized payload to match input")
	}
}

func TestScanSecretPatterns(t *testing.T) {
	t.Parallel()

	// Build all fake secrets dynamically at runtime so gitleaks never flags this file.
	jwtHeader := "eyJ" + strings.Repeat("a", 12)
	jwtPayload := "eyJ" + strings.Repeat("b", 12)
	jwtSig := strings.Repeat("c", 16)

	tests := []struct {
		name   string
		secret string
	}{
		{
			name:   "github_pat",
			secret: "ghp_" + strings.Repeat("a", 36),
		},
		{
			name:   "github_oauth",
			secret: "gho_" + strings.Repeat("b", 36),
		},
		{
			name:   "github_app_token",
			secret: "ghs_" + strings.Repeat("c", 36),
		},
		{
			name:   "github_user_token",
			secret: "ghu_" + strings.Repeat("d", 36),
		},
		{
			name:   "github_fine_grained_pat",
			secret: "github_pat_" + strings.Repeat("e", 24),
		},
		{
			name:   "anthropic_key",
			secret: "sk-" + "ant-" + strings.Repeat("f", 24),
		},
		{
			name:   "openai_key",
			secret: "sk-" + strings.Repeat("g", 24),
		},
		{
			name:   "openai_proj_key",
			secret: "sk-" + "proj-" + strings.Repeat("h", 24),
		},
		{
			name:   "aws_access_key_akia",
			secret: "AKIA" + "0123456789ABCDEF",
		},
		{
			name:   "aws_access_key_asia",
			secret: "ASIA" + "FEDCBA9876543210",
		},
		{
			name:   "slack_bot_token",
			secret: "xox" + "b-" + "1234567890-abcdef123456",
		},
		{
			name:   "slack_app_token",
			secret: "xox" + "a-" + "1234567890-abcdef123456",
		},
		{
			name:   "pem_private_key",
			secret: "-----BEGIN " + "RSA PRIVATE KEY-----",
		},
		{
			name:   "pem_generic_private_key",
			secret: "-----BEGIN " + "PRIVATE KEY-----",
		},
		{
			name:   "bearer_token",
			secret: "Bearer " + strings.Repeat("x", 24),
		},
		{
			name:   "jwt_token",
			secret: jwtHeader + "." + jwtPayload + "." + jwtSig,
		},
	}

	scanner := NewScanner()
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			payload := []byte("output contains " + tc.secret + " in body")
			res := scanner.Scan(payload, HostContext{})

			if !res.Blocked {
				t.Fatalf("expected %s to be blocked", tc.name)
			}

			var foundSecret bool
			for _, f := range res.Findings {
				if f.Kind == FindingSecret {
					foundSecret = true
				}
				if strings.Contains(f.Detail, tc.secret) {
					t.Fatalf("finding detail leaked raw secret: %q", f.Detail)
				}
			}
			if !foundSecret {
				t.Fatalf("expected at least one FindingSecret finding, got %+v", res.Findings)
			}
		})
	}
}

func TestScanHighEntropyAssignment(t *testing.T) {
	t.Parallel()

	scanner := NewScanner()

	// High-entropy 28-char value built dynamically.
	highEntropyVal := "aB3xY9kL2mN5pQ8" + "rS1tU4vW7zZ0cD6"

	t.Run("high_entropy_api_key_blocked", func(t *testing.T) {
		t.Parallel()

		payload := []byte(`api_key = "` + highEntropyVal + `"`)
		res := scanner.Scan(payload, HostContext{})

		if !res.Blocked {
			t.Fatalf("expected high-entropy api_key assignment to be blocked")
		}
		var foundEntropy bool
		for _, f := range res.Findings {
			if f.Kind == FindingHighEntropy {
				foundEntropy = true
			}
			if strings.Contains(f.Detail, highEntropyVal) {
				t.Fatalf("finding detail leaked raw secret value: %q", f.Detail)
			}
		}
		if !foundEntropy {
			t.Fatalf("expected FindingHighEntropy, got %+v", res.Findings)
		}
	})

	t.Run("placeholder_api_key_allowed", func(t *testing.T) {
		t.Parallel()

		payload := []byte(`api_key = "placeholder"`)
		res := scanner.Scan(payload, HostContext{})
		if res.Blocked {
			t.Fatalf("expected placeholder api_key not to be blocked, got %+v", res.Findings)
		}
	})

	t.Run("low_entropy_long_value_allowed", func(t *testing.T) {
		t.Parallel()

		payload := []byte(`password = "` + strings.Repeat("ab", 16) + `"`)
		res := scanner.Scan(payload, HostContext{})
		if res.Blocked {
			t.Fatalf("expected low-entropy value not to be blocked, got %+v", res.Findings)
		}
	})

	t.Run("prose_with_commit_sha_allowed", func(t *testing.T) {
		t.Parallel()

		payload := []byte(`Checked commit 9f86d081884c7d659a2feaa0c55ad015a3bf4f1b and verified tests pass.`)
		res := scanner.Scan(payload, HostContext{})
		if res.Blocked {
			t.Fatalf("expected prose with commit SHA not to be blocked, got %+v", res.Findings)
		}
	})
}

func TestScanWorktreeNormalization(t *testing.T) {
	t.Parallel()

	scanner := NewScanner()
	host := HostContext{
		HomeDir:      "/home/alice",
		Username:     "alice",
		WorktreePath: "/home/alice/.local/state/agent-pool/worktrees/job_1",
	}

	payload := []byte(`error in /home/alice/.local/state/agent-pool/worktrees/job_1/main.go:10: undefined`)
	res := scanner.Scan(payload, host)

	if res.Blocked {
		t.Fatalf("expected worktree path to normalize cleanly without blocking, got %+v", res.Findings)
	}
	want := "error in <repo>/main.go:10: undefined"
	if string(res.Normalized) != want {
		t.Fatalf("Normalized = %q, want %q", string(res.Normalized), want)
	}

	t.Run("trailing_slash_in_worktree_path", func(t *testing.T) {
		t.Parallel()

		hostWithSlash := host
		hostWithSlash.WorktreePath += "/"
		resSlash := scanner.Scan(payload, hostWithSlash)
		if resSlash.Blocked {
			t.Fatalf("expected worktree path with trailing slash not to block, got %+v", resSlash.Findings)
		}
		if string(resSlash.Normalized) != want {
			t.Fatalf("Normalized = %q, want %q", string(resSlash.Normalized), want)
		}
	})
}

func TestScanHostLeakage(t *testing.T) {
	t.Parallel()

	scanner := NewScanner()

	tests := []struct {
		name        string
		payload     string
		host        HostContext
		wantBlocked bool
		leakValue   string
	}{
		{
			name:    "home_dir_outside_worktree",
			payload: "read key from /home/alice/.ssh/id_ed25519",
			host: HostContext{
				HomeDir:      "/home/alice",
				WorktreePath: "/home/alice/.local/state/agent-pool/worktrees/job_1",
			},
			wantBlocked: true,
			leakValue:   "/home/alice",
		},
		{
			name:        "personal_email_case_insensitive",
			payload:     "Author: Alice <ALICE.SMITH@EXAMPLE.COM>",
			host:        HostContext{PersonalEmail: "alice.smith@example.com"},
			wantBlocked: true,
			leakValue:   "alice.smith@example.com",
		},
		{
			name:        "hostname_bounded_match",
			payload:     "built on host dragon-dev.local at noon",
			host:        HostContext{Hostname: "dragon-dev"},
			wantBlocked: true,
			leakValue:   "dragon-dev",
		},
		{
			name:        "hostname_inside_unrelated_word_allowed",
			payload:     "the snapdragon flower blooms in spring",
			host:        HostContext{Hostname: "dragon"},
			wantBlocked: false,
		},
		{
			name:        "short_hostname_ignored",
			payload:     "running on box in cluster",
			host:        HostContext{Hostname: "box"},
			wantBlocked: false,
		},
		{
			name:        "generic_home_dir_stage2_empty_host_context",
			payload:     "see /Users/bob/secret.txt for details",
			host:        HostContext{},
			wantBlocked: true,
			leakValue:   "/Users/bob/",
		},
		{
			name:        "username_path_without_trailing_slash",
			payload:     "home is /home/alice",
			host:        HostContext{Username: "alice"},
			wantBlocked: true,
			leakValue:   "/home/alice",
		},
		{
			name:        "extra_denied_literal",
			payload:     "connected to corp-internal-vault-prod",
			host:        HostContext{ExtraDenied: []string{"corp-internal-vault-prod"}},
			wantBlocked: true,
			leakValue:   "corp-internal-vault-prod",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			res := scanner.Scan([]byte(tc.payload), tc.host)
			if res.Blocked != tc.wantBlocked {
				t.Fatalf("Blocked = %v, want %v (findings: %+v)", res.Blocked, tc.wantBlocked, res.Findings)
			}
			if !tc.wantBlocked {
				return
			}

			var foundHostLeak bool
			for _, f := range res.Findings {
				if f.Kind == FindingHostLeak {
					foundHostLeak = true
				}
				if tc.leakValue != "" && strings.Contains(strings.ToLower(f.Detail), strings.ToLower(tc.leakValue)) {
					t.Fatalf("finding detail leaked host token %q: %q", tc.leakValue, f.Detail)
				}
			}
			if !foundHostLeak {
				t.Fatalf("expected FindingHostLeak, got %+v", res.Findings)
			}
		})
	}
}
