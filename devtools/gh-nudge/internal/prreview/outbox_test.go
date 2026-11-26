package prreview

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

func TestSubmitViaOutbox_ThreadOnlyWithFakeScript(t *testing.T) {
	handler, _, cleanup := setupTestHandler(t)
	defer cleanup()

	repository := models.NewRepository("acme", "widgets")
	prNumber := 42
	target := models.NewPRTarget(prNumber)

	createTestDiffHunks(t, handler, repository, prNumber, []models.DiffHunk{
		{Location: models.NewFileLocation("src/main.go", models.NewLineRange(1, 50)), Side: models.SideRight},
	})
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "src/main.go",
		Line: models.NewSingleLine(10),
		Body: "Consider using a constant here",
		Side: models.SideRight,
	})
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "src/main.go",
		Line: models.NewLineRange(20, 25),
		Body: "Extract this helper",
		Side: models.SideRight,
	})

	ghCalled := false
	handler.ghClient = &MockGitHubClient{
		SubmitReviewFunc: func(_ models.Repository, _ int, _ models.PRReview) error {
			ghCalled = true
			return nil
		},
	}

	argsLog := filepath.Join(t.TempDir(), "argv.txt")
	bodyLog := filepath.Join(t.TempDir(), "lines.txt")
	fakeOutbox := writeFakeOutboxScript(t, fmt.Sprintf(`#!/usr/bin/env sh
set -eu
printf '%%s\n' "$@" > %q
for arg in "$@"; do
  case "$arg" in
    *=*)
      file="${arg#*=}"
      printf '%%s:' "${arg%%=*}" >> %q
      cat "$file" >> %q
      printf '\n---\n' >> %q
      ;;
  esac
done
printf '17\n'
`, argsLog, bodyLog, bodyLog, bodyLog))

	handler.SetOutboxClient(NewCLIOutboxClient([]string{fakeOutbox, "--extra-flag"}))
	var outBuf, errBuf bytes.Buffer
	handler.SetOutputWriters(&outBuf, &errBuf)

	opts := SubmitOptions{
		ViaOutbox:   true,
		OutboxOwner: "review-agent",
		OutboxRef:   "pass-1",
	}
	err := handler.SubmitCommandWithOptions(repository, prNumber, "", "", "", models.NewTextFormatter(), models.ClearAction{}, opts)
	if err != nil {
		t.Fatalf("SubmitCommandWithOptions() error = %v", err)
	}
	if ghCalled {
		t.Fatal("SubmitReview was called; --via-outbox must post nothing itself")
	}
	if got := strings.TrimSpace(outBuf.String()); got != "17" {
		t.Fatalf("stdout = %q, want %q", got, "17")
	}

	rawArgs, err := os.ReadFile(argsLog)
	if err != nil {
		t.Fatalf("read argv log: %v", err)
	}
	argv := strings.Split(strings.TrimSpace(string(rawArgs)), "\n")
	if argv[0] != "--extra-flag" || argv[1] != "submit" {
		t.Fatalf("unexpected argv prefix: %v", argv)
	}
	assertFlagPair(t, argv, "--repo", "acme/widgets")
	assertFlagPair(t, argv, "--pr", "42")
	assertFlagPair(t, argv, "--owner", "review-agent")
	assertFlagPair(t, argv, "--kind", "thread")
	assertFlagPair(t, argv, "--ref", "pass-1")
	if slices.Contains(argv, "--body-file") {
		t.Fatalf("unexpected --body-file in thread-only argv: %v", argv)
	}

	rawLines, err := os.ReadFile(bodyLog)
	if err != nil {
		t.Fatalf("read lines log: %v", err)
	}
	linesStr := string(rawLines)
	if !strings.Contains(linesStr, "src/main.go:10:Consider using a constant here") {
		t.Errorf("missing first line text in %q", linesStr)
	}
	if !strings.Contains(linesStr, "src/main.go:25:Extract this helper") {
		t.Errorf("missing second line text (EndLine=25) in %q", linesStr)
	}

	// Local comments must remain in place, marked with the outbox draft ID.
	stored, err := handler.storage.GetComments(repository, target)
	if err != nil {
		t.Fatalf("GetComments() error = %v", err)
	}
	if len(stored.Comments) != 2 {
		t.Fatalf("stored comments = %d, want 2", len(stored.Comments))
	}
	for i, c := range stored.Comments {
		if c.OutboxDraftID != "17" {
			t.Errorf("comment[%d].OutboxDraftID = %q, want %q", i, c.OutboxDraftID, "17")
		}
		if c.OutboxAfter != "clear" {
			t.Errorf("comment[%d].OutboxAfter = %q, want %q", i, c.OutboxAfter, "clear")
		}
	}
}

func TestSubmitViaOutbox_ReviewEventWithLinesAndBody(t *testing.T) {
	tests := []struct {
		event    string
		wantKind string
	}{
		{event: "COMMENT", wantKind: "comment"},
		{event: "APPROVE", wantKind: "approve"},
		{event: "REQUEST_CHANGES", wantKind: "request-changes"},
	}

	for _, tc := range tests {
		t.Run(tc.event, func(t *testing.T) {
			handler, _, cleanup := setupTestHandler(t)
			defer cleanup()

			repository := models.NewRepository("acme", "widgets")
			prNumber := 99

			createTestDiffHunks(t, handler, repository, prNumber, []models.DiffHunk{
				{Location: models.NewFileLocation("pkg/foo.go", models.NewLineRange(1, 30)), Side: models.SideRight},
			})
			createTestComment(t, handler, repository, prNumber, models.Comment{
				Path: "pkg/foo.go",
				Line: models.NewSingleLine(12),
				Body: "Check nil pointer",
				Side: models.SideRight,
			})

			var capturedArgv []string
			var capturedBody string
			var capturedLineText string
			client := &CLIOutboxClient{
				Command: []string{"gh-outbox"},
				Run: func(_ context.Context, argv []string) ([]byte, []byte, error) {
					capturedArgv = slices.Clone(argv)
					for i := 0; i < len(argv)-1; i++ {
						if argv[i] == "--body-file" {
							data, err := os.ReadFile(argv[i+1])
							if err != nil {
								t.Fatalf("read body file: %v", err)
							}
							capturedBody = string(data)
						}
						if argv[i] == "--line" {
							_, file, _ := strings.Cut(argv[i+1], "=")
							data, err := os.ReadFile(file)
							if err != nil {
								t.Fatalf("read line file: %v", err)
							}
							capturedLineText = string(data)
						}
					}
					return []byte("55\n"), nil, nil
				},
			}
			handler.SetOutboxClient(client)
			var outBuf, errBuf bytes.Buffer
			handler.SetOutputWriters(&outBuf, &errBuf)

			opts := SubmitOptions{
				ViaOutbox:   true,
				OutboxOwner: "agent-a",
			}
			err := handler.SubmitCommandWithOptions(repository, prNumber, "Overall review notes", tc.event, "", models.NewTextFormatter(), models.KeepAction{}, opts)
			if err != nil {
				t.Fatalf("SubmitCommandWithOptions() error = %v", err)
			}

			assertFlagPair(t, capturedArgv, "--kind", tc.wantKind)
			assertFlagPair(t, capturedArgv, "--repo", "acme/widgets")
			assertFlagPair(t, capturedArgv, "--pr", "99")
			assertFlagPair(t, capturedArgv, "--owner", "agent-a")
			if capturedBody != "Overall review notes" {
				t.Errorf("body file = %q, want %q", capturedBody, "Overall review notes")
			}
			if capturedLineText != "Check nil pointer" {
				t.Errorf("line file = %q, want %q", capturedLineText, "Check nil pointer")
			}
			if got := strings.TrimSpace(outBuf.String()); got != "55" {
				t.Errorf("stdout = %q, want %q", got, "55")
			}
		})
	}
}

func TestSubmitViaOutbox_FileFilter(t *testing.T) {
	handler, _, cleanup := setupTestHandler(t)
	defer cleanup()

	repository := models.NewRepository("acme", "widgets")
	prNumber := 7
	target := models.NewPRTarget(prNumber)

	createTestDiffHunks(t, handler, repository, prNumber, []models.DiffHunk{
		{Location: models.NewFileLocation("a.go", models.NewLineRange(1, 50)), Side: models.SideRight},
		{Location: models.NewFileLocation("b.go", models.NewLineRange(1, 50)), Side: models.SideRight},
	})
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "a.go",
		Line: models.NewSingleLine(5),
		Body: "comment on a",
		Side: models.SideRight,
	})
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "b.go",
		Line: models.NewSingleLine(8),
		Body: "comment on b",
		Side: models.SideRight,
	})

	var lineSpecs []string
	handler.SetOutboxClient(&CLIOutboxClient{
		Command: []string{"gh-outbox"},
		Run: func(_ context.Context, argv []string) ([]byte, []byte, error) {
			for i := 0; i < len(argv)-1; i++ {
				if argv[i] == "--line" {
					lineSpecs = append(lineSpecs, argv[i+1])
				}
			}
			return []byte("21\n"), nil, nil
		},
	})
	var outBuf, errBuf bytes.Buffer
	handler.SetOutputWriters(&outBuf, &errBuf)

	opts := SubmitOptions{
		ViaOutbox:   true,
		OutboxOwner: "agent-a",
	}
	if err := handler.SubmitCommandWithOptions(repository, prNumber, "", "", "a.go", models.NewTextFormatter(), models.ClearAction{}, opts); err != nil {
		t.Fatalf("SubmitCommandWithOptions() error = %v", err)
	}
	if len(lineSpecs) != 1 || !strings.HasPrefix(lineSpecs[0], "a.go:5=") {
		t.Fatalf("line specs = %v, want single a.go:5=...", lineSpecs)
	}

	stored, err := handler.storage.GetComments(repository, target)
	if err != nil {
		t.Fatalf("GetComments() error = %v", err)
	}
	if len(stored.Comments) != 2 {
		t.Fatalf("stored comments = %d, want 2", len(stored.Comments))
	}
	for _, c := range stored.Comments {
		switch c.Path {
		case "a.go":
			if c.OutboxDraftID != "21" {
				t.Errorf("a.go OutboxDraftID = %q, want %q", c.OutboxDraftID, "21")
			}
		case "b.go":
			if c.OutboxDraftID != "" {
				t.Errorf("b.go OutboxDraftID = %q, want empty", c.OutboxDraftID)
			}
		default:
			t.Errorf("unexpected file %q", c.Path)
		}
	}
}

func TestSubmitViaOutbox_LintRefusalLeavesCommentsUnchanged(t *testing.T) {
	handler, _, cleanup := setupTestHandler(t)
	defer cleanup()

	repository := models.NewRepository("acme", "widgets")
	prNumber := 12
	target := models.NewPRTarget(prNumber)

	createTestDiffHunks(t, handler, repository, prNumber, []models.DiffHunk{
		{Location: models.NewFileLocation("main.go", models.NewLineRange(1, 50)), Side: models.SideRight},
	})
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "main.go",
		Line: models.NewSingleLine(10),
		Body: "per the user, change this",
		Side: models.SideRight,
	})

	refusal := "gh-outbox: draft refused: it contains internal wording\n  main.go:10: \"per the user\" (internal phrase)\n"
	fakeOutbox := writeFakeOutboxScript(t, fmt.Sprintf(`#!/usr/bin/env sh
cat >&2 <<'EOF'
%sEOF
exit 1
`, refusal))

	handler.SetOutboxClient(NewCLIOutboxClient([]string{fakeOutbox}))
	var outBuf, errBuf bytes.Buffer
	handler.SetOutputWriters(&outBuf, &errBuf)

	opts := SubmitOptions{
		ViaOutbox:   true,
		OutboxOwner: "agent-a",
	}
	err := handler.SubmitCommandWithOptions(repository, prNumber, "", "", "", models.NewTextFormatter(), models.ClearAction{}, opts)
	if err == nil {
		t.Fatal("expected lint refusal error, got nil")
	}
	var outboxErr *OutboxError
	if !errors.As(err, &outboxErr) {
		t.Fatalf("expected *OutboxError, got %T (%v)", err, err)
	}
	if outboxErr.Stderr != refusal {
		t.Errorf("stderr = %q, want %q", outboxErr.Stderr, refusal)
	}
	if outBuf.Len() != 0 {
		t.Errorf("unexpected stdout on lint refusal: %q", outBuf.String())
	}

	stored, err := handler.storage.GetComments(repository, target)
	if err != nil {
		t.Fatalf("GetComments() error = %v", err)
	}
	if len(stored.Comments) != 1 {
		t.Fatalf("stored comments = %d, want 1", len(stored.Comments))
	}
	if stored.Comments[0].OutboxDraftID != "" || stored.Comments[0].OutboxAfter != "" {
		t.Errorf("comment should not be marked on lint refusal: %+v", stored.Comments[0])
	}
}

func TestSubmitViaOutbox_ReconcileAppliesAfterWhenPosted(t *testing.T) {
	tests := []struct {
		name          string
		afterAction   models.Executor
		wantRemaining int
		wantArchived  bool
	}{
		{name: "clear removes posted comments", afterAction: models.ClearAction{}, wantRemaining: 0},
		{name: "keep preserves and unmarks comments", afterAction: models.KeepAction{}, wantRemaining: 1},
		{name: "archive moves posted comments to archive", afterAction: models.ArchiveAction{}, wantRemaining: 0, wantArchived: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			handler, _, cleanup := setupTestHandler(t)
			defer cleanup()

			repository := models.NewRepository("acme", "widgets")
			prNumber := 88
			target := models.NewPRTarget(prNumber)

			createTestDiffHunks(t, handler, repository, prNumber, []models.DiffHunk{
				{Location: models.NewFileLocation("main.go", models.NewLineRange(1, 50)), Side: models.SideRight},
			})
			createTestComment(t, handler, repository, prNumber, models.Comment{
				Path: "main.go",
				Line: models.NewSingleLine(10),
				Body: "fix error handling",
				Side: models.SideRight,
			})

			state := "pending"
			handler.SetOutboxClient(&CLIOutboxClient{
				Command: []string{"gh-outbox"},
				Run: func(_ context.Context, argv []string) ([]byte, []byte, error) {
					switch argv[1] {
					case "submit":
						return []byte("31\n"), nil, nil
					case "show":
						return fmt.Appendf(nil, `{"id":31,"state":%q}`, state), nil, nil
					default:
						return nil, nil, fmt.Errorf("unexpected subcommand: %v", argv)
					}
				},
			})
			var outBuf, errBuf bytes.Buffer
			handler.SetOutputWriters(&outBuf, &errBuf)

			opts := SubmitOptions{ViaOutbox: true, OutboxOwner: "agent-a"}
			if err := handler.SubmitCommandWithOptions(repository, prNumber, "", "", "", models.NewTextFormatter(), tc.afterAction, opts); err != nil {
				t.Fatalf("submit error = %v", err)
			}

			// While pending, reconcile keeps the marked comment in place.
			if err := handler.ReconcileOutbox(context.Background(), repository, target); err != nil {
				t.Fatalf("reconcile pending error = %v", err)
			}
			stored, err := handler.storage.GetComments(repository, target)
			if err != nil {
				t.Fatalf("GetComments() error = %v", err)
			}
			if len(stored.Comments) != 1 || stored.Comments[0].OutboxDraftID != "31" {
				t.Fatalf("pending comment changed prematurely: %+v", stored.Comments)
			}

			// Once posted, reconcile applies --after.
			state = "posted"
			if err := handler.ReconcileOutbox(context.Background(), repository, target); err != nil {
				t.Fatalf("reconcile posted error = %v", err)
			}
			stored, err = handler.storage.GetComments(repository, target)
			if err != nil {
				t.Fatalf("GetComments() after posted error = %v", err)
			}
			if len(stored.Comments) != tc.wantRemaining {
				t.Fatalf("remaining comments = %d, want %d", len(stored.Comments), tc.wantRemaining)
			}
			if tc.wantRemaining > 0 && stored.Comments[0].OutboxDraftID != "" {
				t.Errorf("kept comment still marked: %+v", stored.Comments[0])
			}
			if tc.wantArchived {
				archives, err := handler.storage.ListArchivedSubmissions(repository, target)
				if err != nil {
					t.Fatalf("ListArchivedSubmissions() error = %v", err)
				}
				if archives.TotalArchives != 1 {
					t.Errorf("TotalArchives = %d, want 1", archives.TotalArchives)
				}
			}
		})
	}
}

func TestSubmitDefaultBehaviorUnchangedWithoutFlag(t *testing.T) {
	handler, _, cleanup := setupTestHandler(t)
	defer cleanup()

	repository := models.NewRepository("acme", "widgets")
	prNumber := 3
	target := models.NewPRTarget(prNumber)

	createTestDiffHunks(t, handler, repository, prNumber, []models.DiffHunk{
		{Location: models.NewFileLocation("main.go", models.NewLineRange(1, 50)), Side: models.SideRight},
	})
	createTestComment(t, handler, repository, prNumber, models.Comment{
		Path: "main.go",
		Line: models.NewSingleLine(10),
		Body: "direct review comment",
		Side: models.SideRight,
	})

	var submittedReview models.PRReview
	ghCalled := false
	handler.ghClient = &MockGitHubClient{
		SubmitReviewFunc: func(_ models.Repository, _ int, review models.PRReview) error {
			ghCalled = true
			submittedReview = review
			return nil
		},
	}

	outboxCalled := false
	handler.SetOutboxClient(&CLIOutboxClient{
		Command: []string{"gh-outbox"},
		Run: func(_ context.Context, _ []string) ([]byte, []byte, error) {
			outboxCalled = true
			return nil, nil, nil
		},
	})

	if err := handler.SubmitCommand(repository, prNumber, "LGTM", "COMMENT", "", &MockOutputFormatter{}, models.ClearAction{}); err != nil {
		t.Fatalf("SubmitCommand() error = %v", err)
	}
	if !ghCalled {
		t.Fatal("expected ghClient.SubmitReview to be called")
	}
	if outboxCalled {
		t.Fatal("did not expect outboxClient to be called when ViaOutbox is false")
	}
	if submittedReview.Body != "LGTM" || submittedReview.Event != "COMMENT" || len(submittedReview.Comments) != 1 {
		t.Errorf("unexpected submitted review: %+v", submittedReview)
	}

	stored, err := handler.storage.GetComments(repository, target)
	if err != nil {
		t.Fatalf("GetComments() error = %v", err)
	}
	if len(stored.Comments) != 0 {
		t.Errorf("expected local comments to be cleared immediately, got %d", len(stored.Comments))
	}
}

func writeFakeOutboxScript(t *testing.T, script string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "fake-gh-outbox")
	if err := os.WriteFile(path, []byte(script), 0o700); err != nil {
		t.Fatalf("write fake gh-outbox script: %v", err)
	}
	return path
}

func assertFlagPair(t *testing.T, argv []string, flag, want string) {
	t.Helper()
	for i := 0; i < len(argv)-1; i++ {
		if argv[i] == flag {
			if argv[i+1] != want {
				t.Errorf("flag %s = %q, want %q (argv=%v)", flag, argv[i+1], want, argv)
			}
			return
		}
	}
	t.Errorf("flag %s not found in argv %v", flag, argv)
}
