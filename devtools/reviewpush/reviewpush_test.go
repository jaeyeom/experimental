package reviewpush

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
	"testing"

	executor "github.com/jaeyeom/go-cmdexec"
)

func strPtr(s string) *string { return &s }

// mockCodexRunner returns canned CodexResult values based on mode.
type mockCodexRunner struct {
	mode       string
	calls      int
	aheadState *int
	lastInput  *CodexPassInput
}

func (m *mockCodexRunner) RunPass(_ context.Context, input *CodexPassInput) (*CodexResult, error) {
	m.calls++
	m.lastInput = input
	switch m.mode {
	case "loop":
		pt := input.TargetCommit
		return &CodexResult{
			Status:     "PUSH",
			Summary:    "push oldest",
			PushTarget: &pt,
		}, nil
	case "push-all":
		if *m.aheadState >= 2 {
			return &CodexResult{
				Status:       "PUSH",
				Summary:      "push two commits",
				MultiCommits: true,
				PushTarget:   strPtr("commit-2"),
			}, nil
		}
		pt := input.TargetCommit
		return &CodexResult{
			Status:     "PUSH",
			Summary:    "push oldest",
			PushTarget: &pt,
		}, nil
	case "blocked":
		return &CodexResult{
			Status:        "BLOCKED",
			Summary:       "blocked",
			BlockedReason: strPtr("hard blocker"),
		}, nil
	case "no-progress":
		return &CodexResult{
			Status:       "PUSH",
			Summary:      "bad target",
			MultiCommits: true,
			PushTarget:   strPtr("not-a-commit"),
		}, nil
	case "multi-commits-false-ignores-push-target":
		return &CodexResult{
			Status:     "PUSH",
			Summary:    "cross-check",
			PushTarget: strPtr("commit-2"),
		}, nil
	case "timeout":
		return nil, fmt.Errorf("Codex execution timed out")
	default:
		return nil, fmt.Errorf("unknown mode: %s", m.mode)
	}
}

// testGitState tracks the mock git repository state.
type testGitState struct {
	aheadCount int
	pushLog    []string
	fetchCount int
	// remoteBaseExists controls whether show-ref and symbolic-ref report an
	// existing remote base ref. When false, the remote base is treated as
	// unborn and a push to refs/heads/<base> flips this to true to simulate
	// the branch becoming real after the first push.
	remoteBaseExists bool
	// currentBranch is returned by `rev-parse --abbrev-ref HEAD` when the
	// runner falls back to the current branch for an unborn remote.
	currentBranch string
}

// testGitExecutor implements executor.Executor for testing.
type testGitExecutor struct {
	state *testGitState
}

func (e *testGitExecutor) IsAvailable(_ string) bool { return true }

func (e *testGitExecutor) Execute(_ context.Context, cfg executor.ToolConfig) (*executor.ExecutionResult, error) {
	args := cfg.Args
	if len(args) == 0 {
		return nil, fmt.Errorf("no git subcommand")
	}

	switch args[0] {
	case "fetch":
		e.state.fetchCount++
		return &executor.ExecutionResult{ExitCode: 0}, nil

	case "push":
		if len(args) < 3 {
			return nil, fmt.Errorf("push: missing args")
		}
		refspec := args[2]
		commit := strings.Split(refspec, ":")[0]
		e.state.pushLog = append(e.state.pushLog, refspec)
		switch commit {
		case "commit-1":
			e.state.aheadCount--
		case "commit-2":
			e.state.aheadCount -= 2
		case "commit-3":
			e.state.aheadCount -= 3
		default:
			return nil, fmt.Errorf("unexpected push target: %s", commit)
		}
		// A push to an unborn remote ref creates it, so subsequent
		// fetches will observe the ref as existing.
		e.state.remoteBaseExists = true
		return &executor.ExecutionResult{ExitCode: 0}, nil

	case "rev-parse":
		if len(args) >= 4 {
			candidate := strings.TrimSuffix(args[3], "^{commit}")
			switch candidate {
			case "commit-1", "commit-2", "commit-3":
				return &executor.ExecutionResult{Output: candidate, ExitCode: 0}, nil
			}
		}
		if len(args) >= 3 && args[1] == "--abbrev-ref" && args[2] == "HEAD" {
			if e.state.currentBranch != "" {
				return &executor.ExecutionResult{Output: e.state.currentBranch, ExitCode: 0}, nil
			}
			return nil, fmt.Errorf("no current branch")
		}
		return nil, fmt.Errorf("rev-parse failed")

	case "merge-base":
		if len(args) < 4 || args[1] != "--is-ancestor" {
			return nil, fmt.Errorf("unexpected merge-base args: %v", args)
		}
		left, right := args[2], args[3]
		if right == "HEAD" {
			switch left {
			case "commit-1", "commit-2", "commit-3":
				return &executor.ExecutionResult{ExitCode: 0}, nil
			}
			return nil, fmt.Errorf("not ancestor of HEAD")
		}
		valid := map[string]bool{
			"origin/main:commit-1": true, "origin/main:commit-2": true, "origin/main:commit-3": true,
			"origin/develop:commit-1": true, "origin/develop:commit-2": true, "origin/develop:commit-3": true,
			"commit-1:commit-1": true, "commit-1:commit-2": true, "commit-1:commit-3": true,
			"commit-2:commit-2": true, "commit-2:commit-3": true,
			"commit-3:commit-3": true,
		}
		if valid[left+":"+right] {
			return &executor.ExecutionResult{ExitCode: 0}, nil
		}
		return nil, fmt.Errorf("not ancestor")

	case "symbolic-ref":
		if !e.state.remoteBaseExists {
			return nil, fmt.Errorf("ref refs/remotes/origin/HEAD is not a symbolic ref")
		}
		return &executor.ExecutionResult{Output: "origin/main", ExitCode: 0}, nil

	case "show-ref":
		if !e.state.remoteBaseExists {
			return nil, fmt.Errorf("show-ref failed")
		}
		return &executor.ExecutionResult{ExitCode: 0}, nil

	case "rev-list":
		if len(args) >= 2 && args[1] == "--count" {
			return &executor.ExecutionResult{Output: strconv.Itoa(e.state.aheadCount), ExitCode: 0}, nil
		}
		var commits []string
		switch {
		case e.state.aheadCount >= 3:
			commits = []string{"commit-3", "commit-2", "commit-1"}
		case e.state.aheadCount == 2:
			commits = []string{"commit-2", "commit-1"}
		case e.state.aheadCount == 1:
			commits = []string{"commit-1"}
		}
		return &executor.ExecutionResult{Output: strings.Join(commits, "\n"), ExitCode: 0}, nil

	case "log":
		return &executor.ExecutionResult{
			Output:   "commit abc123\nAuthor: Test User\n\n    Test commit\n\n file.go | 1 +\n",
			ExitCode: 0,
		}, nil

	default:
		return nil, fmt.Errorf("unexpected git command: %s", args[0])
	}
}

func newTestRunner(t *testing.T, state *testGitState, codexMode string) (*Runner, *mockCodexRunner, *bytes.Buffer) {
	t.Helper()
	gitExec := &testGitExecutor{state: state}
	codex := &mockCodexRunner{mode: codexMode, aheadState: &state.aheadCount}
	var buf bytes.Buffer
	runner := &Runner{
		Cfg: Config{
			RepoDir:       "/tmp/test-repo",
			MaxIterations: 100,
			GitBin:        "git",
			CodexBin:      "codex",
		},
		Exec:   gitExec,
		Codex:  codex,
		Stdout: &buf,
		Stderr: &buf,
	}
	return runner, codex, &buf
}

func TestLoopMode(t *testing.T) {
	state := &testGitState{aheadCount: 2, remoteBaseExists: true}
	runner, codex, buf := newTestRunner(t, state, "loop")

	exitCode := runner.Run(context.Background())

	if exitCode != 0 {
		t.Fatalf("expected exit code 0, got %d: %s", exitCode, buf.String())
	}
	if !strings.Contains(buf.String(), "DONE: HEAD is not ahead of origin/main.") {
		t.Errorf("expected DONE message, got: %s", buf.String())
	}
	if state.aheadCount != 0 {
		t.Errorf("expected ahead count 0, got %d", state.aheadCount)
	}
	if codex.calls != 2 {
		t.Errorf("expected 2 codex calls, got %d", codex.calls)
	}
}

func TestBlockedMode(t *testing.T) {
	state := &testGitState{aheadCount: 1, remoteBaseExists: true}
	runner, codex, buf := newTestRunner(t, state, "blocked")

	exitCode := runner.Run(context.Background())

	if exitCode != 2 {
		t.Fatalf("expected exit code 2, got %d: %s", exitCode, buf.String())
	}
	if !strings.Contains(buf.String(), "BLOCKED: review-and-push stopped on commit-1.") {
		t.Errorf("expected BLOCKED message, got: %s", buf.String())
	}
	if !strings.Contains(buf.String(), "Reason: hard blocker") {
		t.Errorf("expected blocked reason in output, got: %s", buf.String())
	}
	if codex.calls != 1 {
		t.Errorf("expected 1 codex call, got %d", codex.calls)
	}
	if len(state.pushLog) != 0 {
		t.Errorf("expected no pushes, got %v", state.pushLog)
	}
}

func TestNoProgressMode(t *testing.T) {
	state := &testGitState{aheadCount: 1, remoteBaseExists: true}
	runner, _, buf := newTestRunner(t, state, "no-progress")

	exitCode := runner.Run(context.Background())

	if exitCode != 2 {
		t.Fatalf("expected exit code 2, got %d: %s", exitCode, buf.String())
	}
	if !strings.Contains(buf.String(), "proposed invalid push target") {
		t.Errorf("expected invalid push target message, got: %s", buf.String())
	}
}

func TestPushAllMode(t *testing.T) {
	state := &testGitState{aheadCount: 2, remoteBaseExists: true}
	runner, codex, buf := newTestRunner(t, state, "push-all")

	exitCode := runner.Run(context.Background())

	if exitCode != 0 {
		t.Fatalf("expected exit code 0, got %d: %s", exitCode, buf.String())
	}
	if !strings.Contains(buf.String(), "DONE: HEAD is not ahead of origin/main.") {
		t.Errorf("expected DONE message, got: %s", buf.String())
	}
	if !strings.Contains(buf.String(), "Pushed through commit-2.") {
		t.Errorf("expected push through commit-2, got: %s", buf.String())
	}
	if codex.calls != 1 {
		t.Errorf("expected 1 codex call, got %d", codex.calls)
	}
	if len(state.pushLog) != 1 || !strings.Contains(state.pushLog[0], "commit-2:refs/heads/main") {
		t.Errorf("expected push of commit-2, got %v", state.pushLog)
	}
}

func TestMultiCommitsCrossCheck(t *testing.T) {
	state := &testGitState{aheadCount: 2, remoteBaseExists: true}
	runner, _, buf := newTestRunner(t, state, "multi-commits-false-ignores-push-target")

	exitCode := runner.Run(context.Background())

	if exitCode != 0 {
		t.Fatalf("expected exit code 0, got %d: %s", exitCode, buf.String())
	}
	// Should push commit-1 (target), not commit-2 (push_target), because multi_commits=false
	if len(state.pushLog) < 1 || !strings.Contains(state.pushLog[0], "commit-1:refs/heads/main") {
		t.Errorf("expected push of commit-1 (ignoring push_target), got %v", state.pushLog)
	}
}

func TestTimeoutMode(t *testing.T) {
	state := &testGitState{aheadCount: 1, remoteBaseExists: true}
	runner, _, buf := newTestRunner(t, state, "timeout")

	exitCode := runner.Run(context.Background())

	if exitCode != 2 {
		t.Fatalf("expected exit code 2, got %d: %s", exitCode, buf.String())
	}
	if !strings.Contains(buf.String(), "BLOCKED: Codex execution failed during iteration 1.") {
		t.Errorf("expected blocked message, got: %s", buf.String())
	}
}

func TestCommitBanner(t *testing.T) {
	state := &testGitState{aheadCount: 1, remoteBaseExists: true}
	runner, _, buf := newTestRunner(t, state, "loop")

	runner.Run(context.Background())

	output := buf.String()
	if !strings.Contains(output, "========================================") {
		t.Errorf("expected banner separator, got: %s", output)
	}
	if !strings.Contains(output, "TARGET COMMIT") {
		t.Errorf("expected TARGET COMMIT header, got: %s", output)
	}
}

func TestMaxIterations(t *testing.T) {
	state := &testGitState{aheadCount: 3, remoteBaseExists: true}
	runner, _, buf := newTestRunner(t, state, "loop")
	runner.Cfg.MaxIterations = 1

	exitCode := runner.Run(context.Background())

	if exitCode != 2 {
		t.Fatalf("expected exit code 2, got %d: %s", exitCode, buf.String())
	}
	if !strings.Contains(buf.String(), "reached max iterations (1)") {
		t.Errorf("expected max iterations message, got: %s", buf.String())
	}
}

func TestBaseBranchOverride(t *testing.T) {
	state := &testGitState{aheadCount: 0, remoteBaseExists: true}
	runner, _, buf := newTestRunner(t, state, "loop")
	runner.Cfg.BaseBranchOverride = "develop"

	exitCode := runner.Run(context.Background())

	if exitCode != 0 {
		t.Fatalf("expected exit code 0, got %d: %s", exitCode, buf.String())
	}
	if !strings.Contains(buf.String(), "DONE: HEAD is not ahead of origin/develop.") {
		t.Errorf("expected develop branch ref, got: %s", buf.String())
	}
}

// TestUnbornRemoteWithBaseOverride exercises the first-push path: an empty
// remote has no origin/main ref, the user passes --base main, and the loop
// reviews and pushes commits until the remote base becomes established.
func TestUnbornRemoteWithBaseOverride(t *testing.T) {
	state := &testGitState{aheadCount: 2, remoteBaseExists: false}
	runner, codex, buf := newTestRunner(t, state, "loop")
	runner.Cfg.BaseBranchOverride = "main"

	// Capture the remote state observed by Codex on each call. The first
	// call happens before any push, when the remote base is still unborn;
	// later calls see the remote ref as existing after the bootstrap push.
	var seenRemoteStates []bool
	runner.Codex = &capturingCodex{inner: codex, seen: &seenRemoteStates}

	exitCode := runner.Run(context.Background())

	if exitCode != 0 {
		t.Fatalf("expected exit code 0, got %d: %s", exitCode, buf.String())
	}
	if !strings.Contains(buf.String(), "DONE: HEAD is not ahead of origin/main.") {
		t.Errorf("expected DONE message, got: %s", buf.String())
	}
	if len(state.pushLog) < 1 {
		t.Fatalf("expected at least one push, got %v", state.pushLog)
	}
	// The very first push must write to refs/heads/main to bootstrap the
	// remote branch.
	if !strings.Contains(state.pushLog[0], ":refs/heads/main") {
		t.Errorf("expected bootstrap push to refs/heads/main, got %v", state.pushLog)
	}
	if state.aheadCount != 0 {
		t.Errorf("expected ahead count 0 after loop, got %d", state.aheadCount)
	}
	if len(seenRemoteStates) < 2 {
		t.Fatalf("expected at least 2 codex calls, got %v", seenRemoteStates)
	}
	if seenRemoteStates[0] != false {
		t.Errorf("expected first Codex call to see RemoteRefExists=false, got %v", seenRemoteStates[0])
	}
	if seenRemoteStates[len(seenRemoteStates)-1] != true {
		t.Errorf("expected final Codex call to see RemoteRefExists=true after bootstrap push, got %v", seenRemoteStates[len(seenRemoteStates)-1])
	}
}

// capturingCodex wraps a CodexRunner and records RemoteRefExists values seen
// on each pass, so tests can assert how the remote-base state evolves across
// iterations.
type capturingCodex struct {
	inner CodexRunner
	seen  *[]bool
}

func (c *capturingCodex) RunPass(ctx context.Context, input *CodexPassInput) (*CodexResult, error) {
	*c.seen = append(*c.seen, input.RemoteRefExists)
	result, err := c.inner.RunPass(ctx, input)
	if err != nil {
		return nil, fmt.Errorf("inner codex pass: %w", err)
	}
	return result, nil
}

// TestUnbornRemoteFallsBackToCurrentBranch verifies that when neither
// origin/HEAD nor any well-known branch ref exists and --base is not set,
// ResolveRemoteBase falls back to the current local branch name with an
// unborn remote, allowing the first push to bootstrap the remote branch.
func TestUnbornRemoteFallsBackToCurrentBranch(t *testing.T) {
	state := &testGitState{
		aheadCount:       1,
		remoteBaseExists: false,
		currentBranch:    "main",
	}
	runner, _, buf := newTestRunner(t, state, "loop")

	exitCode := runner.Run(context.Background())

	if exitCode != 0 {
		t.Fatalf("expected exit code 0, got %d: %s", exitCode, buf.String())
	}
	if !strings.Contains(buf.String(), "origin/main") {
		t.Errorf("expected loop to use origin/main as remote ref, got: %s", buf.String())
	}
	if len(state.pushLog) != 1 || !strings.Contains(state.pushLog[0], "commit-1:refs/heads/main") {
		t.Errorf("expected a single push bootstrapping main, got %v", state.pushLog)
	}
}

// TestUnbornRemoteNoCurrentBranchFails verifies that when the remote is
// empty, no --base override is set, and HEAD is detached, ResolveRemoteBase
// returns a clear configuration error instead of silently proceeding.
func TestUnbornRemoteNoCurrentBranchFails(t *testing.T) {
	state := &testGitState{
		aheadCount:       1,
		remoteBaseExists: false,
		currentBranch:    "",
	}
	runner, _, buf := newTestRunner(t, state, "loop")

	exitCode := runner.Run(context.Background())

	if exitCode != 1 {
		t.Fatalf("expected exit code 1 (config error), got %d: %s", exitCode, buf.String())
	}
	if !strings.Contains(buf.String(), "could not determine remote base branch") {
		t.Errorf("expected config error message, got: %s", buf.String())
	}
}

// TestUnbornRemoteValidatesPushTarget ensures the push target validation
// still rejects invalid commits when the remote base is unborn, even though
// the remote-ancestor check is skipped.
func TestUnbornRemoteValidatesPushTarget(t *testing.T) {
	state := &testGitState{aheadCount: 1, remoteBaseExists: false}
	runner, _, buf := newTestRunner(t, state, "no-progress")
	runner.Cfg.BaseBranchOverride = "main"

	exitCode := runner.Run(context.Background())

	if exitCode != 2 {
		t.Fatalf("expected exit code 2, got %d: %s", exitCode, buf.String())
	}
	if !strings.Contains(buf.String(), "proposed invalid push target") {
		t.Errorf("expected invalid push target message, got: %s", buf.String())
	}
	if len(state.pushLog) != 0 {
		t.Errorf("expected no pushes for invalid target, got %v", state.pushLog)
	}
}

func TestBuildPromptMarksUnbornRemote(t *testing.T) {
	existing := BuildPrompt(&CodexPassInput{
		RepoDir: "/repo", BaseBranch: "main", RemoteRef: "origin/main",
		RemoteRefExists: true, Iteration: 1, AheadCount: 1, TargetCommit: "abc",
	})
	if strings.Contains(existing, "unborn") {
		t.Errorf("expected no unborn marker for existing remote, got: %s", existing)
	}

	unborn := BuildPrompt(&CodexPassInput{
		RepoDir: "/repo", BaseBranch: "main", RemoteRef: "origin/main",
		RemoteRefExists: false, Iteration: 1, AheadCount: 1, TargetCommit: "abc",
	})
	if !strings.Contains(unborn, "unborn") {
		t.Errorf("expected unborn marker in prompt for missing remote, got: %s", unborn)
	}
}

func TestCodexResultJSONParsing(t *testing.T) {
	tests := []struct {
		name     string
		json     string
		expected CodexResult
	}{
		{
			name: "push with target",
			json: `{"status":"PUSH","summary":"ok","multi_commits":false,"push_target":"abc123","blocked_reason":null}`,
			expected: CodexResult{
				Status:     "PUSH",
				Summary:    "ok",
				PushTarget: strPtr("abc123"),
			},
		},
		{
			name: "blocked",
			json: `{"status":"BLOCKED","summary":"issue","multi_commits":false,"push_target":null,"blocked_reason":"security bug"}`,
			expected: CodexResult{
				Status:        "BLOCKED",
				Summary:       "issue",
				BlockedReason: strPtr("security bug"),
			},
		},
		{
			name: "multi commits true",
			json: `{"status":"PUSH","summary":"batch","multi_commits":true,"push_target":"def456","blocked_reason":null}`,
			expected: CodexResult{
				Status:       "PUSH",
				Summary:      "batch",
				MultiCommits: true,
				PushTarget:   strPtr("def456"),
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var result CodexResult
			if err := json.Unmarshal([]byte(tt.json), &result); err != nil {
				t.Fatalf("failed to unmarshal: %v", err)
			}
			if result.Status != tt.expected.Status {
				t.Errorf("status: got %q, want %q", result.Status, tt.expected.Status)
			}
			if result.MultiCommits != tt.expected.MultiCommits {
				t.Errorf("multi_commits: got %v, want %v", result.MultiCommits, tt.expected.MultiCommits)
			}
			if (result.PushTarget == nil) != (tt.expected.PushTarget == nil) {
				t.Errorf("push_target nil mismatch: got %v, want %v", result.PushTarget, tt.expected.PushTarget)
			} else if result.PushTarget != nil && *result.PushTarget != *tt.expected.PushTarget {
				t.Errorf("push_target: got %q, want %q", *result.PushTarget, *tt.expected.PushTarget)
			}
			if (result.BlockedReason == nil) != (tt.expected.BlockedReason == nil) {
				t.Errorf("blocked_reason nil mismatch: got %v, want %v", result.BlockedReason, tt.expected.BlockedReason)
			} else if result.BlockedReason != nil && *result.BlockedReason != *tt.expected.BlockedReason {
				t.Errorf("blocked_reason: got %q, want %q", *result.BlockedReason, *tt.expected.BlockedReason)
			}
		})
	}
}
