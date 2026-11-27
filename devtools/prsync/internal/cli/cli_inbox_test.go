package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/loop"
	executor "github.com/jaeyeom/go-cmdexec"
)

func emptyReviewThreadsFile(t *testing.T) string {
	t.Helper()
	p := filepath.Join(t.TempDir(), "empty-threads.json")
	body := `{"data":{"repository":{"pullRequest":{"reviewThreads":{"pageInfo":{"hasNextPage":false,"endCursor":null},"nodes":[]}}}}}`
	if err := os.WriteFile(p, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return p
}

func writeInboxFile(t *testing.T, dir, name, content string) string {
	t.Helper()
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	p := filepath.Join(dir, name)
	if err := os.WriteFile(p, []byte(content), 0o600); err != nil {
		t.Fatal(err)
	}
	return p
}

func TestDispatchFromInboxFlagValidation(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name string
		args []string
		want string
	}{
		{name: "requires ci-fix", args: []string{"dispatch", "--from-inbox"}, want: "--from-inbox requires --ci-fix"},
		{name: "forbids hint", args: []string{"dispatch", "--ci-fix", "--from-inbox", "--hint", "x"}, want: "cannot combine --from-inbox and --hint"},
		{name: "forbids job", args: []string{"dispatch", "--job", "j-1", "--from-inbox"}, want: "cannot combine --job and --from-inbox"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			var stdout, stderr bytes.Buffer
			code := Execute(context.Background(), tc.args, &stdout, &stderr, executor.NewBasicExecutor())
			if code != ExitUsage || !strings.Contains(stderr.String(), tc.want) {
				t.Fatalf("code=%d stderr=%q, want ExitUsage with %q", code, stderr.String(), tc.want)
			}
		})
	}
}

func TestDispatchCIFixFromInboxPolicyAndLifecycle(t *testing.T) {
	t.Setenv("HOME", t.TempDir())
	ghBin, herdrBin := fixtureBins(t)
	stateDir := t.TempDir()
	statePath := filepath.Join(stateDir, "state.json")
	customInbox := filepath.Join(t.TempDir(), "custom-inbox")

	doc := stdinEligibleDoc()
	doc.PRs[0].Unaddressed = false
	doc.PRs[0].BlockingComments = nil
	doc.PRs[0].State = "open"
	doc.PRs[0].Head = "fix-widget"
	doc.PRs[0].Base = "main"
	doc.PRs[0].HeadSHA = "abc123def456"
	doc.PRs[0].CIState = "failing"
	rawDoc := string(mustScanJSON(t, doc))

	promptLog := filepath.Join(t.TempDir(), "prompts.log")
	t.Setenv("HERDR_FAKE_PROMPT_LOG", promptLog)

	reqJSON := `{"repo":"acme/widgets","pr":123,"head_sha":"abc123def456","hint":"check nil widget","writer":"judge","time":"2026-11-27T01:00:00Z"}`
	reqPath := writeInboxFile(t, customInbox, "01-fix.json", reqJSON+"\n")

	// 1. policy_ci_fix=off gates it off even with --go.
	cfgOff := writeLiveConfig(t, ghBin, herdrBin, statePath, "policy_ci_fix=off", "ci_fix_inbox="+customInbox)
	restore := swapStdin(t, rawDoc)
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"dispatch", "--stdin", "--ci-fix", "--from-inbox", "--config", cfgOff, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("off exit=%d stderr=%q", code, stderr.String())
	}
	gotOff := decodeDispatch(t, stdout.Bytes())
	if len(gotOff.Results) != 1 || gotOff.Results[0].Action != dispatch.ActionSkippedPolicyOff {
		t.Fatalf("off results = %+v, want skipped_policy_off", gotOff.Results)
	}
	if _, err := os.Stat(reqPath); err != nil {
		t.Fatalf("off consumed inbox file: %v", err)
	}

	// 2. policy_ci_fix=dry-run previews without sending or moving.
	cfgDry := writeLiveConfig(t, ghBin, herdrBin, statePath, "policy_ci_fix=dry-run", "ci_fix_inbox="+customInbox)
	restore = swapStdin(t, rawDoc)
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"dispatch", "--stdin", "--ci-fix", "--from-inbox", "--config", cfgDry, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("dry-run exit=%d stderr=%q", code, stderr.String())
	}
	gotDry := decodeDispatch(t, stdout.Bytes())
	if !gotDry.DryRun || len(gotDry.Results) != 1 || gotDry.Results[0].Action != dispatch.ActionWouldDispatch {
		t.Fatalf("dry-run results = %+v", gotDry.Results)
	}
	if !strings.Contains(gotDry.Results[0].RenderedPrompt, "check nil widget") {
		t.Fatalf("dry-run prompt missing hint: %q", gotDry.Results[0].RenderedPrompt)
	}
	if _, err := os.Stat(reqPath); err != nil {
		t.Fatalf("dry-run moved inbox file: %v", err)
	}

	// 3. policy_ci_fix=auto + --go sends once, handles stale and malformed, and dedupes duplicates.
	writeInboxFile(t, customInbox, "00-bad.json", "{bad json")
	writeInboxFile(t, customInbox, "02-stale.json", `{"repo":"acme/widgets","pr":123,"head_sha":"oldsha","hint":"stale"}`+"\n")
	writeInboxFile(t, customInbox, "03-dup.json", reqJSON+"\n")
	cfgAuto := writeLiveConfig(t, ghBin, herdrBin, statePath, "policy_ci_fix=auto", "ci_fix_inbox="+customInbox)

	restore = swapStdin(t, rawDoc)
	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"dispatch", "--stdin", "--ci-fix", "--from-inbox", "--config", cfgAuto, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	restore()
	if code != ExitOK {
		t.Fatalf("auto exit=%d stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	gotAuto := decodeDispatch(t, stdout.Bytes())
	if len(gotAuto.Results) != 4 {
		t.Fatalf("auto results len=%d: %+v", len(gotAuto.Results), gotAuto.Results)
	}
	if gotAuto.Results[0].Action != dispatch.ActionBadRequest ||
		gotAuto.Results[1].Action != dispatch.ActionDispatched ||
		gotAuto.Results[2].Action != dispatch.ActionSkippedStale ||
		gotAuto.Results[3].Action != dispatch.ActionSkippedDeduped {
		t.Fatalf("auto results = %+v", gotAuto.Results)
	}
	if !strings.Contains(stderr.String(), "00-bad.json") {
		t.Fatalf("stderr missing malformed report: %q", stderr.String())
	}
	if _, err := os.Stat(filepath.Join(customInbox, "bad", "00-bad.json")); err != nil {
		t.Fatalf("missing bad/00-bad.json: %v", err)
	}
	if _, err := os.Stat(filepath.Join(customInbox, "done", "01-fix.json")); err != nil {
		t.Fatalf("missing done/01-fix.json: %v", err)
	}
	if _, err := os.Stat(filepath.Join(customInbox, "stale", "02-stale.json")); err != nil {
		t.Fatalf("missing stale/02-stale.json: %v", err)
	}
	if _, err := os.Stat(filepath.Join(customInbox, "done", "03-dup.json")); err != nil {
		t.Fatalf("missing done/03-dup.json: %v", err)
	}
	promptBytes, err := os.ReadFile(promptLog)
	if err != nil {
		t.Fatalf("read prompt log: %v", err)
	}
	if n := strings.Count(string(promptBytes), "agent prompt "); n != 1 {
		t.Fatalf("prompt count = %d, want 1: %q", n, string(promptBytes))
	}

	// 4. Targeted dispatch --ci-fix --from-inbox --go without --stdin fetches PR via gh-fake.sh.
	stateDir2 := t.TempDir()
	statePath2 := filepath.Join(stateDir2, "state.json")
	promptLog2 := filepath.Join(t.TempDir(), "prompts-targeted.log")
	t.Setenv("HERDR_FAKE_PROMPT_LOG", promptLog2)
	cfgTargeted := writeLiveConfig(t, ghBin, herdrBin, statePath2, "policy_ci_fix=auto")
	inboxDir2 := filepath.Join(stateDir2, "ci_fix_inbox")
	writeInboxFile(t, inboxDir2, "targeted.json", `{"repo":"acme/widgets","pr":123,"head_sha":"abc123def456","hint":"targeted inbox hint"}`+"\n")

	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"dispatch", "--ci-fix", "--from-inbox", "--config", cfgTargeted, "--go"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("targeted exit=%d stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	gotTargeted := decodeDispatch(t, stdout.Bytes())
	if len(gotTargeted.Results) != 1 || gotTargeted.Results[0].Action != dispatch.ActionDispatched {
		t.Fatalf("targeted results = %+v, want dispatched", gotTargeted.Results)
	}
	if !strings.Contains(gotTargeted.Results[0].RenderedPrompt, "targeted inbox hint") {
		t.Fatalf("targeted prompt missing hint: %q", gotTargeted.Results[0].RenderedPrompt)
	}
	if _, err := os.Stat(filepath.Join(inboxDir2, "done", "targeted.json")); err != nil {
		t.Fatalf("missing done/targeted.json: %v", err)
	}
}

func TestLoopCIFixInboxBusyTabWaitsForNextRound(t *testing.T) {
	t.Setenv("HOME", t.TempDir())
	ghBin, herdrBin := fixtureBins(t)
	t.Setenv("GH_FAKE_REVIEW_THREADS_FILE", emptyReviewThreadsFile(t))

	// Wrapper around herdr-fake.sh that reports working on the first agent list
	// (round 1 scan) and idle thereafter (round 2 scan + gate/settle).
	countFile := filepath.Join(t.TempDir(), "agent-list-count")
	wrapper := filepath.Join(t.TempDir(), "herdr-wrap.sh")
	script := "#!/bin/sh\nset -eu\nif [ \"${1-} ${2-}\" = \"agent list\" ]; then\n" +
		"  n=0\n  if [ -f " + countFile + " ]; then n=$(cat " + countFile + "); fi\n" +
		"  n=$((n + 1))\n  printf '%s\\n' \"$n\" >" + countFile + "\n" +
		"  if [ \"$n\" -eq 1 ]; then export HERDR_FAKE_AGENT_STATUS=working; else export HERDR_FAKE_AGENT_STATUS=idle; fi\n" +
		"fi\nexec " + herdrBin + " \"$@\"\n"
	if err := os.WriteFile(wrapper, []byte(script), 0o700); err != nil {
		t.Fatal(err)
	}

	promptLog := filepath.Join(t.TempDir(), "loop-prompts.log")
	t.Setenv("HERDR_FAKE_PROMPT_LOG", promptLog)

	stateDir := t.TempDir()
	statePath := filepath.Join(stateDir, "state.json")
	cfgPath := writeLiveConfig(t, ghBin, wrapper, statePath, "policy_ci_fix=auto")
	inboxDir := filepath.Join(stateDir, "ci_fix_inbox")
	reqPath := writeInboxFile(t, inboxDir, "fix-123.json", `{"repo":"acme/widgets","pr":123,"head_sha":"abc123def456","hint":"fix TestLoopCI"}`+"\n")

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"loop", "--config", cfgPath, "--max-rounds", "2"}, &stdout, &stderr, executor.NewBasicExecutor())
	if code != ExitOK {
		t.Fatalf("loop exit=%d stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	lines := strings.Split(strings.TrimSpace(stdout.String()), "\n")
	if len(lines) != 2 {
		t.Fatalf("rounds=%d, want 2:\n%s", len(lines), stdout.String())
	}
	var r1, r2 loop.RoundResult
	if err := json.Unmarshal([]byte(lines[0]), &r1); err != nil {
		t.Fatalf("decode round 1: %v", err)
	}
	if err := json.Unmarshal([]byte(lines[1]), &r2); err != nil {
		t.Fatalf("decode round 2: %v", err)
	}
	if len(r1.CIFixInbox) != 1 || r1.CIFixInbox[0].Action != dispatch.ActionSkippedBusy {
		t.Fatalf("round 1 inbox = %+v, want skipped_busy", r1.CIFixInbox)
	}
	if len(r2.CIFixInbox) != 1 || r2.CIFixInbox[0].Action != dispatch.ActionDispatched {
		t.Fatalf("round 2 inbox = %+v, want dispatched", r2.CIFixInbox)
	}
	if !strings.Contains(r2.CIFixInbox[0].RenderedPrompt, "fix TestLoopCI") {
		t.Fatalf("round 2 prompt missing hint: %q", r2.CIFixInbox[0].RenderedPrompt)
	}
	if _, err := os.Stat(reqPath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("inbox file still exists after round 2: err=%v", err)
	}
	if _, err := os.Stat(filepath.Join(inboxDir, "done", "fix-123.json")); err != nil {
		t.Fatalf("missing done/fix-123.json: %v", err)
	}
}
