package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func TestSkillPrintsFrontmatter(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"skill"}, &stdout, &stderr)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr.String())
	}
	if stderr.Len() != 0 {
		t.Fatalf("stderr = %q", stderr.String())
	}
	out := stdout.String()
	if !strings.HasPrefix(out, "---\nname: factory\n") {
		t.Fatalf("stdout does not start with frontmatter:\n%s", out)
	}
	for _, name := range []string{"next", "start", "--apply", "file"} {
		if !strings.Contains(out, name) {
			t.Fatalf("stdout missing %q:\n%s", name, out)
		}
	}
	if !strings.Contains(out, "merely because a task exists") {
		t.Fatalf("stdout does not say when not to use factory:\n%s", out)
	}
}

func TestSkillDoesNotLoadConfig(t *testing.T) {
	missing := filepath.Join(t.TempDir(), "missing.toml")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"skill", "--config", missing}, &stdout, &stderr)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr.String())
	}
	if !strings.HasPrefix(stdout.String(), "---\nname: factory\n") {
		t.Fatalf("stdout = %s", stdout.String())
	}
}

func TestSkillRejectsArguments(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"skill", "sync"}, &stdout, &stderr)
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitUsage, stderr.String())
	}
	if strings.Contains(stdout.String(), "name: factory") || strings.Contains(stderr.String(), "name: factory") {
		t.Fatalf("skill was printed\nstdout=%s\nstderr=%s", stdout.String(), stderr.String())
	}
}

func TestSkillFlagIsUsageError(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"--skill"}, &stdout, &stderr)
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr = %s", code, ExitUsage, stderr.String())
	}
	if strings.Contains(stdout.String(), "name: factory") || strings.Contains(stderr.String(), "name: factory") {
		t.Fatalf("skill was printed\nstdout=%s\nstderr=%s", stdout.String(), stderr.String())
	}
}

func TestSkillPinsGateRefusalExitCodes(t *testing.T) {
	startWant := skillDocumentedExit(t, "`factory start` exits ([0-9]+) when the gate refuses\\.")
	gateWant := skillDocumentedExit(t, "`factory gate` exits ([0-9]+) even when it prints reasons\\.")
	if startWant == gateWant {
		t.Fatalf("skill documents one exit code %d for both start and gate", startWant)
	}

	cfgPath, cwd := writeStartConfig(t, "", "")
	dbPath := filepath.Join(t.TempDir(), "factory.db")
	seedStartItem(t, dbPath, "Fix it", true)
	src := startFake()
	useSources(t, map[string]*fake.Source{"acme": src})
	tabs := tabsJSON(tabSpec{id: "w1:t9", label: "PRJ-9", status: "working"})
	panes := panesJSON(paneSpec{id: "w1:p9", tab: "w1:t9", status: "working", cwd: cwd})
	startExec := startMockFull(t, rankWorkspaces, tabs, panes, false, cwd, nil)
	stdout, stderr, code := runCmd(t, "start", "PRJ-1", "--config", cfgPath, "--db", dbPath)
	if code != startWant {
		t.Fatalf("start exit = %d, skill documents %d\nstdout:\n%s\nstderr:\n%s", code, startWant, stdout, stderr)
	}
	if !strings.Contains(stderr, "agent PRJ-9 (w1:t9) is working") {
		t.Fatalf("start did not refuse on a working agent\nstdout:\n%s\nstderr:\n%s", stdout, stderr)
	}
	if err := startExec.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
	assertNoClaimCall(t, src)

	gateCfg, gateCwds := writeGateConfig(t, "", gateSrc{name: "acme"})
	gateDB := filepath.Join(t.TempDir(), "factory.db")
	gatePanes := panesJSON(paneSpec{id: "w1:p9", tab: "w1:t9", status: "working", cwd: gateCwds["jira:acme"]})
	gateExec := gateMockPanes(t, tabs, gatePanes, "", "", nil)
	stdout, stderr, code = runCmd(t, "gate", "--config", gateCfg, "--db", gateDB)
	if code != gateWant {
		t.Fatalf("gate exit = %d, skill documents %d\nstdout:\n%s\nstderr:\n%s", code, gateWant, stdout, stderr)
	}
	if stderr != "" {
		t.Fatalf("stderr = %q", stderr)
	}
	if strings.HasPrefix(stdout, "OK") || !strings.Contains(stdout, "agent PRJ-9 (w1:t9) is working") {
		t.Fatalf("gate did not print a reason:\n%s", stdout)
	}
	if err := gateExec.AssertExpectationsMet(); err != nil {
		t.Fatal(err)
	}
}

func TestSkillDocumentsConfigTokensGateAndApproval(t *testing.T) {
	for _, phrase := range []string{
		"--config PATH",
		"A missing file is an error",
		"./factory.toml",
		"$XDG_CONFIG_HOME/factory/factory.toml",
		"~/.config/factory/factory.toml",
		"An empty `XDG_CONFIG_HOME` is an error",
		"factory.example.toml",
		"examples/",
		"exactly one of `token_file` or `token_env`",
		"not when config loads",
		"work against the mirror without the token",
		"names the source and the field",
		"The calling pane in `HERDR_PANE_ID` is skipped",
		"does not by itself block `start`",
		"Another working pane still refuses",
		"lists every counted working tab",
		"ends with `(this pane)`",
		"`gate.ignore_panes` (pane ids)",
		"`gate.ignore_tabs` (tab labels)",
		"`tick` and `run` also exit 0 on a gate refusal",
	} {
		if !strings.Contains(skillText, phrase) {
			t.Fatalf("skill missing %q", phrase)
		}
	}
	for _, banned := range []string{
		"including a gate refusal",
		"counts the calling pane",
		"always blocks `start`",
		"failed at load",
	} {
		if strings.Contains(skillText, banned) {
			t.Fatalf("skill still says %q", banned)
		}
	}
	if strings.Count(skillText, "explicit approval") != 1 {
		t.Fatalf("explicit approval count = %d", strings.Count(skillText, "explicit approval"))
	}
	approval := approvalSentence(t, skillText)
	for _, name := range []string{"`file`", "`link`", "`unlink`", "`release`", "--apply"} {
		if !strings.Contains(approval, name) {
			t.Fatalf("approval sentence missing %s: %s", name, approval)
		}
	}
	if strings.Contains(approval, "release-hold") {
		t.Fatalf("approval sentence includes release-hold: %s", approval)
	}
}

func skillDocumentedExit(t *testing.T, pattern string) int {
	t.Helper()
	matches := regexp.MustCompile(pattern).FindAllStringSubmatch(skillText, -1)
	if len(matches) != 1 || len(matches[0]) != 2 {
		t.Fatalf("skill text matched %q %d times", pattern, len(matches))
	}
	code, err := strconv.Atoi(matches[0][1])
	if err != nil {
		t.Fatal(err)
	}
	return code
}

func approvalSentence(t *testing.T, text string) string {
	t.Helper()
	const needle = "explicit approval"
	i := strings.Index(text, needle)
	if i < 0 {
		t.Fatal("skill does not require explicit approval")
	}
	end := strings.Index(text[i:], ".")
	if end < 0 {
		t.Fatal("approval sentence has no period")
	}
	return text[i : i+end]
}

func TestHelpListsSkill(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"--help"}, &stdout, &stderr)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr.String())
	}
	if !strings.Contains(stdout.String(), "  skill ") {
		t.Fatalf("help does not list skill:\n%s", stdout.String())
	}
}

func TestSkillInstallUninstallStatusCLI(t *testing.T) {
	home, work, _ := withToolTestEnv(t, nil)
	for _, root := range []string{".claude", ".agents", ".gemini", ".grok"} {
		if err := os.MkdirAll(filepath.Join(home, root), 0o750); err != nil {
			t.Fatal(err)
		}
	}

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"skill", "install", "--agent", "all"}, &stdout, &stderr)
	if code != ExitOK || stderr.Len() != 0 {
		t.Fatalf("install code=%d stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	claudePath := filepath.Join(home, ".claude", "skills", "factory", "SKILL.md")
	raw, err := os.ReadFile(claudePath) //nolint:gosec // G304: test temp path
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(raw), "toolcli-tool: factory") {
		t.Fatalf("missing stamp:\n%s", string(raw))
	}

	// Custom --dir override.
	customDir := filepath.Join(work, "custom-skill")
	stdout.Reset()
	code = Execute(context.Background(), []string{"skill", "install", "--dir", customDir}, &stdout, &stderr)
	if code != ExitOK {
		t.Fatalf("custom dir install code=%d stderr=%q", code, stderr.String())
	}
	if _, err := os.Stat(filepath.Join(customDir, "SKILL.md")); err != nil {
		t.Fatal(err)
	}

	// Status --json.
	stdout.Reset()
	code = Execute(context.Background(), []string{"skill", "status", "--json"}, &stdout, &stderr)
	if code != ExitOK {
		t.Fatalf("status code=%d stderr=%q", code, stderr.String())
	}
	var stOut toolcli.SkillStatusOutput
	if err := json.Unmarshal(stdout.Bytes(), &stOut); err != nil {
		t.Fatal(err)
	}
	if stOut.Tool != "factory" || len(stOut.Entries) == 0 {
		t.Fatalf("unexpected status: %+v", stOut)
	}

	// Uninstall.
	stdout.Reset()
	code = Execute(context.Background(), []string{"skill", "uninstall", "--agent", "all"}, &stdout, &stderr)
	if code != ExitOK {
		t.Fatalf("uninstall code=%d stderr=%q", code, stderr.String())
	}
	if _, err := os.Stat(claudePath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("expected %s to be removed", claudePath)
	}
}
