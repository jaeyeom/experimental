package toolcli

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
)

type fakeInstaller struct {
	latest  RevisionInfo
	latestE error
	install func(ctx context.Context, pkg, ref, binDir string) error
	calls   []string
}

func (f *fakeInstaller) LatestRevision(_ context.Context, pkg, ref string) (RevisionInfo, error) {
	f.calls = append(f.calls, "latest:"+pkg+"@"+ref)
	return f.latest, f.latestE
}

func (f *fakeInstaller) Install(ctx context.Context, pkg, ref, binDir string) error {
	f.calls = append(f.calls, "install:"+pkg+"@"+ref+"->"+binDir)
	if f.install != nil {
		return f.install(ctx, pkg, ref, binDir)
	}
	return nil
}

func newTestTool(t *testing.T) (*Tool, string, string) {
	t.Helper()
	home := t.TempDir()
	work := t.TempDir()
	binDir := t.TempDir()
	exePath := filepath.Join(binDir, "factory")
	if err := os.WriteFile(exePath, []byte("bin"), 0o755); err != nil { //nolint:gosec // G306: test binary placeholder
		t.Fatal(err)
	}
	tool := &Tool{
		Name:             "factory",
		ModulePath:       "github.com/jaeyeom/experimental",
		MainPackage:      "github.com/jaeyeom/experimental/devtools/factory/cmd/factory",
		SkillText:        "---\nname: factory\ndescription: \"test\"\n---\n\n# factory\n",
		RequiredCommands: []string{"herdr", "gh", "git"},
		ReadBuildInfo: func() BuildInfo {
			return BuildInfo{
				ModulePath:  "github.com/jaeyeom/experimental",
				Version:     "v0.1.0",
				Commit:      "abc1234",
				VCSRevision: "abc1234",
				BuildTime:   "2026-11-22T00:00:00Z",
			}
		},
		HomeDir:    func() (string, error) { return home, nil },
		WorkDir:    func() (string, error) { return work, nil },
		Executable: func() (string, error) { return exePath, nil },
		LookPath:   func(file string) (string, error) { return filepath.Join(binDir, file), nil },
		NoColor:    true,
	}
	return tool, home, work
}

func TestVersionHumanAndJSON(t *testing.T) {
	tool, _, _ := newTestTool(t)

	var human bytes.Buffer
	if err := tool.RunVersion(false, &human); err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{"factory v0.1.0 (abc1234)", "module: github.com/jaeyeom/experimental", "built: 2026-11-22T00:00:00Z"} {
		if !strings.Contains(human.String(), want) {
			t.Fatalf("human output missing %q:\n%s", want, human.String())
		}
	}

	var js bytes.Buffer
	if err := tool.RunVersion(true, &js); err != nil {
		t.Fatal(err)
	}
	var out VersionOutput
	if err := json.Unmarshal(js.Bytes(), &out); err != nil {
		t.Fatal(err)
	}
	if out.Tool != "factory" || out.Version != "v0.1.0" || out.VCSRevision != "abc1234" {
		t.Fatalf("unexpected json: %+v", out)
	}
}

func TestUpdateCheckAndApply(t *testing.T) {
	tool, home, _ := newTestTool(t)
	inst := &fakeInstaller{latest: RevisionInfo{Version: "v0.2.0", Revision: "def5678"}}
	tool.Installer = inst

	// Install a stale skill first so update reports it.
	claudeDir := filepath.Join(home, ".claude", "skills", "factory")
	if err := os.MkdirAll(claudeDir, 0o750); err != nil {
		t.Fatal(err)
	}
	staleContent := "---\nname: factory\ntoolcli-tool: factory\ntoolcli-version: v0.0.1\ntoolcli-hash: sha256:old\n---\nold\n"
	if err := os.WriteFile(filepath.Join(claudeDir, "SKILL.md"), []byte(staleContent), 0o600); err != nil {
		t.Fatal(err)
	}

	var checkOut bytes.Buffer
	code, err := tool.RunUpdate(context.Background(), true, "latest", false, &checkOut)
	if err != nil || code != 1 {
		t.Fatalf("check code=%d err=%v out=%s", code, err, checkOut.String())
	}
	if !strings.Contains(checkOut.String(), "abc1234 -> def5678 (update available)") {
		t.Fatalf("checkOut:\n%s", checkOut.String())
	}

	var applyOut bytes.Buffer
	code, err = tool.RunUpdate(context.Background(), false, "main", true, &applyOut)
	if err != nil || code != 0 {
		t.Fatalf("apply code=%d err=%v out=%s", code, err, applyOut.String())
	}
	var got UpdateOutput
	if err := json.Unmarshal(applyOut.Bytes(), &got); err != nil {
		t.Fatal(err)
	}
	if !got.Updated || len(got.Binaries) != 1 || !got.Binaries[0].Updated {
		t.Fatalf("unexpected update json: %+v", got)
	}
	if len(got.StaleSkills) != 1 || got.RefreshCommand != "factory skill install --agent all" {
		t.Fatalf("unexpected stale skills: %+v", got)
	}
}

func TestUpdateFailureReturnsCode2(t *testing.T) {
	tool, _, _ := newTestTool(t)
	tool.Installer = &fakeInstaller{latestE: errors.New("network down")}
	var out bytes.Buffer
	code, err := tool.RunUpdate(context.Background(), false, "latest", false, &out)
	if err == nil || code != 2 {
		t.Fatalf("code=%d err=%v", code, err)
	}
}

func TestDoctorExitCodesAndFormat(t *testing.T) {
	tool, _, _ := newTestTool(t)
	tool.Installer = &fakeInstaller{latest: RevisionInfo{Version: "v0.1.0", Revision: "abc1234"}}

	// All ok -> exit 0.
	var okBuf bytes.Buffer
	code, err := tool.RunDoctor(context.Background(), false, false, nil, &okBuf)
	if err != nil || code != 0 {
		t.Fatalf("ok code=%d err=%v\n%s", code, err, okBuf.String())
	}
	if !strings.Contains(okBuf.String(), "✅ ok    binary-on-path") {
		t.Fatalf("okBuf:\n%s", okBuf.String())
	}

	// Offline -> revision warn -> exit 1.
	var warnBuf bytes.Buffer
	code, err = tool.RunDoctor(context.Background(), true, false, nil, &warnBuf)
	if err != nil || code != 1 {
		t.Fatalf("warn code=%d err=%v\n%s", code, err, warnBuf.String())
	}
	if !strings.Contains(warnBuf.String(), "⚠️ warn  revision") || !strings.Contains(warnBuf.String(), "hint:") {
		t.Fatalf("warnBuf:\n%s", warnBuf.String())
	}

	// Missing required command -> fail -> exit 2.
	tool.LookPath = func(file string) (string, error) {
		if file == "herdr" {
			return "", errors.New("not found")
		}
		return "/bin/" + file, nil
	}
	var failBuf bytes.Buffer
	code, err = tool.RunDoctor(context.Background(), true, true, nil, &failBuf)
	if err != nil || code != 2 {
		t.Fatalf("fail code=%d err=%v\n%s", code, err, failBuf.String())
	}
	var docOut DoctorOutput
	if err := json.Unmarshal(failBuf.Bytes(), &docOut); err != nil {
		t.Fatal(err)
	}
	if docOut.Status != StatusFail || docOut.Tool != "factory" {
		t.Fatalf("docOut: %+v", docOut)
	}
}

func TestSkillInstallUninstallStatusAllAgents(t *testing.T) {
	tool, home, work := newTestTool(t)

	// Create agent root directories in temp home so --agent all detects them.
	for _, root := range []string{".claude", ".agents", ".gemini", ".grok"} {
		if err := os.MkdirAll(filepath.Join(home, root), 0o750); err != nil {
			t.Fatal(err)
		}
	}

	// Dry-run writes nothing.
	var dryBuf bytes.Buffer
	if err := tool.RunSkillInstall(SkillInstallOptions{Agents: []string{"all"}, Scope: ScopeUser, DryRun: true}, &dryBuf); err != nil {
		t.Fatal(err)
	}
	expectedPaths := []string{
		filepath.Join(home, ".claude", "skills", "factory", "SKILL.md"),
		filepath.Join(home, ".agents", "skills", "factory", "SKILL.md"),
		filepath.Join(home, ".gemini", "config", "skills", "factory", "SKILL.md"),
		filepath.Join(home, ".grok", "skills", "factory", "SKILL.md"),
	}
	for _, p := range expectedPaths {
		if _, err := os.Stat(p); !errors.Is(err, fs.ErrNotExist) {
			t.Fatalf("dry-run created %s", p)
		}
	}

	// Install for all user agents.
	var instBuf bytes.Buffer
	if err := tool.RunSkillInstall(SkillInstallOptions{Agents: []string{"all"}, Scope: ScopeUser}, &instBuf); err != nil {
		t.Fatal(err)
	}
	for _, p := range expectedPaths {
		raw, err := os.ReadFile(p) //nolint:gosec // G304: test temp path
		if err != nil {
			t.Fatalf("missing %s: %v", p, err)
		}
		s := string(raw)
		if !strings.Contains(s, "toolcli-tool: factory") || !strings.Contains(s, "toolcli-version: abc1234") || !strings.Contains(s, "toolcli-hash: sha256:") {
			t.Fatalf("missing stamp in %s:\n%s", p, s)
		}
	}

	// Foreign file in project scope is refused without --force, overwritten with --force.
	projClaude := filepath.Join(work, ".claude", "skills", "factory", "SKILL.md")
	if err := os.MkdirAll(filepath.Dir(projClaude), 0o750); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(projClaude, []byte("# foreign\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := tool.RunSkillInstall(SkillInstallOptions{Agents: []string{"claude"}, Scope: ScopeProject}, &instBuf); err == nil {
		t.Fatal("expected error overwriting foreign file without --force")
	}

	// Status reports current user installs and foreign project install.
	var statusBuf bytes.Buffer
	if err := tool.RunSkillStatus(true, &statusBuf); err != nil {
		t.Fatal(err)
	}
	var stOut SkillStatusOutput
	if err := json.Unmarshal(statusBuf.Bytes(), &stOut); err != nil {
		t.Fatal(err)
	}
	var sawCurrent, sawForeign bool
	for _, e := range stOut.Entries {
		if e.Agent == AgentClaude && e.Scope == ScopeUser && e.Status == "current" {
			sawCurrent = true
		}
		if e.Agent == AgentClaude && e.Scope == ScopeProject && e.Status == "foreign" {
			sawForeign = true
		}
	}
	if !sawCurrent || !sawForeign {
		t.Fatalf("unexpected status entries: %+v", stOut.Entries)
	}

	// Uninstall removes stamped files, refuses foreign file.
	if err := tool.RunSkillUninstall(SkillUninstallOptions{Agents: []string{"claude"}, Scope: ScopeProject}, &instBuf); err == nil {
		t.Fatal("expected uninstall to refuse foreign file")
	}
	if err := tool.RunSkillUninstall(SkillUninstallOptions{Agents: []string{"all"}, Scope: ScopeUser}, &instBuf); err != nil {
		t.Fatal(err)
	}
	for _, p := range expectedPaths {
		if _, err := os.Stat(p); !errors.Is(err, fs.ErrNotExist) {
			t.Fatalf("uninstall did not remove %s", p)
		}
	}
}
