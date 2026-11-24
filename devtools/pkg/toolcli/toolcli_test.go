package toolcli_test

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

type fakeInstaller struct {
	queries   []string
	rev       toolcli.RevisionInfo
	revErr    error
	installs  []installCall
	installFn func(pkg, ref, binDir string) error
}

type installCall struct {
	pkg    string
	ref    string
	binDir string
}

func (f *fakeInstaller) LatestRevision(_ context.Context, module, ref string) (toolcli.RevisionInfo, error) {
	f.queries = append(f.queries, module+"@"+ref)
	return f.rev, f.revErr
}

func (f *fakeInstaller) Install(_ context.Context, pkg, ref, binDir string) error {
	f.installs = append(f.installs, installCall{pkg: pkg, ref: ref, binDir: binDir})
	if f.installFn != nil {
		return f.installFn(pkg, ref, binDir)
	}
	return nil
}

const sampleSkill = `---
name: mytool
description: "Use mytool for things."
---

# mytool

Instructions here.
`

func newTestTool(t *testing.T, home, work string, inst toolcli.Installer) toolcli.Tool {
	t.Helper()
	exe := filepath.Join(t.TempDir(), "mytool")
	if err := os.WriteFile(exe, []byte("bin"), 0o600); err != nil {
		t.Fatal(err)
	}
	return toolcli.Tool{
		Name:          "mytool",
		MainPackage:   "example.com/mytool/cmd/mytool",
		DaemonService: "mytool follow",
		Companions: []toolcli.Companion{
			{Name: "mytoold", Package: "example.com/mytool/cmd/mytoold", DaemonService: "mytoold"},
		},
		SkillText:        sampleSkill,
		RequiredCommands: []string{"sh"},
		ReadBuildInfo: func() toolcli.BuildInfo {
			return toolcli.BuildInfo{
				ModulePath:  "example.com/mytool",
				Version:     "v0.1.0",
				VCSRevision: "abc1234",
				BuildTime:   "2026-11-20T00:00:00Z",
			}
		},
		Installer:  inst,
		HomeDir:    func() (string, error) { return home, nil },
		WorkDir:    func() (string, error) { return work, nil },
		Executable: func() (string, error) { return exe, nil },
		LookPath: func(file string) (string, error) {
			if file == "mytool" {
				return exe, nil
			}
			return "/bin/" + file, nil
		},
	}
}

func TestVersion(t *testing.T) {
	home := t.TempDir()
	work := t.TempDir()
	tool := newTestTool(t, home, work, &fakeInstaller{})

	var out, errBuf bytes.Buffer
	code, ok := tool.Handle(context.Background(), []string{"version"}, &out, &errBuf)
	if !ok || code != 0 {
		t.Fatalf("version: ok=%v code=%d stderr=%s", ok, code, errBuf.String())
	}
	if !strings.Contains(out.String(), "mytool v0.1.0 (abc1234)") {
		t.Fatalf("unexpected human version output: %s", out.String())
	}

	out.Reset()
	code, ok = tool.Handle(context.Background(), []string{"version", "--json"}, &out, &errBuf)
	if !ok || code != 0 {
		t.Fatalf("version --json: ok=%v code=%d stderr=%s", ok, code, errBuf.String())
	}
	var vo toolcli.VersionOutput
	if err := json.Unmarshal(out.Bytes(), &vo); err != nil {
		t.Fatalf("decode json: %v", err)
	}
	if vo.Tool != "mytool" || vo.VCSRevision != "abc1234" || vo.ModulePath != "example.com/mytool" {
		t.Fatalf("unexpected VersionOutput: %+v", vo)
	}
}

func TestUpdateCheckAndInstall(t *testing.T) {
	home := t.TempDir()
	work := t.TempDir()
	inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: "v0.2.0", Revision: "def5678"}}
	tool := newTestTool(t, home, work, inst)

	// Write a stale skill so update reports it.
	stalePath := filepath.Join(home, ".claude", "skills", "mytool", "SKILL.md")
	if err := os.MkdirAll(filepath.Dir(stalePath), 0o750); err != nil {
		t.Fatal(err)
	}
	staleContent := "---\ntoolcli-tool: mytool\ntoolcli-version: old\ntoolcli-hash: sha256:0000\n---\nold\n"
	if err := os.WriteFile(stalePath, []byte(staleContent), 0o600); err != nil {
		t.Fatal(err)
	}

	var out, errBuf bytes.Buffer
	code, ok := tool.Handle(context.Background(), []string{"update", "--check"}, &out, &errBuf)
	if !ok || code != 1 {
		t.Fatalf("update --check: ok=%v code=%d (want 1) stdout=%s stderr=%s", ok, code, out.String(), errBuf.String())
	}
	if len(inst.installs) != 0 {
		t.Fatalf("update --check must not install binaries, got %d", len(inst.installs))
	}
	if !strings.Contains(out.String(), "abc1234 -> def5678") {
		t.Fatalf("expected revision transition in output, got:\n%s", out.String())
	}

	out.Reset()
	code, ok = tool.Handle(context.Background(), []string{"update", "--ref", "main"}, &out, &errBuf)
	if !ok || code != 0 {
		t.Fatalf("update: ok=%v code=%d stderr=%s", ok, code, errBuf.String())
	}
	if len(inst.installs) != 2 {
		t.Fatalf("expected 2 installs (main + companion), got %d", len(inst.installs))
	}
	if inst.installs[0].ref != "main" || inst.installs[1].pkg != "example.com/mytool/cmd/mytoold" {
		t.Fatalf("unexpected installs: %+v", inst.installs)
	}
	if !strings.Contains(out.String(), "restart needed: mytool follow, mytoold") {
		t.Fatalf("expected restart needed note, got:\n%s", out.String())
	}
	if !strings.Contains(out.String(), "stale skills:") || !strings.Contains(out.String(), "mytool skill install --agent all") {
		t.Fatalf("expected stale skill note and refresh command, got:\n%s", out.String())
	}
}

func TestDoctorExitCodesAndFormat(t *testing.T) {
	home := t.TempDir()
	work := t.TempDir()
	inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: "v0.1.0", Revision: "abc1234"}}

	t.Run("all_ok", func(t *testing.T) {
		tool := newTestTool(t, home, work, inst)
		var out, errBuf bytes.Buffer
		code, _ := tool.Handle(context.Background(), []string{"doctor"}, &out, &errBuf)
		if code != 0 {
			t.Fatalf("expected exit 0, got %d:\n%s", code, out.String())
		}
		if !strings.Contains(out.String(), "ok    binary-on-path") {
			t.Fatalf("unexpected doctor line format:\n%s", out.String())
		}
	})

	t.Run("warn_offline", func(t *testing.T) {
		tool := newTestTool(t, home, work, inst)
		var out, errBuf bytes.Buffer
		code, _ := tool.Handle(context.Background(), []string{"doctor", "--offline"}, &out, &errBuf)
		if code != 1 {
			t.Fatalf("expected exit 1 with --offline warn, got %d:\n%s", code, out.String())
		}
		if !strings.Contains(out.String(), "warn  revision") || !strings.Contains(out.String(), "hint:") {
			t.Fatalf("expected warn revision with hint, got:\n%s", out.String())
		}
	})

	t.Run("fail_extra_check", func(t *testing.T) {
		tool := newTestTool(t, home, work, inst)
		tool.CheckConfig = func() error { return errors.New("bad config") }
		var out, errBuf bytes.Buffer
		code, _ := tool.Handle(context.Background(), []string{"doctor", "--offline"}, &out, &errBuf)
		if code != 2 {
			t.Fatalf("expected exit 2 on config failure, got %d:\n%s", code, out.String())
		}
		if !strings.Contains(out.String(), "fail  config") {
			t.Fatalf("expected fail config line, got:\n%s", out.String())
		}
	})

	t.Run("custom_doctor_flag_and_usage", func(t *testing.T) {
		tool := newTestTool(t, home, work, inst)
		tool.DoctorUsage = "mytool doctor [--custom]"
		tool.RegisterDoctorFlags = func(fs *flag.FlagSet) func(context.Context, io.Writer, io.Writer) (int, bool) {
			custom := fs.Bool("custom", false, "custom mode")
			return func(_ context.Context, stdout, _ io.Writer) (int, bool) {
				if !*custom {
					return 0, false
				}
				_, _ = fmt.Fprintln(stdout, "custom-ok")
				return 0, true
			}
		}
		var out, errBuf bytes.Buffer
		code, _ := tool.Handle(context.Background(), []string{"doctor", "--custom"}, &out, &errBuf)
		if code != 0 || !strings.Contains(out.String(), "custom-ok") {
			t.Fatalf("custom flag: code=%d out=%q err=%q", code, out.String(), errBuf.String())
		}
		out.Reset()
		errBuf.Reset()
		code, _ = tool.Handle(context.Background(), []string{"doctor", "extra"}, &out, &errBuf)
		if code != 2 || !strings.Contains(errBuf.String(), "--custom") {
			t.Fatalf("extra arg: code=%d err=%q", code, errBuf.String())
		}
	})
}

func TestSkillInstallUninstallStatus(t *testing.T) {
	home := t.TempDir()
	work := t.TempDir()
	tool := newTestTool(t, home, work, &fakeInstaller{})
	ctx := context.Background()

	// Test installing for each agent individually into temp home.
	expectedPaths := map[string]string{
		"claude":      filepath.Join(home, ".claude", "skills", "mytool", "SKILL.md"),
		"codex":       filepath.Join(home, ".agents", "skills", "mytool", "SKILL.md"),
		"antigravity": filepath.Join(home, ".gemini", "config", "skills", "mytool", "SKILL.md"),
		"grok":        filepath.Join(home, ".grok", "skills", "mytool", "SKILL.md"),
	}

	for agent, wantPath := range expectedPaths {
		var out, errBuf bytes.Buffer
		code, ok := tool.Handle(ctx, []string{"skill", "install", "--agent", agent}, &out, &errBuf)
		if !ok || code != 0 {
			t.Fatalf("install %s: ok=%v code=%d stderr=%s", agent, ok, code, errBuf.String())
		}
		data, err := os.ReadFile(wantPath)
		if err != nil {
			t.Fatalf("read installed skill for %s at %s: %v", agent, wantPath, err)
		}
		s := string(data)
		if !strings.Contains(s, "toolcli-tool: mytool") || !strings.Contains(s, "toolcli-hash: sha256:") {
			t.Fatalf("missing front-matter stamp in %s:\n%s", wantPath, s)
		}
	}

	// Verify no extra files were written outside the 4 skill paths.
	var allFiles []string
	err := filepath.WalkDir(home, func(p string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() {
			allFiles = append(allFiles, p)
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(allFiles) != len(expectedPaths) {
		t.Fatalf("unexpected files written in home: %v", allFiles)
	}

	// Verify foreign file protection without --force, and overwrite with --force.
	foreignPath := filepath.Join(work, ".claude", "skills", "mytool", "SKILL.md")
	if err := os.MkdirAll(filepath.Dir(foreignPath), 0o750); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(foreignPath, []byte("foreign content"), 0o600); err != nil {
		t.Fatal(err)
	}

	var out, errBuf bytes.Buffer
	code, _ := tool.Handle(ctx, []string{"skill", "install", "--agent", "claude", "--scope", "project"}, &out, &errBuf)
	if code != 2 {
		t.Fatalf("expected refusal (exit 2) overwriting foreign file without --force, got %d", code)
	}

	// Uninstall must also refuse removing foreign file.
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "uninstall", "--agent", "claude", "--scope", "project"}, &out, &errBuf)
	if code != 2 {
		t.Fatalf("expected refusal (exit 2) removing foreign file, got %d", code)
	}

	// Status should report foreign for project claude, and stale when modified.
	if err := os.WriteFile(expectedPaths["grok"], []byte("---\ntoolcli-tool: mytool\ntoolcli-version: old\ntoolcli-hash: sha256:dead\n---\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "status", "--json"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("skill status --json: code=%d stderr=%s", code, errBuf.String())
	}
	var statusOut toolcli.SkillStatusOutput
	if err := json.Unmarshal(out.Bytes(), &statusOut); err != nil {
		t.Fatalf("decode status json: %v", err)
	}
	gotStatus := map[string]string{}
	for _, e := range statusOut.Entries {
		gotStatus[string(e.Agent)+":"+string(e.Scope)] = e.Status
	}
	if gotStatus["claude:user"] != "current" || gotStatus["grok:user"] != "stale" || gotStatus["claude:project"] != "foreign" {
		t.Fatalf("unexpected statuses: %+v", gotStatus)
	}

	// Uninstall removes own files.
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "uninstall", "--agent", "all"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("uninstall all: code=%d stderr=%s", code, errBuf.String())
	}
	for agent, p := range expectedPaths {
		if _, err := os.Stat(p); !errors.Is(err, fs.ErrNotExist) {
			t.Fatalf("expected %s (%s) to be removed", agent, p)
		}
	}
}

func TestConformanceHelper(t *testing.T) {
	toolcli.RunConformance(t, toolcli.ConformanceConfig{
		NewTool: func(homeDir, workDir string, inst toolcli.Installer) toolcli.Tool {
			return newTestTool(t, homeDir, workDir, inst)
		},
	})
}

func TestRevisionQueryUsesModuleAndMainByDefault(t *testing.T) {
	inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: "v0.1.0", Revision: "abc1234"}}
	tool := newTestTool(t, t.TempDir(), t.TempDir(), inst)
	var out, errBuf bytes.Buffer
	tool.Handle(context.Background(), []string{"update", "--check"}, &out, &errBuf)
	tool.Handle(context.Background(), []string{"update", "--check", "--ref", "v0.1.0"}, &out, &errBuf)
	tool.Handle(context.Background(), []string{"doctor"}, &out, &errBuf)
	want := []string{"example.com/mytool@main", "example.com/mytool@v0.1.0", "example.com/mytool@main"}
	if strings.Join(inst.queries, ",") != strings.Join(want, ",") {
		t.Fatalf("queries = %v, want %v", inst.queries, want)
	}
}

func TestGoInstallerQueriesModulePath(t *testing.T) {
	var got []string
	inst := toolcli.GoInstaller{Run: func(_ context.Context, args ...string) ([]byte, error) {
		got = args
		return []byte(`{"Version":"v0.1.1-0.20261123015735-80ec822bed1a","Time":"2026-11-23T01:57:35Z","Origin":{"Hash":"80ec822bed1a0000"}}`), nil
	}}
	info, err := inst.LatestRevision(context.Background(), "example.com/mytool", "main")
	if err != nil {
		t.Fatal(err)
	}
	if strings.Join(got, " ") != "list -m -json example.com/mytool@main" {
		t.Fatalf("go args = %v", got)
	}
	if info.Revision != "80ec822bed1a0000" || info.Time.IsZero() {
		t.Fatalf("info = %+v", info)
	}
}

func TestOlderResolvedRevisionRefusesAndDoctorSaysAhead(t *testing.T) {
	old := time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC) // installed build is 2026-11-20
	inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: "v0.1.0", Revision: "old0000", Time: old}}
	tool := newTestTool(t, t.TempDir(), t.TempDir(), inst)

	var out, errBuf bytes.Buffer
	code, _ := tool.Handle(context.Background(), []string{"update"}, &out, &errBuf)
	if code != 2 || !strings.Contains(errBuf.String(), "--force") || len(inst.installs) != 0 {
		t.Fatalf("update: code=%d err=%q installs=%d", code, errBuf.String(), len(inst.installs))
	}
	errBuf.Reset()
	code, _ = tool.Handle(context.Background(), []string{"update", "--force"}, &out, &errBuf)
	if code != 0 || len(inst.installs) != 2 {
		t.Fatalf("update --force: code=%d err=%q installs=%d", code, errBuf.String(), len(inst.installs))
	}

	out.Reset()
	tool.Handle(context.Background(), []string{"doctor"}, &out, &errBuf)
	if !strings.Contains(out.String(), "ok    revision") || !strings.Contains(out.String(), "ahead of main") {
		t.Fatalf("doctor:\n%s", out.String())
	}
}

func TestNewerMainRevisionReportsUpdateAvailable(t *testing.T) {
	newer := time.Date(2026, 11, 22, 0, 0, 0, 0, time.UTC)
	inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: "v0.1.1-x", Revision: "new0000", Time: newer}}
	tool := newTestTool(t, t.TempDir(), t.TempDir(), inst)
	var out, errBuf bytes.Buffer
	code, _ := tool.Handle(context.Background(), []string{"update", "--check"}, &out, &errBuf)
	if code != 1 || !strings.Contains(out.String(), "update available") {
		t.Fatalf("code=%d out=%s", code, out.String())
	}
}

func TestPseudoVersionMatchesFullSHA(t *testing.T) {
	const (
		pseudo  = "v0.1.1-0.20261123163641-0508ba726c28"
		fullSHA = "0508ba726c28de93a9770f85a47e62e5085d4a8f"
	)

	newPseudoTool := func(inst toolcli.Installer) toolcli.Tool {
		tool := newTestTool(t, t.TempDir(), t.TempDir(), inst)
		tool.ReadBuildInfo = func() toolcli.BuildInfo {
			return toolcli.BuildInfo{
				ModulePath: "example.com/mytool",
				Version:    pseudo,
			}
		}
		return tool
	}

	t.Run("same_commit_reports_up_to_date", func(t *testing.T) {
		inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: pseudo, Revision: fullSHA}}
		tool := newPseudoTool(inst)

		var out, errBuf bytes.Buffer
		code, ok := tool.Handle(context.Background(), []string{"update", "--check"}, &out, &errBuf)
		if !ok || code != 0 {
			t.Fatalf("update --check: ok=%v code=%d (want 0) out=%q err=%q", ok, code, out.String(), errBuf.String())
		}
		wantLine := "mytool: up to date (" + pseudo + ")"
		if !strings.Contains(out.String(), wantLine) {
			t.Fatalf("update --check output = %q, want substring %q", out.String(), wantLine)
		}

		out.Reset()
		errBuf.Reset()
		code, ok = tool.Handle(context.Background(), []string{"doctor"}, &out, &errBuf)
		if !ok || code != 0 {
			t.Fatalf("doctor: ok=%v code=%d (want 0) out=%q err=%q", ok, code, out.String(), errBuf.String())
		}
		if !strings.Contains(out.String(), "ok    revision") || !strings.Contains(out.String(), pseudo) {
			t.Fatalf("doctor output:\n%s", out.String())
		}
	})

	t.Run("different_commit_reports_update_available", func(t *testing.T) {
		inst := &fakeInstaller{rev: toolcli.RevisionInfo{
			Version:  "v0.1.1-0.20261123170000-deadbeef1234",
			Revision: "deadbeef1234567890abcdef1234567890abcdef",
		}}
		tool := newPseudoTool(inst)

		var out, errBuf bytes.Buffer
		code, ok := tool.Handle(context.Background(), []string{"update", "--check"}, &out, &errBuf)
		if !ok || code != 1 || !strings.Contains(out.String(), "update available") {
			t.Fatalf("update --check: ok=%v code=%d out=%q err=%q", ok, code, out.String(), errBuf.String())
		}

		out.Reset()
		errBuf.Reset()
		code, ok = tool.Handle(context.Background(), []string{"doctor"}, &out, &errBuf)
		if !ok || code != 1 || !strings.Contains(out.String(), "warn  revision") {
			t.Fatalf("doctor: ok=%v code=%d out=%q err=%q", ok, code, out.String(), errBuf.String())
		}
	})
}

func TestTaggedVersionComparisons(t *testing.T) {
	newTaggedTool := func(ver string, inst toolcli.Installer) toolcli.Tool {
		tool := newTestTool(t, t.TempDir(), t.TempDir(), inst)
		tool.ReadBuildInfo = func() toolcli.BuildInfo {
			return toolcli.BuildInfo{
				ModulePath: "example.com/mytool",
				Version:    ver,
			}
		}
		return tool
	}

	t.Run("matching_tag_reports_up_to_date", func(t *testing.T) {
		inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: "v0.1.0", Revision: "v0.1.0"}}
		tool := newTaggedTool("v0.1.0", inst)
		var out, errBuf bytes.Buffer
		code, _ := tool.Handle(context.Background(), []string{"update", "--check"}, &out, &errBuf)
		if code != 0 || !strings.Contains(out.String(), "up to date (v0.1.0)") {
			t.Fatalf("code=%d out=%q err=%q", code, out.String(), errBuf.String())
		}
	})

	t.Run("matching_tag_with_resolved_sha_reports_up_to_date", func(t *testing.T) {
		inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: "v0.1.0", Revision: "0508ba726c28de93a9770f85a47e62e5085d4a8f"}}
		tool := newTaggedTool("v0.1.0", inst)
		var out, errBuf bytes.Buffer
		code, _ := tool.Handle(context.Background(), []string{"update", "--check"}, &out, &errBuf)
		if code != 0 || !strings.Contains(out.String(), "up to date (v0.1.0)") {
			t.Fatalf("code=%d out=%q err=%q", code, out.String(), errBuf.String())
		}
	})

	t.Run("different_tag_reports_update_available", func(t *testing.T) {
		inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: "v0.2.0", Revision: "v0.2.0"}}
		tool := newTaggedTool("v0.1.0", inst)
		var out, errBuf bytes.Buffer
		code, _ := tool.Handle(context.Background(), []string{"update", "--check"}, &out, &errBuf)
		if code != 1 || !strings.Contains(out.String(), "update available") {
			t.Fatalf("code=%d out=%q err=%q", code, out.String(), errBuf.String())
		}
	})

	t.Run("tag_prefix_is_not_treated_as_match", func(t *testing.T) {
		inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: "v1.10.0-rc1", Revision: "v1.10.0-rc1"}}
		tool := newTaggedTool("v1.10.0", inst)
		var out, errBuf bytes.Buffer
		code, _ := tool.Handle(context.Background(), []string{"update", "--check"}, &out, &errBuf)
		if code != 1 || !strings.Contains(out.String(), "update available") {
			t.Fatalf("code=%d out=%q err=%q", code, out.String(), errBuf.String())
		}
	})
}
