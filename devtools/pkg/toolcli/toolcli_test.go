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
		inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: "v1.10.0", Revision: "v1.10.0"}}
		tool := newTaggedTool("v1.10.0-rc1", inst)
		var out, errBuf bytes.Buffer
		code, _ := tool.Handle(context.Background(), []string{"update", "--check"}, &out, &errBuf)
		if code != 1 || !strings.Contains(out.String(), "update available") {
			t.Fatalf("code=%d out=%q err=%q", code, out.String(), errBuf.String())
		}
	})

	t.Run("older_tag_reports_ahead_and_refuses_downgrade", func(t *testing.T) {
		for _, tc := range []struct {
			name      string
			installed string
			latest    string
		}{
			{name: "older_minor", installed: "v0.2.0", latest: "v0.1.0"},
			{name: "prerelease_of_installed", installed: "v1.10.0", latest: "v1.10.0-rc1"},
		} {
			t.Run(tc.name, func(t *testing.T) {
				inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: tc.latest, Revision: tc.latest}}
				tool := newTaggedTool(tc.installed, inst)

				var out, errBuf bytes.Buffer
				code, _ := tool.Handle(context.Background(), []string{"update", "--check"}, &out, &errBuf)
				if code != 0 || strings.Contains(out.String(), "update available") {
					t.Fatalf("update --check: code=%d out=%q err=%q", code, out.String(), errBuf.String())
				}

				errBuf.Reset()
				code, _ = tool.Handle(context.Background(), []string{"update"}, &out, &errBuf)
				if code != 2 || !strings.Contains(errBuf.String(), "--force") || len(inst.installs) != 0 {
					t.Fatalf("update: code=%d err=%q installs=%d", code, errBuf.String(), len(inst.installs))
				}
			})
		}
	})
}

func TestPseudoVersionTimestampComparisons(t *testing.T) {
	const (
		newerTS  = "2026-11-27T03:34:24Z"
		olderTS  = "2026-11-27T02:00:00Z"
		newestTS = "2026-11-27T04:00:00Z"
	)
	olderTime := time.Date(2026, 11, 27, 2, 0, 0, 0, time.UTC)
	newestTime := time.Date(2026, 11, 27, 4, 0, 0, 0, time.UTC)

	newPseudoTool := func(ver string, inst toolcli.Installer) toolcli.Tool {
		tool := newTestTool(t, t.TempDir(), t.TempDir(), inst)
		tool.ReadBuildInfo = func() toolcli.BuildInfo {
			return toolcli.BuildInfo{
				ModulePath: "example.com/mytool",
				Version:    ver,
			}
		}
		return tool
	}

	t.Run("installed_pseudo_newer_than_main_no_update_and_refuses_downgrade", func(t *testing.T) {
		pseudoVersions := []string{
			"v0.0.0-20261127033424-aaaaaaaaaaaa",
			"v0.1.1-0.20261127033424-aaaaaaaaaaaa",
			"v1.2.3-pre.0.20261127033424-aaaaaaaaaaaa",
			"v2.0.0-20261127033424-aaaaaaaaaaaa+incompatible",
			"v0.1.1-0.20261127033424-aaaaaaaaaaaa+dirty",
		}
		for _, ver := range pseudoVersions {
			t.Run(ver, func(t *testing.T) {
				inst := &fakeInstaller{rev: toolcli.RevisionInfo{
					Version:  "v0.1.1-0.20261127020000-bbbbbbbbbbbb",
					Revision: "bbbbbbbbbbbb1234567890abcdef1234567890ab",
					Time:     olderTime,
				}}
				tool := newPseudoTool(ver, inst)

				var out, errBuf bytes.Buffer
				code, ok := tool.Handle(context.Background(), []string{"update", "--check"}, &out, &errBuf)
				if !ok || code != 0 {
					t.Fatalf("update --check: ok=%v code=%d out=%q err=%q", ok, code, out.String(), errBuf.String())
				}
				if strings.Contains(out.String(), "update available") || !strings.Contains(out.String(), "ahead of main") {
					t.Fatalf("update --check output = %q, want ahead of main without update available", out.String())
				}

				out.Reset()
				errBuf.Reset()
				code, ok = tool.Handle(context.Background(), []string{"update", "--check", "--json"}, &out, &errBuf)
				if !ok || code != 0 {
					t.Fatalf("update --check --json: ok=%v code=%d err=%q", ok, code, errBuf.String())
				}
				var uo toolcli.UpdateOutput
				if err := json.Unmarshal(out.Bytes(), &uo); err != nil {
					t.Fatalf("decode json: %v", err)
				}
				if uo.UpdateAvailable || !uo.Ahead || uo.CurrentTime != newerTS || uo.LatestTime != olderTS {
					t.Fatalf("unexpected UpdateOutput: %+v", uo)
				}

				out.Reset()
				errBuf.Reset()
				code, _ = tool.Handle(context.Background(), []string{"update"}, &out, &errBuf)
				if code != 2 || !strings.Contains(errBuf.String(), "--force") || len(inst.installs) != 0 {
					t.Fatalf("update without --force: code=%d err=%q installs=%d", code, errBuf.String(), len(inst.installs))
				}

				out.Reset()
				errBuf.Reset()
				code, _ = tool.Handle(context.Background(), []string{"doctor"}, &out, &errBuf)
				if code != 0 || !strings.Contains(out.String(), "ok    revision") || !strings.Contains(out.String(), "ahead of main") {
					t.Fatalf("doctor: code=%d out=%q err=%q", code, out.String(), errBuf.String())
				}
			})
		}
	})

	t.Run("installed_pseudo_older_than_latest_offers_update_and_json_times", func(t *testing.T) {
		inst := &fakeInstaller{rev: toolcli.RevisionInfo{
			Version:  "v0.1.1-0.20261127040000-cccccccccccc",
			Revision: "cccccccccccc1234567890abcdef1234567890ab",
			Time:     newestTime,
		}}
		tool := newPseudoTool("v0.1.1-0.20261127033424-aaaaaaaaaaaa", inst)

		var out, errBuf bytes.Buffer
		code, ok := tool.Handle(context.Background(), []string{"update", "--check"}, &out, &errBuf)
		if !ok || code != 1 || !strings.Contains(out.String(), "update available") {
			t.Fatalf("update --check: ok=%v code=%d out=%q err=%q", ok, code, out.String(), errBuf.String())
		}

		out.Reset()
		errBuf.Reset()
		code, ok = tool.Handle(context.Background(), []string{"update", "--check", "--json"}, &out, &errBuf)
		if !ok || code != 1 {
			t.Fatalf("update --check --json: ok=%v code=%d err=%q", ok, code, errBuf.String())
		}
		var uo toolcli.UpdateOutput
		if err := json.Unmarshal(out.Bytes(), &uo); err != nil {
			t.Fatalf("decode json: %v", err)
		}
		if !uo.UpdateAvailable || uo.Ahead || uo.CurrentTime != newerTS || uo.LatestTime != newestTS {
			t.Fatalf("unexpected UpdateOutput: %+v", uo)
		}
	})

	t.Run("vcs_build_time_json_includes_both_times", func(t *testing.T) {
		inst := &fakeInstaller{rev: toolcli.RevisionInfo{
			Version:  "v0.1.1-0.20261127040000-cccccccccccc",
			Revision: "cccccccccccc",
			Time:     newestTime,
		}}
		tool := newTestTool(t, t.TempDir(), t.TempDir(), inst)
		var out, errBuf bytes.Buffer
		code, _ := tool.Handle(context.Background(), []string{"update", "--check", "--json"}, &out, &errBuf)
		if code != 1 {
			t.Fatalf("code=%d out=%q err=%q", code, out.String(), errBuf.String())
		}
		var uo toolcli.UpdateOutput
		if err := json.Unmarshal(out.Bytes(), &uo); err != nil {
			t.Fatalf("decode json: %v", err)
		}
		if uo.CurrentTime != "2026-11-20T00:00:00Z" || uo.LatestTime != newestTS {
			t.Fatalf("unexpected times in UpdateOutput: %+v", uo)
		}
	})
}

func TestGoInstallerLatestRevisionDirectForBranchRefs(t *testing.T) {
	type call struct {
		env  map[string]string
		args []string
	}

	t.Run("branch_ref_uses_goproxy_direct", func(t *testing.T) {
		var calls []call
		inst := toolcli.GoInstaller{
			RunEnv: func(_ context.Context, env map[string]string, args ...string) ([]byte, error) {
				calls = append(calls, call{env: env, args: args})
				return []byte(`{"Version":"v0.1.1-0.20261127033424-aaaaaaaaaaaa","Time":"2026-11-27T03:34:24Z","Origin":{"Hash":"aaaaaaaaaaaa1234"}}`), nil
			},
		}
		info, err := inst.LatestRevision(context.Background(), "example.com/mytool", "main")
		if err != nil {
			t.Fatal(err)
		}
		if len(calls) != 1 || calls[0].env["GOPROXY"] != "direct" {
			t.Fatalf("expected 1 call with GOPROXY=direct, got %+v", calls)
		}
		if info.Revision != "aaaaaaaaaaaa1234" {
			t.Fatalf("info = %+v", info)
		}
	})

	t.Run("branch_ref_falls_back_when_direct_fails", func(t *testing.T) {
		var calls []call
		inst := toolcli.GoInstaller{
			RunEnv: func(_ context.Context, env map[string]string, args ...string) ([]byte, error) {
				calls = append(calls, call{env: env, args: args})
				if env["GOPROXY"] == "direct" {
					return nil, errors.New("direct failed")
				}
				return []byte(`{"Version":"v0.1.1-0.20261127020000-bbbbbbbbbbbb","Time":"2026-11-27T02:00:00Z","Origin":{"Hash":"bbbbbbbbbbbb1234"}}`), nil
			},
		}
		info, err := inst.LatestRevision(context.Background(), "example.com/mytool", "main")
		if err != nil {
			t.Fatal(err)
		}
		if len(calls) != 2 || calls[0].env["GOPROXY"] != "direct" || calls[1].env != nil {
			t.Fatalf("expected direct then fallback nil env, got %+v", calls)
		}
		if info.Revision != "bbbbbbbbbbbb1234" {
			t.Fatalf("info = %+v", info)
		}
	})

	t.Run("semver_and_sha_refs_do_not_force_direct", func(t *testing.T) {
		for _, ref := range []string{"v0.1.0", "0508ba726c28de93a9770f85a47e62e5085d4a8f"} {
			var calls []call
			inst := toolcli.GoInstaller{
				RunEnv: func(_ context.Context, env map[string]string, args ...string) ([]byte, error) {
					calls = append(calls, call{env: env, args: args})
					return []byte(`{"Version":"v0.1.0"}`), nil
				},
			}
			if _, err := inst.LatestRevision(context.Background(), "example.com/mytool", ref); err != nil {
				t.Fatalf("ref %q: %v", ref, err)
			}
			if len(calls) != 1 || calls[0].env != nil {
				t.Fatalf("ref %q: expected 1 call with nil env, got %+v", ref, calls)
			}
		}
	})
}

func TestExtraSkillsInstallUninstallStatusDoctorUpdate(t *testing.T) {
	const extraSkill = "---\nname: mytool-extra\ndescription: \"Extra skill.\"\n---\n\n# mytool-extra\n"

	home := t.TempDir()
	work := t.TempDir()
	if err := os.MkdirAll(filepath.Join(home, ".claude"), 0o750); err != nil {
		t.Fatal(err)
	}
	inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: "v0.1.0", Revision: "abc1234"}}
	tool := newTestTool(t, home, work, inst)
	tool.ExtraSkills = []toolcli.Skill{{Name: "mytool-extra", Text: extraSkill}}

	ctx := context.Background()
	primaryPath := filepath.Join(home, ".claude", "skills", "mytool", "SKILL.md")
	extraPath := filepath.Join(home, ".claude", "skills", "mytool-extra", "SKILL.md")

	// --dir is rejected when multiple skills are configured.
	var out, errBuf bytes.Buffer
	code, _ := tool.Handle(ctx, []string{"skill", "install", "--dir", t.TempDir()}, &out, &errBuf)
	if code != 2 {
		t.Fatalf("expected exit 2 for --dir with ExtraSkills, got %d (stderr=%s)", code, errBuf.String())
	}

	// Dry-run prints both primary and extra skill paths without writing.
	out.Reset()
	errBuf.Reset()
	code, ok := tool.Handle(ctx, []string{"skill", "install", "--agent", "all", "--force", "--dry-run"}, &out, &errBuf)
	if !ok || code != 0 {
		t.Fatalf("dry-run install: ok=%v code=%d stderr=%s", ok, code, errBuf.String())
	}
	if !strings.Contains(out.String(), "would install claude (user): "+primaryPath) ||
		!strings.Contains(out.String(), "would install claude (user): "+extraPath) {
		t.Fatalf("dry-run missing expected paths:\n%s", out.String())
	}
	if _, err := os.Stat(extraPath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("dry-run wrote %s", extraPath)
	}

	// Foreign file on extra skill blocks install without --force and leaves primary unwritten.
	if err := os.MkdirAll(filepath.Dir(extraPath), 0o750); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(extraPath, []byte("foreign extra"), 0o600); err != nil {
		t.Fatal(err)
	}
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "install", "--agent", "all"}, &out, &errBuf)
	if code != 2 {
		t.Fatalf("expected refusal on foreign extra skill, got %d", code)
	}
	if _, err := os.Stat(primaryPath); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("install wrote primary %s despite foreign extra skill refusal", primaryPath)
	}

	// --force installs both primary and extra skills.
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "install", "--agent", "all", "--force"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("force install failed: code=%d stderr=%s", code, errBuf.String())
	}
	extraData, err := os.ReadFile(extraPath)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(extraData), "toolcli-tool: mytool-extra") {
		t.Fatalf("extra skill missing toolcli-tool stamp:\n%s", string(extraData))
	}

	// status --json reports both skills with current status.
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "status", "--json"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("status --json: code=%d stderr=%s", code, errBuf.String())
	}
	var st toolcli.SkillStatusOutput
	if err := json.Unmarshal(out.Bytes(), &st); err != nil {
		t.Fatal(err)
	}
	got := map[string]toolcli.SkillStatusEntry{}
	for _, e := range st.Entries {
		got[e.Path] = e
	}
	if got[primaryPath].Status != "current" || got[primaryPath].Skill != "mytool" {
		t.Fatalf("unexpected primary entry: %+v", got[primaryPath])
	}
	if got[extraPath].Status != "current" || got[extraPath].Skill != "mytool-extra" {
		t.Fatalf("unexpected extra entry: %+v", got[extraPath])
	}

	// Stale extra skill is reported by doctor and update --check with extra skill's CurrentHash.
	if err := os.WriteFile(extraPath, []byte("---\ntoolcli-tool: mytool-extra\ntoolcli-version: old\ntoolcli-hash: sha256:0000\n---\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"doctor", "--offline"}, &out, &errBuf)
	if code != 1 || !strings.Contains(out.String(), "claude/mytool-extra (user)") {
		t.Fatalf("doctor: code=%d out=%s", code, out.String())
	}

	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"update", "--check", "--json"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("update --check --json: code=%d stderr=%s", code, errBuf.String())
	}
	var uo toolcli.UpdateOutput
	if err := json.Unmarshal(out.Bytes(), &uo); err != nil {
		t.Fatal(err)
	}
	if len(uo.StaleSkills) != 1 || uo.StaleSkills[0].Path != extraPath {
		t.Fatalf("unexpected staleSkills: %+v", uo.StaleSkills)
	}
	if uo.StaleSkills[0].CurrentHash == st.CurrentHash {
		t.Fatalf("extra skill CurrentHash %q must differ from primary hash %q", uo.StaleSkills[0].CurrentHash, st.CurrentHash)
	}

	// Uninstall removes both primary and extra skills.
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "uninstall", "--agent", "all"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("uninstall: code=%d stderr=%s", code, errBuf.String())
	}
	for _, p := range []string{primaryPath, extraPath} {
		if _, err := os.Stat(p); !errors.Is(err, fs.ErrNotExist) {
			t.Fatalf("expected %s to be removed", p)
		}
	}
}

func TestRunHelpers(t *testing.T) {
	home := t.TempDir()
	work := t.TempDir()
	inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: "v0.2.0", Revision: "def5678"}}
	tool := newTestTool(t, home, work, inst)
	tool.ModulePath = "example.com/override"
	tool.NoColor = true
	tool.ReadBuildInfo = func() toolcli.BuildInfo {
		return toolcli.BuildInfo{
			Version: "v0.1.0",
			Commit:  "abc1234",
		}
	}

	t.Run("RunVersion", func(t *testing.T) {
		var human, js bytes.Buffer
		if err := tool.RunVersion(false, &human); err != nil {
			t.Fatal(err)
		}
		if !strings.Contains(human.String(), "mytool v0.1.0 (abc1234)") {
			t.Fatalf("unexpected human output: %s", human.String())
		}
		if err := tool.RunVersion(true, &js); err != nil {
			t.Fatal(err)
		}
		var vo toolcli.VersionOutput
		if err := json.Unmarshal(js.Bytes(), &vo); err != nil {
			t.Fatal(err)
		}
		if vo.Commit != "abc1234" {
			t.Fatalf("expected commit abc1234, got %+v", vo)
		}
	})

	t.Run("RunUpdate", func(t *testing.T) {
		var out bytes.Buffer
		code, err := tool.RunUpdate(context.Background(), toolcli.UpdateOptions{Check: true}, &out)
		if err != nil || code != 1 {
			t.Fatalf("check: code=%d err=%v out=%s", code, err, out.String())
		}
		if inst.queries[len(inst.queries)-1] != "example.com/override@"+toolcli.DefaultRef {
			t.Fatalf("expected ModulePath fallback with DefaultRef, got %v", inst.queries)
		}

		noMain := tool
		noMain.MainPackage = ""
		if code, err := noMain.RunUpdate(context.Background(), toolcli.UpdateOptions{}, &out); err == nil || code != 2 {
			t.Fatalf("expected error for empty MainPackage, got code=%d err=%v", code, err)
		}
	})

	t.Run("RunDoctor", func(t *testing.T) {
		var out bytes.Buffer
		extra := []toolcli.CheckResult{{Name: "custom", Status: toolcli.StatusWarn, Detail: "heads up"}}
		code, err := tool.RunDoctor(context.Background(), toolcli.DoctorOptions{Offline: true, JSON: true, Extra: extra}, &out)
		if err != nil || code != 1 {
			t.Fatalf("RunDoctor: code=%d err=%v out=%s", code, err, out.String())
		}
		var do toolcli.DoctorOutput
		if err := json.Unmarshal(out.Bytes(), &do); err != nil {
			t.Fatal(err)
		}
		if len(do.Checks) != 6 || do.Checks[5].Name != "custom" {
			t.Fatalf("unexpected checks: %+v", do.Checks)
		}
	})

	t.Run("RunSkillHelpers", func(t *testing.T) {
		if err := os.MkdirAll(filepath.Join(home, ".claude"), 0o750); err != nil {
			t.Fatal(err)
		}
		var raw bytes.Buffer
		if err := tool.RunSkill(&raw); err != nil {
			t.Fatal(err)
		}
		if raw.String() != sampleSkill {
			t.Fatalf("RunSkill mismatch")
		}
		var out bytes.Buffer
		// Empty Scope defaults to ScopeUser.
		if err := tool.RunSkillInstall(toolcli.SkillInstallOptions{Agents: []string{"claude"}}, &out); err != nil {
			t.Fatal(err)
		}
		var statusBuf bytes.Buffer
		if err := tool.RunSkillStatus(true, &statusBuf); err != nil {
			t.Fatal(err)
		}
		var so toolcli.SkillStatusOutput
		if err := json.Unmarshal(statusBuf.Bytes(), &so); err != nil {
			t.Fatal(err)
		}
		var sawCurrent bool
		for _, e := range so.Entries {
			if e.Agent == toolcli.AgentClaude && e.Scope == toolcli.ScopeUser && e.Status == "current" {
				sawCurrent = true
			}
		}
		if !sawCurrent {
			t.Fatalf("expected current claude user entry, got %+v", so.Entries)
		}
		out.Reset()
		if err := tool.RunSkillUninstall(toolcli.SkillUninstallOptions{Agents: []string{"claude"}}, &out); err != nil {
			t.Fatal(err)
		}
	})
}

func TestGoInstallerRunEnvModulePathAndLDFlags(t *testing.T) {
	type call struct {
		env  map[string]string
		args []string
	}
	var calls []call
	inst := toolcli.GoInstaller{
		ModulePath: "example.com/mod",
		LDFlags: func(rev toolcli.RevisionInfo) string {
			return fmt.Sprintf("-X main.ver=%s -X main.rev=%s", rev.Version, rev.Revision)
		},
		RunEnv: func(_ context.Context, env map[string]string, args ...string) ([]byte, error) {
			calls = append(calls, call{env: env, args: args})
			if len(args) > 0 && args[0] == "list" {
				return []byte(`{"Version":"v1.2.3","Origin":{"Hash":"deadbeef1234"}}`), nil
			}
			return nil, nil
		},
	}

	if err := inst.Install(context.Background(), "example.com/mod/cmd/tool", "", "/tmp/bin"); err != nil {
		t.Fatal(err)
	}
	if len(calls) != 2 {
		t.Fatalf("expected 2 calls (list + install), got %d: %+v", len(calls), calls)
	}
	if strings.Join(calls[0].args, " ") != "list -m -json example.com/mod@main" {
		t.Fatalf("unexpected list args: %v", calls[0].args)
	}
	wantInstall := "install -ldflags -X main.ver=v1.2.3 -X main.rev=deadbeef1234 example.com/mod/cmd/tool@main"
	if strings.Join(calls[1].args, " ") != wantInstall {
		t.Fatalf("install args = %q, want %q", strings.Join(calls[1].args, " "), wantInstall)
	}
	if calls[1].env["GOBIN"] != "/tmp/bin" {
		t.Fatalf("GOBIN = %q, want /tmp/bin", calls[1].env["GOBIN"])
	}
}

func TestSkillCompanionFilesInstallStatusDoctorUpdateUninstall(t *testing.T) {
	const (
		setupV1 = "# Setup v1\n\nStep 1.\n"
		setupV2 = "# Setup v2\n\nStep 1.\nStep 2.\n"
		extraMd = "# Extra ref\n"
	)
	const extraSkill = "---\nname: mytool-extra\ndescription: \"Extra skill.\"\n---\n\n# mytool-extra\n"

	home := t.TempDir()
	work := t.TempDir()
	for _, root := range []string{".claude", ".agents"} {
		if err := os.MkdirAll(filepath.Join(home, root), 0o750); err != nil {
			t.Fatal(err)
		}
	}

	inst := &fakeInstaller{rev: toolcli.RevisionInfo{Version: "v0.1.0", Revision: "abc1234"}}
	tool := newTestTool(t, home, work, inst)
	tool.SkillFiles = []toolcli.SkillFile{
		{Path: "references/setup.md", Content: setupV1},
	}
	tool.ExtraSkills = []toolcli.Skill{
		{
			Name:  "mytool-extra",
			Text:  extraSkill,
			Files: []toolcli.SkillFile{{Path: "references/extra.md", Content: extraMd}},
		},
	}

	ctx := context.Background()
	claudeSkill := filepath.Join(home, ".claude", "skills", "mytool", "SKILL.md")
	claudeSetup := filepath.Join(home, ".claude", "skills", "mytool", "references", "setup.md")
	codexSetup := filepath.Join(home, ".agents", "skills", "mytool", "references", "setup.md")
	claudeExtraSetup := filepath.Join(home, ".claude", "skills", "mytool-extra", "references", "extra.md")

	// Dry-run does not write companion files and keeps `would install <agent> (user): <SKILL.md>` format.
	var out, errBuf bytes.Buffer
	code, ok := tool.Handle(ctx, []string{"skill", "install", "--agent", "all", "--force", "--dry-run"}, &out, &errBuf)
	if !ok || code != 0 {
		t.Fatalf("dry-run install: ok=%v code=%d stderr=%s", ok, code, errBuf.String())
	}
	if !strings.Contains(out.String(), "would install claude (user): "+claudeSkill) {
		t.Fatalf("dry-run output missing SKILL.md line:\n%s", out.String())
	}
	if strings.Contains(out.String(), "would install claude (user): "+claudeSetup) {
		t.Fatalf("dry-run output must not emit extra would-install line for companion file:\n%s", out.String())
	}
	if _, err := os.Stat(claudeSetup); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("dry-run wrote %s", claudeSetup)
	}

	// Real install writes both SKILL.md and companion files for primary and extra skills.
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "install", "--agent", "all"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("install all: code=%d stderr=%s", code, errBuf.String())
	}
	for path, want := range map[string]string{
		claudeSetup:      setupV1,
		codexSetup:       setupV1,
		claudeExtraSetup: extraMd,
	} {
		got, err := os.ReadFile(path)
		if err != nil {
			t.Fatalf("read %s: %v", path, err)
		}
		if string(got) != want {
			t.Fatalf("%s content = %q, want %q", path, string(got), want)
		}
	}

	// Status and doctor report current when SKILL.md and companion files match.
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "status", "--json"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("status --json: code=%d stderr=%s", code, errBuf.String())
	}
	var st toolcli.SkillStatusOutput
	if err := json.Unmarshal(out.Bytes(), &st); err != nil {
		t.Fatal(err)
	}
	statusByPath := map[string]string{}
	for _, e := range st.Entries {
		statusByPath[e.Path] = e.Status
	}
	if statusByPath[claudeSkill] != "current" {
		t.Fatalf("expected current status for %s, got %q", claudeSkill, statusByPath[claudeSkill])
	}

	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"doctor"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("doctor expected 0 when current, got %d:\n%s", code, out.String())
	}

	// Modified companion file (while SKILL.md is untouched) -> stale in status, doctor, and update --check.
	if err := os.WriteFile(claudeSetup, []byte("outdated"), 0o600); err != nil {
		t.Fatal(err)
	}
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "status", "--json"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("status --json: code=%d stderr=%s", code, errBuf.String())
	}
	if err := json.Unmarshal(out.Bytes(), &st); err != nil {
		t.Fatal(err)
	}
	for _, e := range st.Entries {
		if e.Path == claudeSkill && e.Status != "stale" {
			t.Fatalf("modified companion file: status = %q, want stale", e.Status)
		}
	}
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"doctor", "--offline"}, &out, &errBuf)
	if code != 1 || !strings.Contains(out.String(), "stale: claude (user)") {
		t.Fatalf("doctor modified companion: code=%d out=\n%s", code, out.String())
	}
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"update", "--check", "--json"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("update --check --json: code=%d stderr=%s", code, errBuf.String())
	}
	var uo toolcli.UpdateOutput
	if err := json.Unmarshal(out.Bytes(), &uo); err != nil {
		t.Fatal(err)
	}
	if len(uo.StaleSkills) != 1 || uo.StaleSkills[0].Path != claudeSkill {
		t.Fatalf("expected stale skill for modified companion, got %+v", uo.StaleSkills)
	}

	// Missing companion file (while SKILL.md is untouched) -> also stale.
	if err := os.Remove(claudeSetup); err != nil {
		t.Fatal(err)
	}
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "status", "--json"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("status --json: code=%d stderr=%s", code, errBuf.String())
	}
	if err := json.Unmarshal(out.Bytes(), &st); err != nil {
		t.Fatal(err)
	}
	for _, e := range st.Entries {
		if e.Path == claudeSkill && e.Status != "stale" {
			t.Fatalf("missing companion file: status = %q, want stale", e.Status)
		}
	}

	// Upgrading companion file content on an already-owned skill succeeds WITHOUT --force.
	tool.SkillFiles = []toolcli.SkillFile{{Path: "references/setup.md", Content: setupV2}}
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "install", "--agent", "all"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("upgrade without --force failed: code=%d stderr=%s", code, errBuf.String())
	}
	gotV2, err := os.ReadFile(claudeSetup)
	if err != nil {
		t.Fatal(err)
	}
	if string(gotV2) != setupV2 {
		t.Fatalf("upgraded companion = %q, want %q", string(gotV2), setupV2)
	}

	// Uninstall --dry-run preserves both SKILL.md and companion files; real uninstall removes both and empty references dir.
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "uninstall", "--agent", "all", "--dry-run"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("uninstall --dry-run: code=%d stderr=%s", code, errBuf.String())
	}
	if _, err := os.Stat(claudeSetup); err != nil {
		t.Fatalf("uninstall --dry-run removed %s: %v", claudeSetup, err)
	}

	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "uninstall", "--agent", "all"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("uninstall all: code=%d stderr=%s", code, errBuf.String())
	}
	for _, p := range []string{claudeSkill, claudeSetup, codexSetup, claudeExtraSetup} {
		if _, err := os.Stat(p); !errors.Is(err, fs.ErrNotExist) {
			t.Fatalf("expected %s to be removed", p)
		}
	}
	if _, err := os.Stat(filepath.Dir(claudeSetup)); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("expected empty references dir %s to be removed", filepath.Dir(claudeSetup))
	}

	// --dir and --scope project support companion files, and foreign companion file blocks install without --force.
	tool.ExtraSkills = nil
	customDir := filepath.Join(work, "custom-skill")
	customSetup := filepath.Join(customDir, "references", "setup.md")
	if err := os.MkdirAll(filepath.Dir(customSetup), 0o750); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(customSetup, []byte("foreign user notes"), 0o600); err != nil {
		t.Fatal(err)
	}
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "install", "--dir", customDir}, &out, &errBuf)
	if code != 2 {
		t.Fatalf("expected exit 2 overwriting foreign companion without --force, got %d", code)
	}
	if _, err := os.Stat(filepath.Join(customDir, "SKILL.md")); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("SKILL.md written despite foreign companion refusal")
	}

	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "install", "--dir", customDir, "--force"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("force install --dir: code=%d stderr=%s", code, errBuf.String())
	}
	gotCustom, err := os.ReadFile(customSetup)
	if err != nil {
		t.Fatal(err)
	}
	if string(gotCustom) != setupV2 {
		t.Fatalf("--dir companion = %q, want %q", string(gotCustom), setupV2)
	}

	// Project scope install + uninstall.
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "install", "--agent", "claude", "--scope", "project"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("project install: code=%d stderr=%s", code, errBuf.String())
	}
	projSetup := filepath.Join(work, ".claude", "skills", "mytool", "references", "setup.md")
	if _, err := os.Stat(projSetup); err != nil {
		t.Fatalf("project companion missing: %v", err)
	}
	out.Reset()
	errBuf.Reset()
	code, _ = tool.Handle(ctx, []string{"skill", "uninstall", "--agent", "claude", "--scope", "project"}, &out, &errBuf)
	if code != 0 {
		t.Fatalf("project uninstall: code=%d stderr=%s", code, errBuf.String())
	}
	if _, err := os.Stat(projSetup); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("expected %s to be removed", projSetup)
	}
}

func TestSkillCompanionFilePathValidation(t *testing.T) {
	invalidPaths := []string{
		"",
		"   ",
		"/etc/passwd",
		"../escape.md",
		"references/../../escape.md",
		"./references/setup.md",
		"references//setup.md",
		`references\setup.md`,
		".",
		"..",
		"SKILL.md",
		"skill.md",
		"references/",
	}
	for _, bad := range invalidPaths {
		t.Run("invalid_"+bad, func(t *testing.T) {
			home := t.TempDir()
			work := t.TempDir()
			if err := os.MkdirAll(filepath.Join(home, ".claude"), 0o750); err != nil {
				t.Fatal(err)
			}
			tool := newTestTool(t, home, work, &fakeInstaller{})
			tool.SkillFiles = []toolcli.SkillFile{{Path: bad, Content: "x"}}

			for _, args := range [][]string{
				{"skill", "install", "--agent", "claude"},
				{"skill", "uninstall", "--agent", "claude"},
				{"skill", "status", "--json"},
			} {
				var out, errBuf bytes.Buffer
				code, _ := tool.Handle(context.Background(), args, &out, &errBuf)
				if code != 2 {
					t.Fatalf("%v with path %q: code=%d, want 2 (stderr=%s)", args, bad, code, errBuf.String())
				}
			}

			var docOut, docErr bytes.Buffer
			code, _ := tool.Handle(context.Background(), []string{"doctor", "--offline"}, &docOut, &docErr)
			if code != 2 || !strings.Contains(docOut.String(), "fail  skills") {
				t.Fatalf("doctor with invalid path %q: code=%d out=%s", bad, code, docOut.String())
			}
		})
	}

	t.Run("duplicate_paths", func(t *testing.T) {
		home := t.TempDir()
		work := t.TempDir()
		tool := newTestTool(t, home, work, &fakeInstaller{})
		tool.SkillFiles = []toolcli.SkillFile{
			{Path: "references/setup.md", Content: "a"},
			{Path: "references/Setup.md", Content: "b"},
		}
		var out, errBuf bytes.Buffer
		code, _ := tool.Handle(context.Background(), []string{"skill", "install", "--dir", t.TempDir()}, &out, &errBuf)
		if code != 2 {
			t.Fatalf("expected exit 2 for duplicate companion paths, got %d", code)
		}
	})
}
