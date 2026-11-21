package toolcli

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// ConformanceConfig configures RunConformance for a specific tool.
type ConformanceConfig struct {
	// NewTool returns a Tool configured with the given temporary home, work
	// directory, and fake installer.
	NewTool func(homeDir, workDir string, inst Installer) Tool
}

type conformanceFakeInstaller struct{}

func (conformanceFakeInstaller) LatestRevision(context.Context, string, string) (RevisionInfo, error) {
	return RevisionInfo{Version: "v0.9.9", Revision: "newrev123456"}, nil
}

func (conformanceFakeInstaller) Install(context.Context, string, string, string) error {
	return nil
}

// RunConformance verifies that a tool's Tool configuration satisfies the
// standard CLI protocol for version, update, doctor, and skill subcommands.
func RunConformance(t *testing.T, cfg ConformanceConfig) {
	t.Helper()
	home := t.TempDir()
	work := t.TempDir()
	tool := cfg.NewTool(home, work, conformanceFakeInstaller{})
	if tool.HomeDir == nil {
		tool.HomeDir = func() (string, error) { return home, nil }
	}
	if tool.WorkDir == nil {
		tool.WorkDir = func() (string, error) { return work, nil }
	}
	if tool.Installer == nil {
		tool.Installer = conformanceFakeInstaller{}
	}

	ctx := context.Background()
	t.Run("version", func(t *testing.T) { checkConformanceVersion(ctx, t, tool) })
	t.Run("doctor_offline", func(t *testing.T) { checkConformanceDoctor(ctx, t, tool) })
	t.Run("update_check", func(t *testing.T) { checkConformanceUpdate(ctx, t, tool) })
	t.Run("skill_roundtrip", func(t *testing.T) { checkConformanceSkill(ctx, t, tool, home) })
}

func checkConformanceVersion(ctx context.Context, t *testing.T, tool Tool) {
	t.Helper()
	var out, errBuf bytes.Buffer
	code, ok := tool.Handle(ctx, []string{"version", "--json"}, &out, &errBuf)
	if !ok || code != 0 {
		t.Fatalf("version --json: ok=%v code=%d stderr=%s", ok, code, errBuf.String())
	}
	var vo VersionOutput
	if err := json.Unmarshal(out.Bytes(), &vo); err != nil {
		t.Fatalf("decode version json: %v", err)
	}
	if vo.Tool != tool.Name {
		t.Fatalf("tool = %q, want %q", vo.Tool, tool.Name)
	}
}

func checkConformanceDoctor(ctx context.Context, t *testing.T, tool Tool) {
	t.Helper()
	var out, errBuf bytes.Buffer
	code, ok := tool.Handle(ctx, []string{"doctor", "--offline", "--json"}, &out, &errBuf)
	if !ok || code == 2 {
		t.Fatalf("doctor --offline --json: ok=%v code=%d stdout=%s stderr=%s", ok, code, out.String(), errBuf.String())
	}
	var do DoctorOutput
	if err := json.Unmarshal(out.Bytes(), &do); err != nil {
		t.Fatalf("decode doctor json: %v", err)
	}
	if len(do.Checks) < 5 {
		t.Fatalf("expected at least 5 standard checks, got %d", len(do.Checks))
	}
}

func checkConformanceUpdate(ctx context.Context, t *testing.T, tool Tool) {
	t.Helper()
	var out, errBuf bytes.Buffer
	code, ok := tool.Handle(ctx, []string{"update", "--check", "--json"}, &out, &errBuf)
	if !ok || (code != 0 && code != 1) {
		t.Fatalf("update --check --json: ok=%v code=%d stderr=%s", ok, code, errBuf.String())
	}
	var uo UpdateOutput
	if err := json.Unmarshal(out.Bytes(), &uo); err != nil {
		t.Fatalf("decode update json: %v", err)
	}
	if len(uo.Binaries) == 0 {
		t.Fatalf("expected at least 1 binary in update output")
	}
}

func checkConformanceSkill(ctx context.Context, t *testing.T, tool Tool, home string) {
	t.Helper()
	var rawOut, errBuf bytes.Buffer
	code, ok := tool.Handle(ctx, []string{"skill"}, &rawOut, &errBuf)
	if !ok || code != 0 || strings.TrimSpace(rawOut.String()) == "" {
		t.Fatalf("skill: ok=%v code=%d len=%d stderr=%s", ok, code, rawOut.Len(), errBuf.String())
	}
	if err := os.MkdirAll(filepath.Join(home, ".claude"), 0o750); err != nil {
		t.Fatal(err)
	}
	var instOut bytes.Buffer
	code, ok = tool.Handle(ctx, []string{"skill", "install", "--agent", "claude", "--scope", "user"}, &instOut, &errBuf)
	if !ok || code != 0 {
		t.Fatalf("skill install: ok=%v code=%d stderr=%s", ok, code, errBuf.String())
	}
	var stOut bytes.Buffer
	code, ok = tool.Handle(ctx, []string{"skill", "status", "--json"}, &stOut, &errBuf)
	if !ok || code != 0 {
		t.Fatalf("skill status: ok=%v code=%d stderr=%s", ok, code, errBuf.String())
	}
	var uninstOut bytes.Buffer
	code, ok = tool.Handle(ctx, []string{"skill", "uninstall", "--agent", "claude", "--scope", "user"}, &uninstOut, &errBuf)
	if !ok || code != 0 {
		t.Fatalf("skill uninstall: ok=%v code=%d stderr=%s", ok, code, errBuf.String())
	}
}
