package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime/debug"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
	pkgversion "github.com/jaeyeom/experimental/devtools/pkg/version"
)

func TestExecuteVersionJSON(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"version", "--json"}, &stdout, &stderr)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr.String())
	}
	var got toolcli.VersionOutput
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatalf("version output is not JSON: %v\n%s", err, stdout.String())
	}
	if got.Tool != "factory" {
		t.Fatalf("tool = %q, want factory", got.Tool)
	}
	want := pkgversion.ReadInfo()
	if got.Version != want.Version ||
		got.ModulePath != want.ModulePath ||
		got.VCSRevision != want.VCSRevision ||
		got.VCSModified != want.VCSModified ||
		got.BuildTime != want.BuildTime {
		t.Fatalf("version = %+v, want %+v", got, want)
	}
	if vcsRevision() != "" && got.VCSRevision == "" {
		t.Fatalf("vcsRevision stayed empty with vcs.revision set: %+v", got)
	}
}

func TestExecuteVersionUsesPkgVersion(t *testing.T) {
	prev := pkgversion.Version
	pkgversion.Version = "v9.8.7"
	t.Cleanup(func() { pkgversion.Version = prev })

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"version", "--json"}, &stdout, &stderr)
	if code != ExitOK || stderr.Len() != 0 {
		t.Fatalf("exit = %d, stderr = %s", code, stderr.String())
	}
	var got toolcli.VersionOutput
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatalf("version output is not JSON: %v\n%s", err, stdout.String())
	}
	if got.Version != "v9.8.7" {
		t.Fatalf("version = %q, want v9.8.7", got.Version)
	}
}

func TestExecuteVersionHuman(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"version"}, &stdout, &stderr)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr.String())
	}
	if stderr.Len() != 0 {
		t.Fatalf("stderr = %q", stderr.String())
	}
	if !strings.HasPrefix(stdout.String(), "factory ") {
		t.Fatalf("stdout = %q, want factory prefix", stdout.String())
	}
}

func vcsRevision() string {
	info, ok := debug.ReadBuildInfo()
	if !ok || info == nil {
		return ""
	}
	for _, setting := range info.Settings {
		if setting.Key == "vcs.revision" {
			return setting.Value
		}
	}
	return ""
}

func TestExecuteExitCodes(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want int
	}{
		{name: "version", args: []string{"version"}, want: ExitOK},
		{name: "unknown command", args: []string{"nosuchcmd"}, want: ExitUsage},
		{name: "help", args: []string{"--help"}, want: ExitOK},
		{name: "bad flag", args: []string{"--nope"}, want: ExitUsage},
		{name: "bad log level", args: []string{"--log-level", "loud"}, want: ExitUsage},
		{name: "bad log level on version", args: []string{"version", "--log-level", "loud"}, want: ExitUsage},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var stdout, stderr bytes.Buffer
			got := Execute(context.Background(), tc.args, &stdout, &stderr)
			if got != tc.want {
				t.Fatalf("Execute(%v) = %d, want %d, stderr = %s", tc.args, got, tc.want, stderr.String())
			}
		})
	}
}

func TestUnknownCommandIsUsage(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"nosuchcmd"}, &stdout, &stderr)
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d", code, ExitUsage)
	}
	if strings.HasPrefix(stderr.String(), "factory:") {
		t.Fatalf("usage error printed as a command error: %s", stderr.String())
	}
}

func TestVersionDoesNotLoadConfig(t *testing.T) {
	missing := filepath.Join(t.TempDir(), "missing.toml")
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"version", "--json", "--config", missing}, &stdout, &stderr)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr.String())
	}
	if !strings.Contains(stdout.String(), `"version"`) {
		t.Fatalf("stdout = %s", stdout.String())
	}
}

func TestLoadConfigUsesFlag(t *testing.T) {
	dir := t.TempDir()
	cwd := t.TempDir()
	path := filepath.Join(dir, "factory.toml")
	body := fmt.Sprintf(`
[[source]]
name = "experimental"
kind = "github"
repo = "you/yourrepo"
cwd = %q
base = "main"
start_prompt = "work {id}"
`, cwd)
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	root := newRoot(context.Background(), io.Discard)
	root.SetOut(io.Discard)
	root.SetErr(io.Discard)
	root.SetArgs([]string{"version", "--config", path})
	if err := root.Execute(); err != nil {
		t.Fatal(err)
	}
	cfg, err := loadConfig(root)
	if err != nil {
		t.Fatalf("loadConfig: %v", err)
	}
	if cfg.Path != path {
		t.Fatalf("Path = %q, want %q", cfg.Path, path)
	}
	if len(cfg.Sources) != 1 || cfg.Sources[0].Name != "experimental" {
		t.Fatalf("sources = %+v", cfg.Sources)
	}
}

func TestCommandErrorPrefix(t *testing.T) {
	var stderr bytes.Buffer
	code := exitCode(errors.New("boom"), &stderr)
	if code != ExitError {
		t.Fatalf("exit = %d, want %d", code, ExitError)
	}
	if stderr.String() != "factory: boom\n" {
		t.Fatalf("stderr = %q", stderr.String())
	}
}
