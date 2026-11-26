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
)

func TestExecuteVersionJSON(t *testing.T) {
	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"version", "--json"}, &stdout, &stderr)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr = %s", code, stderr.String())
	}
	var got map[string]any
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatalf("version output is not JSON: %v\n%s", err, stdout.String())
	}
	if got["tool"] != "factory" {
		t.Fatalf("tool = %v, want factory", got["tool"])
	}
	want, err := currentVersion()
	if err != nil {
		t.Fatalf("currentVersion: %v", err)
	}
	if got["version"] != want.Version || got["commit"] != want.Commit {
		t.Fatalf("version = %#v, want %#v", got, want)
	}
	if vcsRevision() != "" && (got["version"] == devVersion || got["commit"] == unknownCommit) {
		t.Fatalf("version stayed dev/unknown with vcs.revision set: %#v", got)
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

func TestResolveVersion(t *testing.T) {
	const rev = "abcdef1234567890aaaa"
	const when = "2026-11-01T12:00:00Z"
	short := rev[:12]
	devel := func(modified string, withTime, withRev bool) *debug.BuildInfo {
		info := &debug.BuildInfo{Main: debug.Module{Version: "(devel)"}}
		if withRev {
			info.Settings = append(info.Settings, debug.BuildSetting{Key: "vcs.revision", Value: rev})
		}
		if withTime {
			info.Settings = append(info.Settings, debug.BuildSetting{Key: "vcs.time", Value: when})
		}
		if modified != "" {
			info.Settings = append(info.Settings, debug.BuildSetting{Key: "vcs.modified", Value: modified})
		}
		return info
	}
	tests := []struct {
		name       string
		version    string
		commit     string
		info       *debug.BuildInfo
		ok         bool
		wantVer    string
		wantCommit string
		wantErr    string
	}{
		{
			name: "ldflags win", version: "v1.2.3", commit: "abc",
			info: &debug.BuildInfo{
				Main: debug.Module{Version: "v9.9.9"},
				Settings: []debug.BuildSetting{
					{Key: "vcs.revision", Value: rev},
					{Key: "vcs.modified", Value: "true"},
				},
			},
			ok: true, wantVer: "v1.2.3", wantCommit: "abc",
		},
		{name: "no build info", version: devVersion, commit: unknownCommit, wantVer: devVersion, wantCommit: unknownCommit},
		{
			name: "unexpected modified", version: devVersion, commit: unknownCommit, ok: true,
			info:    &debug.BuildInfo{Settings: []debug.BuildSetting{{Key: "vcs.modified", Value: "maybe"}}},
			wantErr: `version: expected vcs.modified true or false, found "maybe"`,
		},
		{
			name: "module version dirty", version: devVersion, commit: unknownCommit, ok: true,
			info: &debug.BuildInfo{
				Main: debug.Module{Version: "v1.2.3"},
				Settings: []debug.BuildSetting{
					{Key: "vcs.revision", Value: rev},
					{Key: "vcs.modified", Value: "true"},
				},
			},
			wantVer: "v1.2.3+dirty", wantCommit: rev + "+dirty",
		},
		{
			name: "devel time and revision", version: "", commit: "", ok: true,
			info: devel("false", true, true), wantVer: when + "-" + short, wantCommit: rev,
		},
		{
			name: "devel time only", version: devVersion, commit: unknownCommit, ok: true,
			info: devel("false", true, false), wantVer: when, wantCommit: unknownCommit,
		},
		{
			name: "devel revision only", version: devVersion, commit: unknownCommit, ok: true,
			info: devel("", false, true), wantVer: short, wantCommit: rev,
		},
		{
			name: "devel dirty", version: devVersion, commit: unknownCommit, ok: true,
			info: devel("true", true, true), wantVer: when + "-" + short + "+dirty", wantCommit: rev + "+dirty",
		},
		{
			name: "no vcs stays dev", version: devVersion, commit: unknownCommit, ok: true,
			info:    &debug.BuildInfo{Main: debug.Module{Version: "(devel)"}},
			wantVer: devVersion, wantCommit: unknownCommit,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := resolveVersion(tc.version, tc.commit, tc.info, tc.ok)
			if tc.wantErr != "" {
				if err == nil || err.Error() != tc.wantErr {
					t.Fatalf("resolveVersion() error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("resolveVersion() error = %v", err)
			}
			if got.Version != tc.wantVer || got.Commit != tc.wantCommit {
				t.Fatalf("resolveVersion() = %+v, want version %q commit %q", got, tc.wantVer, tc.wantCommit)
			}
		})
	}
}
