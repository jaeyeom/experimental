package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

type cliFakeInstaller struct {
	latest toolcli.RevisionInfo
	err    error
	calls  []string
}

func (f *cliFakeInstaller) LatestRevision(_ context.Context, pkg, ref string) (toolcli.RevisionInfo, error) {
	f.calls = append(f.calls, "latest:"+pkg+"@"+ref)
	return f.latest, f.err
}

func (f *cliFakeInstaller) Install(_ context.Context, pkg, ref, binDir string) error {
	f.calls = append(f.calls, "install:"+pkg+"@"+ref+"->"+binDir)
	return f.err
}

func withToolTestEnv(t *testing.T, inst toolcli.Installer) (home, work, exeDir string) {
	t.Helper()
	home = t.TempDir()
	work = t.TempDir()
	exeDir = t.TempDir()
	exePath := filepath.Join(exeDir, "factory")
	if err := os.WriteFile(exePath, []byte("bin"), 0o755); err != nil { //nolint:gosec // G306: test binary placeholder
		t.Fatal(err)
	}

	prevInst := testToolInstaller
	prevHome := testToolHomeDir
	prevWork := testToolWorkDir
	prevExe := testToolExecutable
	prevLook := testToolLookPath
	prevBI := testToolBuildInfo
	t.Cleanup(func() {
		testToolInstaller = prevInst
		testToolHomeDir = prevHome
		testToolWorkDir = prevWork
		testToolExecutable = prevExe
		testToolLookPath = prevLook
		testToolBuildInfo = prevBI
	})

	testToolInstaller = inst
	testToolHomeDir = func() (string, error) { return home, nil }
	testToolWorkDir = func() (string, error) { return work, nil }
	testToolExecutable = func() (string, error) { return exePath, nil }
	testToolLookPath = func(file string) (string, error) { return filepath.Join(exeDir, file), nil }
	testToolBuildInfo = func() toolcli.BuildInfo {
		return toolcli.BuildInfo{
			ModulePath:  factoryModulePath,
			Version:     "v0.1.0",
			Commit:      "abc1234",
			VCSRevision: "abc1234",
		}
	}
	return home, work, exeDir
}

func TestUpdateCheckAndApplyCLI(t *testing.T) {
	inst := &cliFakeInstaller{latest: toolcli.RevisionInfo{Version: "v0.2.0", Revision: "def5678"}}
	_, _, exeDir := withToolTestEnv(t, inst)

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"update", "--check"}, &stdout, &stderr)
	if code != 1 || stderr.Len() != 0 {
		t.Fatalf("check code=%d stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	if !strings.Contains(stdout.String(), "abc1234 -> def5678 (update available)") {
		t.Fatalf("stdout:\n%s", stdout.String())
	}

	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"update", "--json"}, &stdout, &stderr)
	if code != ExitOK || stderr.Len() != 0 {
		t.Fatalf("update code=%d stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
	var got toolcli.UpdateOutput
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatal(err)
	}
	if !got.Updated || len(inst.calls) == 0 || !strings.HasSuffix(inst.calls[len(inst.calls)-1], "->"+exeDir) {
		t.Fatalf("got=%+v calls=%v", got, inst.calls)
	}
	if inst.calls[0] != "latest:"+factoryModulePath+"@"+toolcli.DefaultRef {
		t.Fatalf("expected default ref %q, got calls=%v", toolcli.DefaultRef, inst.calls)
	}
}

func TestUpdateRefusesDowngradeWithoutForce(t *testing.T) {
	old := time.Date(2026, 11, 1, 0, 0, 0, 0, time.UTC)
	inst := &cliFakeInstaller{latest: toolcli.RevisionInfo{Version: "v0.1.0", Revision: "old0000", Time: old}}
	withToolTestEnv(t, inst)
	prevBI := testToolBuildInfo
	testToolBuildInfo = func() toolcli.BuildInfo {
		bi := prevBI()
		bi.BuildTime = "2026-11-20T00:00:00Z"
		return bi
	}

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"update"}, &stdout, &stderr)
	if code != 2 || !strings.Contains(stderr.String(), "--force") {
		t.Fatalf("expected downgrade refusal, got code=%d stderr=%q", code, stderr.String())
	}

	stdout.Reset()
	stderr.Reset()
	code = Execute(context.Background(), []string{"update", "--force"}, &stdout, &stderr)
	if code != ExitOK || stderr.Len() != 0 {
		t.Fatalf("update --force: code=%d stderr=%q stdout=%q", code, stderr.String(), stdout.String())
	}
}

func TestUpdateFailureCLI(t *testing.T) {
	inst := &cliFakeInstaller{err: errors.New("fetch failed")}
	withToolTestEnv(t, inst)

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"update"}, &stdout, &stderr)
	if code != 2 || !strings.Contains(stderr.String(), "fetch failed") {
		t.Fatalf("code=%d stderr=%q", code, stderr.String())
	}
}

func TestUpdatePseudoVersionTimestampCLI(t *testing.T) {
	olderTime := time.Date(2026, 11, 27, 2, 0, 0, 0, time.UTC)
	newerTime := time.Date(2026, 11, 27, 4, 0, 0, 0, time.UTC)

	t.Run("newer_installed_pseudo_reports_ahead_and_refuses_downgrade", func(t *testing.T) {
		inst := &cliFakeInstaller{latest: toolcli.RevisionInfo{
			Version:  "v0.1.1-0.20261127020000-bbbbbbbbbbbb",
			Revision: "bbbbbbbbbbbb1234",
			Time:     olderTime,
		}}
		withToolTestEnv(t, inst)
		testToolBuildInfo = func() toolcli.BuildInfo {
			return toolcli.BuildInfo{
				ModulePath: factoryModulePath,
				Version:    "v0.1.1-0.20261127033424-aaaaaaaaaaaa",
			}
		}

		var stdout, stderr bytes.Buffer
		code := Execute(context.Background(), []string{"update", "--check", "--json"}, &stdout, &stderr)
		if code != 0 || stderr.Len() != 0 {
			t.Fatalf("check --json: code=%d stderr=%q stdout=%q", code, stderr.String(), stdout.String())
		}
		var got toolcli.UpdateOutput
		if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
			t.Fatal(err)
		}
		if got.UpdateAvailable || !got.Ahead || got.CurrentTime != "2026-11-27T03:34:24Z" || got.LatestTime != "2026-11-27T02:00:00Z" {
			t.Fatalf("unexpected UpdateOutput: %+v", got)
		}

		stdout.Reset()
		stderr.Reset()
		code = Execute(context.Background(), []string{"update"}, &stdout, &stderr)
		if code != 2 || !strings.Contains(stderr.String(), "--force") {
			t.Fatalf("expected downgrade refusal, got code=%d stderr=%q", code, stderr.String())
		}
	})

	t.Run("older_installed_pseudo_offers_update", func(t *testing.T) {
		inst := &cliFakeInstaller{latest: toolcli.RevisionInfo{
			Version:  "v0.1.1-0.20261127040000-cccccccccccc",
			Revision: "cccccccccccc1234",
			Time:     newerTime,
		}}
		withToolTestEnv(t, inst)
		testToolBuildInfo = func() toolcli.BuildInfo {
			return toolcli.BuildInfo{
				ModulePath: factoryModulePath,
				Version:    "v0.1.1-0.20261127033424-aaaaaaaaaaaa",
			}
		}

		var stdout, stderr bytes.Buffer
		code := Execute(context.Background(), []string{"update", "--check", "--json"}, &stdout, &stderr)
		if code != 1 || stderr.Len() != 0 {
			t.Fatalf("check --json: code=%d stderr=%q stdout=%q", code, stderr.String(), stdout.String())
		}
		var got toolcli.UpdateOutput
		if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
			t.Fatal(err)
		}
		if !got.UpdateAvailable || got.Ahead || got.CurrentTime != "2026-11-27T03:34:24Z" || got.LatestTime != "2026-11-27T04:00:00Z" {
			t.Fatalf("unexpected UpdateOutput: %+v", got)
		}
	})
}
