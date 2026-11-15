package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/orgctl"
)

func writeSpec(t *testing.T, body string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "spec.json")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestCheckApprovalWordFlag(t *testing.T) {
	path := writeSpec(t, `{"roles":{"coord/lead":{"decides":["ship the build"]}}}`)
	var stderr bytes.Buffer
	if code := run([]string{"check", path}, &stderr); code != 0 {
		t.Fatalf("exit %d, stderr %s", code, stderr.String())
	}
	stderr.Reset()
	code := run([]string{"check", "--approval-word", "ship", path}, &stderr)
	if code != 1 || !strings.Contains(stderr.String(), `approval word "ship"`) {
		t.Fatalf("exit %d, stderr %s", code, stderr.String())
	}
}

func TestCheckPrintsAllowWarning(t *testing.T) {
	path := writeSpec(t, `{"roles":{"coord/lead":{"decides":["merge PRs"]}},"allowApprovalWords":["merge"]}`)
	var stderr bytes.Buffer
	code := run([]string{"check", path}, &stderr)
	if code != 0 || !strings.Contains(stderr.String(), `warning: allowApprovalWords "merge"`) {
		t.Fatalf("exit %d, stderr %s", code, stderr.String())
	}
}

func TestPlanExitCodes(t *testing.T) {
	body := `{"machines":{"north":{"host":"north.example"}},"roles":{"coord/lead":{"machine":"north","workspace":"hq","tab":"lead","cwd":"/work"}}}`
	path := writeSpec(t, body)
	before, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	match := func(orgctl.Spec) (orgctl.Live, error) {
		return orgctl.Live{
			Local:    "north",
			Tabs:     []orgctl.LiveTab{{Workspace: "hq", Label: "lead"}},
			Names:    []orgctl.LiveName{{Name: "coord/lead"}},
			Agents:   []orgctl.LiveAgent{{Name: "coord/lead", Cwd: "/work"}},
			CronRead: []string{"/work"},
		}, nil
	}
	var stdout, stderr bytes.Buffer
	if code := runIO([]string{"plan", path}, &stdout, &stderr, match); code != 0 {
		t.Fatalf("clean exit %d\n%s%s", code, stdout.String(), stderr.String())
	}
	stdout.Reset()
	missing := func(orgctl.Spec) (orgctl.Live, error) {
		return orgctl.Live{Local: "north"}, nil
	}
	code := runIO([]string{"plan", "--machine", "north", path}, &stdout, &stderr, missing)
	if code != 2 || !strings.Contains(stdout.String(), "+ coord/lead tab hq/lead") {
		t.Fatalf("changes exit %d\n%s", code, stdout.String())
	}
	stdout.Reset()
	remote := func(orgctl.Spec) (orgctl.Live, error) {
		return orgctl.Live{Local: "south"}, nil
	}
	code = runIO([]string{"plan", "--role", "coord/lead", path}, &stdout, &stderr, remote)
	if code != 2 || !strings.Contains(stdout.String(), "remote: not read") {
		t.Fatalf("remote exit %d\n%s", code, stdout.String())
	}
	stdout.Reset()
	code = runIO([]string{"plan", "--machine", "south", path}, &stdout, &stderr, missing)
	if code != 0 || stdout.Len() != 0 {
		t.Fatalf("filtered exit %d\n%s", code, stdout.String())
	}
	code = runIO([]string{"plan", filepath.Join(t.TempDir(), "missing.json")}, &stdout, &stderr, match)
	if code != 1 {
		t.Fatalf("missing spec exit %d", code)
	}
	bad := writeSpec(t, `{`)
	if code := runIO([]string{"plan", bad}, &stdout, &stderr, match); code != 1 {
		t.Fatalf("bad spec exit %d", code)
	}
	after, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if string(after) != string(before) {
		t.Fatal("plan wrote the spec")
	}
}

func TestDriftExitCodes(t *testing.T) {
	path := writeSpec(t, `{"roles":{"coord/lead":{"workspace":"hq","tab":"lead","cwd":"/work"}}}`)
	var stdout, stderr bytes.Buffer
	clean := func(orgctl.Spec) (orgctl.Live, error) {
		return orgctl.Live{
			Tabs:  []orgctl.LiveTab{{Workspace: "hq", Label: "lead"}},
			Names: []orgctl.LiveName{{Name: "coord/lead"}},
		}, nil
	}
	if code := runIO([]string{"drift", path}, &stdout, &stderr, clean); code != 0 {
		t.Fatalf("clean exit %d\n%s%s", code, stdout.String(), stderr.String())
	}
	extra := func(orgctl.Spec) (orgctl.Live, error) {
		return orgctl.Live{Tabs: []orgctl.LiveTab{{Workspace: "hq", Label: "stray"}}}, nil
	}
	stdout.Reset()
	code := runIO([]string{"drift", path}, &stdout, &stderr, extra)
	if code != 2 || !strings.Contains(stdout.String(), "USER: close tab stray in hq") {
		t.Fatalf("drift exit %d\n%s", code, stdout.String())
	}
}
