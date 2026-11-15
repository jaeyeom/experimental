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

func TestPlanUnmanagedBriefStaysClean(t *testing.T) {
	path := writeSpec(t, `{"roles":{"desk/clerk":{"machine":"local","workspace":"front","tab":"clerk","cwd":"/work/desk"}}}`)
	load := func(orgctl.Spec) (orgctl.Live, error) {
		return orgctl.Live{
			Local:    "local",
			Tabs:     []orgctl.LiveTab{{Workspace: "front", Label: "clerk"}},
			Names:    []orgctl.LiveName{{Name: "desk/clerk"}},
			Agents:   []orgctl.LiveAgent{{Name: "desk/clerk", Cwd: "/work/desk"}},
			CronRead: []string{"/work/desk"},
			Briefs:   map[string]string{"desk/clerk": orgctl.BriefUnmanaged},
		}, nil
	}
	var stdout, stderr bytes.Buffer
	code := runIO([]string{"plan", path}, &stdout, &stderr, load)
	if code != 0 || !strings.Contains(stdout.String(), "i desk/clerk brief unmanaged") {
		t.Fatalf("exit %d\n%s%s", code, stdout.String(), stderr.String())
	}
}

func TestCheckRejectsRelativeCwd(t *testing.T) {
	path := writeSpec(t, `{"roles":{"desk/clerk":{"cwd":"proj"}}}`)
	var stderr bytes.Buffer
	code := run([]string{"check", path}, &stderr)
	if code != 1 || !strings.Contains(stderr.String(), "role desk/clerk: cwd must be absolute or start with ~") {
		t.Fatalf("exit %d, stderr %s", code, stderr.String())
	}
	for _, cwd := range []string{"/proj", "~/proj"} {
		path = writeSpec(t, `{"roles":{"desk/clerk":{"cwd":"`+cwd+`"}}}`)
		stderr.Reset()
		if code := run([]string{"check", path}, &stderr); code != 0 {
			t.Fatalf("cwd %s exit %d, stderr %s", cwd, code, stderr.String())
		}
	}
}

func TestReadCronsExpandsHome(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	dir := filepath.Join(home, "proj", ".claude")
	if err := os.MkdirAll(dir, 0o700); err != nil {
		t.Fatal(err)
	}
	body := `{"tasks":[{"id":"daily","cron":"0 9 * * 1-5","prompt":"hi","createdAt":1761955200000}]}`
	if err := os.WriteFile(filepath.Join(dir, "scheduled_tasks.json"), []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	var live orgctl.Live
	readCrons(orgctl.Role{
		Cwd:       "~/proj",
		Schedules: []orgctl.Schedule{{ID: "daily", Cron: "0 9 * * 1-5", Durable: true}},
	}, &live)
	proj := filepath.Join(home, "proj")
	readCrons(orgctl.Role{Cwd: "~/proj"}, &live)
	if len(live.Crons) != 1 || live.Crons[0].Cwd != proj || live.Crons[0].ID != "daily" {
		t.Fatalf("crons = %#v", live.Crons)
	}
	if len(live.CronRead) != 1 || live.CronRead[0] != proj {
		t.Fatalf("read = %#v", live.CronRead)
	}

	live = orgctl.Live{}
	readCrons(orgctl.Role{
		Cwd:       "~/missing",
		Schedules: []orgctl.Schedule{{ID: "daily", Durable: true}},
	}, &live)
	if len(live.Crons) != 0 || len(live.CronRead) != 0 || len(live.MissingCwd) != 1 || live.MissingCwd[0] != filepath.Join(home, "missing") {
		t.Fatalf("missing cwd live = %#v", live)
	}
}

func TestCheckRejectsBadGlob(t *testing.T) {
	path := writeSpec(t, `{"pools":{"hands":{"namePattern":"["}}}`)
	var stderr bytes.Buffer
	code := run([]string{"check", path}, &stderr)
	if code != 1 || !strings.Contains(stderr.String(), `pool hands: namePattern "[":`) {
		t.Fatalf("exit %d, stderr %s", code, stderr.String())
	}
}

func TestCheckWarnsOutsideScope(t *testing.T) {
	path := writeSpec(t, `{"roles":{"desk/clerk":{"workspace":"misc"}},"pools":{"hands":{"workspace":"workers"}},"scope":{"workspaces":["org-a"]}}`)
	var stderr bytes.Buffer
	code := run([]string{"check", path}, &stderr)
	if code != 0 || !strings.Contains(stderr.String(), `warning: role desk/clerk workspace "misc" is outside the scope`) {
		t.Fatalf("exit %d, stderr %s", code, stderr.String())
	}
	if !strings.Contains(stderr.String(), `warning: pool hands workspace "workers" is outside the scope`) {
		t.Fatalf("stderr %s", stderr.String())
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
	if code != 2 || !strings.Contains(stdout.String(), "add to spec, or USER: close tab stray in hq if it is no longer needed") {
		t.Fatalf("drift exit %d\n%s", code, stdout.String())
	}
}

func TestImportRefusesOverwrite(t *testing.T) {
	dir := t.TempDir()
	out := filepath.Join(dir, "org.pkl")
	if err := os.WriteFile(out, []byte("keep\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	load := func(orgctl.Spec) (orgctl.Live, error) {
		return orgctl.Live{
			Names: []orgctl.LiveName{{Name: "coord/lead", When: "mornings", Server: "local"}},
			Agents: []orgctl.LiveAgent{{
				Name: "coord/lead", Machine: "local", Workspace: "hq", Tab: "lead",
				Cwd: "/work/lead", Kind: "coordinator",
			}},
		}, nil
	}
	var stdout, stderr bytes.Buffer
	code := runIO([]string{"import", "--out", out}, &stdout, &stderr, load)
	if code == 0 || !strings.Contains(stderr.String(), "overwrite") {
		t.Fatalf("refuse exit %d stderr %s", code, stderr.String())
	}
	body, err := os.ReadFile(out)
	if err != nil {
		t.Fatal(err)
	}
	if string(body) != "keep\n" {
		t.Fatalf("file changed without --force: %s", body)
	}
	stderr.Reset()
	code = runIO([]string{"import", "--out", out, "--force"}, &stdout, &stderr, load)
	if code != 0 {
		t.Fatalf("force exit %d stderr %s", code, stderr.String())
	}
	body, err = os.ReadFile(out)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(body), "coord/lead") || string(body) == "keep\n" {
		t.Fatalf("forced file = %s", body)
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 1 || entries[0].Name() != "org.pkl" {
		t.Fatalf("dir entries = %v", entries)
	}
}
