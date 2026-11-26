package main

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func TestPanelCLISetListRemoveAndValidation(t *testing.T) {
	useState(t)

	// Bad id is refused.
	if _, stderr, code := runCLI("panel", "set", "Bad_ID", "--title", "T", "--kind", "kv", "--data", `{"a":1}`); code != 2 || !strings.Contains(stderr, "[a-z0-9-]+") {
		t.Fatalf("bad id = (%d, %q)", code, stderr)
	}

	// Bad JSON is refused.
	if _, stderr, code := runCLI("panel", "set", "ci-status", "--title", "T", "--kind", "kv", "--data", `{bad`); code != 2 || !strings.Contains(stderr, "JSON") {
		t.Fatalf("bad json = (%d, %q)", code, stderr)
	}

	// Both --data and --file is refused.
	f := filepath.Join(t.TempDir(), "data.json")
	if err := os.WriteFile(f, []byte(`{"x":1}`), 0o600); err != nil {
		t.Fatal(err)
	}
	if _, stderr, code := runCLI("panel", "set", "ci-status", "--title", "T", "--kind", "kv", "--data", `{"x":1}`, "--file", f); code != 2 || !strings.Contains(stderr, "exactly one") {
		t.Fatalf("both data and file = (%d, %q)", code, stderr)
	}

	// Round-trip via --data.
	stdout, stderr, code := runCLI("panel", "set", "ci-status", "--title", "CI Status", "--kind", "kv", "--data", `{"ok":true}`, "--stale-after", "5m", "--source", "ci-bot")
	if code != 0 {
		t.Fatalf("panel set = (%d, %q, %q)", code, stdout, stderr)
	}

	// Round-trip via --file.
	stdout, stderr, code = runCLI("panel", "set", "deploy-1", "--title", "Deploy", "--kind", "table", "--file", f, "--source", "deployer")
	if code != 0 {
		t.Fatalf("panel set --file = (%d, %q, %q)", code, stdout, stderr)
	}

	stdout, stderr, code = runCLI("panel", "ls", "--json")
	if code != 0 {
		t.Fatalf("panel ls --json = (%d, %q)", code, stderr)
	}
	var items []PanelListItem
	if err := json.Unmarshal([]byte(stdout), &items); err != nil {
		t.Fatalf("decode panel ls json: %v\n%s", err, stdout)
	}
	if len(items) != 2 || items[0].ID != "ci-status" || items[0].Title != "CI Status" || items[0].Kind != "kv" ||
		items[0].StaleAfter != "5m" || items[0].Source != "ci-bot" || items[0].Age == "" || items[0].Stale {
		t.Fatalf("items[0] = %+v", items)
	}
	if items[1].ID != "deploy-1" || items[1].Source != "deployer" {
		t.Fatalf("items[1] = %+v", items[1])
	}

	// Plain text ls includes id, title, kind, age.
	stdout, stderr, code = runCLI("panel", "ls")
	if code != 0 || !strings.Contains(stdout, "ci-status\tCI Status\tkv\t") {
		t.Fatalf("panel ls = (%d, %q, %q)", code, stdout, stderr)
	}

	// Remove panel.
	if stdout, stderr, code := runCLI("panel", "rm", "deploy-1"); code != 0 || !strings.Contains(stdout, "removed deploy-1") {
		t.Fatalf("panel rm = (%d, %q, %q)", code, stdout, stderr)
	}
	stdout, _, code = runCLI("panel", "ls", "--json")
	if code != 0 {
		t.Fatal(code)
	}
	if err := json.Unmarshal([]byte(stdout), &items); err != nil || len(items) != 1 || items[0].ID != "ci-status" {
		t.Fatalf("after rm = %+v, err=%v", items, err)
	}
}

func TestPanelUnreadableAndDoctorReport(t *testing.T) {
	useState(t)
	dir, err := decide.DefaultDir()
	if err != nil {
		t.Fatal(err)
	}
	store, err := decide.OpenPanels(dir)
	if err != nil {
		t.Fatal(err)
	}
	// Stale panel.
	store.SetClock(func() time.Time { return time.Now().Add(-10 * time.Minute) })
	if _, err := store.Set(decide.Panel{
		ID:         "old-panel",
		Title:      "Old",
		Kind:       "kv",
		Data:       json.RawMessage(`{"ok":false}`),
		StaleAfter: "1m",
	}); err != nil {
		t.Fatal(err)
	}
	// Bad JSON panel.
	if err := os.WriteFile(filepath.Join(decide.PanelsDir(dir), "broken.json"), []byte("{not-json"), 0o600); err != nil {
		t.Fatal(err)
	}
	// Missing required field.
	if err := os.WriteFile(filepath.Join(decide.PanelsDir(dir), "missing.json"), []byte(`{"id":"missing","kind":"kv","data":{},"updatedAt":"2026-11-26T12:00:00Z"}`), 0o600); err != nil {
		t.Fatal(err)
	}

	stdout, stderr, code := runCLI("panel", "ls")
	if code != 0 {
		t.Fatalf("panel ls = (%d, %q)", code, stderr)
	}
	if !strings.Contains(stdout, "panel unreadable: broken.json") || !strings.Contains(stdout, "panel unreadable: missing.json") {
		t.Fatalf("panel ls missing unreadable lines:\n%s", stdout)
	}
	if !strings.Contains(stdout, "old-panel\tOld\tkv\t") || !strings.Contains(stdout, "\tstale") {
		t.Fatalf("panel ls missing stale marker:\n%s", stdout)
	}

	res := checkDecidePanels(context.Background(), true)
	if res.Status != toolcli.StatusFail {
		t.Fatalf("checkDecidePanels = %+v, want fail", res)
	}
	for _, want := range []string{"broken.json", "missing.json", "old-panel"} {
		if !strings.Contains(res.Detail, want) {
			t.Fatalf("doctor detail %q missing %q", res.Detail, want)
		}
	}

	// Remove bad panels so only stale remains -> StatusWarn.
	_ = os.Remove(filepath.Join(decide.PanelsDir(dir), "broken.json"))
	_ = os.Remove(filepath.Join(decide.PanelsDir(dir), "missing.json"))
	res = checkDecidePanels(context.Background(), true)
	if res.Status != toolcli.StatusWarn || !strings.Contains(res.Detail, "old-panel") {
		t.Fatalf("checkDecidePanels stale only = %+v", res)
	}
}

func TestHubPanelSetFileAndRemove(t *testing.T) {
	_, state := setupHub(t, `{"hub": "me@hub"}`)
	hubDir, _, _ := hubDecide(t, `{}`)

	localFile := filepath.Join(t.TempDir(), "local-panel.json")
	if err := os.WriteFile(localFile, []byte(`{"hello":"world"}`), 0o600); err != nil {
		t.Fatal(err)
	}

	var stdout, stderr bytes.Buffer
	code := run([]string{"panel", "set", "remote-1", "--title", "Remote", "--kind", "kv", "--file", localFile, "--source", "laptop"}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("hub panel set --file = (%d, %q, %q)", code, stdout.String(), stderr.String())
	}

	// Verify it was written on the hub and NOT locally.
	assertNoLocalState(t, state)

	stdout.Reset()
	stderr.Reset()
	code = run([]string{"panel", "ls", "--json"}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("hub panel ls = (%d, %q)", code, stderr.String())
	}
	var items []PanelListItem
	if err := json.Unmarshal(stdout.Bytes(), &items); err != nil || len(items) != 1 || items[0].ID != "remote-1" || items[0].Source != "laptop" {
		t.Fatalf("hub panel ls = %+v, err=%v", items, err)
	}

	// Remove via hub.
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"panel", "rm", "remote-1"}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("hub panel rm = (%d, %q, %q)", code, stdout.String(), stderr.String())
	}
	hubStore, err := decide.OpenPanels(hubDir)
	if err != nil {
		t.Fatal(err)
	}
	after, err := hubStore.List()
	if err != nil || len(after) != 0 {
		t.Fatalf("hub panels after rm = %+v, %v", after, err)
	}
	assertNoLocalState(t, state)
}

func TestHubPanelSetBadFileDoesNotCallSSH(t *testing.T) {
	fake, state := setupHub(t, `{"hub": "me@hub"}`)
	badFile := filepath.Join(t.TempDir(), "bad.json")
	if err := os.WriteFile(badFile, []byte(`{not-json`), 0o600); err != nil {
		t.Fatal(err)
	}
	var stderr bytes.Buffer
	code := run([]string{"panel", "set", "p1", "--title", "T", "--kind", "kv", "--file", badFile}, &bytes.Buffer{}, &stderr)
	if code != 2 || !strings.Contains(stderr.String(), "valid non-null JSON") {
		t.Fatalf("bad hub file = (%d, %q)", code, stderr.String())
	}
	if fake.Read("decide.args") != "" || fake.Read("argv") != "" {
		t.Fatalf("ssh ran on invalid local file: %q / %q", fake.Read("decide.args"), fake.Read("argv"))
	}
	assertNoLocalState(t, state)
}
