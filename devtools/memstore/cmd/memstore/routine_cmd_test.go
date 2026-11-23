package main

import (
	"encoding/json"
	"encoding/xml"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestCLIRoutineSubcommands(t *testing.T) {
	cfgPath, env := setupTestCLI(t)
	if code, _, errOut := runCLI(env, "init", "--vault", "work"); code != exitOK {
		t.Fatalf("init work exit=%d, stderr=%s", code, errOut)
	}

	dir := filepath.Dir(cfgPath)
	routinesDir := filepath.Join(dir, "routines")
	if err := os.MkdirAll(routinesDir, 0o700); err != nil {
		t.Fatalf("mkdir routines: %v", err)
	}
	handlerPath := filepath.Join(dir, "handler.sh")
	if err := os.WriteFile(handlerPath, []byte("#!/bin/sh\nprintf '{\"stateUpdates\":{\"cursor\":\"c1\"}}\\n'\n"), 0o700); err != nil {
		t.Fatalf("write handler: %v", err)
	}

	manifestTOML := "id = \"daily-sync\"\nvault = \"work\"\nprofile = \"work\"\nschedule = \"@every 1h\"\n[handler]\ncommand = [\"" + handlerPath + "\"]\noutput_format = \"json\"\n"
	if err := os.WriteFile(filepath.Join(routinesDir, "daily-sync.toml"), []byte(manifestTOML), 0o600); err != nil {
		t.Fatalf("write manifest: %v", err)
	}

	// 1. routine list --json
	code, out, errOut := runCLI(env, "routine", "list", "--json")
	if code != exitOK || !strings.Contains(out, `"ID": "daily-sync"`) {
		t.Fatalf("routine list exit=%d stderr=%s out=%s", code, errOut, out)
	}

	// 2. routine tick --dry-run --json
	code, out, errOut = runCLI(env, "routine", "tick", "--dry-run", "--json")
	if code != exitOK || !strings.Contains(out, `"dryRun": true`) {
		t.Fatalf("routine tick --dry-run exit=%d stderr=%s out=%s", code, errOut, out)
	}

	// 3. routine run daily-sync --json
	code, out, errOut = runCLI(env, "routine", "run", "daily-sync", "--json")
	if code != exitOK || !strings.Contains(out, `"status": "ok"`) {
		t.Fatalf("routine run exit=%d stderr=%s out=%s", code, errOut, out)
	}

	// 4. routine runs daily-sync --json
	code, out, errOut = runCLI(env, "routine", "runs", "daily-sync", "--json")
	if code != exitOK {
		t.Fatalf("routine runs exit=%d stderr=%s", code, errOut)
	}
	var runs []map[string]any
	if err := json.Unmarshal([]byte(out), &runs); err != nil || len(runs) != 1 {
		t.Fatalf("unexpected routine runs output (err=%v): %s", err, out)
	}

	// 5. routine status --json
	code, out, errOut = runCLI(env, "routine", "status", "--json")
	if code != exitOK || !strings.Contains(out, `"lastStatus": "ok"`) {
		t.Fatalf("routine status exit=%d stderr=%s out=%s", code, errOut, out)
	}

	// 6. routine state get/set
	code, out, errOut = runCLI(env, "routine", "state", "get", "daily-sync", "cursor")
	if code != exitOK || strings.TrimSpace(out) != `"c1"` {
		t.Fatalf("routine state get exit=%d stderr=%s out=%q", code, errOut, out)
	}
	code, _, errOut = runCLI(env, "routine", "state", "set", "daily-sync", "cursor", "c2")
	if code != exitOK {
		t.Fatalf("routine state set exit=%d stderr=%s", code, errOut)
	}
	code, out, errOut = runCLI(env, "routine", "state", "get", "daily-sync", "cursor")
	if code != exitOK || strings.TrimSpace(out) != `"c2"` {
		t.Fatalf("routine state get after set exit=%d stderr=%s out=%q", code, errOut, out)
	}

	// 7. routine launchd-plist
	code, out, errOut = runCLI(env, "routine", "launchd-plist", "--interval-seconds", "300")
	if code != exitOK {
		t.Fatalf("routine launchd-plist exit=%d stderr=%s", code, errOut)
	}
	var xmlDoc any
	if err := xml.Unmarshal([]byte(out), &xmlDoc); err != nil {
		t.Fatalf("invalid plist XML: %v\n%s", err, out)
	}
	if !strings.Contains(out, "<string>tick</string>") || !strings.Contains(out, "<integer>300</integer>") {
		t.Errorf("unexpected plist content:\n%s", out)
	}
}
