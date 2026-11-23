package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

func setupTestCLI(t *testing.T) (string, map[string]string) {
	t.Helper()
	dir := t.TempDir()
	cfgPath := filepath.Join(dir, "config.toml")
	workDB := filepath.Join(dir, "vaults", "work.db")
	personalDB := filepath.Join(dir, "vaults", "personal.db")

	cfgText := fmt.Sprintf(`
[vaults.work]
path = %q
residency = "local-only"

[vaults.personal]
path = %q
residency = "local-only"

[profiles.admin]
vaults = ["work", "personal"]
compartments = ["general", "engineering", "finance", "personal"]
max_sensitivity = "restricted"
capabilities = ["read", "write", "write-episodic", "export"]

[profiles.ops]
vaults = ["work"]
compartments = ["general"]
max_sensitivity = "normal"
capabilities = ["admin"]

[profiles.work]
vaults = ["work"]
compartments = ["engineering"]
max_sensitivity = "normal"
capabilities = ["read", "write"]
`, workDB, personalDB)

	if err := os.WriteFile(cfgPath, []byte(cfgText), 0o600); err != nil {
		t.Fatalf("write config: %v", err)
	}
	env := map[string]string{
		"MEMSTORE_CONFIG":  cfgPath,
		"MEMSTORE_PROFILE": "admin",
		"MEMSTORE_CALLER":  "test-cli",
	}
	return cfgPath, env
}

func runCLI(env map[string]string, args ...string) (int, string, string) {
	var stdout, stderr bytes.Buffer
	a := &app{
		stdout: &stdout,
		stderr: &stderr,
		getenv: func(k string) string { return env[k] },
		now:    func() time.Time { return time.Date(2026, 6, 1, 12, 0, 0, 0, time.UTC) },
	}
	code := a.run(args)
	return code, stdout.String(), stderr.String()
}

func TestCLIEndToEndAndExitCodes(t *testing.T) {
	_, env := setupTestCLI(t)

	// 1. version --json and doctor --offline --json
	code, out, errOut := runCLI(env, "version", "--json")
	if code != exitOK {
		t.Fatalf("version --json exit=%d, stderr=%s", code, errOut)
	}
	var ver map[string]any
	if err := json.Unmarshal([]byte(out), &ver); err != nil {
		t.Fatalf("unmarshal version json: %v (%s)", err, out)
	}

	code, out, errOut = runCLI(env, "doctor", "--offline", "--json")
	if code != exitOK && code != exitError {
		t.Fatalf("doctor --offline --json unexpected exit=%d, stderr=%s", code, errOut)
	}
	var doc map[string]any
	if err := json.Unmarshal([]byte(out), &doc); err != nil {
		t.Fatalf("unmarshal doctor json: %v (%s)", err, out)
	}
	if _, ok := doc["status"]; !ok {
		t.Errorf("unexpected doctor output: %s", out)
	}

	// 2. init --vault
	if code, _, errOut := runCLI(env, "init", "--vault", "work"); code != exitOK {
		t.Fatalf("init work exit=%d, stderr=%s", code, errOut)
	}
	if code, _, errOut := runCLI(env, "init", "--vault", "personal"); code != exitOK {
		t.Fatalf("init personal exit=%d, stderr=%s", code, errOut)
	}

	// 3. profile check (exit 0 for valid, exit 2 for unknown)
	code, out, errOut = runCLI(env, "profile", "check", "work", "--json")
	if code != exitOK {
		t.Fatalf("profile check work exit=%d, stderr=%s", code, errOut)
	}
	if !strings.Contains(out, `"maxSensitivity": "normal"`) {
		t.Errorf("unexpected profile check output: %s", out)
	}
	if code, _, _ := runCLI(env, "profile", "check", "missing"); code != exitUsage {
		t.Errorf("profile check missing exit=%d, want %d", code, exitUsage)
	}

	// 4. entity put & get
	code, _, errOut = runCLI(env, "entity", "put", "person", "ada",
		"--vault", "work", "--title", "Ada Lovelace",
		"--alias", "Countess", "--compartment", "engineering",
		"--summary", "Analytical engine pioneer", "--pin-l1", "--json")
	if code != exitOK {
		t.Fatalf("entity put ada exit=%d, stderr=%s", code, errOut)
	}

	code, out, errOut = runCLI(env, "entity", "get", "person:ada", "--json")
	if code != exitOK {
		t.Fatalf("entity get ada exit=%d, stderr=%s", code, errOut)
	}
	var view memstore.EntityView
	if err := json.Unmarshal([]byte(out), &view); err != nil {
		t.Fatalf("unmarshal entity view: %v", err)
	}
	if view.Entity.Title != "Ada Lovelace" || !view.Entity.PinnedL1 {
		t.Errorf("unexpected entity view: %+v", view.Entity)
	}

	// 5. resolve (0 ok, 2 not-found, 3 ambiguous)
	if code, _, errOut := runCLI(env, "resolve", "Countess", "--json"); code != exitOK {
		t.Fatalf("resolve Countess exit=%d, stderr=%s", code, errOut)
	}
	if code, _, _ := runCLI(env, "resolve", "nobody"); code != exitUsage {
		t.Errorf("resolve nobody exit=%d, want %d", code, exitUsage)
	}

	// Add a second entity with matching prefix "ada" to trigger exit 3 (ambiguous).
	if code, _, errOut := runCLI(env, "entity", "put", "person", "adam",
		"--vault", "work", "--title", "Adam Smith", "--compartment", "engineering"); code != exitOK {
		t.Fatalf("entity put adam exit=%d, stderr=%s", code, errOut)
	}
	if code, _, _ := runCLI(env, "resolve", "ada"); code != exitOK {
		t.Errorf("exact slug resolve ada exit=%d, want %d", code, exitOK)
	}
	if code, _, _ := runCLI(env, "resolve", "ad"); code != exitAmbiguous {
		t.Errorf("ambiguous prefix resolve ad exit=%d, want %d", code, exitAmbiguous)
	}

	// 6. fact set & list
	if code, _, errOut := runCLI(env, "fact", "set", "person:ada", "role", "Principal Mathematician",
		"--vault", "work", "--compartment", "engineering", "--valid-until", "2030-01-01", "--json"); code != exitOK {
		t.Fatalf("fact set exit=%d, stderr=%s", code, errOut)
	}
	code, out, errOut = runCLI(env, "fact", "list", "person:ada", "--json")
	if code != exitOK {
		t.Fatalf("fact list exit=%d, stderr=%s", code, errOut)
	}
	var facts []memstore.Fact
	if err := json.Unmarshal([]byte(out), &facts); err != nil || len(facts) != 1 {
		t.Fatalf("unexpected fact list (%v): %s", err, out)
	}

	// 7. note set (--body and --file) & list
	if code, _, errOut := runCLI(env, "note", "set", "person:ada", "bio",
		"--vault", "work", "--heading", "Biography", "--body", "Wrote first computer program.", "--compartment", "engineering"); code != exitOK {
		t.Fatalf("note set --body exit=%d, stderr=%s", code, errOut)
	}
	noteFile := filepath.Join(t.TempDir(), "note.md")
	if err := os.WriteFile(noteFile, []byte("Updated via file: bernoulli_note_token."), 0o600); err != nil {
		t.Fatal(err)
	}
	if code, _, errOut := runCLI(env, "note", "set", "person:ada", "notes",
		"--vault", "work", "--heading", "Research", "--file", noteFile, "--order", "200", "--compartment", "engineering"); code != exitOK {
		t.Fatalf("note set --file exit=%d, stderr=%s", code, errOut)
	}
	code, out, errOut = runCLI(env, "note", "list", "person:ada", "--json")
	if code != exitOK {
		t.Fatalf("note list exit=%d, stderr=%s", code, errOut)
	}
	var notes []memstore.NoteBlock
	if err := json.Unmarshal([]byte(out), &notes); err != nil || len(notes) != 2 {
		t.Fatalf("unexpected note list (%v): %s", err, out)
	}

	// 8. event add & list
	if code, _, errOut := runCLI(env, "event", "add", "--vault", "work", "--channel", "slack",
		"--entity", "person:ada", "--date", "2026-05-15", "--source-ref", "s-1",
		"--compartment", "engineering", "--summary", "Shipped analytical_engine_v2"); code != exitOK {
		t.Fatalf("event add exit=%d, stderr=%s", code, errOut)
	}
	code, out, errOut = runCLI(env, "event", "list", "--entity", "person:ada", "--since", "2026-05-01", "--limit", "5", "--json")
	if code != exitOK {
		t.Fatalf("event list exit=%d, stderr=%s", code, errOut)
	}
	var events []memstore.EpisodicEvent
	if err := json.Unmarshal([]byte(out), &events); err != nil || len(events) != 1 {
		t.Fatalf("unexpected event list (%v): %s", err, out)
	}

	// 9. search
	code, out, errOut = runCLI(env, "search", "bernoulli_note_token", "--kind", "note", "--limit", "5", "--json")
	if code != exitOK {
		t.Fatalf("search exit=%d, stderr=%s", code, errOut)
	}
	var hits []memstore.SearchHit
	if err := json.Unmarshal([]byte(out), &hits); err != nil || len(hits) != 1 {
		t.Fatalf("unexpected search hits (%v): %s", err, out)
	}

	// Policy denial returns exitUsage (2).
	if code, _, _ := runCLI(env, "--profile", "work", "entity", "put", "person", "secret",
		"--vault", "personal", "--title", "Secret"); code != exitUsage {
		t.Errorf("policy denied exit=%d, want %d", code, exitUsage)
	}

	// 10. log
	code, out, errOut = runCLI(env, "log", "--vault", "work", "--caller", "test-cli", "--json")
	if code != exitOK {
		t.Fatalf("log exit=%d, stderr=%s", code, errOut)
	}
	var logs []memstore.AccessLogEntry
	if err := json.Unmarshal([]byte(out), &logs); err != nil || len(logs) == 0 {
		t.Fatalf("unexpected access log (%v): %s", err, out)
	}

	// 11. exitError (1) on bad config syntax and exitUsage (2) on unknown command.
	badCfg := filepath.Join(t.TempDir(), "bad.toml")
	if err := os.WriteFile(badCfg, []byte("not = [valid toml"), 0o600); err != nil {
		t.Fatal(err)
	}
	if code, _, _ := runCLI(map[string]string{"MEMSTORE_CONFIG": badCfg}, "init", "--vault", "work"); code != exitError {
		t.Errorf("bad config exit=%d, want %d", code, exitError)
	}
	if code, _, _ := runCLI(env, "no-such-command"); code != exitUsage {
		t.Errorf("unknown command exit=%d, want %d", code, exitUsage)
	}
}

func TestCLIExportCommand(t *testing.T) {
	dir := t.TempDir()
	cfgPath := filepath.Join(dir, "config.toml")
	workDB := filepath.Join(dir, "vaults", "work.db")
	cfgText := fmt.Sprintf(`
[vaults.work]
path = %q
residency = "local-only"

[profiles.exporter]
vaults = ["work"]
compartments = ["engineering"]
max_sensitivity = "normal"
capabilities = ["read", "write", "export"]

[profiles.reader]
vaults = ["work"]
compartments = ["engineering"]
max_sensitivity = "normal"
capabilities = ["read"]
`, workDB)
	if err := os.WriteFile(cfgPath, []byte(cfgText), 0o600); err != nil {
		t.Fatalf("write config: %v", err)
	}
	env := map[string]string{
		"MEMSTORE_CONFIG": cfgPath,
		"MEMSTORE_CALLER": "cli-export-test",
	}

	if code, _, errOut := runCLI(env, "init", "--vault", "work"); code != exitOK {
		t.Fatalf("init work exit=%d, stderr=%s", code, errOut)
	}
	if code, _, errOut := runCLI(env, "entity", "put", "person", "ada",
		"--profile", "exporter", "--vault", "work", "--title", "Ada Lovelace",
		"--compartment", "engineering", "--pin-l1"); code != exitOK {
		t.Fatalf("entity put exit=%d, stderr=%s", code, errOut)
	}

	// Missing export capability -> exitUsage (2).
	outDir := filepath.Join(dir, "exported")
	if code, _, _ := runCLI(env, "export", "--profile", "reader", "--out", outDir); code != exitUsage {
		t.Errorf("export without capability exit=%d, want %d", code, exitUsage)
	}

	// Git work-tree refusal -> exitError (1), --allow-git -> exitOK (0).
	gitDir := filepath.Join(dir, "repo")
	if err := os.MkdirAll(filepath.Join(gitDir, ".git"), 0o700); err != nil {
		t.Fatal(err)
	}
	if code, _, _ := runCLI(env, "export", "--profile", "exporter", "--out", filepath.Join(gitDir, "mem")); code != exitError {
		t.Errorf("export into git dir exit=%d, want %d", code, exitError)
	}

	// First export --json.
	code, out, errOut := runCLI(env, "export", "--profile", "exporter", "--out", outDir, "--by", "tester", "--reason", "cli-test", "--json")
	if code != exitOK {
		t.Fatalf("export --json exit=%d, stderr=%s", code, errOut)
	}
	var rep memstore.ExportReport
	if err := json.Unmarshal([]byte(out), &rep); err != nil {
		t.Fatalf("unmarshal export report: %v (%s)", err, out)
	}
	if rep.Written == 0 || rep.Removed != 0 {
		t.Errorf("unexpected first export report: %+v", rep)
	}

	// Second export (text output) -> written=0.
	code, out, errOut = runCLI(env, "export", "--profile", "exporter", "--out", outDir)
	if code != exitOK {
		t.Fatalf("second export exit=%d, stderr=%s", code, errOut)
	}
	if !strings.Contains(out, "written=0") || !strings.Contains(out, "removed=0") {
		t.Errorf("unexpected second export output: %q", out)
	}
}

func TestCLIImportAsideAndTriage(t *testing.T) {
	_, env := setupTestCLI(t)

	if code, _, errOut := runCLI(env, "init", "--vault", "work"); code != exitOK {
		t.Fatalf("init work exit=%d, stderr=%s", code, errOut)
	}
	if code, _, errOut := runCLI(env, "init", "--vault", "personal"); code != exitOK {
		t.Fatalf("init personal exit=%d, stderr=%s", code, errOut)
	}

	srcDir := t.TempDir()
	for rel, content := range map[string]string{
		"people/bob.md":    "---\ntitle: Bob Builder\naliases:\n  - Bob\n---\n\n# Bob Builder\n\n## Structured Attributes\n\n- **role**: Architect\n\n## Notes\n\nBuilds systems.\n",
		"sites/k8s.md":     "---\ntitle: K8s Prod\n---\n\n# K8s Prod\n\n## Info\n\nPrimary cluster.\n",
		"concepts/skip.md": "---\ntitle: Skip Me\n---\n\n# Skip Me\n",
	} {
		p := filepath.Join(srcDir, filepath.FromSlash(rel))
		if err := os.MkdirAll(filepath.Dir(p), 0o700); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(p, []byte(content), 0o600); err != nil {
			t.Fatal(err)
		}
	}

	rulesPath := filepath.Join(t.TempDir(), "rules.toml")
	if err := os.WriteFile(rulesPath, []byte("[[rule]]\nglob = \"sites/*\"\nvault = \"work\"\ncompartment = \"engineering\"\nsensitivity = \"normal\"\nmode = \"direct\"\n"), 0o600); err != nil {
		t.Fatal(err)
	}

	// 1. Dry-run.
	code, out, errOut := runCLI(env, "import-aside", "--src", srcDir, "--vault", "personal", "--rules", rulesPath, "--dry-run", "--json")
	if code != exitOK {
		t.Fatalf("import-aside --dry-run exit=%d, stderr=%s", code, errOut)
	}
	var dryRep memstore.ImportReport
	if err := json.Unmarshal([]byte(out), &dryRep); err != nil {
		t.Fatalf("unmarshal dry-run report: %v (%s)", err, out)
	}
	if dryRep.Entities != 3 || dryRep.DirectWrites != 1 || dryRep.TriageStaged != 2 {
		t.Fatalf("unexpected dry-run report: %+v", dryRep)
	}

	// 2. Real import.
	code, out, errOut = runCLI(env, "import-aside", "--src", srcDir, "--vault", "personal", "--rules", rulesPath, "--json")
	if code != exitOK {
		t.Fatalf("import-aside exit=%d, stderr=%s", code, errOut)
	}
	var rep memstore.ImportReport
	if err := json.Unmarshal([]byte(out), &rep); err != nil {
		t.Fatalf("unmarshal import report: %v (%s)", err, out)
	}
	if rep.TriageStaged != 2 || len(rep.TriageIDs) != 2 {
		t.Fatalf("unexpected import report: %+v", rep)
	}

	// 3. List pending triage items.
	code, out, errOut = runCLI(env, "triage", "list", "--status", "pending", "--json")
	if code != exitOK {
		t.Fatalf("triage list exit=%d, stderr=%s", code, errOut)
	}
	var items []memstore.TriageItem
	if err := json.Unmarshal([]byte(out), &items); err != nil {
		t.Fatalf("unmarshal triage list: %v (%s)", err, out)
	}
	if len(items) != 2 {
		t.Fatalf("expected 2 pending triage items, got %d", len(items))
	}

	var bobID, skipID string
	for _, it := range items {
		switch it.TargetEntityID {
		case "person:bob":
			bobID = it.ID
		case "concept:skip":
			skipID = it.ID
		}
	}
	if bobID == "" || skipID == "" {
		t.Fatalf("missing expected triage items: %+v", items)
	}

	// 4. Approve person:bob with overrides and reject concept:skip.
	if code, _, errOut := runCLI(env, "triage", "approve", bobID, "--compartment", "engineering", "--sensitivity", "normal", "--json"); code != exitOK {
		t.Fatalf("triage approve exit=%d, stderr=%s", code, errOut)
	}
	if code, _, errOut := runCLI(env, "triage", "reject", skipID, "--json"); code != exitOK {
		t.Fatalf("triage reject exit=%d, stderr=%s", code, errOut)
	}

	// 5. Verify person:bob is now accessible and concept:skip is not.
	if code, _, errOut := runCLI(env, "entity", "get", "person:bob", "--vault", "personal", "--json"); code != exitOK {
		t.Fatalf("entity get person:bob after approve exit=%d, stderr=%s", code, errOut)
	}
	if code, _, _ := runCLI(env, "entity", "get", "concept:skip", "--vault", "personal"); code != exitUsage {
		t.Errorf("entity get rejected concept:skip exit=%d, want %d", code, exitUsage)
	}
}

func TestCLISyncCheckRoundTrip(t *testing.T) {
	_, env := setupTestCLI(t)

	if code, _, errOut := runCLI(env, "init", "--vault", "work"); code != exitOK {
		t.Fatalf("init work exit=%d, stderr=%s", code, errOut)
	}
	if code, _, errOut := runCLI(env, "init", "--vault", "personal"); code != exitOK {
		t.Fatalf("init personal exit=%d, stderr=%s", code, errOut)
	}
	if code, _, errOut := runCLI(env, "entity", "put", "person", "ada",
		"--vault", "work", "--title", "Ada Lovelace",
		"--compartment", "engineering", "--json"); code != exitOK {
		t.Fatalf("entity put ada exit=%d, stderr=%s", code, errOut)
	}
	if code, _, errOut := runCLI(env, "note", "set", "person:ada", "bio",
		"--vault", "work", "--heading", "Bio", "--body", "Original bio.",
		"--compartment", "engineering", "--json"); code != exitOK {
		t.Fatalf("note set exit=%d, stderr=%s", code, errOut)
	}

	exportDir := filepath.Join(t.TempDir(), "out")
	if code, _, errOut := runCLI(env, "export", "--profile", "admin", "--out", exportDir, "--json"); code != exitOK {
		t.Fatalf("export exit=%d, stderr=%s", code, errOut)
	}

	// Clean sync-check.
	code, out, errOut := runCLI(env, "sync-check", "--profile", "admin", "--dir", exportDir, "--json")
	if code != exitOK {
		t.Fatalf("sync-check clean exit=%d, stderr=%s", code, errOut)
	}
	var cleanRep memstore.SyncCheckReport
	if err := json.Unmarshal([]byte(out), &cleanRep); err != nil {
		t.Fatalf("unmarshal clean sync-check: %v (%s)", err, out)
	}
	if len(cleanRep.Modified) != 0 || len(cleanRep.Added) != 0 || len(cleanRep.Deleted) != 0 {
		t.Fatalf("expected clean sync-check, got %+v", cleanRep)
	}

	// Edit exported Markdown externally.
	adaPath := filepath.Join(exportDir, "people", "ada.md")
	data, err := os.ReadFile(adaPath)
	if err != nil {
		t.Fatalf("read ada.md: %v", err)
	}
	updated := strings.Replace(string(data), "Original bio.", "Edited externally.", 1)
	if err := os.WriteFile(adaPath, []byte(updated), 0o600); err != nil {
		t.Fatalf("write ada.md: %v", err)
	}

	// Stage via CLI.
	code, out, errOut = runCLI(env, "sync-check", "--profile", "admin", "--dir", exportDir, "--stage", "--by", "editor", "--reason", "manual-fix")
	if code != exitOK {
		t.Fatalf("sync-check --stage exit=%d, stderr=%s", code, errOut)
	}
	if !strings.Contains(out, "modified=1") || !strings.Contains(out, "staged=1") {
		t.Errorf("unexpected human sync-check output: %s", out)
	}

	// Re-run --stage: idempotent.
	code, out, errOut = runCLI(env, "sync-check", "--profile", "admin", "--dir", exportDir, "--stage", "--json")
	if code != exitOK {
		t.Fatalf("sync-check --stage 2nd exit=%d, stderr=%s", code, errOut)
	}
	var secondRep memstore.SyncCheckReport
	if err := json.Unmarshal([]byte(out), &secondRep); err != nil {
		t.Fatalf("unmarshal 2nd sync-check: %v (%s)", err, out)
	}
	if secondRep.Staged != 0 || secondRep.Skipped != 1 || len(secondRep.Changes) != 1 {
		t.Fatalf("expected idempotent 2nd sync-check, got %+v", secondRep)
	}

	// Approve staged triage item, re-export, and confirm clean sync-check.
	if code, _, errOut := runCLI(env, "triage", "approve", secondRep.Changes[0].TriageID, "--vault", "work", "--json"); code != exitOK {
		t.Fatalf("triage approve exit=%d, stderr=%s", code, errOut)
	}
	if code, _, errOut := runCLI(env, "export", "--profile", "admin", "--out", exportDir, "--json"); code != exitOK {
		t.Fatalf("re-export exit=%d, stderr=%s", code, errOut)
	}
	code, out, errOut = runCLI(env, "sync-check", "--profile", "admin", "--dir", exportDir, "--json")
	if code != exitOK {
		t.Fatalf("final sync-check exit=%d, stderr=%s", code, errOut)
	}
	var finalRep memstore.SyncCheckReport
	if err := json.Unmarshal([]byte(out), &finalRep); err != nil {
		t.Fatalf("unmarshal final sync-check: %v (%s)", err, out)
	}
	if len(finalRep.Modified) != 0 || len(finalRep.Added) != 0 || len(finalRep.Deleted) != 0 {
		t.Errorf("expected clean final sync-check, got %+v", finalRep)
	}
}

func TestCLIAgeAndBackup(t *testing.T) {
	_, env := setupTestCLI(t)
	for _, name := range []string{"work", "personal"} {
		if code, _, errOut := runCLI(env, "init", "--vault", name); code != exitOK {
			t.Fatalf("init: %d %s", code, errOut)
		}
	}
	code, _, errOut := runCLI(env, "event", "add", "--vault", "work", "--channel", "slack", "--summary", "old", "--date", "2020-01-02")
	if code != exitOK {
		t.Fatalf("event add: %d %s", code, errOut)
	}

	// admin capability is required for both maintenance commands.
	if code, _, _ := runCLI(env, "age", "--vault", "work"); code != exitUsage {
		t.Errorf("age without admin exit=%d, want %d", code, exitUsage)
	}
	out := filepath.Join(t.TempDir(), "snap.db")
	if code, _, _ := runCLI(env, "backup", "--vault", "work", "--out", out); code != exitUsage {
		t.Errorf("backup without admin exit=%d, want %d", code, exitUsage)
	}

	env["MEMSTORE_PROFILE"] = "ops"
	var res memstore.AgeResult
	code, stdout, errOut := runCLI(env, "age", "--vault", "work", "--retention-days", "30", "--dry-run", "--json")
	if code != exitOK {
		t.Fatalf("age dry-run: %d %s", code, errOut)
	}
	if err := json.Unmarshal([]byte(stdout), &res); err != nil || res.CompactedEvents != 1 {
		t.Fatalf("dry-run result %q: %+v %v", stdout, res, err)
	}
	for i, want := range []int{1, 0} {
		code, stdout, errOut = runCLI(env, "age", "--vault", "work", "--retention-days", "30", "--json")
		if code != exitOK {
			t.Fatalf("age run %d: %d %s", i, code, errOut)
		}
		res = memstore.AgeResult{}
		if err := json.Unmarshal([]byte(stdout), &res); err != nil || res.CompactedEvents != want {
			t.Errorf("run %d result %q: %+v %v", i, stdout, res, err)
		}
	}

	if code, _, _ := runCLI(env, "backup", "--vault", "work", "--out", out, "--require-encrypted"); code != exitError {
		t.Errorf("require-encrypted without cmd exit=%d, want %d", code, exitError)
	}
	code, stdout, errOut = runCLI(env, "backup", "--vault", "work", "--out", out, "--json")
	if code != exitOK {
		t.Fatalf("backup: %d %s", code, errOut)
	}
	var rep memstore.BackupReport
	if err := json.Unmarshal([]byte(stdout), &rep); err != nil || rep.Path != out || rep.Encrypted {
		t.Errorf("backup report %q: %+v %v", stdout, rep, err)
	}
	if fi, err := os.Stat(out); err != nil || fi.Mode().Perm() != 0o600 {
		t.Errorf("backup file: %v %v", fi, err)
	}
}
