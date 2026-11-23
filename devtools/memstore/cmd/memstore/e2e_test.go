package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/memstore"
)

const readmePath = "../../README.org"

var generatedIDPattern = regexp.MustCompile(`\b(fact|note|evt|tri|run)_[0-9a-f]{8,}\b`)

// orgBlock returns the body of the block after "#+NAME: name" in doc.
func orgBlock(t *testing.T, doc, name string) string {
	t.Helper()
	i := strings.Index(doc, "#+NAME: "+name+"\n")
	if i < 0 {
		t.Fatalf("README has no block named %s", name)
	}
	start := strings.Index(doc[i:], "\n#+begin_") + i + 1
	start += strings.Index(doc[start:], "\n") + 1
	end := strings.Index(doc[start:], "#+end_") + start
	return doc[start:end]
}

// splitWords splits a command line like a POSIX shell for the simple cases
// the README uses: spaces, single quotes, and double quotes.
func splitWords(t *testing.T, line string) []string {
	t.Helper()
	var words []string
	var cur strings.Builder
	inWord := false
	var quote rune
	for _, r := range line {
		switch {
		case quote != 0 && r == quote:
			quote = 0
		case quote != 0:
			cur.WriteRune(r)
		case r == '\'' || r == '"':
			quote, inWord = r, true
		case r == ' ' || r == '\t':
			if inWord {
				words = append(words, cur.String())
				cur.Reset()
				inWord = false
			}
		default:
			cur.WriteRune(r)
			inWord = true
		}
	}
	if quote != 0 {
		t.Fatalf("unterminated quote in %q", line)
	}
	if inWord {
		words = append(words, cur.String())
	}
	return words
}

// normalizeWalkthroughOutput replaces temp paths and random hex IDs with
// deterministic placeholders matching README.org.
func normalizeWalkthroughOutput(raw, baseDir string) string {
	s := strings.ReplaceAll(raw, baseDir, "~/.local/share/memstore")
	seen := make(map[string]string)
	counts := make(map[string]int)
	return generatedIDPattern.ReplaceAllStringFunc(s, func(match string) string {
		if rep, ok := seen[match]; ok {
			return rep
		}
		prefix, _, _ := strings.Cut(match, "_")
		counts[prefix]++
		rep := fmt.Sprintf("%s_%04d", prefix, counts[prefix])
		seen[match] = rep
		return rep
	})
}

func setupWalkthroughEnv(t *testing.T) (string, map[string]string) {
	t.Helper()
	baseDir := t.TempDir()
	cfgPath := filepath.Join(baseDir, "config.toml")
	workDB := filepath.Join(baseDir, "vaults", "work.db")
	personalDB := filepath.Join(baseDir, "vaults", "personal.db")

	cfgText := fmt.Sprintf(`
retention_days = 365

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
capabilities = ["read", "write", "write-episodic", "export", "admin"]

[profiles.work]
vaults = ["work"]
compartments = ["general", "engineering"]
max_sensitivity = "normal"
capabilities = ["read", "write", "write-episodic", "export"]

[profiles.personal]
vaults = ["personal"]
compartments = ["general", "personal"]
max_sensitivity = "private"
capabilities = ["read", "write", "write-episodic"]
`, workDB, personalDB)
	if err := os.WriteFile(cfgPath, []byte(cfgText), 0o600); err != nil {
		t.Fatalf("write walkthrough config: %v", err)
	}

	routinesDir := filepath.Join(baseDir, "routines")
	if err := os.MkdirAll(routinesDir, 0o700); err != nil {
		t.Fatalf("mkdir routines: %v", err)
	}
	handlerPath := filepath.Join(baseDir, "daily-brief.sh")
	handlerScript := "#!/bin/sh\nprintf '{\"stateUpdates\":{\"cursor\":\"2026-06-01\"}}\\n'\n"
	if err := os.WriteFile(handlerPath, []byte(handlerScript), 0o700); err != nil {
		t.Fatalf("write walkthrough handler: %v", err)
	}
	manifestTOML := fmt.Sprintf("id = \"daily-brief\"\nvault = \"work\"\nprofile = \"work\"\nschedule = \"@every 24h\"\n[handler]\ncommand = [%q]\noutput_format = \"json\"\n", handlerPath)
	if err := os.WriteFile(filepath.Join(routinesDir, "daily-brief.toml"), []byte(manifestTOML), 0o600); err != nil {
		t.Fatalf("write walkthrough manifest: %v", err)
	}

	env := map[string]string{
		"MEMSTORE_CONFIG":  cfgPath,
		"MEMSTORE_PROFILE": "admin",
		"MEMSTORE_CALLER":  "walkthrough",
	}
	return baseDir, env
}

// TestReadmeWalkthrough runs every command of the README walkthrough against
// a fixed clock and compares the normalized transcript with walkthrough-output.
func TestReadmeWalkthrough(t *testing.T) {
	data, err := os.ReadFile(readmePath)
	if err != nil {
		t.Fatal(err)
	}
	doc := string(data)
	script := orgBlock(t, doc, "walkthrough")
	baseDir, env := setupWalkthroughEnv(t)

	var transcript strings.Builder
	for _, line := range strings.Split(script, "\n") {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		if strings.HasPrefix(line, "printf ") {
			target := filepath.Join(baseDir, "export", "people", "ada.md")
			f, err := os.OpenFile(target, os.O_APPEND|os.O_WRONLY, 0o600)
			if err != nil {
				t.Fatalf("open %s for walkthrough edit: %v", target, err)
			}
			if _, err := f.WriteString("\nUpdated bio from editor.\n"); err != nil {
				_ = f.Close()
				t.Fatalf("append walkthrough edit: %v", err)
			}
			_ = f.Close()
			continue
		}
		words := splitWords(t, line)
		if words[0] != "memstore" {
			t.Fatalf("walkthrough line is not a memstore command: %q", line)
		}
		args := make([]string, len(words)-1)
		for i, w := range words[1:] {
			w = strings.ReplaceAll(w, "~/.local/share/memstore", baseDir)
			if w == "tri_0001" {
				w = findSinglePendingTriageID(t, env)
			}
			args[i] = w
		}
		code, out, errOut := runCLI(env, args...)
		if code != exitOK {
			t.Fatalf("%s: exit %d\n%s", line, code, errOut)
		}
		transcript.WriteString("$ " + line + "\n" + out)
	}

	got := normalizeWalkthroughOutput(transcript.String(), baseDir)
	want := orgBlock(t, doc, "walkthrough-output")
	if got != want {
		t.Errorf("README walkthrough output mismatch\n--- got ---\n%s\n--- want ---\n%s", got, want)
	}
}

func findSinglePendingTriageID(t *testing.T, env map[string]string) string {
	t.Helper()
	code, out, errOut := runCLI(env, "triage", "list", "--status", "pending", "--json")
	if code != exitOK {
		t.Fatalf("triage list exit=%d stderr=%s", code, errOut)
	}
	var items []memstore.TriageItem
	if err := json.Unmarshal([]byte(out), &items); err != nil {
		t.Fatalf("unmarshal triage list: %v", err)
	}
	if len(items) == 0 {
		t.Fatal("expected at least one pending triage item")
	}
	return items[0].ID
}

func runCLIAt(env map[string]string, now time.Time, args ...string) (int, string, string) {
	var stdout, stderr bytes.Buffer
	a := &app{
		stdout: &stdout,
		stderr: &stderr,
		getenv: func(k string) string { return env[k] },
		now:    func() time.Time { return now },
	}
	code := a.run(args)
	return code, stdout.String(), stderr.String()
}

// TestEndToEndLifecycle exercises all memstore layers together: multi-vault
// init, Aside import + triage approval, cross-profile isolation across CLI/export/
// HTTP, Markdown export + sync-check + triage round-trip, routine tick with
// outbox, scoped inbox sync push/pull idempotency, and age TTL/rollup.
func TestEndToEndLifecycle(t *testing.T) {
	baseDir := t.TempDir()
	cfgPath := filepath.Join(baseDir, "config.toml")
	sharedDB := filepath.Join(baseDir, "vaults", "shared.db")
	workDB := filepath.Join(baseDir, "vaults", "work.db")
	personalDB := filepath.Join(baseDir, "vaults", "personal.db")

	cfgText := fmt.Sprintf(`
retention_days = 365

[vaults.shared]
path = %q
residency = "local-only"

[vaults.work]
path = %q
residency = "local-only"

[vaults.personal]
path = %q
residency = "local-only"

[profiles.admin]
vaults = ["shared", "work", "personal"]
compartments = ["general", "engineering", "finance", "personal"]
max_sensitivity = "restricted"
capabilities = ["read", "write", "write-episodic", "export", "admin"]

[profiles.work]
vaults = ["shared", "work"]
compartments = ["general", "engineering"]
max_sensitivity = "normal"
capabilities = ["read", "write", "write-episodic", "export"]

[profiles.personal]
vaults = ["shared", "personal"]
compartments = ["general", "personal"]
max_sensitivity = "private"
capabilities = ["read", "write", "write-episodic"]
`, sharedDB, workDB, personalDB)
	if err := os.WriteFile(cfgPath, []byte(cfgText), 0o600); err != nil {
		t.Fatalf("write config: %v", err)
	}
	env := map[string]string{
		"MEMSTORE_CONFIG":  cfgPath,
		"MEMSTORE_PROFILE": "admin",
		"MEMSTORE_CALLER":  "e2e-test",
	}
	baseNow := time.Date(2026, 6, 1, 12, 0, 0, 0, time.UTC)

	// 1. Init shared, work, and personal vaults.
	for _, v := range []string{"shared", "work", "personal"} {
		if code, _, errOut := runCLIAt(env, baseNow, "init", "--vault", v); code != exitOK {
			t.Fatalf("init %s failed (%d): %s", v, code, errOut)
		}
	}

	// 2. Import synthetic Aside fixture -> classify & triage approve.
	asideDir := filepath.Join(baseDir, "aside-src")
	if err := os.MkdirAll(filepath.Join(asideDir, "people"), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Join(asideDir, "sites"), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(asideDir, "sites", "prod.md"), []byte("---\ntitle: Prod\n---\n# Prod\n\n## Structured Attributes\n\n- **region**: us-west-2\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(asideDir, "people", "alice.md"), []byte("---\ntitle: Alice\n---\n# Alice\n\nPrivate note.\n\n## Background\n\nMet in 2025.\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	rulesPath := filepath.Join(baseDir, "rules.toml")
	rulesTOML := "[[rule]]\nglob = \"sites/*\"\nvault = \"work\"\ncompartment = \"engineering\"\nsensitivity = \"normal\"\nmode = \"direct\"\n"
	if err := os.WriteFile(rulesPath, []byte(rulesTOML), 0o600); err != nil {
		t.Fatal(err)
	}

	code, out, errOut := runCLIAt(env, baseNow, "import-aside", "--src", asideDir, "--vault", "personal", "--rules", rulesPath, "--json")
	if code != exitOK {
		t.Fatalf("import-aside failed (%d): %s", code, errOut)
	}
	var impRep memstore.ImportReport
	if err := json.Unmarshal([]byte(out), &impRep); err != nil {
		t.Fatalf("unmarshal import report: %v", err)
	}
	if impRep.DirectWrites == 0 || impRep.TriageStaged == 0 {
		t.Fatalf("expected direct writes and staged triage items, got %+v", impRep)
	}
	for _, tid := range impRep.TriageIDs {
		if code, _, errOut := runCLIAt(env, baseNow, "triage", "approve", tid); code != exitOK {
			t.Fatalf("triage approve %s failed (%d): %s", tid, code, errOut)
		}
	}

	// 3. Seed restricted fact in work & verify work profile isolation across CLI, export, and HTTP.
	if code, _, errOut := runCLIAt(env, baseNow, "fact", "set", "site:prod", "api_token", "secret-xyz",
		"--vault", "work", "--compartment", "engineering", "--sensitivity", "restricted"); code != exitOK {
		t.Fatalf("fact set restricted failed (%d): %s", code, errOut)
	}
	if code, _, errOut := runCLIAt(env, baseNow, "fact", "set", "site:prod", "temp_flag", "true",
		"--vault", "work", "--compartment", "engineering", "--sensitivity", "normal", "--valid-until", "2026-06-10"); code != exitOK {
		t.Fatalf("fact set ttl failed (%d): %s", code, errOut)
	}

	// entity get as work: cannot see personal entity person:alice, and cannot see restricted fact on site:prod.
	if code, _, _ := runCLIAt(env, baseNow, "--profile", "work", "entity", "get", "person:alice"); code != exitUsage {
		t.Errorf("work profile entity get person:alice exit=%d, want %d", code, exitUsage)
	}
	code, out, errOut = runCLIAt(env, baseNow, "--profile", "work", "entity", "get", "site:prod", "--json")
	if code != exitOK {
		t.Fatalf("work entity get site:prod failed (%d): %s", code, errOut)
	}
	if strings.Contains(out, "secret-xyz") {
		t.Errorf("work profile entity get leaked restricted fact: %s", out)
	}

	// search as work: must not see Alice or secret-xyz.
	code, out, errOut = runCLIAt(env, baseNow, "--profile", "work", "search", "Alice")
	if code != exitOK {
		t.Fatalf("work search Alice failed (%d): %s", code, errOut)
	}
	if strings.TrimSpace(out) != "" {
		t.Errorf("work profile search leaked personal entity: %s", out)
	}

	// export as work: must not contain Alice or secret-xyz.
	workExportDir := filepath.Join(baseDir, "work-export")
	if code, _, errOut := runCLIAt(env, baseNow, "export", "--profile", "work", "--out", workExportDir); code != exitOK {
		t.Fatalf("work export failed (%d): %s", code, errOut)
	}
	if _, err := os.Stat(filepath.Join(workExportDir, "people", "alice.md")); !errors.Is(err, fs.ErrNotExist) {
		t.Errorf("work export leaked personal entity file")
	}
	prodMD, err := os.ReadFile(filepath.Join(workExportDir, "sites", "prod.md"))
	if err != nil {
		t.Fatalf("read exported prod.md: %v", err)
	}
	if strings.Contains(string(prodMD), "secret-xyz") {
		t.Errorf("work export leaked restricted fact")
	}

	// serve http bound to work profile: verify HTTP isolation.
	cfg, err := memstore.LoadConfig(cfgPath, func(k string) string { return env[k] })
	if err != nil {
		t.Fatalf("LoadConfig: %v", err)
	}
	workSvc, err := memstore.OpenService(cfg, "work")
	if err != nil {
		t.Fatalf("OpenService(work): %v", err)
	}
	ts := httptest.NewServer(memstore.NewServer(workSvc, memstore.ServerConfig{}))
	resp, err := http.Get(ts.URL + "/v1/entities/person:alice")
	if err != nil {
		t.Fatalf("GET /v1/entities/person:alice: %v", err)
	}
	_ = resp.Body.Close()
	if resp.StatusCode != http.StatusNotFound {
		t.Errorf("HTTP work profile /v1/entities/person:alice status=%d, want 404", resp.StatusCode)
	}
	sResp, err := http.Get(ts.URL + "/v1/search?q=Alice")
	if err != nil {
		t.Fatalf("GET /v1/search: %v", err)
	}
	var httpHits []memstore.SearchHit
	if err := json.NewDecoder(sResp.Body).Decode(&httpHits); err != nil {
		t.Fatalf("decode HTTP search: %v", err)
	}
	_ = sResp.Body.Close()
	if len(httpHits) != 0 {
		t.Errorf("HTTP work profile /v1/search leaked hits: %+v", httpHits)
	}
	ts.Close()
	_ = workSvc.Close()

	// 4. Simulate external Markdown edit -> sync-check --stage -> triage approve -> re-export clean.
	editedProd := string(prodMD) + "\n## Runbook\n\nRestart pods safely.\n"
	if err := os.WriteFile(filepath.Join(workExportDir, "sites", "prod.md"), []byte(editedProd), 0o600); err != nil {
		t.Fatal(err)
	}
	code, out, errOut = runCLIAt(env, baseNow, "sync-check", "--profile", "work", "--dir", workExportDir, "--stage", "--json")
	if code != exitOK {
		t.Fatalf("sync-check --stage failed (%d): %s", code, errOut)
	}
	var scRep memstore.SyncCheckReport
	if err := json.Unmarshal([]byte(out), &scRep); err != nil {
		t.Fatalf("unmarshal sync-check: %v", err)
	}
	if len(scRep.Modified) != 1 || scRep.Staged != 1 {
		t.Fatalf("unexpected sync-check report: %+v", scRep)
	}
	tid := findSinglePendingTriageID(t, env)
	if code, _, errOut := runCLIAt(env, baseNow, "triage", "approve", tid); code != exitOK {
		t.Fatalf("triage approve sync-check item failed (%d): %s", code, errOut)
	}
	if code, _, errOut := runCLIAt(env, baseNow, "export", "--profile", "work", "--out", workExportDir); code != exitOK {
		t.Fatalf("re-export failed (%d): %s", code, errOut)
	}
	code, out, errOut = runCLIAt(env, baseNow, "sync-check", "--profile", "work", "--dir", workExportDir, "--json")
	if code != exitOK {
		t.Fatalf("clean sync-check failed (%d): %s", code, errOut)
	}
	var cleanRep memstore.SyncCheckReport
	if err := json.Unmarshal([]byte(out), &cleanRep); err != nil {
		t.Fatalf("unmarshal clean sync-check: %v", err)
	}
	if len(cleanRep.Modified) != 0 || len(cleanRep.Added) != 0 || len(cleanRep.Deleted) != 0 {
		t.Errorf("expected clean sync-check after re-export, got %+v", cleanRep)
	}

	// 5. Run deterministic routine via routine tick -> verify cursor, event, and outbox item.
	routinesDir := filepath.Join(baseDir, "routines")
	if err := os.MkdirAll(routinesDir, 0o700); err != nil {
		t.Fatal(err)
	}
	handlerPath := filepath.Join(baseDir, "e2e-routine.sh")
	handlerBody := `#!/bin/sh
printf '{"stateUpdates":{"cursor":"v2"},"events":[{"entityId":"site:prod","compartment":"engineering","sensitivity":"normal","channel":"routine","summary":"Routine check ok"}],"outboxActions":[{"actionKind":"notify","question":"Approve alert?","payload":{"channel":"ops"}}]}\n'
`
	if err := os.WriteFile(handlerPath, []byte(handlerBody), 0o700); err != nil {
		t.Fatal(err)
	}
	manifestTOML := fmt.Sprintf("id = \"e2e-check\"\nvault = \"work\"\nprofile = \"work\"\nschedule = \"@every 1h\"\n[handler]\ncommand = [%q]\noutput_format = \"json\"\n", handlerPath)
	if err := os.WriteFile(filepath.Join(routinesDir, "e2e-check.toml"), []byte(manifestTOML), 0o600); err != nil {
		t.Fatal(err)
	}

	code, out, errOut = runCLIAt(env, baseNow, "routine", "tick", "--json")
	if code != exitOK {
		t.Fatalf("routine tick failed (%d): %s", code, errOut)
	}
	var tickRes []memstore.RoutineRunResult
	if err := json.Unmarshal([]byte(out), &tickRes); err != nil {
		t.Fatalf("unmarshal routine tick: %v", err)
	}
	if len(tickRes) != 1 || tickRes[0].Status != memstore.RoutineRunOK || tickRes[0].EventsAdded != 1 || tickRes[0].OutboxAdded != 1 {
		t.Fatalf("unexpected routine tick result: %+v", tickRes)
	}
	code, out, errOut = runCLIAt(env, baseNow, "routine", "state", "get", "e2e-check", "cursor")
	if code != exitOK || strings.TrimSpace(out) != `"v2"` {
		t.Fatalf("routine state get cursor exit=%d out=%q err=%s", code, out, errOut)
	}
	vWork, err := memstore.OpenVault(cfg.Vaults["work"])
	if err != nil {
		t.Fatalf("OpenVault(work): %v", err)
	}
	outbox, err := vWork.ListRoutineOutbox(context.Background(), memstore.RoutineOutboxFilter{RoutineID: "e2e-check"})
	_ = vWork.Close()
	if err != nil {
		t.Fatalf("ListRoutineOutbox: %v", err)
	}
	if len(outbox) != 1 || outbox[0].ActionKind != "notify" {
		t.Fatalf("unexpected routine_outbox items: %+v", outbox)
	}

	// 6. Push an envelope to inbox -> sync pull -> verify idempotent ingestion & ack.
	inboxDir := filepath.Join(baseDir, "inbox")
	if code, _, errOut := runCLIAt(env, baseNow, "sync", "push", "--inbox-dir", inboxDir,
		"--id", "env_e2e_1", "--compartment", "engineering", "--sensitivity", "normal",
		"--summary", "Cloud build passed"); code != exitOK {
		t.Fatalf("sync push failed (%d): %s", code, errOut)
	}
	code, out, errOut = runCLIAt(env, baseNow, "sync", "pull", "--inbox-dir", inboxDir, "--vault", "work", "--profile", "work", "--json")
	if code != exitOK {
		t.Fatalf("sync pull failed (%d): %s", code, errOut)
	}
	var pullRep memstore.PullReport
	if err := json.Unmarshal([]byte(out), &pullRep); err != nil {
		t.Fatalf("unmarshal pull report: %v", err)
	}
	if pullRep.Ingested != 1 || pullRep.Acked != 1 {
		t.Fatalf("expected ingested=1 acked=1, got %+v", pullRep)
	}
	code, out, errOut = runCLIAt(env, baseNow, "sync", "pull", "--inbox-dir", inboxDir, "--vault", "work", "--profile", "work", "--json")
	if code != exitOK {
		t.Fatalf("second sync pull failed (%d): %s", code, errOut)
	}
	var pullRep2 memstore.PullReport
	if err := json.Unmarshal([]byte(out), &pullRep2); err != nil {
		t.Fatalf("unmarshal second pull report: %v", err)
	}
	if pullRep2.Listed != 0 || pullRep2.Ingested != 0 {
		t.Errorf("expected empty acked inbox on second pull, got %+v", pullRep2)
	}

	// 7. Run age with advanced clock -> verify TTL expiration & episodic rollup.
	futureNow := baseNow.AddDate(1, 2, 0) // +14 months (> 365 days)
	code, out, errOut = runCLIAt(env, futureNow, "age", "--vault", "work", "--profile", "admin", "--json")
	if code != exitOK {
		t.Fatalf("age failed (%d): %s", code, errOut)
	}
	var ageRes memstore.AgeResult
	if err := json.Unmarshal([]byte(out), &ageRes); err != nil {
		t.Fatalf("unmarshal age result: %v", err)
	}
	if ageRes.ExpiredFacts == 0 || ageRes.CompactedEvents == 0 || ageRes.Summaries == 0 {
		t.Errorf("expected expired facts and compacted events, got %+v", ageRes)
	}
}
