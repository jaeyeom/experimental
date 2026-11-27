package orgctl

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestValidateRoleName(t *testing.T) {
	valid := []string{
		"desk/clerk",
		"coord/lead",
		"solo",
		"area/sub/worker-1",
	}
	for _, role := range valid {
		if err := ValidateRoleName(role); err != nil {
			t.Errorf("ValidateRoleName(%q) unexpected error: %v", role, err)
		}
	}

	invalid := []string{
		"",
		".",
		"..",
		"../etc/passwd",
		"desk/../escape",
		"desk/.",
		"desk/..",
		"/abs/path",
		"trailing/",
		"double//slash",
		`back\slash`,
		"null\x00byte",
		"ctrl\nchar",
	}
	for _, role := range invalid {
		if err := ValidateRoleName(role); err == nil {
			t.Errorf("ValidateRoleName(%q) = nil, want error", role)
		}
	}
}

func TestDefaultCronStateDirAndPath(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	t.Setenv("XDG_STATE_HOME", "")

	dir, err := DefaultCronStateDir()
	if err != nil {
		t.Fatalf("DefaultCronStateDir() error: %v", err)
	}
	wantDefault := filepath.Join(home, ".local", "state", "orgctl", "crons")
	if dir != wantDefault {
		t.Fatalf("DefaultCronStateDir() = %q, want %q", dir, wantDefault)
	}

	xdg := filepath.Join(t.TempDir(), "state")
	t.Setenv("XDG_STATE_HOME", xdg)
	dir, err = DefaultCronStateDir()
	if err != nil {
		t.Fatalf("DefaultCronStateDir() with XDG error: %v", err)
	}
	wantXDG := filepath.Join(xdg, "orgctl", "crons")
	if dir != wantXDG {
		t.Fatalf("DefaultCronStateDir() = %q, want %q", dir, wantXDG)
	}

	gotPath, err := PublishedCronPath("", "desk/clerk")
	if err != nil {
		t.Fatalf("PublishedCronPath error: %v", err)
	}
	wantPath := filepath.Join(wantXDG, "desk", "clerk.json")
	if gotPath != wantPath {
		t.Fatalf("PublishedCronPath = %q, want %q", gotPath, wantPath)
	}

	if _, err := PublishedCronPath(xdg, "../escape"); err == nil {
		t.Fatal("PublishedCronPath allowed path traversal")
	}
}

func TestParseAndPublishCronsAtomic(t *testing.T) {
	now := time.Date(2026, 11, 27, 5, 0, 0, 0, time.UTC)
	created := now.Add(-2 * time.Hour)
	expiry := now.Add(7 * 24 * time.Hour)

	arrayInput := `[{"id":"aabbccdd","schedule":"0 8 * * *","prompt":"daily check","created":"` + created.Format(time.RFC3339) + `","expiry":"` + expiry.Format(time.RFC3339) + `"}]`
	items, err := ParsePublishedCrons([]byte(arrayInput))
	if err != nil {
		t.Fatalf("ParsePublishedCrons array error: %v", err)
	}
	if len(items) != 1 || items[0].ID != "aabbccdd" || items[0].Schedule != "0 8 * * *" || !items[0].Expiry.Equal(expiry) {
		t.Fatalf("items = %#v", items)
	}

	objInput := `{"crons":[{"id":"11223344","cron":"*/15 * * * *","createdAt":"` + created.Format(time.RFC3339) + `","expiresAt":"` + expiry.Format(time.RFC3339) + `"}]}`
	items, err = ParsePublishedCrons([]byte(objInput))
	if err != nil {
		t.Fatalf("ParsePublishedCrons object error: %v", err)
	}
	if len(items) != 1 || items[0].ID != "11223344" || items[0].Schedule != "*/15 * * * *" {
		t.Fatalf("items = %#v", items)
	}

	emptyItems, err := ParsePublishedCrons([]byte(`[]`))
	if err != nil {
		t.Fatalf("ParsePublishedCrons empty error: %v", err)
	}
	if len(emptyItems) != 0 {
		t.Fatalf("emptyItems = %#v, want empty", emptyItems)
	}

	for _, bad := range []string{`{`, `[{"schedule":"* * * * *"}]`, `[{"id":"abc"}]`} {
		if _, err := ParsePublishedCrons([]byte(bad)); err == nil {
			t.Errorf("ParsePublishedCrons(%s) = nil, want error", bad)
		}
	}

	dir := t.TempDir()
	path, err := PublishCrons(dir, "desk/clerk", items, now)
	if err != nil {
		t.Fatalf("PublishCrons error: %v", err)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile(%s): %v", path, err)
	}
	var doc PublishedCrons
	if err := json.Unmarshal(data, &doc); err != nil {
		t.Fatalf("Unmarshal published file: %v", err)
	}
	if doc.Role != "desk/clerk" || !doc.PublishedAt.Equal(now) || len(doc.Crons) != 1 {
		t.Fatalf("doc = %#v", doc)
	}
	entries, err := os.ReadDir(filepath.Dir(path))
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 1 || entries[0].Name() != "clerk.json" {
		t.Fatalf("unexpected temp files left in %s: %v", filepath.Dir(path), entries)
	}
}

func TestReadPublishedCronsModTimeFallbackAndMalformed(t *testing.T) {
	dir := t.TempDir()
	modTime := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)

	// Hand-written JSON without publishedAt falls back to file ModTime.
	p, err := PublishedCronPath(dir, "desk/clerk")
	if err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Dir(p), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(p, []byte(`{"crons":[{"id":"c1","schedule":"0 8 * * *"}]}`), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Chtimes(p, modTime, modTime); err != nil {
		t.Fatal(err)
	}

	// Malformed JSON for another role.
	badPath, err := PublishedCronPath(dir, "desk/bad")
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(badPath, []byte(`{bad`), 0o600); err != nil {
		t.Fatal(err)
	}

	spec := Spec{
		Roles: map[string]Role{
			"desk/clerk": {Machine: "local"},
			"desk/bad":   {Machine: "local"},
			"desk/none":  {Machine: "local"},
		},
	}
	got := ReadPublishedCrons(spec, "local", dir)
	if _, ok := got["desk/none"]; ok {
		t.Fatalf("desk/none should not be in PublishedCrons map")
	}
	if pub, ok := got["desk/clerk"]; !ok || !pub.PublishedAt.Equal(modTime) || len(pub.Crons) != 1 {
		t.Fatalf("desk/clerk = %#v, want modTime %s", pub, modTime)
	}
	if pub, ok := got["desk/bad"]; !ok || !strings.Contains(pub.ReadError, "parse") {
		t.Fatalf("desk/bad = %#v, want ReadError", pub)
	}
}
