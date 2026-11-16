package decide

import (
	"os"
	"path/filepath"
	"slices"
	"testing"
)

func TestSetFavoriteContactsRoundTrips(t *testing.T) {
	home := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", home)
	path := filepath.Join(home, "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(`{"hiddenContacts": ["zed"]}`+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	stored, err := SetFavoriteContacts([]string{" ops/inbox ", "", "builder", "ops/inbox"})
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{"ops/inbox", "builder"}; !slices.Equal(stored, want) {
		t.Fatalf("stored = %q, want %q", stored, want)
	}
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(cfg.FavoriteContacts, stored) || !slices.Equal(cfg.HiddenContacts, []string{"zed"}) {
		t.Fatalf("reloaded favorites %q hidden %q", cfg.FavoriteContacts, cfg.HiddenContacts)
	}
}
