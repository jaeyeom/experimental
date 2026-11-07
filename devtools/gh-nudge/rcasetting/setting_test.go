package rcasetting

import (
	"os"
	"path/filepath"
	"testing"
)

func TestForUsesRepoMarker(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "config.yaml")
	body := []byte(`
settings:
  rca_marker: "RCA {event_id} class={class}"
repo_settings:
  acme/widgets:
    rca_marker: "ENTRY {event_id} class={class}"
`)
	if err := os.WriteFile(path, body, 0o600); err != nil {
		t.Fatalf("write config: %v", err)
	}
	src := &Source{Path: path}
	got, err := src.For("acme", "widgets")
	if err != nil {
		t.Fatalf("For() error = %v", err)
	}
	if got != "ENTRY {event_id} class={class}" {
		t.Fatalf("For(repo) = %q", got)
	}
	got, err = src.For("acme", "other")
	if err != nil {
		t.Fatalf("For(other) error = %v", err)
	}
	if got != "RCA {event_id} class={class}" {
		t.Fatalf("For(other) = %q", got)
	}
}

func TestForNoHome(t *testing.T) {
	t.Setenv("HOME", "")
	src := &Source{}
	got, err := src.For("acme", "widgets")
	if err != nil {
		t.Fatalf("For() error = %v", err)
	}
	if got != "" {
		t.Fatalf("For() = %q, want empty", got)
	}
}

func TestForMissingFile(t *testing.T) {
	src := &Source{Path: filepath.Join(t.TempDir(), "missing.yaml")}
	got, err := src.For("acme", "widgets")
	if err != nil {
		t.Fatalf("For() error = %v", err)
	}
	if got != "" {
		t.Fatalf("For() = %q, want empty", got)
	}
}
