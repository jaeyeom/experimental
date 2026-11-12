package main

import (
	"os"
	"path/filepath"
	"testing"
)

func TestOpenAuditLogAppendsUnderConfigDir(t *testing.T) {
	home := t.TempDir()
	want := filepath.Join(home, ".config", "gh-nudge", "approve-log.jsonl")
	for _, line := range []string{"one\n", "two\n"} {
		file, path, err := openAuditLog(home)
		if err != nil {
			t.Fatal(err)
		}
		if path != want {
			t.Fatalf("path = %q, want %q", path, want)
		}
		if _, err := file.WriteString(line); err != nil {
			t.Fatal(err)
		}
		if err := file.Close(); err != nil {
			t.Fatal(err)
		}
	}
	data, err := os.ReadFile(want)
	if err != nil {
		t.Fatal(err)
	}
	if string(data) != "one\ntwo\n" {
		t.Fatalf("log = %q", data)
	}
}
