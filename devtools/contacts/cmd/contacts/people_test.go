package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestAddEditMergeDelete(t *testing.T) {
	ta := newTestApp(t)
	if id := ta.mustRun("add", "Ada Lovelace", "--handle", "chat:ada.l", "--alias", "countess", "--team", "core"); id != "ada-lovelace\n" {
		t.Fatalf("add printed %q", id)
	}
	code, _, errOut := ta.run("add", "Imposter", "--handle", "chat:ada.l")
	if code != exitError || !strings.Contains(errOut, "already belongs to ada-lovelace") {
		t.Errorf("duplicate handle: exit %d, %s", code, errOut)
	}
	if code, _, _ := ta.run("add", "X", "--handle", "phone:1"); code != exitUsage {
		t.Errorf("bad handle kind: exit %d, want %d", code, exitUsage)
	}
	ta.mustRun("edit", "countess", "--role", "lead", "--handle", "code:ada-l")

	ta.mustRun("add", "A. Lovelace", "--handle", "email:ada@example.com")
	notes := filepath.Join(ta.dataDir, "people", "a-lovelace.md")
	if err := os.WriteFile(notes, []byte("met at the conference\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if out := ta.mustRun("merge", "@ada.l", "email:ada@example.com"); out != "merged a-lovelace into ada-lovelace\n" {
		t.Errorf("merge printed %q", out)
	}
	if _, err := os.Stat(filepath.Join(ta.dataDir, "people", "ada-lovelace.md")); err != nil {
		t.Errorf("merged notes missing: %v", err)
	}

	if code, _, _ := ta.run("delete", "ada-lovelace"); code != exitUsage {
		t.Errorf("delete without --yes: exit %d", code)
	}
	ta.mustRun("delete", "ada@example.com", "--yes")
	if code, _, _ := ta.run("edit", "ada-lovelace", "--team", "x"); code != exitUsage {
		t.Errorf("edit deleted person: exit %d, want %d", code, exitUsage)
	}
}

func TestAmbiguousPersonListsCandidates(t *testing.T) {
	ta := newTestApp(t)
	ta.mustRun("add", "Sam Lee", "--team", "core")
	ta.mustRun("add", "Sam Lee", "--team", "infra")
	code, _, errOut := ta.run("edit", "sam lee", "--role", "x")
	if code != exitAmbiguous {
		t.Errorf("exit = %d, want %d", code, exitAmbiguous)
	}
	for _, want := range []string{"sam-lee\tSam Lee\tcore", "sam-lee-2\tSam Lee\tinfra"} {
		if !strings.Contains(errOut, want) {
			t.Errorf("stderr lacks %q:\n%s", want, errOut)
		}
	}
}
