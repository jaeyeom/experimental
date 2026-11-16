package main

import (
	"os"
	"path/filepath"
	"testing"
)

func resolveFixture(t *testing.T) *testApp {
	t.Helper()
	ta := newTestApp(t)
	ta.mustRun("add", "Ada Lovelace", "--alias", "countess", "--handle", "chat:ada.l", "--handle", "code:ada-l",
		"--team", "core", "--role", "lead", "--tz", "Asia/Seoul", "--channel", "chat")
	ta.mustRun("add", "Sam Lee", "--team", "core")
	ta.mustRun("add", "Sam Lee", "--team", "infra")
	if err := os.WriteFile(filepath.Join(ta.dataDir, "people", "ada-lovelace.md"), []byte("notes\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	return ta
}

func TestResolveGolden(t *testing.T) {
	ta := resolveFixture(t)
	for _, tc := range []struct {
		name  string
		query string
		code  int
	}{
		{"single", "@ada.l", exitOK},
		{"ambiguous", "sam", exitAmbiguous},
		{"none", "nobody", exitUsage},
	} {
		for _, asJSON := range []bool{false, true} {
			args := []string{"resolve", tc.query, "--reason", "test"}
			file := "resolve_" + tc.name + ".txt"
			if asJSON {
				args = append(args, "--json")
				file = "resolve_" + tc.name + ".json"
			}
			code, out, _ := ta.run(args...)
			if code != tc.code {
				t.Errorf("%v: exit %d, want %d", args, code, tc.code)
			}
			ta.golden(file, out)
		}
	}
}
