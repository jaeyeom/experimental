package cli

import (
	"io"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/spf13/cobra"
)

// TestSynopsisListsRegisteredFlags keeps the user guide's command lines
// in step with the flags the binary actually registers.
func TestSynopsisListsRegisteredFlags(t *testing.T) {
	readme := readmeText(t)
	if strings.Count(readme, "\n") > 400 {
		t.Fatalf("README has %d lines, want at most 400", strings.Count(readme, "\n"))
	}
	headLines := strings.Split(readme, "\n")
	if len(headLines) < 10 {
		t.Fatalf("README has %d lines", len(headLines))
	}
	head := strings.Join(headLines[:10], "\n")
	for _, want := range []string{"herdr.dev", "PROJ-123", "pull request"} {
		if !strings.Contains(head, want) {
			t.Errorf("first 10 lines lack %q:\n%s", want, head)
		}
	}
	if !strings.Contains(readme, "global flag ~--log-level~") {
		t.Error(`README does not document --log-level as a global flag`)
	}

	syn := synopsisLines(t, readme)
	var logFile *runlog.File
	root := newRoot(io.Discard, io.Discard, nil, &logFile)
	for _, cmd := range root.Commands() {
		line, ok := syn[cmd.Name()]
		if !ok {
			t.Errorf("no synopsis line for %s", cmd.Name())
			continue
		}
		for _, name := range localFlagNames(cmd) {
			if !flagInSynopsis(line, name) {
				t.Errorf("%s synopsis missing --%s\n%s", cmd.Name(), name, line)
			}
		}
	}
}

var flagUsageLine = regexp.MustCompile(`^\s+(?:-\S,\s+)?--([A-Za-z0-9-]+)`)

func localFlagNames(cmd *cobra.Command) []string {
	var names []string
	for _, line := range strings.Split(cmd.LocalFlags().FlagUsages(), "\n") {
		m := flagUsageLine.FindStringSubmatch(line)
		if m == nil || m[1] == "help" {
			continue
		}
		names = append(names, m[1])
	}
	return names
}

func flagInSynopsis(line, name string) bool {
	return regexp.MustCompile(`--` + regexp.QuoteMeta(name) + `(?:[=\s\]]|$)`).MatchString(line)
}

func synopsisLines(t *testing.T, readme string) map[string]string {
	t.Helper()
	const start = "#+begin_example\n"
	i := strings.Index(readme, start)
	if i < 0 {
		t.Fatal("synopsis example block not found")
	}
	rest := readme[i+len(start):]
	j := strings.Index(rest, "#+end_example")
	if j < 0 {
		t.Fatal("synopsis example block is not closed")
	}
	out := map[string]string{}
	for _, line := range strings.Split(rest[:j], "\n") {
		line = strings.TrimSpace(line)
		fields := strings.Fields(line)
		if len(fields) < 2 || fields[0] != "prsync" {
			continue
		}
		out[fields[1]] = line
	}
	return out
}

func readmeText(t *testing.T) string {
	t.Helper()
	var candidates []string
	if _, file, _, ok := runtime.Caller(0); ok {
		candidates = append(candidates, filepath.Join(filepath.Dir(file), "..", "..", "README.org"))
	}
	if src := os.Getenv("TEST_SRCDIR"); src != "" {
		ws := os.Getenv("TEST_WORKSPACE")
		if ws == "" {
			ws = "_main"
		}
		candidates = append(candidates, filepath.Join(src, ws, "devtools/prsync/README.org"))
	}
	for _, path := range candidates {
		body, err := os.ReadFile(path)
		if err != nil {
			continue
		}
		return string(body)
	}
	t.Fatal("README.org not found")
	return ""
}
