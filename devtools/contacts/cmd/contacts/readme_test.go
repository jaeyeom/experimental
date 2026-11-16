package main

import (
	"os"
	"strings"
	"testing"
)

const readmePath = "../../README.org"

// orgBlock returns the body of the block after "#+NAME: name" in doc and
// the byte offsets of that body.
func orgBlock(t *testing.T, doc, name string) (body string, start, end int) {
	t.Helper()
	i := strings.Index(doc, "#+NAME: "+name+"\n")
	if i < 0 {
		t.Fatalf("README has no block named %s", name)
	}
	start = strings.Index(doc[i:], "\n#+begin_") + i + 1
	start += strings.Index(doc[start:], "\n") + 1
	end = strings.Index(doc[start:], "#+end_") + start
	return doc[start:end], start, end
}

// splitWords splits a command line like a POSIX shell for the simple cases
// the README uses: spaces, single quotes and double quotes.
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

// TestReadmeWalkthrough runs every command of the README walkthrough as
// written and compares the transcript with the README's output block.
// -update rewrites that block.
func TestReadmeWalkthrough(t *testing.T) {
	data, err := os.ReadFile(readmePath)
	if err != nil {
		t.Fatal(err)
	}
	doc := string(data)
	script, _, _ := orgBlock(t, doc, "walkthrough")
	ta := newTestApp(t)
	var transcript strings.Builder
	for _, line := range strings.Split(script, "\n") {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		words := splitWords(t, line)
		if words[0] != "contacts" {
			t.Fatalf("walkthrough line is not a contacts command: %q", line)
		}
		code, out, errOut := ta.run(words[1:]...)
		if code != exitOK {
			t.Fatalf("%s: exit %d\n%s", line, code, errOut)
		}
		transcript.WriteString("$ " + line + "\n" + out)
	}
	got := strings.ReplaceAll(transcript.String(), ta.dataDir, "~/.local/share/contacts")
	want, start, end := orgBlock(t, doc, "walkthrough-output")
	if *updateGolden {
		if err := os.WriteFile(readmePath, []byte(doc[:start]+got+doc[end:]), 0o600); err != nil {
			t.Fatal(err)
		}
		return
	}
	if got != want {
		t.Errorf("README walkthrough output is stale (run with -update)\n--- got ---\n%s\n--- want ---\n%s", got, want)
	}
}
