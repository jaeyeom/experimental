package main

import (
	"bytes"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"testing"
)

func TestReadmeExamplesMatchTheBinary(t *testing.T) {
	readme := readmeText(t)
	lines := strings.Split(readme, "\n")
	if len(lines) < 10 {
		t.Fatalf("README has %d lines", len(lines))
	}
	head := strings.Join(lines[:10], "\n")
	for _, want := range []string{"box-renderer", "bordered", "Quick start"} {
		if !strings.Contains(head, want) {
			t.Errorf("first 10 lines lack %q", want)
		}
	}
	install := "go install github.com/jaeyeom/experimental/devtools/ascii-art/cmd/box-renderer@latest"
	if got, use := strings.Index(readme, install), strings.Index(readme, "\nbox-renderer "); got < 0 || use < 0 || got > use {
		t.Error("go install line is missing or comes after a usage command")
	}
	for _, gone := range []string{
		"gantt-chart", "graph-viz", "--indent", "internal/gantt",
		"internal/graph", "GitHub Issue", "gabyx-githooks", "makefile-workflow", "go-dev",
	} {
		if strings.Contains(readme, gone) {
			t.Errorf("README still mentions %q", gone)
		}
	}
	for _, want := range []string{`--prefix`, `""`, `--width`, `0 = auto`, `--connector`, `│|▼`, `--border`, `single`, `--padding`, `2`} {
		if !strings.Contains(readme, want) {
			t.Errorf("README lacks %q", want)
		}
	}

	examples := readmeExamples(t, readme)
	if len(examples) < 2 {
		t.Fatalf("found %d runnable examples", len(examples))
	}
	for _, ex := range examples {
		var buf bytes.Buffer
		if err := run(ex.args, strings.NewReader(ex.stdin), &buf); err != nil {
			t.Fatalf("run %q: %v", ex.script, err)
		}
		if strings.TrimRight(buf.String(), "\n") != strings.TrimRight(ex.output, "\n") {
			t.Errorf("output mismatch for %q\ngot:\n%s\nwant:\n%s", ex.script, buf.String(), ex.output)
		}
	}
}

type readmeExample struct {
	script string
	args   []string
	stdin  string
	output string
}

var (
	bashBlock    = regexp.MustCompile(`(?s)#\+BEGIN_SRC bash\n(.*?)\n#\+END_SRC`)
	exampleBlock = regexp.MustCompile(`(?s)#\+BEGIN_EXAMPLE\n(.*?)\n#\+END_EXAMPLE`)
)

func readmeExamples(t *testing.T, doc string) []readmeExample {
	t.Helper()
	var out []readmeExample
	for _, match := range bashBlock.FindAllStringSubmatchIndex(doc, -1) {
		script := strings.TrimSpace(doc[match[2]:match[3]])
		args, stdin, ok := parseBoxCommand(script)
		if !ok {
			continue
		}
		loc := exampleBlock.FindStringSubmatchIndex(doc[match[1]:])
		if loc == nil {
			t.Fatalf("no example output after %q", script)
		}
		out = append(out, readmeExample{
			script: script,
			args:   args,
			stdin:  stdin,
			output: doc[match[1]+loc[2] : match[1]+loc[3]],
		})
	}
	return out
}

func parseBoxCommand(script string) ([]string, string, bool) {
	const pipe = " | box-renderer"
	if strings.Contains(script, pipe) {
		parts := strings.SplitN(script, pipe, 2)
		const prefix = "printf '%s\\n' "
		if !strings.HasPrefix(parts[0], prefix) {
			return nil, "", false
		}
		lines := splitWords(strings.TrimPrefix(parts[0], prefix))
		return splitWords(strings.TrimSpace(parts[1])), strings.Join(lines, "\n") + "\n", true
	}
	if script == "box-renderer" || strings.HasPrefix(script, "box-renderer ") {
		return splitWords(strings.TrimPrefix(script, "box-renderer")), "", true
	}
	return nil, "", false
}

func splitWords(s string) []string {
	var words []string
	var cur strings.Builder
	quote := false
	for _, r := range s {
		switch {
		case r == '\'':
			quote = !quote
		case r == ' ' && !quote:
			if cur.Len() > 0 {
				words = append(words, cur.String())
				cur.Reset()
			}
		default:
			cur.WriteRune(r)
		}
	}
	if cur.Len() > 0 {
		words = append(words, cur.String())
	}
	return words
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
		candidates = append(candidates, filepath.Join(src, ws, "devtools/ascii-art/README.org"))
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
