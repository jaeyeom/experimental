package orglint

import (
	"bytes"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestParseSample(t *testing.T) {
	data, err := os.ReadFile("testdata/sample.orgtest")
	if err != nil {
		t.Fatal(err)
	}
	d := ParseString(string(data))
	tests := []struct {
		name      string
		got, want any
	}{
		{"headlines", d.Headlines, []Headline{
			{Line: 5, EndLine: 18, Level: 1, Title: "Top heading", Todo: "TODO", Tags: []string{"a", "b"}},
			{Line: 12, EndLine: 18, Level: 2, Title: "Child"},
			{Line: 19, EndLine: 20, Level: 1, Title: "Next"},
		}},
		{"keywords", d.Keywords, []Keyword{
			{1, "TITLE", "Sample", false},
			{2, "NAME", "intro", true},
			{3, "CAPTION", "A caption", true},
		}},
		{"blocks", d.Blocks, []Block{{Line: 16, EndLine: 18, Name: "SRC", Params: "go :tangle no"}}},
		{"drawers", d.Drawers, []Drawer{{
			Line: 7, EndLine: 9, Name: "PROPERTIES",
			Properties: []Property{{8, "CUSTOM_ID", "top"}},
		}}},
		{"planning", d.Plannings, []Planning{{6, "SCHEDULED: <2026-01-01 Thu>"}}},
		{"links", d.Links, []Link{
			{10, BracketLink, "#top", "the top"},
			{10, BracketLink, "https://example.com", ""},
			{10, AngleLink, "https://example.org", ""},
			{11, PlainLink, "https://example.net/x", ""},
		}},
		{"footnotes", d.Footnotes, []Footnote{{11, "1", false}, {20, "1", true}}},
		{"items", d.Items, []ListItem{
			{13, 0, "-", ""}, {14, 2, "1.", ""}, {15, 0, "-", "X"},
		}},
	}
	for _, tc := range tests {
		if !reflect.DeepEqual(tc.got, tc.want) {
			t.Errorf("%s:\n got %+v\nwant %+v", tc.name, tc.got, tc.want)
		}
	}
}

func TestUnclosed(t *testing.T) {
	d := ParseString("#+BEGIN_SRC go\nx\n:DRAWER:\ntext\n* H\n")
	if len(d.Blocks) != 1 || d.Blocks[0].EndLine != 0 {
		t.Errorf("blocks = %+v", d.Blocks)
	}
	if len(d.Headlines) != 0 {
		t.Errorf("block should swallow rest: %+v", d.Headlines)
	}
	d = ParseString("* H\n:DRAWER:\ntext\n* H2\n")
	if len(d.Headlines) != 2 || len(d.Drawers) != 1 || d.Drawers[0].EndLine != 0 {
		t.Errorf("headlines=%+v drawers=%+v", d.Headlines, d.Drawers)
	}
}

// TestParseRepositoryFiles parses the org files available to the test: the
// parity fixtures and the devtools README.
func TestParseRepositoryFiles(t *testing.T) {
	files, _ := filepath.Glob("../linters/testdata/orglint/*.orgtest")
	files = append(files, "../README.org")
	if len(files) < 2 {
		t.Fatal("no org files found")
	}
	for _, f := range files {
		data, err := os.ReadFile(f)
		if err != nil {
			t.Fatal(err)
		}
		d, err := Parse(bytes.NewReader(data))
		if err != nil {
			t.Errorf("%s: %v", f, err)
			continue
		}
		if want := strings.Count(string(data), "\n"); len(d.Lines) < want {
			t.Errorf("%s: %d lines, want at least %d", f, len(d.Lines), want)
		}
	}
	data, _ := os.ReadFile("../README.org")
	d := ParseString(string(data))
	if len(d.Headlines) == 0 || d.Headlines[0].Level != 1 {
		t.Errorf("README headlines = %+v", d.Headlines)
	}
	for _, h := range d.Headlines {
		if h.Title == "" || h.EndLine < h.Line {
			t.Errorf("bad headline %+v", h)
		}
	}
}

func TestRun(t *testing.T) {
	var out bytes.Buffer
	found, err := Run(&out, []string{"testdata/sample.orgtest"}, []Checker{{
		Name: "demo",
		Check: func(d *Document) []Finding {
			return []Finding{{Line: d.Blocks[0].Line, Message: "a block"}}
		},
	}})
	if err != nil || !found {
		t.Fatalf("found=%v err=%v", found, err)
	}
	if want := "testdata/sample.orgtest:16: demo: a block\n"; out.String() != want {
		t.Errorf("got %q, want %q", out.String(), want)
	}
	out.Reset()
	if found, err = Run(&out, []string{"testdata/sample.orgtest"}, nil); found || err != nil || out.Len() != 0 {
		t.Errorf("empty checker list: found=%v err=%v out=%q", found, err, out.String())
	}
	if _, err = Run(&out, []string{"testdata/missing.org"}, nil); err == nil {
		t.Error("want error for missing file")
	}
}
