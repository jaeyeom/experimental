package ghask

import (
	"regexp"
	"slices"
	"testing"
)

func TestExpandArgv(t *testing.T) {
	tmpl := []string{"asker", "--file={file}", "{pr}", "{repo}", "lit"}
	got, err := expandArgv(tmpl, map[string]string{"file": "/t/m.txt", "pr": "7", "repo": "o/r"})
	if err != nil {
		t.Fatalf("expandArgv() error = %v", err)
	}
	want := []string{"asker", "--file=/t/m.txt", "7", "o/r", "lit"}
	if !slices.Equal(got, want) {
		t.Errorf("expandArgv() = %q, want %q", got, want)
	}
	if _, err := expandArgv([]string{"x", "{question}"}, map[string]string{"file": "f"}); err == nil {
		t.Error("expandArgv() with unknown placeholder error = nil")
	}
}

func TestExtractID(t *testing.T) {
	tests := []struct {
		name    string
		pattern string
		out     string
		want    string
		wantOK  bool
	}{
		{name: "group", pattern: `thread-([0-9a-f]+)`, out: "created thread-abc12\n", want: "abc12", wantOK: true},
		{name: "whole match", pattern: `T[0-9]+`, out: "id: T99 ok", want: "T99", wantOK: true},
		{name: "first match wins", pattern: `id=(\w+)`, out: "id=a\nid=b\n", want: "a", wantOK: true},
		{name: "no match", pattern: `thread-([0-9a-f]+)`, out: "nothing here", wantOK: false},
		{name: "empty group", pattern: `id=(\w*)`, out: "id=", wantOK: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, ok := extractID(regexp.MustCompile(tc.pattern), []byte(tc.out))
			if ok != tc.wantOK || got != tc.want {
				t.Errorf("extractID() = %q, %v; want %q, %v", got, ok, tc.want, tc.wantOK)
			}
		})
	}
}
