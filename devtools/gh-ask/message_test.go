package ghask

import (
	"os"
	"strings"
	"testing"
)

func validRequest() AskRequest {
	return AskRequest{
		Repo:      "octo/demo",
		PR:        42,
		HeadSHA:   "0123456789abcdef0123456789abcdef01234567",
		Path:      "pkg/a.go",
		Side:      "RIGHT",
		StartLine: 10,
		EndLine:   12,
		Selection: "x := 1",
		Question:  "Why?",
		PageURL:   "https://github.com/octo/demo/pull/42/files#diff-abc",
	}
}

func TestRenderMessage(t *testing.T) {
	req := validRequest()
	req.Deep = true
	got := RenderMessage(req)
	want := "[gh-ask] PR 42 · pkg/a.go:10-12 (RIGHT) · deep=yes\n" +
		"\nQuestion:\nWhy?\n" +
		"\nCode:\nx := 1\n" +
		"\nLink:\nhttps://github.com/octo/demo/pull/42/files#diff-abc\n"
	if got != want {
		t.Errorf("RenderMessage() =\n%s\nwant\n%s", got, want)
	}
	req.Deep = false
	if first, _, _ := strings.Cut(RenderMessage(req), "\n"); !strings.HasSuffix(first, "deep=no") {
		t.Errorf("first line = %q, want deep=no", first)
	}
}

func TestValidateRequest(t *testing.T) {
	tests := []struct {
		name     string
		mutate   func(*AskRequest)
		wantSize bool
	}{
		{name: "bad repo", mutate: func(r *AskRequest) { r.Repo = "octo" }},
		{name: "dash repo", mutate: func(r *AskRequest) { r.Repo = "-o/demo" }},
		{name: "zero pr", mutate: func(r *AskRequest) { r.PR = 0 }},
		{name: "bad sha", mutate: func(r *AskRequest) { r.HeadSHA = "zz" }},
		{name: "empty path", mutate: func(r *AskRequest) { r.Path = "" }},
		{name: "newline path", mutate: func(r *AskRequest) { r.Path = "a\n[gh-ask] PR 1" }},
		{name: "bad side", mutate: func(r *AskRequest) { r.Side = "MIDDLE" }},
		{name: "zero start", mutate: func(r *AskRequest) { r.StartLine = 0 }},
		{name: "end before start", mutate: func(r *AskRequest) { r.EndLine = 9 }},
		{name: "empty question", mutate: func(r *AskRequest) { r.Question = " " }},
		{name: "non-github link", mutate: func(r *AskRequest) { r.PageURL = "https://evil.example/x" }},
		{name: "big selection", mutate: func(r *AskRequest) { r.Selection = strings.Repeat("a", MaxSelectionBytes+1) }, wantSize: true},
		{name: "big question", mutate: func(r *AskRequest) { r.Question = strings.Repeat("a", MaxQuestionBytes+1) }, wantSize: true},
		{name: "big path", mutate: func(r *AskRequest) { r.Path = strings.Repeat("a", MaxPathBytes+1) }, wantSize: true},
		{name: "big link", mutate: func(r *AskRequest) { r.PageURL = "https://github.com/" + strings.Repeat("a", MaxPageURLBytes) }, wantSize: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			req := validRequest()
			tc.mutate(&req)
			err := req.Validate()
			if err == nil {
				t.Fatal("Validate() error = nil")
			}
			if got := isTooLarge(err); got != tc.wantSize {
				t.Errorf("isTooLarge(%v) = %v, want %v", err, got, tc.wantSize)
			}
		})
	}
	req := validRequest()
	req.Selection = strings.Repeat("a", MaxSelectionBytes)
	req.Question = strings.Repeat("a", MaxQuestionBytes)
	if err := req.Validate(); err != nil {
		t.Errorf("Validate() at the caps error = %v", err)
	}
}

func TestWriteMessageFile(t *testing.T) {
	dir := t.TempDir()
	path, err := writeMessageFile(dir, "hello\n")
	if err != nil {
		t.Fatalf("writeMessageFile() error = %v", err)
	}
	info, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	if perm := info.Mode().Perm(); perm != 0o600 {
		t.Errorf("mode = %o, want 600", perm)
	}
	data, err := os.ReadFile(path)
	if err != nil || string(data) != "hello\n" {
		t.Errorf("content = %q, %v", data, err)
	}
}
