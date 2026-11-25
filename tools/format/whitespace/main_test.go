package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestStripTrailingWhitespace(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name        string
		in          string
		want        string
		wantChanged bool
	}{
		{
			name:        "empty",
			in:          "",
			want:        "",
			wantChanged: false,
		},
		{
			name:        "clean",
			in:          "# Title\n\nSome text.\n",
			want:        "# Title\n\nSome text.\n",
			wantChanged: false,
		},
		{
			name:        "trailing spaces",
			in:          "# Title  \nBody\n",
			want:        "# Title\nBody\n",
			wantChanged: true,
		},
		{
			name:        "trailing tab",
			in:          "* Heading\t\n",
			want:        "* Heading\n",
			wantChanged: true,
		},
		{
			name:        "trailing carriage return",
			in:          "line1\r\nline2\r\n",
			want:        "line1\nline2\n",
			wantChanged: true,
		},
		{
			name:        "blank line with spaces",
			in:          "line1\n   \nline2\n",
			want:        "line1\n\nline2\n",
			wantChanged: true,
		},
		{
			name:        "no trailing newline",
			in:          "line1 ",
			want:        "line1",
			wantChanged: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, changed := stripTrailingWhitespace([]byte(tc.in))
			if changed != tc.wantChanged {
				t.Fatalf("changed = %v, want %v", changed, tc.wantChanged)
			}
			if string(got) != tc.want {
				t.Fatalf("got %q, want %q", string(got), tc.want)
			}
		})
	}
}

func TestRunCheckAndWrite(t *testing.T) {
	t.Parallel()

	dir := t.TempDir()
	mdFile := filepath.Join(dir, "doc.md")
	orgFile := filepath.Join(dir, "notes.org")
	cleanFile := filepath.Join(dir, "clean.md")

	if err := os.WriteFile(mdFile, []byte("# Hello   \nWorld\n"), 0o600); err != nil {
		t.Fatalf("write doc.md: %v", err)
	}
	if err := os.WriteFile(orgFile, []byte("* Heading\t\n"), 0o600); err != nil {
		t.Fatalf("write notes.org: %v", err)
	}
	if err := os.WriteFile(cleanFile, []byte("# Clean\n"), 0o600); err != nil {
		t.Fatalf("write clean.md: %v", err)
	}

	var stdout, stderr bytes.Buffer
	code := run([]string{"-check", mdFile, orgFile, cleanFile}, &stdout, &stderr)
	if code == 0 {
		t.Fatalf("expected non-zero exit code for trailing whitespace")
	}
	out := stdout.String()
	if !strings.Contains(out, mdFile) || !strings.Contains(out, orgFile) {
		t.Fatalf("expected both dirty files in output, got:\n%s", out)
	}
	if strings.Contains(out, cleanFile) {
		t.Fatalf("did not expect clean file in output, got:\n%s", out)
	}

	// Fix with -w.
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"-w", mdFile, orgFile, cleanFile}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("expected exit 0 for -w, got %d (stderr: %s)", code, stderr.String())
	}

	// Check should now pass.
	stdout.Reset()
	stderr.Reset()
	code = run([]string{"-check", mdFile, orgFile, cleanFile}, &stdout, &stderr)
	if code != 0 {
		t.Fatalf("expected exit 0 after -w, got %d (stdout: %s, stderr: %s)", code, stdout.String(), stderr.String())
	}
}
