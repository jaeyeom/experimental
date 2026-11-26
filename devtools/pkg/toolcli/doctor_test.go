package toolcli

import (
	"bytes"
	"os"
	"strings"
	"testing"
)

func TestPrintDoctorHumanPlainAndColored(t *testing.T) {
	checks := []CheckResult{
		{Name: "binary-on-path", Status: StatusOK, Detail: "/usr/local/bin/mytool"},
		{Name: "revision", Status: StatusWarn, Detail: "skipped (--offline)", Hint: "run without --offline"},
		{Name: "very-long-check-name", Status: StatusFail, Detail: "broken", Hint: "fix it"},
	}

	t.Run("plain", func(t *testing.T) {
		var buf bytes.Buffer
		if err := printDoctorHumanColored(&buf, checks, false); err != nil {
			t.Fatal(err)
		}
		got := buf.String()
		if strings.Contains(got, "\x1b[") {
			t.Fatalf("plain output must not contain ANSI escapes:\n%s", got)
		}
		want := strings.Join([]string{
			"✅ ok    binary-on-path        /usr/local/bin/mytool",
			"⚠️ warn  revision              skipped (--offline)",
			"                               hint: run without --offline",
			"❌ fail  very-long-check-name  broken",
			"                               hint: fix it",
			"",
		}, "\n")
		if got != want {
			t.Fatalf("plain output mismatch:\ngot:\n%s\nwant:\n%s", got, want)
		}
	})

	t.Run("colored", func(t *testing.T) {
		var buf bytes.Buffer
		if err := printDoctorHumanColored(&buf, checks, true); err != nil {
			t.Fatal(err)
		}
		got := buf.String()
		for _, wantSub := range []string{
			ansiGreen + "✅ ok  " + ansiReset,
			ansiYellow + "⚠️ warn" + ansiReset,
			ansiRed + "❌ fail" + ansiReset,
			ansiDim + "hint: run without --offline" + ansiReset,
			ansiDim + "hint: fix it" + ansiReset,
		} {
			if !strings.Contains(got, wantSub) {
				t.Fatalf("colored output missing %q:\n%s", wantSub, got)
			}
		}
	})
}

func TestUseColor(t *testing.T) {
	t.Run("non_file_writer", func(t *testing.T) {
		t.Setenv("NO_COLOR", "")
		t.Setenv("TERM", "xterm-256color")
		var buf bytes.Buffer
		if useColor(&buf) {
			t.Fatal("expected useColor=false for bytes.Buffer")
		}
	})

	t.Run("pipe_is_not_terminal", func(t *testing.T) {
		t.Setenv("NO_COLOR", "")
		t.Setenv("TERM", "xterm-256color")
		r, w, err := os.Pipe()
		if err != nil {
			t.Fatal(err)
		}
		defer r.Close()
		defer w.Close()
		if useColor(w) {
			t.Fatal("expected useColor=false for pipe")
		}
	})

	t.Run("no_color_env", func(t *testing.T) {
		t.Setenv("NO_COLOR", "1")
		if useColor(os.Stdout) {
			t.Fatal("expected useColor=false when NO_COLOR is set")
		}
	})

	t.Run("dumb_term_env", func(t *testing.T) {
		t.Setenv("NO_COLOR", "")
		t.Setenv("TERM", "dumb")
		if useColor(os.Stdout) {
			t.Fatal("expected useColor=false when TERM=dumb")
		}
	})
}
