package tui

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestMutedTimeAndDetailAreReadable(t *testing.T) {
	cfg := t.TempDir()
	t.Setenv("XDG_CONFIG_HOME", cfg)
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("NO_COLOR", "")
	writeColorConfig(t, cfg, "{}\n")

	dir := t.TempDir()
	led := openLedger(t, dir)
	now := time.Date(2026, 11, 8, 12, 0, 0, 0, time.UTC)
	mustCreate(t, led, &decide.Item{
		Question:  "detail body",
		Owner:     "ada",
		Kind:      decide.KindRelay,
		State:     decide.StateOpen,
		CreatedAt: now.Add(-5 * time.Hour),
		Messages: []decide.Message{
			{Author: "ada", Time: now.Add(-5 * time.Hour), Text: "detail body"},
			{Author: "human", Time: now.Add(-time.Hour), Text: "human note"},
		},
	})
	m := New(dir)
	m.Now = func() time.Time { return now }
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width = 80
	m.height = 24

	list := m.View()
	if got := sgrBefore(t, list, "5h"); got != "\x1b[90m" || sgrFaint(got) {
		t.Fatalf("list time %q", got)
	}

	press(t, m, "o")
	view := m.View()
	if got := sgrBefore(t, view, "From:"); got != "\x1b[37m" || sgrFaint(got) {
		t.Fatalf("muted body %q", got)
	}
	if got := sgrBefore(t, view, "5h"); got != "\x1b[90m" || sgrFaint(got) {
		t.Fatalf("time %q", got)
	}
	body := lineContaining(view, "detail body")
	if body == "" || lineHasFaint(body) {
		t.Fatalf("detail line %q", body)
	}

	writeColorConfig(t, cfg, `{"colors":{"muted":"7"}}`+"\n")
	ansi := m.View()
	if got := sgrBefore(t, ansi, "From:"); got != "\x1b[37m" || sgrFaint(got) {
		t.Fatalf("muted 7 %q", got)
	}

	writeColorConfig(t, cfg, `{"colors":{"muted":"#b0b0b0"}}`+"\n")
	hex := m.View()
	got := sgrBefore(t, hex, "From:")
	if got != "\x1b[38;2;176;176;176m" || got == "\x1b[37m" || sgrFaint(got) {
		t.Fatalf("muted hex %q", got)
	}

	writeColorConfig(t, cfg, `{"colors":{"time":"4"}}`+"\n")
	timed := m.View()
	if got := sgrBefore(t, timed, "From:"); got != "\x1b[37m" {
		t.Fatalf("missing muted key %q", got)
	}
	if got := sgrBefore(t, timed, "5h"); got != "\x1b[34m" || sgrFaint(got) {
		t.Fatalf("time override %q", got)
	}

	writeColorConfig(t, cfg, `{"colors":{"agent":"1","human":"5"}}`+"\n")
	authors := m.View()
	adaLine := lineContaining(authors, "5h")
	if got := sgrBeforeLine(t, adaLine, "ada"); got != "\x1b[31m" {
		t.Fatalf("agent %q\n%s", got, adaLine)
	}
	humanLine := lineContaining(authors, "1h")
	if got := sgrBeforeLine(t, humanLine, "human"); got != "\x1b[35m" {
		t.Fatalf("human %q\n%s", got, humanLine)
	}

	t.Setenv("NO_COLOR", "1")
	writeColorConfig(t, cfg, `{"colors":{"muted":"#b0b0b0","time":"#112233"}}`+"\n")
	plain := m.View()
	if hasColorSequence(plain) {
		t.Fatalf("NO_COLOR view has color\n%s", plain)
	}
	vis := visibleLine(plain)
	for _, needle := range []string{"From:", "5h", "detail body"} {
		if !strings.Contains(vis, needle) {
			t.Fatalf("NO_COLOR missing %q\n%s", needle, vis)
		}
	}
}

func writeColorConfig(t *testing.T, cfg, body string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Join(cfg, "decide"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(cfg, "decide", "config.json"), []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
}

func sgrBefore(t *testing.T, view, needle string) string {
	t.Helper()
	line := lineContaining(view, needle)
	if line == "" {
		t.Fatalf("missing %q\n%s", needle, view)
	}
	return sgrBeforeLine(t, line, needle)
}

func sgrBeforeLine(t *testing.T, line, needle string) string {
	t.Helper()
	i := strings.Index(line, needle)
	if i < 0 {
		t.Fatalf("line missing %q\n%s", needle, line)
	}
	start := strings.LastIndex(line[:i], "\x1b[")
	if start < 0 {
		t.Fatalf("no sgr before %q\n%s", needle, line)
	}
	rel := strings.Index(line[start:], "m")
	if rel < 0 {
		t.Fatalf("open sgr before %q\n%s", needle, line)
	}
	return line[start : start+rel+1]
}

func sgrFaint(seq string) bool {
	inner := strings.TrimSuffix(strings.TrimPrefix(seq, "\x1b["), "m")
	parts := strings.Split(inner, ";")
	for i := 0; i < len(parts); i++ {
		if parts[i] == "38" || parts[i] == "48" {
			if i+1 < len(parts) && parts[i+1] == "5" && i+2 < len(parts) {
				i += 2
				continue
			}
			if i+1 < len(parts) && parts[i+1] == "2" && i+4 < len(parts) {
				i += 4
				continue
			}
		}
		if parts[i] == "2" {
			return true
		}
	}
	return false
}

func lineHasFaint(s string) bool {
	for i := 0; i < len(s); {
		if s[i] != 0x1b || i+1 >= len(s) || s[i+1] != '[' {
			i++
			continue
		}
		j := i + 2
		for j < len(s) && ((s[j] >= '0' && s[j] <= '9') || s[j] == ';') {
			j++
		}
		if j >= len(s) || s[j] != 'm' {
			i++
			continue
		}
		if sgrFaint(s[i : j+1]) {
			return true
		}
		i = j + 1
	}
	return false
}
