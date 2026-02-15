package box

import (
	"fmt"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/text/canvas"
)

func TestNew(t *testing.T) {
	b := New()
	if b == nil {
		t.Fatal("New() returned nil")
	}
}

func TestSingleSectionAutoWidth(t *testing.T) {
	got := New().
		AddSection("Hello").
		Render()

	lines := strings.Split(got, "\n")
	if len(lines) != 3 {
		t.Fatalf("expected 3 lines, got %d:\n%s", len(lines), got)
	}

	// Top and bottom borders should be same length
	if len(lines[0]) != len(lines[2]) {
		t.Errorf("top border len %d != bottom border len %d", len(lines[0]), len(lines[2]))
	}

	// Content line should contain "Hello"
	if !strings.Contains(lines[1], "Hello") {
		t.Errorf("content line should contain 'Hello', got %q", lines[1])
	}

	// All lines should be same rune length
	for i, line := range lines {
		if runeLen(line) != runeLen(lines[0]) {
			t.Errorf("line %d rune length %d != line 0 rune length %d", i, runeLen(line), runeLen(lines[0]))
		}
	}
}

func TestMultipleSectionsDefaultConnector(t *testing.T) {
	got := New().
		AddSection("Title").
		AddSection("Description").
		Render()

	// Should contain connector characters between sections
	if !strings.Contains(got, "│") {
		t.Error("output should contain vertical connector '│'")
	}
	if !strings.Contains(got, "▼") {
		t.Error("output should contain arrow connector '▼'")
	}
	if !strings.Contains(got, "Title") {
		t.Error("output should contain 'Title'")
	}
	if !strings.Contains(got, "Description") {
		t.Error("output should contain 'Description'")
	}
}

func TestCustomConnector(t *testing.T) {
	got := New().
		WithConnector("||", "VV").
		AddSection("A").
		AddSection("B").
		Render()

	if !strings.Contains(got, "||") {
		t.Error("output should contain custom connector '||'")
	}
	if !strings.Contains(got, "VV") {
		t.Error("output should contain custom connector 'VV'")
	}
}

func TestExplicitWidth(t *testing.T) {
	width := 40
	got := New().
		WithWidth(width).
		AddSection("Short").
		Render()

	lines := strings.Split(got, "\n")
	for i, line := range lines {
		if runeLen(line) != width {
			t.Errorf("line %d: expected rune width %d, got %d: %q", i, width, runeLen(line), line)
		}
	}
}

func TestBorderStyleDouble(t *testing.T) {
	got := New().
		WithStyle(canvas.BorderDouble).
		AddSection("Hello").
		Render()

	if !strings.Contains(got, "╔") {
		t.Error("double border should contain '╔'")
	}
	if !strings.Contains(got, "║") {
		t.Error("double border should contain '║'")
	}
}

func TestBorderStyleRounded(t *testing.T) {
	got := New().
		WithStyle(canvas.BorderRounded).
		AddSection("Hello").
		Render()

	if !strings.Contains(got, "╭") {
		t.Error("rounded border should contain '╭'")
	}
}

func TestBorderStyleASCII(t *testing.T) {
	got := New().
		WithStyle(canvas.BorderASCII).
		AddSection("Hello").
		Render()

	if !strings.Contains(got, "+") {
		t.Error("ASCII border should contain '+'")
	}
	if !strings.Contains(got, "-") {
		t.Error("ASCII border should contain '-'")
	}
	if !strings.Contains(got, "|") {
		t.Error("ASCII border should contain '|'")
	}
}

func TestCommentPrefix(t *testing.T) {
	got := New().
		WithPrefix("// ").
		AddSection("Hello").
		Render()

	lines := strings.Split(got, "\n")
	for i, line := range lines {
		if !strings.HasPrefix(line, "// ") {
			t.Errorf("line %d should start with '// ', got %q", i, line)
		}
	}
}

func TestGoldenTargetOutput(t *testing.T) {
	want := strings.Join([]string{
		"┌─────────────────────────────────────────────────────────┐",
		"│  gabyx-githooks-setup                                   │",
		"│  Shared Git hooks run automatically on commit and push  │",
		"│                            │                            │",
		"│                            ▼                            │",
		"│  makefile-workflow                                      │",
		"│  Hooks call `make check` — format, lint, test, build    │",
		"│                            │                            │",
		"│                            ▼                            │",
		"│  *-dev (e.g. go-dev)                                    │",
		"│  Language-specific conventions the checks enforce       │",
		"└─────────────────────────────────────────────────────────┘",
	}, "\n")

	got := New().
		AddSection(
			"gabyx-githooks-setup",
			"Shared Git hooks run automatically on commit and push",
		).
		AddSection(
			"makefile-workflow",
			"Hooks call `make check` — format, lint, test, build",
		).
		AddSection(
			"*-dev (e.g. go-dev)",
			"Language-specific conventions the checks enforce",
		).
		Render()

	if got != want {
		t.Errorf("golden test failed.\nwant:\n%s\n\ngot:\n%s", want, got)
		// Show line-by-line diff
		wantLines := strings.Split(want, "\n")
		gotLines := strings.Split(got, "\n")
		maxLines := len(wantLines)
		if len(gotLines) > maxLines {
			maxLines = len(gotLines)
		}
		for i := 0; i < maxLines; i++ {
			var wl, gl string
			if i < len(wantLines) {
				wl = wantLines[i]
			}
			if i < len(gotLines) {
				gl = gotLines[i]
			}
			if wl != gl {
				t.Errorf("line %d diff:\n  want(%d): %q\n  got (%d): %q", i, runeLen(wl), wl, runeLen(gl), gl)
			}
		}
	}
}

func TestEmptySection(t *testing.T) {
	got := New().
		AddSection().
		Render()

	// Should produce a valid box even with no content lines
	lines := strings.Split(got, "\n")
	if len(lines) < 2 {
		t.Errorf("expected at least 2 lines (top+bottom border), got %d", len(lines))
	}
}

func TestSingleCharacterLine(t *testing.T) {
	got := New().
		WithPadding(0).
		AddSection("X").
		Render()

	if !strings.Contains(got, "X") {
		t.Error("output should contain 'X'")
	}

	// All lines should be same rune length
	lines := strings.Split(got, "\n")
	for i, line := range lines {
		if runeLen(line) != runeLen(lines[0]) {
			t.Errorf("line %d rune length %d != line 0 rune length %d", i, runeLen(line), runeLen(lines[0]))
		}
	}
}

func TestPadding(t *testing.T) {
	got := New().
		WithPadding(4).
		AddSection("Hi").
		Render()

	lines := strings.Split(got, "\n")
	// The content line should have 4 spaces of padding after the border char
	contentLine := lines[1]
	// After '│' there should be 4 spaces before 'Hi'
	afterBorder := strings.TrimPrefix(contentLine, "│")
	if !strings.HasPrefix(afterBorder, "    Hi") {
		t.Errorf("expected 4-space padding before content, got %q", contentLine)
	}
}

func TestMultipleSectionsMultipleLines(t *testing.T) {
	got := New().
		AddSection("Line1a", "Line1b").
		AddSection("Line2a", "Line2b").
		Render()

	if !strings.Contains(got, "Line1a") {
		t.Error("missing Line1a")
	}
	if !strings.Contains(got, "Line1b") {
		t.Error("missing Line1b")
	}
	if !strings.Contains(got, "Line2a") {
		t.Error("missing Line2a")
	}
	if !strings.Contains(got, "Line2b") {
		t.Error("missing Line2b")
	}
}

func TestWithWidthZeroAutoCalculate(t *testing.T) {
	// Width 0 should auto-calculate (same as default)
	auto := New().
		WithWidth(0).
		AddSection("Hello World").
		Render()

	def := New().
		AddSection("Hello World").
		Render()

	if auto != def {
		t.Errorf("WithWidth(0) should produce same result as default.\nauto:\n%s\n\ndef:\n%s", auto, def)
	}
}

// runeLen returns the number of runes in a string.
func runeLen(s string) int {
	return len([]rune(s))
}

func ExampleBox_Render() {
	output := New().
		AddSection(
			"gabyx-githooks-setup",
			"Shared Git hooks run automatically on commit and push",
		).
		AddSection(
			"makefile-workflow",
			"Hooks call `make check` — format, lint, test, build",
		).
		AddSection(
			"*-dev (e.g. go-dev)",
			"Language-specific conventions the checks enforce",
		).
		Render()

	fmt.Println(output)
	// Output:
	// ┌─────────────────────────────────────────────────────────┐
	// │  gabyx-githooks-setup                                   │
	// │  Shared Git hooks run automatically on commit and push  │
	// │                            │                            │
	// │                            ▼                            │
	// │  makefile-workflow                                      │
	// │  Hooks call `make check` — format, lint, test, build    │
	// │                            │                            │
	// │                            ▼                            │
	// │  *-dev (e.g. go-dev)                                    │
	// │  Language-specific conventions the checks enforce       │
	// └─────────────────────────────────────────────────────────┘
}

func ExampleBox_Render_prefix() {
	output := New().
		WithPrefix("// ").
		WithStyle(canvas.BorderRounded).
		AddSection("Hello, World!").
		Render()

	fmt.Println(output)
	// Output:
	// // ╭─────────────────╮
	// // │  Hello, World!  │
	// // ╰─────────────────╯
}
