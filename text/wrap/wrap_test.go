package wrap

import (
	"strings"
	"testing"
)

func TestText(t *testing.T) {
	tests := []struct {
		name     string
		text     string
		width    int
		expected string
		lines    int
	}{
		{
			name:     "simple text",
			text:     "hello world this is a test",
			width:    10,
			expected: "hello\nworld this\nis a test",
			lines:    3,
		},
		{
			name:     "exact width",
			text:     "hello world",
			width:    11,
			expected: "hello world",
			lines:    1,
		},
		{
			name:     "single word",
			text:     "hello",
			width:    10,
			expected: "hello",
			lines:    1,
		},
		{
			name:     "empty text",
			text:     "",
			width:    10,
			expected: "",
			lines:    0,
		},
		{
			name:     "zero width",
			text:     "hello world",
			width:    0,
			expected: "hello world",
			lines:    1,
		},
		{
			name:     "negative width",
			text:     "hello world",
			width:    -5,
			expected: "hello world",
			lines:    1,
		},
		{
			name:     "multiple paragraphs",
			text:     "first paragraph\n\nsecond paragraph here",
			width:    10,
			expected: "first\nparagraph\n\nsecond\nparagraph\nhere",
			lines:    6,
		},
		{
			name:     "whitespace handling",
			text:     "  hello   world  ",
			width:    8,
			expected: "hello\nworld",
			lines:    2,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			wrapped, lines := Text(tt.text, tt.width)
			if wrapped != tt.expected {
				t.Errorf("Text() wrapped = %q, expected %q", wrapped, tt.expected)
			}
			if lines != tt.lines {
				t.Errorf("Text() lines = %d, expected %d", lines, tt.lines)
			}
		})
	}
}

func TestTextIndent(t *testing.T) {
	tests := []struct {
		name     string
		text     string
		width    int
		indent   string
		expected string
		lines    int
	}{
		{
			name:     "basic indent",
			text:     "hello world this is a test",
			width:    15,
			indent:   "  ",
			expected: "hello world\n  this is a\n  test",
			lines:    3,
		},
		{
			name:     "large indent",
			text:     "hello world test",
			width:    10,
			indent:   "    ",
			expected: "hello\n    world\n    test",
			lines:    3,
		},
		{
			name:     "indent larger than width",
			text:     "hello world",
			width:    5,
			indent:   "      ",
			expected: "hello world",
			lines:    1,
		},
		{
			name:     "empty indent",
			text:     "hello world test",
			width:    10,
			indent:   "",
			expected: "hello\nworld test",
			lines:    2,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			wrapped, lines := TextIndent(tt.text, tt.width, tt.indent)
			if wrapped != tt.expected {
				t.Errorf("TextIndent() wrapped = %q, expected %q", wrapped, tt.expected)
			}
			if lines != tt.lines {
				t.Errorf("TextIndent() lines = %d, expected %d", lines, tt.lines)
			}
		})
	}
}

func TestWordWrap(t *testing.T) {
	tests := []struct {
		name     string
		text     string
		width    int
		expected string
		lines    int
	}{
		{
			name:     "normal words",
			text:     "hello world test",
			width:    10,
			expected: "hello\nworld test",
			lines:    2,
		},
		{
			name:     "long word",
			text:     "hello verylongwordthatexceedswidth test",
			width:    10,
			expected: "hello\nverylongwo\nrdthatexce\nedswidth\ntest",
			lines:    5,
		},
		{
			name:     "multiple long words",
			text:     "superlongword anotherlongword",
			width:    8,
			expected: "superlon\ngword\nanotherl\nongword",
			lines:    4,
		},
		{
			name:     "word exactly width",
			text:     "hello exactword test",
			width:    9,
			expected: "hello\nexactword\ntest",
			lines:    3,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			wrapped, lines := WordWrap(tt.text, tt.width)
			if wrapped != tt.expected {
				t.Errorf("WordWrap() wrapped = %q, expected %q", wrapped, tt.expected)
			}
			if lines != tt.lines {
				t.Errorf("WordWrap() lines = %d, expected %d", lines, tt.lines)
			}
		})
	}
}

func TestSoftWrap(t *testing.T) {
	tests := []struct {
		name     string
		text     string
		width    int
		expected string
		lines    int
	}{
		{
			name:     "break at spaces",
			text:     "hello world this is a test",
			width:    12,
			expected: "hello world\nthis is a\ntest",
			lines:    3,
		},
		{
			name:     "no spaces to break",
			text:     "helloworldthisisatest",
			width:    10,
			expected: "helloworldthisisatest",
			lines:    1,
		},
		{
			name:     "mixed content",
			text:     "hello world, this-is a test!",
			width:    15,
			expected: "hello world,\nthis-is a test!",
			lines:    2,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			wrapped, lines := SoftWrap(tt.text, tt.width)
			if wrapped != tt.expected {
				t.Errorf("SoftWrap() wrapped = %q, expected %q", wrapped, tt.expected)
			}
			if lines != tt.lines {
				t.Errorf("SoftWrap() lines = %d, expected %d", lines, tt.lines)
			}
		})
	}
}

func TestCountLines(t *testing.T) {
	tests := []struct {
		name     string
		text     string
		expected int
	}{
		{
			name:     "empty string",
			text:     "",
			expected: 0,
		},
		{
			name:     "single line",
			text:     "hello",
			expected: 1,
		},
		{
			name:     "two lines",
			text:     "hello\nworld",
			expected: 2,
		},
		{
			name:     "three lines",
			text:     "hello\nworld\ntest",
			expected: 3,
		},
		{
			name:     "trailing newline",
			text:     "hello\nworld\n",
			expected: 3,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := countLines(tt.text)
			if result != tt.expected {
				t.Errorf("countLines() = %d, expected %d", result, tt.expected)
			}
		})
	}
}

func TestBreakLongWord(t *testing.T) {
	tests := []struct {
		name     string
		word     string
		width    int
		expected []string
	}{
		{
			name:     "word shorter than width",
			word:     "hello",
			width:    10,
			expected: []string{"hello"},
		},
		{
			name:     "word exactly width",
			word:     "hello",
			width:    5,
			expected: []string{"hello"},
		},
		{
			name:     "word longer than width",
			word:     "verylongword",
			width:    4,
			expected: []string{"very", "long", "word"},
		},
		{
			name:     "word with unicode",
			word:     "café",
			width:    2,
			expected: []string{"ca", "fé"},
		},
		{
			name:     "single character width",
			word:     "hello",
			width:    1,
			expected: []string{"h", "e", "l", "l", "o"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := breakLongWord(tt.word, tt.width)
			if len(result) != len(tt.expected) {
				t.Errorf("breakLongWord() length = %d, expected %d", len(result), len(tt.expected))
				return
			}
			for i, line := range result {
				if line != tt.expected[i] {
					t.Errorf("breakLongWord()[%d] = %q, expected %q", i, line, tt.expected[i])
				}
			}
		})
	}
}

func TestWrapParagraph(t *testing.T) {
	tests := []struct {
		name     string
		text     string
		width    int
		expected []string
	}{
		{
			name:     "empty paragraph",
			text:     "",
			width:    10,
			expected: []string{""},
		},
		{
			name:     "whitespace only",
			text:     "   ",
			width:    10,
			expected: []string{""},
		},
		{
			name:     "single word",
			text:     "hello",
			width:    10,
			expected: []string{"hello"},
		},
		{
			name:     "multiple words fitting",
			text:     "hello world",
			width:    15,
			expected: []string{"hello world"},
		},
		{
			name:     "multiple words not fitting",
			text:     "hello world test",
			width:    10,
			expected: []string{"hello", "world test"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := wrapParagraph(tt.text, tt.width)
			if len(result) != len(tt.expected) {
				t.Errorf("wrapParagraph() length = %d, expected %d", len(result), len(tt.expected))
				return
			}
			for i, line := range result {
				if line != tt.expected[i] {
					t.Errorf("wrapParagraph()[%d] = %q, expected %q", i, line, tt.expected[i])
				}
			}
		})
	}
}

// Benchmark tests.
func BenchmarkText(b *testing.B) {
	text := strings.Repeat("hello world this is a test of text wrapping functionality ", 100)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		Text(text, 80)
	}
}

func BenchmarkWordWrap(b *testing.B) {
	text := strings.Repeat("hello world this is a test of text wrapping functionality ", 100)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		WordWrap(text, 80)
	}
}

func BenchmarkSoftWrap(b *testing.B) {
	text := strings.Repeat("hello world this is a test of text wrapping functionality ", 100)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		SoftWrap(text, 80)
	}
}
