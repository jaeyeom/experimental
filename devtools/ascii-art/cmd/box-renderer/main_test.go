package main

import (
	"bytes"
	"strings"
	"testing"
)

func TestRunWithSectionFlags(t *testing.T) {
	var buf bytes.Buffer
	err := run([]string{
		"--section", "Title|Description",
	}, strings.NewReader(""), &buf)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	out := buf.String()
	if !strings.Contains(out, "Title") {
		t.Error("output should contain 'Title'")
	}
	if !strings.Contains(out, "Description") {
		t.Error("output should contain 'Description'")
	}
	if !strings.Contains(out, "┌") {
		t.Error("output should contain box border")
	}
}

func TestRunWithMultipleSections(t *testing.T) {
	var buf bytes.Buffer
	err := run([]string{
		"--section", "Section A",
		"--section", "Section B",
	}, strings.NewReader(""), &buf)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	out := buf.String()
	if !strings.Contains(out, "Section A") {
		t.Error("output should contain 'Section A'")
	}
	if !strings.Contains(out, "Section B") {
		t.Error("output should contain 'Section B'")
	}
	// Should have connector between sections
	if !strings.Contains(out, "▼") {
		t.Error("output should contain connector '▼'")
	}
}

func TestRunWithStdin(t *testing.T) {
	input := "Line A\nLine B\n---\nLine C\n"
	var buf bytes.Buffer
	err := run([]string{}, strings.NewReader(input), &buf)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	out := buf.String()
	if !strings.Contains(out, "Line A") {
		t.Error("output should contain 'Line A'")
	}
	if !strings.Contains(out, "Line C") {
		t.Error("output should contain 'Line C'")
	}
}

func TestRunWithBorderStyle(t *testing.T) {
	tests := []struct {
		border string
		expect string
	}{
		{"single", "┌"},
		{"double", "╔"},
		{"rounded", "╭"},
		{"ascii", "+"},
	}

	for _, tt := range tests {
		var buf bytes.Buffer
		err := run([]string{
			"--section", "Hello",
			"--border", tt.border,
		}, strings.NewReader(""), &buf)
		if err != nil {
			t.Errorf("border=%s: unexpected error: %v", tt.border, err)
			continue
		}
		if !strings.Contains(buf.String(), tt.expect) {
			t.Errorf("border=%s: expected %q in output", tt.border, tt.expect)
		}
	}
}

func TestRunWithPrefix(t *testing.T) {
	var buf bytes.Buffer
	err := run([]string{
		"--section", "Hello",
		"--prefix", "// ",
	}, strings.NewReader(""), &buf)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	lines := strings.Split(strings.TrimRight(buf.String(), "\n"), "\n")
	for i, line := range lines {
		if !strings.HasPrefix(line, "// ") {
			t.Errorf("line %d should start with '// ', got %q", i, line)
		}
	}
}

func TestRunWithCustomConnector(t *testing.T) {
	var buf bytes.Buffer
	err := run([]string{
		"--section", "A",
		"--section", "B",
		"--connector", "↓|↓",
	}, strings.NewReader(""), &buf)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	out := buf.String()
	if !strings.Contains(out, "↓") {
		t.Error("output should contain custom connector '↓'")
	}
	// Should have two connector lines (split by |)
	if strings.Count(out, "↓") < 2 {
		t.Error("output should contain at least two '↓' characters")
	}
}

func TestRunWithWidth(t *testing.T) {
	var buf bytes.Buffer
	err := run([]string{
		"--section", "Short",
		"--width", "40",
	}, strings.NewReader(""), &buf)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	lines := strings.Split(strings.TrimRight(buf.String(), "\n"), "\n")
	for i, line := range lines {
		if runeLen(line) != 40 {
			t.Errorf("line %d: expected width 40, got %d: %q", i, runeLen(line), line)
		}
	}
}

func TestRunInvalidBorder(t *testing.T) {
	var buf bytes.Buffer
	err := run([]string{
		"--section", "Hello",
		"--border", "invalid",
	}, strings.NewReader(""), &buf)
	if err == nil {
		t.Fatal("expected error for invalid border style")
	}
}

func TestRunNoInput(t *testing.T) {
	var buf bytes.Buffer
	err := run([]string{}, strings.NewReader(""), &buf)
	if err == nil {
		t.Fatal("expected error when no sections provided")
	}
}

func TestReadSectionsFromReader(t *testing.T) {
	input := "Title\nDescription\n---\nAnother Title\nAnother Desc\n"
	sections, err := readSectionsFromReader(strings.NewReader(input))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(sections) != 2 {
		t.Fatalf("expected 2 sections, got %d", len(sections))
	}
	if sections[0] != "Title|Description" {
		t.Errorf("section 0: expected 'Title|Description', got %q", sections[0])
	}
	if sections[1] != "Another Title|Another Desc" {
		t.Errorf("section 1: expected 'Another Title|Another Desc', got %q", sections[1])
	}
}

func TestReadSectionsFromReaderEmpty(t *testing.T) {
	sections, err := readSectionsFromReader(strings.NewReader(""))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(sections) != 0 {
		t.Errorf("expected 0 sections, got %d", len(sections))
	}
}

func runeLen(s string) int {
	return len([]rune(s))
}
