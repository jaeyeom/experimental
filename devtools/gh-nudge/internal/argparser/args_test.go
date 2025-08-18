package argparser

import (
	"reflect"
	"testing"
)

func TestArgParser_BasicPositionals(t *testing.T) {
	parser := NewArgParser([]string{"arg1", "arg2", "arg3"})

	if parser.PositionalCount() != 3 {
		t.Errorf("Expected 3 positionals, got %d", parser.PositionalCount())
	}

	if parser.GetPositional(0) != "arg1" {
		t.Errorf("Expected 'arg1', got '%s'", parser.GetPositional(0))
	}

	if parser.GetPositional(1) != "arg2" {
		t.Errorf("Expected 'arg2', got '%s'", parser.GetPositional(1))
	}

	if parser.GetPositional(2) != "arg3" {
		t.Errorf("Expected 'arg3', got '%s'", parser.GetPositional(2))
	}

	// Test out of bounds
	if parser.GetPositional(3) != "" {
		t.Errorf("Expected empty string for out of bounds, got '%s'", parser.GetPositional(3))
	}
}

func TestArgParser_LongOptions(t *testing.T) {
	parser := NewArgParser([]string{"--verbose", "--format", "json", "--count=5"})

	if !parser.HasOption("verbose") {
		t.Error("Expected verbose option to be present")
	}

	if parser.GetOption("verbose") != "true" {
		t.Errorf("Expected 'true' for verbose, got '%s'", parser.GetOption("verbose"))
	}

	if parser.GetOption("format") != "json" {
		t.Errorf("Expected 'json' for format, got '%s'", parser.GetOption("format"))
	}

	if parser.GetOption("count") != "5" {
		t.Errorf("Expected '5' for count, got '%s'", parser.GetOption("count"))
	}
}

func TestArgParser_ShortOptions(t *testing.T) {
	parser := NewArgParser([]string{"-v", "-f", "yaml", "-c", "10"})

	if !parser.HasOption("v") {
		t.Error("Expected v option to be present")
	}

	if parser.GetOption("v") != "true" {
		t.Errorf("Expected 'true' for v, got '%s'", parser.GetOption("v"))
	}

	if parser.GetOption("f") != "yaml" {
		t.Errorf("Expected 'yaml' for f, got '%s'", parser.GetOption("f"))
	}

	if parser.GetOption("c") != "10" {
		t.Errorf("Expected '10' for c, got '%s'", parser.GetOption("c"))
	}
}

func TestArgParser_MixedOptionsAndPositionals(t *testing.T) {
	parser := NewArgParser([]string{"pos1", "--verbose", "pos2", "-f", "json", "pos3"})

	// verbose is a boolean flag so it doesn't consume pos2
	// f is not a boolean flag so it consumes json
	expectedPositionals := []string{"pos1", "pos2", "pos3"}
	if !reflect.DeepEqual(parser.GetPositionals(), expectedPositionals) {
		t.Errorf("Expected %v, got %v", expectedPositionals, parser.GetPositionals())
	}

	if !parser.HasOption("verbose") {
		t.Error("Expected verbose option to be present")
	}

	if parser.GetOption("f") != "json" {
		t.Errorf("Expected 'json' for f, got '%s'", parser.GetOption("f"))
	}
}

func TestArgParser_DoubleDashSeparator(t *testing.T) {
	parser := NewArgParser([]string{"pos1", "--verbose", "--", "--not-an-option", "-f", "pos2"})

	expectedPositionals := []string{"pos1", "--not-an-option", "-f", "pos2"}
	if !reflect.DeepEqual(parser.GetPositionals(), expectedPositionals) {
		t.Errorf("Expected %v, got %v", expectedPositionals, parser.GetPositionals())
	}

	if !parser.HasOption("verbose") {
		t.Error("Expected verbose option to be present")
	}

	// Options after -- should not be parsed as options
	if parser.HasOption("f") {
		t.Error("Expected f option to not be present after --")
	}
}

func TestArgParser_HelpFlags(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{"short help", []string{"-h"}},
		{"long help", []string{"--help"}},
		{"help with other args", []string{"pos1", "-h", "pos2"}},
		{"help with options", []string{"--verbose", "--help", "-f", "json"}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			parser := NewArgParser(tt.args)
			if !parser.IsHelp() {
				t.Error("Expected help to be true")
			}
		})
	}
}

func TestArgParser_MultipleValues(t *testing.T) {
	parser := NewArgParser([]string{"--include", "file1", "--include", "file2", "--include", "file3"})

	expectedValues := []string{"file1", "file2", "file3"}
	if !reflect.DeepEqual(parser.GetOptionValues("include"), expectedValues) {
		t.Errorf("Expected %v, got %v", expectedValues, parser.GetOptionValues("include"))
	}

	// First value should be accessible via GetOption
	if parser.GetOption("include") != "file1" {
		t.Errorf("Expected 'file1', got '%s'", parser.GetOption("include"))
	}
}

func TestArgParser_EqualsFormat(t *testing.T) {
	parser := NewArgParser([]string{"--format=json", "--count=42", "--verbose=false"})

	if parser.GetOption("format") != "json" {
		t.Errorf("Expected 'json', got '%s'", parser.GetOption("format"))
	}

	if parser.GetOption("count") != "42" {
		t.Errorf("Expected '42', got '%s'", parser.GetOption("count"))
	}

	if parser.GetOption("verbose") != "false" {
		t.Errorf("Expected 'false', got '%s'", parser.GetOption("verbose"))
	}
}

func TestArgParser_EmptyArgs(t *testing.T) {
	parser := NewArgParser([]string{})

	if parser.PositionalCount() != 0 {
		t.Errorf("Expected 0 positionals, got %d", parser.PositionalCount())
	}

	if parser.IsHelp() {
		t.Error("Expected help to be false for empty args")
	}

	if parser.HasOption("any") {
		t.Error("Expected no options for empty args")
	}
}

func TestArgParser_ValidateOptions(t *testing.T) {
	parser := NewArgParser([]string{"--verbose", "--format", "json", "--unknown"})

	// Should pass with allowed options
	err := parser.ValidateOptions([]string{"verbose", "format", "unknown"})
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}

	// Should fail with unknown option
	err = parser.ValidateOptions([]string{"verbose", "format"})
	if err == nil {
		t.Error("Expected error for unknown option")
	}

	expectedError := "unknown option: --unknown"
	if err.Error() != expectedError {
		t.Errorf("Expected '%s', got '%s'", expectedError, err.Error())
	}
}

func TestArgParser_RequirePositionals(t *testing.T) {
	parser := NewArgParser([]string{"arg1", "arg2"})

	// Should pass with minimum requirement
	err := parser.RequirePositionals(2, "command <arg1> <arg2>")
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}

	// Should pass with less than minimum
	err = parser.RequirePositionals(1, "command <arg1>")
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}

	// Should fail with more than available
	err = parser.RequirePositionals(3, "command <arg1> <arg2> <arg3>")
	if err == nil {
		t.Error("Expected error for insufficient arguments")
	}
}

func TestArgParser_RequireExactPositionals(t *testing.T) {
	parser := NewArgParser([]string{"arg1", "arg2"})

	// Should pass with exact count
	err := parser.RequireExactPositionals(2, "command <arg1> <arg2>")
	if err != nil {
		t.Errorf("Expected no error, got %v", err)
	}

	// Should fail with different count
	err = parser.RequireExactPositionals(1, "command <arg1>")
	if err == nil {
		t.Error("Expected error for wrong argument count")
	}

	err = parser.RequireExactPositionals(3, "command <arg1> <arg2> <arg3>")
	if err == nil {
		t.Error("Expected error for wrong argument count")
	}
}

func TestArgParser_ComplexScenario(t *testing.T) {
	// Simulate: mycommand file1 --verbose --format=json --include file2 --include file3 -- --not-option file4
	parser := NewArgParser([]string{
		"file1", "--verbose", "--format=json", "--include", "file2", "--include", "file3", "--", "--not-option", "file4",
	})

	expectedPositionals := []string{"file1", "--not-option", "file4"}
	if !reflect.DeepEqual(parser.GetPositionals(), expectedPositionals) {
		t.Errorf("Expected %v, got %v", expectedPositionals, parser.GetPositionals())
	}

	if !parser.HasOption("verbose") {
		t.Error("Expected verbose option")
	}

	if parser.GetOption("format") != "json" {
		t.Errorf("Expected 'json', got '%s'", parser.GetOption("format"))
	}

	expectedIncludes := []string{"file2", "file3"}
	if !reflect.DeepEqual(parser.GetOptionValues("include"), expectedIncludes) {
		t.Errorf("Expected %v, got %v", expectedIncludes, parser.GetOptionValues("include"))
	}
}

func TestArgParser_BooleanFlags(t *testing.T) {
	parser := NewArgParser([]string{"--verbose", "--debug", "--quiet"})

	if parser.GetOption("verbose") != "true" {
		t.Errorf("Expected 'true' for verbose, got '%s'", parser.GetOption("verbose"))
	}

	if parser.GetOption("debug") != "true" {
		t.Errorf("Expected 'true' for debug, got '%s'", parser.GetOption("debug"))
	}

	if parser.GetOption("quiet") != "true" {
		t.Errorf("Expected 'true' for quiet, got '%s'", parser.GetOption("quiet"))
	}
}

func TestArgParser_OptionWithDashInValue(t *testing.T) {
	parser := NewArgParser([]string{"--message", "hello-world", "--format", "json"})

	if parser.GetOption("message") != "hello-world" {
		t.Errorf("Expected 'hello-world', got '%s'", parser.GetOption("message"))
	}

	if parser.GetOption("format") != "json" {
		t.Errorf("Expected 'json', got '%s'", parser.GetOption("format"))
	}
}

func TestArgParser_GetOptionValues_NonExistent(t *testing.T) {
	parser := NewArgParser([]string{"--verbose"})

	values := parser.GetOptionValues("nonexistent")
	if values != nil {
		t.Errorf("Expected nil for non-existent option, got %v", values)
	}
}

func TestArgParser_GetOption_NonExistent(t *testing.T) {
	parser := NewArgParser([]string{"--verbose"})

	value := parser.GetOption("nonexistent")
	if value != "" {
		t.Errorf("Expected empty string for non-existent option, got '%s'", value)
	}
}

func TestArgParser_WithBooleanFlags(t *testing.T) {
	// Test custom boolean flags
	parser := NewArgParser([]string{"--custom-flag", "file.txt", "--another-bool", "--output", "result.txt"},
		WithBooleanFlags("custom-flag", "another-bool"))

	// custom-flag should be treated as boolean and not consume file.txt
	if parser.GetOption("custom-flag") != "true" {
		t.Errorf("Expected 'true' for custom-flag, got '%s'", parser.GetOption("custom-flag"))
	}

	// another-bool should be treated as boolean
	if parser.GetOption("another-bool") != "true" {
		t.Errorf("Expected 'true' for another-bool, got '%s'", parser.GetOption("another-bool"))
	}

	// output should consume result.txt as value
	if parser.GetOption("output") != "result.txt" {
		t.Errorf("Expected 'result.txt' for output, got '%s'", parser.GetOption("output"))
	}

	// file.txt should be a positional argument
	expectedPositionals := []string{"file.txt"}
	if !reflect.DeepEqual(parser.GetPositionals(), expectedPositionals) {
		t.Errorf("Expected %v, got %v", expectedPositionals, parser.GetPositionals())
	}
}

func TestArgParser_WithBooleanFlags_OverrideDefault(t *testing.T) {
	// Test that custom boolean flags are added to defaults, not replacing them
	parser := NewArgParser([]string{"--verbose", "--custom", "--output", "file.txt"},
		WithBooleanFlags("custom"))

	// Default boolean flag should still work
	if parser.GetOption("verbose") != "true" {
		t.Errorf("Expected 'true' for verbose, got '%s'", parser.GetOption("verbose"))
	}

	// Custom boolean flag should work
	if parser.GetOption("custom") != "true" {
		t.Errorf("Expected 'true' for custom, got '%s'", parser.GetOption("custom"))
	}

	// Non-boolean flag should consume value
	if parser.GetOption("output") != "file.txt" {
		t.Errorf("Expected 'file.txt' for output, got '%s'", parser.GetOption("output"))
	}
}

func TestArgParser_WithBooleanFlags_Empty(t *testing.T) {
	// Test that empty WithBooleanFlags still has default flags
	parser := NewArgParser([]string{"--verbose", "file.txt"}, WithBooleanFlags())

	// Default boolean flags should still work
	if parser.GetOption("verbose") != "true" {
		t.Errorf("Expected 'true' for verbose, got '%s'", parser.GetOption("verbose"))
	}

	// file.txt should be positional since verbose is still a boolean flag
	expectedPositionals := []string{"file.txt"}
	if !reflect.DeepEqual(parser.GetPositionals(), expectedPositionals) {
		t.Errorf("Expected %v, got %v", expectedPositionals, parser.GetPositionals())
	}
}

func TestArgParser_BackwardCompatibility(t *testing.T) {
	// Test that existing code without WithBooleanFlags still works
	parser := NewArgParser([]string{"--verbose", "--debug", "file.txt"})

	// Default boolean flags should work as before
	if parser.GetOption("verbose") != "true" {
		t.Errorf("Expected 'true' for verbose, got '%s'", parser.GetOption("verbose"))
	}

	if parser.GetOption("debug") != "true" {
		t.Errorf("Expected 'true' for debug, got '%s'", parser.GetOption("debug"))
	}

	// file.txt should be positional
	expectedPositionals := []string{"file.txt"}
	if !reflect.DeepEqual(parser.GetPositionals(), expectedPositionals) {
		t.Errorf("Expected %v, got %v", expectedPositionals, parser.GetPositionals())
	}
}

func TestArgParser_WithBooleanFlags_MultipleOptions(t *testing.T) {
	// Test multiple WithBooleanFlags calls
	parser := NewArgParser([]string{"--flag1", "--flag2", "--flag3", "file.txt"},
		WithBooleanFlags("flag1"),
		WithBooleanFlags("flag2", "flag3"))

	// All custom flags should be boolean
	if parser.GetOption("flag1") != "true" {
		t.Errorf("Expected 'true' for flag1, got '%s'", parser.GetOption("flag1"))
	}

	if parser.GetOption("flag2") != "true" {
		t.Errorf("Expected 'true' for flag2, got '%s'", parser.GetOption("flag2"))
	}

	if parser.GetOption("flag3") != "true" {
		t.Errorf("Expected 'true' for flag3, got '%s'", parser.GetOption("flag3"))
	}

	// file.txt should be positional
	expectedPositionals := []string{"file.txt"}
	if !reflect.DeepEqual(parser.GetPositionals(), expectedPositionals) {
		t.Errorf("Expected %v, got %v", expectedPositionals, parser.GetPositionals())
	}
}

func TestArgParser_NonBooleanFlagWithValue(t *testing.T) {
	// Test case that demonstrates potential issue: boolean flags should NOT consume next argument
	// when there's a value that looks like an argument but should be left as positional

	// Test with a known boolean flag - verbose should not consume "file.txt" as its value
	parser1 := NewArgParser([]string{"--verbose", "file.txt"})

	t.Logf("Boolean flag test: verbose value='%s', positionals=%v", parser1.GetOption("verbose"), parser1.GetPositionals())

	if parser1.GetOption("verbose") != "true" {
		t.Errorf("Expected 'true' for verbose flag, got '%s'", parser1.GetOption("verbose"))
	}

	expectedPositionals1 := []string{"file.txt"}
	if !reflect.DeepEqual(parser1.GetPositionals(), expectedPositionals1) {
		t.Errorf("Expected %v positionals for boolean flag, got %v", expectedPositionals1, parser1.GetPositionals())
	}

	// Test the specific issue: --comment-id ID vs --comment-id=ID
	// Test space syntax (potentially broken)
	args1 := []string{"resolve", "owner/repo", "123", "--comment-id", "a1b2c3d4"}
	parser2 := NewArgParser(args1)

	t.Logf("Space syntax test: comment-id='%s', positionals=%v", parser2.GetOption("comment-id"), parser2.GetPositionals())

	if parser2.GetOption("comment-id") != "a1b2c3d4" {
		t.Fatalf("ISSUE FOUND: --comment-id a1b2c3d4 should set comment-id to 'a1b2c3d4', got '%s'", parser2.GetOption("comment-id"))
	}

	expectedPositionals2 := []string{"resolve", "owner/repo", "123"}
	if !reflect.DeepEqual(parser2.GetPositionals(), expectedPositionals2) {
		t.Fatalf("ISSUE FOUND: --comment-id a1b2c3d4 should consume the ID, leaving positionals %v, got %v", expectedPositionals2, parser2.GetPositionals())
	}

	// Test equals syntax (should work)
	args2 := []string{"resolve", "owner/repo", "123", "--comment-id=a1b2c3d4"}
	parser3 := NewArgParser(args2)

	t.Logf("Equals syntax test: comment-id='%s', positionals=%v", parser3.GetOption("comment-id"), parser3.GetPositionals())

	if parser3.GetOption("comment-id") != "a1b2c3d4" {
		t.Errorf("Expected 'a1b2c3d4' for comment-id with equals syntax, got '%s'", parser3.GetOption("comment-id"))
	}

	expectedPositionals3 := []string{"resolve", "owner/repo", "123"}
	if !reflect.DeepEqual(parser3.GetPositionals(), expectedPositionals3) {
		t.Errorf("Expected %v positionals with equals syntax, got %v", expectedPositionals3, parser3.GetPositionals())
	}

	// Both should produce identical results
	if parser2.GetOption("comment-id") != parser3.GetOption("comment-id") {
		t.Errorf("Space and equals syntax should produce same result: space='%s', equals='%s'",
			parser2.GetOption("comment-id"), parser3.GetOption("comment-id"))
	}

	// Test what happens when we have exactly 2 positionals + comment-id flag
	// This mimics the exact issue where resolve command requires exactly 2 positionals
	if parser2.PositionalCount() != 3 {
		t.Errorf("Expected exactly 3 positionals for space syntax, got %d", parser2.PositionalCount())
	}

	if parser3.PositionalCount() != 3 {
		t.Errorf("Expected exactly 3 positionals for equals syntax, got %d", parser3.PositionalCount())
	}
}

func TestArgParser_AutoDetectArgsIssue(t *testing.T) {
	// Test the specific issue where autoDetectArgs function incorrectly splits arguments
	// This test mimics the exact scenario: gh-pr-review delete --comment-id 7ff

	args := []string{"--comment-id", "7ff"}
	parser := NewArgParser(args)

	t.Logf("delete --comment-id 7ff: comment-id='%s', positionals=%v", parser.GetOption("comment-id"), parser.GetPositionals())

	// The key issue: comment-id should get value "7ff", not "true"
	if parser.GetOption("comment-id") != "7ff" {
		t.Fatalf("CRITICAL BUG: --comment-id 7ff should set comment-id to '7ff', got '%s'", parser.GetOption("comment-id"))
	}

	// There should be no positional arguments
	if parser.PositionalCount() != 0 {
		t.Fatalf("CRITICAL BUG: --comment-id 7ff should not create positional args, got %v", parser.GetPositionals())
	}

	// Test with equals syntax for comparison
	args2 := []string{"--comment-id=7ff"}
	parser2 := NewArgParser(args2)

	if parser2.GetOption("comment-id") != "7ff" {
		t.Errorf("Expected '7ff' for equals syntax, got '%s'", parser2.GetOption("comment-id"))
	}

	if parser2.PositionalCount() != 0 {
		t.Errorf("Expected no positionals for equals syntax, got %v", parser2.GetPositionals())
	}

	// Both should produce identical results
	if parser.GetOption("comment-id") != parser2.GetOption("comment-id") {
		t.Errorf("Space and equals syntax should produce same result: space='%s', equals='%s'",
			parser.GetOption("comment-id"), parser2.GetOption("comment-id"))
	}
}
