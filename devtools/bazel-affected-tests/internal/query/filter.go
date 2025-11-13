package query

import (
	"log/slog"
	"path/filepath"
	"strings"
)

// FormatTestFilter filters format tests based on staged file types.
type FormatTestFilter struct {
	stagedFiles []string
}

// NewFormatTestFilter creates a new format test filter.
func NewFormatTestFilter(stagedFiles []string, debug bool) *FormatTestFilter {
	if debug {
		slog.SetLogLoggerLevel(slog.LevelDebug)
	}
	return &FormatTestFilter{
		stagedFiles: stagedFiles,
	}
}

// Filter filters the given tests, keeping only format tests that match staged file types.
func (f *FormatTestFilter) Filter(tests []string) []string {
	// Check what file types we have in staged files
	fileTypes := f.detectFileTypes()

	var filtered []string
	for _, test := range tests {
		if f.shouldIncludeTest(test, fileTypes) {
			filtered = append(filtered, test)
		}
	}
	return filtered
}

// detectFileTypes returns a map of file types present in staged files.
func (f *FormatTestFilter) detectFileTypes() map[string]bool {
	types := make(map[string]bool)
	hasHFiles := false

	for _, file := range f.stagedFiles {
		ext := strings.ToLower(filepath.Ext(file))
		base := filepath.Base(file)

		if ext == ".h" {
			hasHFiles = true
		}

		fileType := detectFileType(ext, base)
		if fileType != "" {
			types[fileType] = true
		}
	}

	// Handle .h file ambiguity
	if hasHFiles {
		resolveHeaderFileTypes(types)
	}

	return types
}

func detectFileType(ext, base string) string {
	// C++ files
	if isCppExtension(ext) {
		return "cpp"
	}

	// C files
	if ext == ".c" {
		return "c"
	}

	// Go files
	if ext == ".go" {
		return "go"
	}

	// Jsonnet files
	if ext == ".jsonnet" || ext == ".libsonnet" {
		return "jsonnet"
	}

	// Protocol Buffer files
	if ext == ".proto" {
		return "proto"
	}

	// Python files
	if ext == ".py" {
		return "python"
	}

	// Rust files
	if ext == ".rs" {
		return "rust"
	}

	// Starlark/Bazel files
	if isStarlarkFile(ext, base) {
		return "starlark"
	}

	return ""
}

func isCppExtension(ext string) bool {
	cppExtensions := []string{".cpp", ".cc", ".cxx", ".c++", ".hpp", ".hxx", ".hh"}
	for _, cppExt := range cppExtensions {
		if ext == cppExt {
			return true
		}
	}
	return false
}

func isStarlarkFile(ext, base string) bool {
	if ext == ".bzl" {
		return true
	}

	starlarkFiles := []string{"BUILD", "BUILD.bazel", "WORKSPACE", "WORKSPACE.bazel", "MODULE", "MODULE.bazel"}
	for _, starlarkFile := range starlarkFiles {
		if base == starlarkFile {
			return true
		}
	}
	return false
}

func resolveHeaderFileTypes(types map[string]bool) {
	switch {
	case !types["cpp"] && !types["c"]:
		// .h files only - could be either, so include both
		types["c"] = true
		types["cpp"] = true
	case !types["cpp"] && types["c"]:
		// C files exist, no C++ files - treat .h as C
		// Already have types["c"] = true
	case types["cpp"]:
		// C++ files exist - .h files are already covered by C++
		// Already have types["cpp"] = true
	}
}

// shouldIncludeTest determines if a test should be included based on file types.
func (f *FormatTestFilter) shouldIncludeTest(test string, fileTypes map[string]bool) bool {
	switch test {
	case "//tools/format:format_test_C++_with_clang-format":
		return fileTypes["cpp"]
	case "//tools/format:format_test_C_with_clang-format":
		return fileTypes["c"]
	case "//tools/format:format_test_Go_with_gofmt":
		return fileTypes["go"]
	case "//tools/format:format_test_Jsonnet_with_jsonnetfmt":
		return fileTypes["jsonnet"]
	case "//tools/format:format_test_Protocol_Buffer_with_buf":
		return fileTypes["proto"]
	case "//tools/format:format_test_Python_with_ruff":
		return fileTypes["python"]
	case "//tools/format:format_test_Rust_with_rustfmt":
		return fileTypes["rust"]
	case "//tools/format:format_test_Starlark_with_buildifier":
		return fileTypes["starlark"]
	default:
		// Keep all non-format tests
		return true
	}
}
