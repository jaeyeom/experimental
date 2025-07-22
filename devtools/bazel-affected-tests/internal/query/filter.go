package query

import (
	"path/filepath"
	"strings"
)

// FormatTestFilter filters format tests based on staged file types.
type FormatTestFilter struct {
	stagedFiles []string
	debug       bool
}

// NewFormatTestFilter creates a new format test filter.
func NewFormatTestFilter(stagedFiles []string, debug bool) *FormatTestFilter {
	return &FormatTestFilter{
		stagedFiles: stagedFiles,
		debug:       debug,
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

		// Track .h files separately as they could be C or C++
		if ext == ".h" {
			hasHFiles = true
		}

		// C++ files
		if ext == ".cpp" || ext == ".cc" || ext == ".cxx" || ext == ".c++" ||
			ext == ".hpp" || ext == ".hxx" || ext == ".hh" {
			types["cpp"] = true
		}

		// C files
		if ext == ".c" {
			types["c"] = true
		}

		// Go files
		if ext == ".go" {
			types["go"] = true
		}

		// Jsonnet files
		if ext == ".jsonnet" || ext == ".libsonnet" {
			types["jsonnet"] = true
		}

		// Protocol Buffer files
		if ext == ".proto" {
			types["proto"] = true
		}

		// Python files
		if ext == ".py" {
			types["python"] = true
		}

		// Rust files
		if ext == ".rs" {
			types["rust"] = true
		}

		// Starlark/Bazel files
		if ext == ".bzl" || base == "BUILD" || base == "BUILD.bazel" ||
			base == "WORKSPACE" || base == "WORKSPACE.bazel" ||
			base == "MODULE" || base == "MODULE.bazel" {
			types["starlark"] = true
		}
	}

	// If we have .h files but no C++ files, assume they're C headers
	// If we have C++ files, .h files are included with C++
	// If we have neither, include both C and C++ tests to be safe
	if hasHFiles {
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

	return types
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
