package query

import (
	"reflect"
	"sort"
	"testing"
)

func TestFormatTestFilter(t *testing.T) {
	tests := []struct {
		name        string
		stagedFiles []string
		inputTests  []string
		wantTests   []string
	}{
		{
			name:        "filter C++ format test with C++ files",
			stagedFiles: []string{"src/main.cpp", "include/header.h"},
			inputTests: []string{
				"//src:test",
				"//tools/format:format_test_C++_with_clang-format",
				"//tools/format:format_test_Python_with_ruff",
			},
			wantTests: []string{
				"//src:test",
				"//tools/format:format_test_C++_with_clang-format",
			},
		},
		{
			name:        "filter Go format test with Go files",
			stagedFiles: []string{"main.go", "lib/helper.go"},
			inputTests: []string{
				"//test:unit_test",
				"//tools/format:format_test_Go_with_gofmt",
				"//tools/format:format_test_Python_with_ruff",
			},
			wantTests: []string{
				"//test:unit_test",
				"//tools/format:format_test_Go_with_gofmt",
			},
		},
		{
			name:        "filter multiple format tests",
			stagedFiles: []string{"main.py", "test.go", "BUILD.bazel"},
			inputTests: []string{
				"//test:all",
				"//tools/format:format_test_Go_with_gofmt",
				"//tools/format:format_test_Python_with_ruff",
				"//tools/format:format_test_Starlark_with_buildifier",
				"//tools/format:format_test_C++_with_clang-format",
			},
			wantTests: []string{
				"//test:all",
				"//tools/format:format_test_Go_with_gofmt",
				"//tools/format:format_test_Python_with_ruff",
				"//tools/format:format_test_Starlark_with_buildifier",
			},
		},
		{
			name:        "no format tests when no matching files",
			stagedFiles: []string{"README.md", "docs/guide.txt"},
			inputTests: []string{
				"//test:doc_test",
				"//tools/format:format_test_Go_with_gofmt",
				"//tools/format:format_test_Python_with_ruff",
			},
			wantTests: []string{
				"//test:doc_test",
			},
		},
		{
			name:        "keep all non-format tests",
			stagedFiles: []string{"main.go"},
			inputTests: []string{
				"//test:unit_test",
				"//test:integration_test",
				"//lib:lib_test",
			},
			wantTests: []string{
				"//test:unit_test",
				"//test:integration_test",
				"//lib:lib_test",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			filter := NewFormatTestFilter(tt.stagedFiles, false)
			gotTests := filter.Filter(tt.inputTests)

			// Sort for consistent comparison
			sort.Strings(gotTests)
			sort.Strings(tt.wantTests)

			if !reflect.DeepEqual(gotTests, tt.wantTests) {
				t.Errorf("Filter() = %v, want %v", gotTests, tt.wantTests)
			}
		})
	}
}

func TestDetectFileTypes(t *testing.T) {
	tests := []struct {
		name        string
		stagedFiles []string
		wantTypes   map[string]bool
	}{
		{
			name:        "detect C++ files",
			stagedFiles: []string{"main.cpp", "test.cc", "header.hpp"},
			wantTypes:   map[string]bool{"cpp": true},
		},
		{
			name:        "detect C files",
			stagedFiles: []string{"main.c"},
			wantTypes:   map[string]bool{"c": true},
		},
		{
			name:        "detect .h files only as both C and C++",
			stagedFiles: []string{"header.h"},
			wantTypes:   map[string]bool{"c": true, "cpp": true},
		},
		{
			name:        "detect C files with headers",
			stagedFiles: []string{"main.c", "header.h"},
			wantTypes:   map[string]bool{"c": true},
		},
		{
			name:        "detect Go files",
			stagedFiles: []string{"main.go", "test_test.go"},
			wantTypes:   map[string]bool{"go": true},
		},
		{
			name:        "detect Python files",
			stagedFiles: []string{"script.py", "__init__.py"},
			wantTypes:   map[string]bool{"python": true},
		},
		{
			name:        "detect Starlark files",
			stagedFiles: []string{"BUILD", "rules.bzl", "WORKSPACE.bazel"},
			wantTypes:   map[string]bool{"starlark": true},
		},
		{
			name:        "detect mixed files",
			stagedFiles: []string{"main.go", "test.py", "BUILD", "lib.rs"},
			wantTypes:   map[string]bool{"go": true, "python": true, "starlark": true, "rust": true},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			filter := NewFormatTestFilter(tt.stagedFiles, false)
			gotTypes := filter.detectFileTypes()

			if !reflect.DeepEqual(gotTypes, tt.wantTypes) {
				t.Errorf("detectFileTypes() = %v, want %v", gotTypes, tt.wantTypes)
			}
		})
	}
}
