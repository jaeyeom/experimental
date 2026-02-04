package config

import "testing"

func TestMatchPattern(t *testing.T) {
	tests := []struct {
		name    string
		pattern string
		file    string
		want    bool
	}{
		// ** recursive matching
		{
			name:    "**/BUILD matches foo/bar/BUILD",
			pattern: "**/BUILD",
			file:    "foo/bar/BUILD",
			want:    true,
		},
		{
			name:    "**/BUILD matches BUILD at root",
			pattern: "**/BUILD",
			file:    "BUILD",
			want:    true,
		},
		{
			name:    "**/BUILD matches a/b/c/BUILD",
			pattern: "**/BUILD",
			file:    "a/b/c/BUILD",
			want:    true,
		},
		{
			name:    "**/*.bzl matches tools/defs.bzl",
			pattern: "**/*.bzl",
			file:    "tools/defs.bzl",
			want:    true,
		},
		{
			name:    "**/*.bzl matches foo.bzl at root",
			pattern: "**/*.bzl",
			file:    "foo.bzl",
			want:    true,
		},
		{
			name:    "**/*.bzl matches a/b/c.bzl",
			pattern: "**/*.bzl",
			file:    "a/b/c.bzl",
			want:    true,
		},
		{
			name:    "**/BUILD.bazel matches foo/BUILD.bazel",
			pattern: "**/BUILD.bazel",
			file:    "foo/BUILD.bazel",
			want:    true,
		},
		{
			name:    "**/BUILD.bazel does not match foo/BUILD",
			pattern: "**/BUILD.bazel",
			file:    "foo/BUILD",
			want:    false,
		},
		// Exact matching
		{
			name:    "WORKSPACE matches WORKSPACE at root",
			pattern: "WORKSPACE",
			file:    "WORKSPACE",
			want:    true,
		},
		{
			name:    "WORKSPACE does not match foo/WORKSPACE",
			pattern: "WORKSPACE",
			file:    "foo/WORKSPACE",
			want:    false,
		},
		// Simple glob matching
		{
			name:    "*.go matches foo.go at root",
			pattern: "*.go",
			file:    "foo.go",
			want:    true,
		},
		{
			name:    "*.go does not match dir/foo.go",
			pattern: "*.go",
			file:    "dir/foo.go",
			want:    false,
		},
		{
			name:    "*.go does not match foo.txt",
			pattern: "*.go",
			file:    "foo.txt",
			want:    false,
		},
		// No match cases
		{
			name:    "**/BUILD does not match BUILD.bazel",
			pattern: "**/BUILD",
			file:    "BUILD.bazel",
			want:    false,
		},
		{
			name:    "**/*.go does not match foo.txt",
			pattern: "**/*.go",
			file:    "foo.txt",
			want:    false,
		},
		{
			name:    "foo/bar does not match foo/baz",
			pattern: "foo/bar",
			file:    "foo/baz",
			want:    false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := MatchPattern(tt.pattern, tt.file)
			if got != tt.want {
				t.Errorf("MatchPattern(%q, %q) = %v, want %v", tt.pattern, tt.file, got, tt.want)
			}
		})
	}
}
