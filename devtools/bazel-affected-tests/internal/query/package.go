package query

import (
	"os"
	"path/filepath"
	"strings"
)

// FindBazelPackage finds the nearest Bazel package for a given file.
// A Bazel package is a directory containing a BUILD or BUILD.bazel file.
func FindBazelPackage(filePath string) (string, bool) {
	dir := filepath.Dir(filePath)

	// Walk up the directory tree to find the nearest BUILD file
	for dir != "." && dir != "/" && dir != "" {
		if hasBuildFile(dir) {
			// Convert to Bazel package label format
			if dir == "." {
				return "//", true
			}
			return "//" + strings.ReplaceAll(dir, string(filepath.Separator), "/"), true
		}
		dir = filepath.Dir(dir)
	}

	// Check if root directory has a BUILD file
	if hasBuildFile(".") {
		return "//", true
	}

	// No BUILD file found - this file is not part of any Bazel package
	return "", false
}

func hasBuildFile(dir string) bool {
	_, err1 := os.Stat(filepath.Join(dir, "BUILD"))
	_, err2 := os.Stat(filepath.Join(dir, "BUILD.bazel"))
	return err1 == nil || err2 == nil
}
