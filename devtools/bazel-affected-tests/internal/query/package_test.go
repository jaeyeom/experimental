package query

import (
	"os"
	"testing"
)

func TestFindBazelPackage(t *testing.T) {
	// Create a temporary directory structure
	tmpDir, err := os.MkdirTemp("", "bazel-test")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	// Save current dir and change to temp dir
	origDir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	if err := os.Chdir(tmpDir); err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := os.Chdir(origDir); err != nil {
			t.Fatal(err)
		}
	}()

	// Create test directory structure
	if err := os.MkdirAll("src/lib", 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll("src/lib/subdir", 0o755); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Create("BUILD.bazel"); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Create("src/BUILD"); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Create("src/lib/BUILD.bazel"); err != nil {
		t.Fatal(err)
	}
	// These files don't need error handling for test purposes
	_, _ = os.Create("src/lib/file.go")        // File in lib
	_, _ = os.Create("src/lib/subdir/file.go") // File in subdir (no BUILD)
	_, _ = os.Create("src/no_build/file.go")   // File with no BUILD

	tests := []struct {
		name      string
		file      string
		wantPkg   string
		wantFound bool
	}{
		{
			name:      "file in root",
			file:      "main.go",
			wantPkg:   "//",
			wantFound: true,
		},
		{
			name:      "file in src",
			file:      "src/main.go",
			wantPkg:   "//src",
			wantFound: true,
		},
		{
			name:      "file in lib",
			file:      "src/lib/file.go",
			wantPkg:   "//src/lib",
			wantFound: true,
		},
		{
			name:      "file in subdir without BUILD",
			file:      "src/lib/subdir/file.go",
			wantPkg:   "//src/lib",
			wantFound: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pkg, found := FindBazelPackage(tt.file)
			if found != tt.wantFound {
				t.Errorf("FindBazelPackage(%q) found = %v, want %v", tt.file, found, tt.wantFound)
			}
			if found && pkg != tt.wantPkg {
				t.Errorf("FindBazelPackage(%q) = %q, want %q", tt.file, pkg, tt.wantPkg)
			}
		})
	}
}
