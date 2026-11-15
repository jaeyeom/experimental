package orgctl

import (
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
)

// ExpandPath expands a leading ~ or ~/ to the current user's home.
// An absolute path, and any other path, is returned unchanged.
func ExpandPath(path string) (string, error) {
	if path != "~" && !strings.HasPrefix(path, "~/") {
		return path, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("home directory: %w", err)
	}
	if path == "~" {
		return home, nil
	}
	return filepath.Join(home, path[2:]), nil
}

// roleDir is the role cwd with a leading ~ expanded.
// A home lookup error leaves the original path in place.
func roleDir(role Role) string {
	dir, err := ExpandPath(role.Cwd)
	if err != nil {
		return role.Cwd
	}
	return dir
}

// CheckCwd rejects a relative role cwd.
// An empty cwd, an absolute cwd, and a cwd that starts with ~ are allowed.
func (s Spec) CheckCwd() error {
	names := make([]string, 0, len(s.Roles))
	for name := range s.Roles {
		names = append(names, name)
	}
	slices.Sort(names)
	for _, name := range names {
		if cwdOK(s.Roles[name].Cwd) {
			continue
		}
		return fmt.Errorf("role %s: cwd must be absolute or start with ~", name)
	}
	return nil
}

func cwdOK(cwd string) bool {
	return cwd == "" || filepath.IsAbs(cwd) || cwd == "~" || strings.HasPrefix(cwd, "~/")
}
