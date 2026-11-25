// Command whitespace checks for or strips trailing whitespace in Markdown and
// Org files.
package main

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"unicode"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("whitespace", flag.ContinueOnError)
	fs.SetOutput(stderr)
	check := fs.Bool("check", false, "fail if any file has trailing whitespace")
	write := fs.Bool("w", false, "strip trailing whitespace in-place")
	workspace := fs.String("workspace", "", "path to a root file (e.g. MODULE.bazel) to scan the git workspace")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if *check == *write {
		fmt.Fprintln(stderr, "whitespace: specify exactly one of -check or -w")
		return 2
	}

	files, err := collectFiles(fs.Args(), *workspace)
	if err != nil {
		fmt.Fprintf(stderr, "whitespace: %v\n", err)
		return 1
	}

	var dirty []string
	for _, path := range files {
		changed, err := processFile(path, *write)
		if err != nil {
			fmt.Fprintf(stderr, "%s: %v\n", path, err)
			return 1
		}
		if changed {
			dirty = append(dirty, path)
		}
	}

	if *check && len(dirty) > 0 {
		fmt.Fprintln(stdout, "Trailing whitespace found in:")
		for _, path := range dirty {
			fmt.Fprintln(stdout, path)
		}
		return 1
	}
	return 0
}

func collectFiles(positional []string, workspaceFile string) ([]string, error) {
	if len(positional) > 0 {
		return positional, nil
	}

	root, err := resolveWorkspaceRoot(workspaceFile)
	if err != nil {
		return nil, err
	}
	if err := os.Chdir(root); err != nil {
		return nil, fmt.Errorf("chdir %s: %w", root, err)
	}

	cmd := exec.Command("git", "ls-files", "-z", "--cached", "--modified", "--others", "--exclude-standard", "--", "*.md", "*.org")
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("git ls-files: %w", err)
	}

	var files []string
	seen := make(map[string]struct{})
	for _, raw := range bytes.Split(out, []byte{0}) {
		if len(raw) == 0 {
			continue
		}
		path := string(raw)
		if _, ok := seen[path]; ok {
			continue
		}
		seen[path] = struct{}{}
		info, err := os.Lstat(path)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				continue
			}
			return nil, fmt.Errorf("lstat %s: %w", path, err)
		}
		if !info.Mode().IsRegular() {
			continue
		}
		files = append(files, path)
	}
	return files, nil
}

func resolveWorkspaceRoot(workspaceFile string) (string, error) {
	if workspaceFile != "" {
		resolved, err := filepath.EvalSymlinks(workspaceFile)
		if err != nil {
			return "", fmt.Errorf("resolve workspace file %s: %w", workspaceFile, err)
		}
		return filepath.Dir(resolved), nil
	}
	if dir := os.Getenv("BUILD_WORKSPACE_DIRECTORY"); dir != "" {
		return dir, nil
	}
	cwd, err := os.Getwd()
	if err != nil {
		return "", fmt.Errorf("getwd: %w", err)
	}
	return cwd, nil
}

func processFile(path string, write bool) (bool, error) {
	info, err := os.Lstat(path)
	if err != nil {
		return false, fmt.Errorf("lstat file: %w", err)
	}
	src, err := os.ReadFile(path)
	if err != nil {
		return false, fmt.Errorf("read file: %w", err)
	}
	cleaned, changed := stripTrailingWhitespace(src)
	if !changed {
		return false, nil
	}
	if write {
		if err := os.WriteFile(path, cleaned, info.Mode().Perm()); err != nil {
			return false, fmt.Errorf("write file: %w", err)
		}
	}
	return true, nil
}

// stripTrailingWhitespace removes trailing horizontal/vertical whitespace
// before each newline (and at EOF), matching `rg '\s$'`.
func stripTrailingWhitespace(src []byte) ([]byte, bool) {
	if len(src) == 0 {
		return src, false
	}
	lines := strings.Split(string(src), "\n")
	changed := false
	for i, line := range lines {
		trimmed := strings.TrimRightFunc(line, unicode.IsSpace)
		if len(trimmed) != len(line) {
			lines[i] = trimmed
			changed = true
		}
	}
	if !changed {
		return src, false
	}
	return []byte(strings.Join(lines, "\n")), true
}
