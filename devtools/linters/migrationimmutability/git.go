package migrationimmutability

import (
	"bytes"
	"errors"
	"fmt"
	"os/exec"
	"strings"
)

// git runs git in dir and returns trimmed stdout.
func git(dir string, args ...string) (string, error) {
	out, err := gitRaw(dir, args...)
	return strings.TrimSpace(out), err
}

// gitRaw runs git in dir and returns stdout untouched (for -z output).
func gitRaw(dir string, args ...string) (string, error) {
	cmd := exec.Command("git", append([]string{"-C", dir}, args...)...)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return stdout.String(), fmt.Errorf("git %s: %w: %s", strings.Join(args, " "), err, strings.TrimSpace(stderr.String()))
	}
	return stdout.String(), nil
}

// gitOK runs git in dir and reports whether it exited 0, returning trimmed
// stdout. Exit status 1 and above are reported as false, not as an error;
// only a failure to run git at all is an error.
func gitOK(dir string, args ...string) (string, bool, error) {
	out, err := gitRaw(dir, args...)
	if err == nil {
		return strings.TrimSpace(out), true, nil
	}
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		return strings.TrimSpace(out), false, nil
	}
	return "", false, err
}

type change struct {
	Status string
	Path   string
}

// parseNameStatus parses `git diff --name-status --no-renames -z` output.
func parseNameStatus(out string) ([]change, error) {
	fields := strings.Split(out, "\x00")
	if n := len(fields); n > 0 && fields[n-1] == "" {
		fields = fields[:n-1]
	}
	if len(fields)%2 != 0 {
		return nil, fmt.Errorf("unexpected git diff --name-status -z output: %q", out)
	}
	changes := make([]change, 0, len(fields)/2)
	for i := 0; i < len(fields); i += 2 {
		changes = append(changes, change{Status: fields[i], Path: fields[i+1]})
	}
	return changes, nil
}

func splitZ(out string) []string {
	var paths []string
	for _, p := range strings.Split(out, "\x00") {
		if p != "" {
			paths = append(paths, p)
		}
	}
	return paths
}
