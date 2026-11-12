// Package main implements gh-throttle, a drop-in wrapper for gh that
// runs every call through a per-user limiter (see devtools/pkg/ghlimit).
//
// It takes the same arguments as gh and runs the real gh, found as
// $GH_THROTTLE_GH or else the first gh on PATH that is not gh-throttle
// itself. Stdin, stdout, stderr and the exit code pass through.
//
// Usage:
//
//	gh-throttle pr view 123        # as gh pr view 123
//	gh-throttle --stats            # summarise the last hour's waits
//
// Knobs: GH_THROTTLE_MAX_CONCURRENT, GH_THROTTLE_MUTATION_GAP,
// GH_THROTTLE_BACKOFF, GH_THROTTLE_MAX_BACKOFF, and GH_THROTTLE_OFF=1 to
// bypass the limiter.
package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/ghlimit"
	"github.com/jaeyeom/experimental/devtools/pkg/procinfo"
)

const (
	envGH  = "GH_THROTTLE_GH"
	envOff = "GH_THROTTLE_OFF"
	// envHeld is set for the gh child. A gh-throttle run under it, such
	// as from a gh extension, passes through instead of waiting for the
	// slot its ancestor holds.
	envHeld = "GH_THROTTLE_HELD"
)

func main() {
	self, err := os.Executable()
	if err != nil {
		self = os.Args[0]
	}
	os.Exit(run(os.Args[1:], os.Getenv, self, os.Stdin, os.Stdout, os.Stderr))
}

// run runs gh-throttle and returns its exit code.
func run(args []string, getenv func(string) string, self string, stdin io.Reader, stdout, stderr io.Writer) int {
	fail := func(err error) int {
		fmt.Fprintf(stderr, "gh-throttle: %v\n", err)
		return 1
	}
	if len(args) == 1 && args[0] == "--stats" {
		if err := ghlimit.WriteStats(stdout, ghlimit.LogPath(getenv), time.Now(), time.Hour); err != nil {
			return fail(err)
		}
		return 0
	}
	gh, err := findGH(getenv, self)
	if err != nil {
		return fail(err)
	}
	if getenv(envOff) == "1" || getenv(envHeld) != "" {
		code, err := passThrough(gh, args, stdin, stdout, stderr)
		if err != nil {
			return fail(err)
		}
		return code
	}
	cfg, err := ghlimit.ConfigFromEnv(getenv)
	if err != nil {
		return fail(err)
	}
	logPath := ghlimit.LogPath(getenv)
	limiter := &ghlimit.Limiter{
		Dir:    ghlimit.StateDir(getenv),
		Config: cfg,
		Log: func(e ghlimit.Entry) error {
			e.PID = os.Getpid()
			e.Caller = parentName()
			return ghlimit.AppendLog(logPath, e)
		},
	}
	runner, err := ghlimit.NewExecRunner(gh, args, stdin, stdout, stderr)
	if err != nil {
		return fail(err)
	}
	runner.Env = append(os.Environ(), envHeld+"=1")
	runner.StdoutDirect = isTerminal(stdout)
	runner.StderrLive = isTerminal(stderr)
	res, err := limiter.Run(context.Background(), args, runner.Run)
	if err != nil {
		return fail(err)
	}
	if !res.StderrShown {
		_, _ = stderr.Write(res.Stderr)
	}
	return res.ExitCode
}

// findGH is $GH_THROTTLE_GH, or else the first executable gh on PATH
// that does not resolve to self.
func findGH(getenv func(string) string, self string) (string, error) {
	selfReal := resolve(self)
	if gh := getenv(envGH); gh != "" {
		if resolve(gh) == selfReal {
			return "", fmt.Errorf("%s=%s is gh-throttle itself", envGH, gh)
		}
		return gh, nil
	}
	for _, dir := range filepath.SplitList(getenv("PATH")) {
		if dir == "" {
			dir = "."
		}
		candidate := filepath.Join(dir, "gh")
		info, err := os.Stat(candidate)
		if err != nil || !info.Mode().IsRegular() || info.Mode().Perm()&0o111 == 0 {
			continue
		}
		if resolve(candidate) == selfReal {
			continue
		}
		return candidate, nil
	}
	return "", errors.New("no gh on PATH other than gh-throttle; set " + envGH)
}

// resolve is path with symlinks resolved, or path when that fails.
func resolve(path string) string {
	if abs, err := filepath.Abs(path); err == nil {
		path = abs
	}
	if resolved, err := filepath.EvalSymlinks(path); err == nil {
		return resolved
	}
	return path
}

// passThrough runs gh with the given stdio and no limiter.
func passThrough(gh string, args []string, stdin io.Reader, stdout, stderr io.Writer) (int, error) {
	// The path is the user's gh, and args are the caller's gh arguments.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command(gh, args...) //nolint:gosec // G204, G702: the user's gh with the caller's argument list, not a shell
	cmd.Stdin, cmd.Stdout, cmd.Stderr = stdin, stdout, stderr
	err := cmd.Run()
	var exitErr *exec.ExitError
	switch {
	case err == nil:
		return 0, nil
	case errors.As(err, &exitErr):
		return max(exitErr.ExitCode(), 1), nil
	default:
		return 0, fmt.Errorf("run %s: %w", gh, err)
	}
}

// isTerminal reports whether w is a terminal or other character device,
// which gh should get as is.
func isTerminal(w io.Writer) bool {
	f, ok := w.(*os.File)
	if !ok {
		return false
	}
	info, err := f.Stat()
	return err == nil && info.Mode()&os.ModeCharDevice != 0
}

// parentName is the name of the process that ran gh-throttle, or "" when
// it cannot be read.
func parentName() string {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	procs, err := procinfo.LocalTable().List(ctx)
	if err != nil {
		return ""
	}
	ppid := os.Getppid()
	for _, p := range procs {
		if p.PID == ppid {
			if p.Name != "" {
				return p.Name
			}
			return procinfo.Argv0Base(p)
		}
	}
	return ""
}
