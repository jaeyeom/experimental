// Package main implements gh-throttle, a drop-in wrapper for gh that
// runs every call through a per-user limiter (see devtools/pkg/ghlimit).
//
// It takes the same arguments as gh and runs the real gh, found as
// $GH_THROTTLE_GH or else the first gh on PATH that is not gh-throttle
// itself. Stdin, stdout, stderr and the exit code pass through.
//
// Usage:
//
//	gh-throttle pr view 123                   # as gh pr view 123
//	gh-throttle --stats [--since 1h] [--json] # summarise calls and waits
//
// Knobs: GH_THROTTLE_MAX_CONCURRENT, GH_THROTTLE_MUTATION_GAP,
// GH_THROTTLE_BACKOFF, GH_THROTTLE_MAX_BACKOFF, GH_THROTTLE_OFF=1 to
// bypass the limiter, GH_THROTTLE_CACHE_TTL to turn on the read cache,
// GH_THROTTLE_NO_CACHE=1 to skip it for one call, and
// GH_THROTTLE_POINTS_PER_MIN and GH_THROTTLE_GRAPHQL_SECS_PER_MIN for
// the per-minute budgets.
package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
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
	if len(args) > 0 && args[0] == "--stats" {
		if err := writeStats(args[1:], getenv, stdout); err != nil {
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
	logPath, callLogPath := ghlimit.LogPath(getenv), ghlimit.CallLogPath(getenv)
	caller := lookupCaller()
	cwd, _ := os.Getwd()
	limiter := &ghlimit.Limiter{
		Dir:    ghlimit.StateDir(getenv),
		Config: cfg,
		Log: func(e ghlimit.Entry) error {
			e.PID = os.Getpid()
			e.Caller, _ = caller()
			return ghlimit.AppendLog(logPath, e)
		},
		LogCall: func(e ghlimit.CallEntry) error {
			e.PID = os.Getpid()
			e.Caller, e.CallerChain = caller()
			e.Cwd = cwd
			return ghlimit.AppendCallLog(callLogPath, e, ghlimit.MaxCallLogSize)
		},
	}
	runner, err := ghlimit.NewExecRunner(gh, args, stdin, stdout, stderr)
	if err != nil {
		return fail(err)
	}
	runner.Env = append(os.Environ(), envHeld+"=1")
	runner.StdoutDirect = isTerminal(stdout)
	runner.StderrLive = isTerminal(stderr)
	cache := &ghlimit.Cache{
		Dir:        ghlimit.CacheDir(getenv),
		TTL:        cfg.CacheTTL,
		Bypass:     getenv(ghlimit.EnvNoCache) == "1",
		Getenv:     getenv,
		GitRemotes: gitRemotes,
	}
	if cache.TTL > 0 && !cache.Bypass {
		cache.Identity = ghlimit.CacheIdentity(getenv)
		runner.KeepStdout = true
	}
	if ghlimit.ClassifyCall(args).AsksRateLimit() {
		// Kept up to a bound, for the rateLimit { cost remaining } the
		// call log records.
		runner.KeepStdout = true
	}
	res, err := cache.Run(context.Background(), limiter, args, runner.Run, stdout)
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

// gitRemotes lists the working directory's git remotes for the read
// cache, so it can tell the repository without a gh API call.
func gitRemotes() (string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	out, err := exec.CommandContext(ctx, "git", "config", "--get-regexp", `^remote\..*\.(url|gh-resolved)$`).Output()
	if err != nil {
		return "", fmt.Errorf("list git remotes: %w", err)
	}
	return string(out), nil
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

// writeStats prints the --stats summary; args are what follows
// --stats: --since DURATION (default 1h) and --json.
func writeStats(args []string, getenv func(string) string, stdout io.Writer) error {
	fs := flag.NewFlagSet("gh-throttle --stats", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	since := fs.Duration("since", time.Hour, "how far back to summarise")
	asJSON := fs.Bool("json", false, "print JSON")
	if err := fs.Parse(args); err != nil {
		return fmt.Errorf("--stats: %w", err)
	}
	if fs.NArg() > 0 || *since <= 0 {
		return errors.New("usage: gh-throttle --stats [--since 1h] [--json]")
	}
	s, err := ghlimit.ReadStats(ghlimit.LogPath(getenv), ghlimit.CallLogPath(getenv), time.Now(), *since)
	if err != nil {
		return fmt.Errorf("read stats: %w", err)
	}
	if *asJSON {
		enc := json.NewEncoder(stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(s); err != nil {
			return fmt.Errorf("write stats: %w", err)
		}
		return nil
	}
	return s.WriteText(stdout) //nolint:wrapcheck // WriteText wraps its own errors
}

// lookupCaller starts reading the process table in the background, so
// it overlaps the gh run, and returns a function that waits for the
// caller and its chain (see callerChain).
func lookupCaller() func() (string, []string) {
	var (
		name  string
		chain []string
	)
	done := make(chan struct{})
	go func() {
		defer close(done)
		ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
		defer cancel()
		procs, err := procinfo.LocalTable().List(ctx)
		if err == nil {
			name, chain = callerChain(procs, os.Getppid())
		}
	}()
	return func() (string, []string) {
		<-done
		return name, chain
	}
}

// callerChain walks up from ppid to the first ancestor that is not a
// shell. It returns that ancestor's name, or the last name when every
// ancestor is a shell, and the names on the way, starting with ppid's.
func callerChain(procs []procinfo.Process, ppid int) (string, []string) {
	byPID := make(map[int]procinfo.Process, len(procs))
	for _, p := range procs {
		byPID[p.PID] = p
	}
	var chain []string
	for pid, seen := ppid, map[int]bool{}; !seen[pid]; {
		seen[pid] = true
		p, ok := byPID[pid]
		if !ok {
			break
		}
		name := p.Name
		if name == "" {
			name = procinfo.Argv0Base(p)
		}
		chain = append(chain, name)
		if !procinfo.IsShell(p) {
			return name, chain
		}
		pid = p.PPID
	}
	if len(chain) == 0 {
		return "", nil
	}
	return chain[0], chain
}
