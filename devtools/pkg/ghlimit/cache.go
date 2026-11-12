package ghlimit

import (
	"bufio"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"net/url"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// ReasonCacheHit is the log reason for a read served from the cache.
const ReasonCacheHit = "cache_hit"

// Cache is an opt-in read cache in front of a Limiter. It serves the
// stdout of a recent identical successful read (gh pr view or gh pr list
// with --json, or a gh api GET) without running gh, and a mutation
// through it clears the cached reads of the mutated repository.
//
// An entry is a file per key in a directory per repository, with the
// time its run started as its modification time. A repository's
// invalidation is a marker file whose modification time is when the
// mutation finished; an entry older than its marker is stale, so a read
// that overlapped a mutation is not served.
type Cache struct {
	Dir string
	// TTL is how long an entry is served. Zero turns lookups and stores
	// off; mutations still clear entries other callers stored.
	TTL time.Duration
	// Bypass skips the lookup and the store for this call.
	Bypass bool
	// Identity tells apart the GitHub users that may share Dir; see
	// CacheIdentity.
	Identity string
	// Getenv reads GH_REPO and GH_HOST.
	Getenv func(string) string
	// GitRemotes returns the output of
	// git config --get-regexp '^remote\..*\.(url|gh-resolved)$'
	// in the working directory, used to tell its repository.
	GitRemotes func() (string, error)
}

// Run runs gh args through l, or serves them from the cache. A hit
// writes the cached stdout to stdout without taking a slot or waiting
// for the mutation gap, logs a cache_hit line, and returns a Result
// with exit code 0. A miss stores the stdout of a successful run when
// run kept it (Result.StdoutKept).
func (c *Cache) Run(ctx context.Context, l *Limiter, args []string, run Runner, stdout io.Writer) (Result, error) {
	if IsMutation(args) {
		res, err := l.Run(ctx, args, run)
		c.invalidate(args, l.now())
		return res, err
	}
	if c.TTL <= 0 || c.Bypass {
		return l.Run(ctx, args, run)
	}
	repo, ok := c.readRepo(args)
	if !ok {
		return l.Run(ctx, args, run)
	}
	path := c.entryPath(repo, args)
	if data, ok := c.lookup(path, repo, l.now()); ok {
		if _, err := stdout.Write(data); err != nil {
			return Result{}, fmt.Errorf("write stdout: %w", err)
		}
		if l.Log != nil {
			_ = l.Log(Entry{Time: l.now(), Args: strings.Join(Redact(args), " "), Reason: ReasonCacheHit})
		}
		l.logCacheHit(args)
		return Result{StdoutLen: int64(len(data))}, nil
	}
	// The directory exists before gh runs, so a mutation that finishes
	// meanwhile leaves a marker that makes this entry stale.
	dirErr := c.makeDir(filepath.Dir(path))
	start := l.now()
	res, err := l.Run(ctx, args, run)
	if dirErr == nil && err == nil && res.ExitCode == 0 && res.StdoutKept {
		// A failed store only costs the next call a gh run.
		_ = c.store(path, start, res.Stdout)
	}
	return res, err
}

// makeDir creates the private entry directory dir under Dir.
func (c *Cache) makeDir(dir string) error {
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return fmt.Errorf("create cache dir: %w", err)
	}
	for _, d := range []string{c.Dir, dir} {
		if err := os.Chmod(d, 0o700); err != nil {
			return fmt.Errorf("make %s private: %w", d, err)
		}
	}
	return nil
}

func hashHex(parts ...string) string {
	h := sha256.New()
	for _, p := range parts {
		h.Write([]byte(p))
		h.Write([]byte{0})
	}
	return hex.EncodeToString(h.Sum(nil))
}

func (c *Cache) repoDir(repo string) string { return filepath.Join(c.Dir, hashHex(repo)[:16]) }

func (c *Cache) entryPath(repo string, args []string) string {
	return filepath.Join(c.repoDir(repo), hashHex(append([]string{c.Identity, repo}, args...)...))
}

const allMarker = "all.inval"

// lookup returns the entry at path when it is younger than TTL and than
// the invalidation markers of repo. An expired entry is removed.
func (c *Cache) lookup(path, repo string, now time.Time) ([]byte, bool) {
	info, err := os.Stat(path)
	if err != nil {
		return nil, false
	}
	start := info.ModTime()
	if age := now.Sub(start); age < 0 || age >= c.TTL {
		_ = os.Remove(path)
		return nil, false
	}
	for _, marker := range []string{filepath.Join(c.Dir, allMarker), c.repoDir(repo) + ".inval"} {
		if m, err := os.Stat(marker); err == nil && start.Before(m.ModTime()) {
			return nil, false
		}
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, false
	}
	return data, true
}

// store writes data to path atomically, dated start.
func (c *Cache) store(path string, start time.Time, data []byte) error {
	dir := filepath.Dir(path)
	if err := c.makeDir(dir); err != nil {
		return err
	}
	f, err := os.CreateTemp(dir, ".tmp-*")
	if err != nil {
		return fmt.Errorf("create cache entry: %w", err)
	}
	defer os.Remove(f.Name())
	if _, err := f.Write(data); err != nil {
		_ = f.Close()
		return fmt.Errorf("write cache entry: %w", err)
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("close cache entry: %w", err)
	}
	if err := os.Chtimes(f.Name(), start, start); err != nil {
		return fmt.Errorf("date cache entry: %w", err)
	}
	if err := os.Rename(f.Name(), path); err != nil {
		return fmt.Errorf("store cache entry: %w", err)
	}
	return nil
}

// invalidate clears the entries of the repositories mutation args
// touch, or every entry when that cannot be told. It does nothing when
// no cache exists.
func (c *Cache) invalidate(args []string, now time.Time) {
	if _, err := os.Stat(c.Dir); err != nil {
		return
	}
	repos := c.mutationRepos(args)
	if len(repos) == 0 {
		_ = mark(filepath.Join(c.Dir, allMarker), now)
		entries, _ := os.ReadDir(c.Dir)
		for _, e := range entries {
			if e.IsDir() {
				_ = os.RemoveAll(filepath.Join(c.Dir, e.Name()))
			}
		}
		return
	}
	for _, repo := range repos {
		_ = mark(c.repoDir(repo)+".inval", now)
		_ = os.RemoveAll(c.repoDir(repo))
	}
}

// mark writes the marker file at path dated now.
func mark(path string, now time.Time) error {
	if err := os.WriteFile(path, nil, 0o600); err != nil {
		return fmt.Errorf("write marker: %w", err)
	}
	if err := os.Chtimes(path, now, now); err != nil {
		return fmt.Errorf("date marker: %w", err)
	}
	return nil
}

// prReadFlags are the flags of gh pr view and gh pr list a cached read
// may use, mapped to whether they take a value. Any other flag, such as
// --web, makes the call uncacheable.
var prReadFlags = map[string]bool{
	"--json": true, "-q": true, "--jq": true, "-t": true, "--template": true,
	"-R": true, "--repo": true, "-c": false, "--comments": false,
	"-A": true, "--author": true, "-a": true, "--assignee": true,
	"-B": true, "--base": true, "-H": true, "--head": true,
	"-l": true, "--label": true, "-L": true, "--limit": true,
	"-S": true, "--search": true, "-s": true, "--state": true,
	"--app": true, "-d": false, "--draft": false,
}

// readRepo reports whether args are a cacheable read and the
// repository ("host/owner/repo", or "" for none) it reads.
func (c *Cache) readRepo(args []string) (string, bool) {
	switch {
	case len(args) >= 2 && args[0] == "pr" && (args[1] == "view" || args[1] == "list"):
		return c.prReadRepo(args[1], args[2:])
	case len(args) >= 1 && args[0] == "api":
		return c.apiReadRepo(args[1:])
	}
	return "", false
}

func (c *Cache) prReadRepo(sub string, args []string) (string, bool) {
	positional, repoFlag, hasJSON, ok := parsePRRead(args)
	// gh pr view with no argument shows the current branch's PR, which
	// the repository alone does not tell.
	wantPositional := 0
	if sub == "view" {
		wantPositional = 1
	}
	if !ok || !hasJSON || len(positional) != wantPositional {
		return "", false
	}
	if wantPositional == 1 && strings.Contains(positional[0], "://") {
		return c.repoFromURL(positional[0])
	}
	return c.flagOrCwdRepo(repoFlag)
}

// parsePRRead splits gh pr view or list args into positional arguments,
// the -R value and whether --json is given. ok is false for a flag not
// in prReadFlags or a flag missing its value.
func parsePRRead(args []string) (positional []string, repoFlag string, hasJSON, ok bool) {
	for i := 0; i < len(args); i++ {
		arg := args[i]
		if !strings.HasPrefix(arg, "-") || arg == "-" {
			positional = append(positional, arg)
			continue
		}
		name, value, hasValue := splitFlag(arg)
		takesValue, known := prReadFlags[name]
		if !known {
			return nil, "", false, false
		}
		if takesValue && !hasValue {
			if i+1 >= len(args) {
				return nil, "", false, false
			}
			i++
			value = args[i]
		}
		switch name {
		case "--json":
			hasJSON = true
		case "-R", "--repo":
			repoFlag = value
		}
	}
	return positional, repoFlag, hasJSON, true
}

// flagOrCwdRepo is the repository of a -R value, or of the working
// directory when there is none.
func (c *Cache) flagOrCwdRepo(repoFlag string) (string, bool) {
	if repoFlag != "" {
		return c.parseRepo(repoFlag)
	}
	return c.cwdRepo()
}

func (c *Cache) apiReadRepo(args []string) (string, bool) {
	call := parseAPIArgs(args)
	switch {
	case call.method == "" && len(call.fields) > 0, call.method != "" && call.method != "GET":
		return "", false
	}
	for _, arg := range args {
		if arg == "--input" || strings.HasPrefix(arg, "--input=") {
			return "", false
		}
	}
	endpoint := strings.TrimPrefix(call.endpoint, "/")
	switch {
	case endpoint == "", endpoint == "graphql", strings.Contains(endpoint, "://"),
		strings.Contains(endpoint, "{branch}"):
		return "", false
	case strings.Contains(endpoint, "{owner}"), strings.Contains(endpoint, "{repo}"):
		return c.cwdRepo()
	}
	return c.apiPathRepo(endpoint, call.hostname)
}

// apiPathRepo is the repository of a repos/OWNER/REPO/... endpoint, or
// "" for an endpoint outside one.
func (c *Cache) apiPathRepo(endpoint, hostname string) (string, bool) {
	rest, ok := strings.CutPrefix(endpoint, "repos/")
	if !ok {
		return "", true
	}
	rest, _, _ = strings.Cut(rest, "?")
	parts := strings.SplitN(rest, "/", 3)
	if len(parts) < 2 || parts[0] == "" || parts[1] == "" {
		return "", false
	}
	if hostname == "" {
		hostname = c.defaultHost()
	}
	return repoName(hostname, parts[0], parts[1]), true
}

// mutationRepos is the repositories a mutation may change, or nil when
// that cannot be told.
func (c *Cache) mutationRepos(args []string) []string {
	if args[0] == "api" {
		return c.apiMutationRepos(args[1:])
	}
	var repos []string
	repoFlag := ""
	for i, arg := range args {
		name, value, hasValue := splitFlag(arg)
		if name == "-R" || name == "--repo" {
			if !hasValue && i+1 < len(args) {
				value = args[i+1]
			}
			repoFlag = value
			continue
		}
		if !hasValue {
			value = arg
		}
		if strings.Contains(value, "://") {
			if repo, ok := c.repoFromURL(value); ok {
				repos = append(repos, repo)
			}
		}
	}
	if repo, ok := c.flagOrCwdRepo(repoFlag); ok {
		repos = append(repos, repo)
	}
	return repos
}

// apiMutationRepos is the repository of a gh api mutation's endpoint,
// or nil for one outside repos/, such as graphql.
func (c *Cache) apiMutationRepos(args []string) []string {
	call := parseAPIArgs(args)
	endpoint := strings.TrimPrefix(call.endpoint, "/")
	var (
		repo string
		ok   bool
	)
	switch {
	case strings.Contains(endpoint, "{owner}"), strings.Contains(endpoint, "{repo}"):
		repo, ok = c.cwdRepo()
	case strings.HasPrefix(endpoint, "repos/"):
		repo, ok = c.apiPathRepo(endpoint, call.hostname)
	}
	if !ok {
		return nil
	}
	return []string{repo}
}

func (c *Cache) getenv(name string) string {
	if c.Getenv == nil {
		return ""
	}
	return c.Getenv(name)
}

func (c *Cache) defaultHost() string {
	if h := c.getenv("GH_HOST"); h != "" {
		return h
	}
	return "github.com"
}

func repoName(host, owner, repo string) string {
	return strings.ToLower(host + "/" + owner + "/" + strings.TrimSuffix(repo, ".git"))
}

// parseRepo reads a -R or GH_REPO value: OWNER/REPO, HOST/OWNER/REPO or
// a URL.
func (c *Cache) parseRepo(v string) (string, bool) {
	if strings.Contains(v, "://") {
		return c.repoFromURL(v)
	}
	parts := strings.Split(v, "/")
	for _, p := range parts {
		if p == "" {
			return "", false
		}
	}
	switch len(parts) {
	case 2:
		return repoName(c.defaultHost(), parts[0], parts[1]), true
	case 3:
		return repoName(parts[0], parts[1], parts[2]), true
	}
	return "", false
}

// repoFromURL is the repository of a URL such as
// https://github.com/OWNER/REPO/pull/1 or ssh://git@host/OWNER/REPO.git.
func (c *Cache) repoFromURL(raw string) (string, bool) {
	u, err := url.Parse(raw)
	if err != nil || u.Hostname() == "" {
		return "", false
	}
	parts := strings.Split(strings.Trim(u.Path, "/"), "/")
	if len(parts) < 2 || parts[0] == "" || parts[1] == "" {
		return "", false
	}
	return repoName(u.Hostname(), parts[0], parts[1]), true
}

// remoteRepo reads a git remote URL, including the scp-like
// git@host:OWNER/REPO.git.
func (c *Cache) remoteRepo(raw string) (string, bool) {
	if !strings.Contains(raw, "://") {
		hostPart, path, ok := strings.Cut(raw, ":")
		if !ok {
			return "", false
		}
		_, host, _ := strings.Cut(hostPart, "@")
		if host == "" {
			host = hostPart
		}
		raw = "ssh://" + host + "/" + path
	}
	return c.repoFromURL(raw)
}

// cwdRepo is the repository gh resolves in the working directory:
// GH_REPO, else the git remote gh set as default (gh-resolved), else
// the only remote. With several remotes and no default it is unknown,
// rather than guessing gh's choice.
func (c *Cache) cwdRepo() (string, bool) {
	if v := c.getenv("GH_REPO"); v != "" {
		return c.parseRepo(v)
	}
	if c.GitRemotes == nil {
		return "", false
	}
	out, err := c.GitRemotes()
	if err != nil {
		return "", false
	}
	urls := map[string]string{}
	resolved := map[string]string{}
	scanner := bufio.NewScanner(strings.NewReader(out))
	for scanner.Scan() {
		key, value, _ := strings.Cut(strings.TrimSpace(scanner.Text()), " ")
		name, ok := strings.CutPrefix(key, "remote.")
		if !ok {
			continue
		}
		if remote, ok := strings.CutSuffix(name, ".url"); ok {
			urls[remote] = value
		} else if remote, ok := strings.CutSuffix(name, ".gh-resolved"); ok {
			resolved[remote] = value
		}
	}
	for remote, value := range resolved {
		if value != "base" {
			return c.parseRepo(value)
		}
		if u, ok := urls[remote]; ok {
			return c.remoteRepo(u)
		}
	}
	if len(urls) != 1 {
		return "", false
	}
	for _, u := range urls {
		return c.remoteRepo(u)
	}
	return "", false
}

// CacheIdentity tells apart the GitHub users whose reads may share a
// cache: the OS user, the gh host and config directory, the token
// variables and the content of gh's hosts.yml (which names the active
// account). Only a hash of it reaches disk.
func CacheIdentity(getenv func(string) string) string {
	configDir := getenv("GH_CONFIG_DIR")
	if configDir == "" {
		base := getenv("XDG_CONFIG_HOME")
		if base == "" {
			base = filepath.Join(getenv("HOME"), ".config")
		}
		configDir = filepath.Join(base, "gh")
	}
	hosts, _ := os.ReadFile(filepath.Join(configDir, "hosts.yml"))
	parts := []string{strconv.Itoa(os.Getuid()), getenv("GH_HOST"), configDir, string(hosts)}
	for _, name := range []string{"GH_TOKEN", "GITHUB_TOKEN", "GH_ENTERPRISE_TOKEN", "GITHUB_ENTERPRISE_TOKEN"} {
		parts = append(parts, getenv(name))
	}
	return hashHex(parts...)
}
