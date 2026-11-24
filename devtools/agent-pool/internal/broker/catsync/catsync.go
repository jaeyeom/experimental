// Package catsync synchronizes repository skill catalogs from each
// repository's default branch into the central broker store.
package catsync

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker"
	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/catalog"
)

const catalogFilePath = ".agent-pool/catalog.json"

// RepoConfig describes a repository whose default-branch catalog should be
// synced by the broker.
type RepoConfig struct {
	Name          string `toml:"name"`           // Repository slug, e.g. "org/core-api".
	GitURL        string `toml:"git_url"`        // Local path, file://, or https:// git remote.
	DefaultBranch string `toml:"default_branch"` // Optional; auto-detected from remote HEAD symref when empty.
}

// Syncer maintains bare git mirrors and syncs validated skill catalogs from
// configured repositories into a broker.Store.
type Syncer struct {
	store    broker.Store
	cacheDir string
	repos    []RepoConfig
	logger   *slog.Logger

	mu        sync.Mutex
	repoLocks map[string]*sync.Mutex
}

// NewSyncer creates a Syncer that persists catalogs to store and caches bare
// git mirrors under cacheDir.
func NewSyncer(store broker.Store, cacheDir string, repos []RepoConfig, logger *slog.Logger) *Syncer {
	if logger == nil {
		logger = slog.New(slog.NewTextHandler(io.Discard, nil))
	}
	copied := make([]RepoConfig, len(repos))
	copy(copied, repos)
	return &Syncer{
		store:     store,
		cacheDir:  cacheDir,
		repos:     copied,
		logger:    logger,
		repoLocks: make(map[string]*sync.Mutex),
	}
}

// SyncOnce syncs every configured repository once and returns a map of
// repository names to any errors encountered.
func (s *Syncer) SyncOnce(ctx context.Context) map[string]error {
	errs := make(map[string]error)
	for _, rc := range s.repos {
		if _, err := s.SyncRepo(ctx, rc); err != nil {
			errs[rc.Name] = err
		}
	}
	return errs
}

// SyncRepo fetches the default branch of rc into a bare git mirror, validates
// .agent-pool/catalog.json and all referenced SKILL.md files on the pinned
// commit, and upserts the catalog record into the broker store. On any failure
// it logs a warning and leaves any previously stored valid record untouched.
func (s *Syncer) SyncRepo(ctx context.Context, rc RepoConfig) (broker.CatalogRecord, error) {
	rec, err := s.syncRepo(ctx, rc)
	if err != nil {
		s.logger.Warn("catalog sync failed; preserving last known good catalog",
			"repo", rc.Name,
			"error", err,
		)
		return broker.CatalogRecord{}, err
	}
	return rec, nil
}

// Run performs an immediate SyncOnce and then syncs periodically on interval
// until ctx is canceled.
func (s *Syncer) Run(ctx context.Context, interval time.Duration) {
	s.SyncOnce(ctx)
	if ctx.Err() != nil {
		return
	}
	if interval <= 0 {
		<-ctx.Done()
		return
	}

	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
			s.SyncOnce(ctx)
		}
	}
}

func (s *Syncer) syncRepo(ctx context.Context, rc RepoConfig) (broker.CatalogRecord, error) {
	if err := s.validateRepoConfig(rc); err != nil {
		return broker.CatalogRecord{}, err
	}

	repoMu := s.repoMutex(rc.Name)
	repoMu.Lock()
	defer repoMu.Unlock()

	mirror, err := s.mirrorPath(rc.Name)
	if err != nil {
		return broker.CatalogRecord{}, err
	}
	if err := ensureBareMirror(ctx, mirror); err != nil {
		return broker.CatalogRecord{}, err
	}

	branch, err := resolveDefaultBranch(ctx, rc)
	if err != nil {
		return broker.CatalogRecord{}, err
	}

	sha, err := fetchAndResolveSHA(ctx, mirror, rc.GitURL, branch)
	if err != nil {
		return broker.CatalogRecord{}, err
	}

	raw, err := readAndValidateCatalog(ctx, mirror, sha)
	if err != nil {
		return broker.CatalogRecord{}, err
	}

	rec := broker.CatalogRecord{
		Repo:          rc.Name,
		DefaultBranch: branch,
		CatalogSha:    sha,
		CatalogJSON:   raw,
		UpdatedAt:     time.Now().UTC(),
	}
	if err := s.store.UpsertCatalog(ctx, rec); err != nil {
		return broker.CatalogRecord{}, fmt.Errorf("upsert catalog for %q: %w", rc.Name, err)
	}
	return rec, nil
}

func (s *Syncer) validateRepoConfig(rc RepoConfig) error {
	if s.store == nil {
		return errors.New("broker store must not be nil")
	}
	if rc.Name == "" {
		return errors.New("repo name must not be empty")
	}
	if rc.GitURL == "" {
		return fmt.Errorf("git_url for %q must not be empty", rc.Name)
	}
	if strings.HasPrefix(rc.GitURL, "-") {
		return fmt.Errorf("invalid git_url %q: must not start with '-'", rc.GitURL)
	}
	if rc.DefaultBranch != "" {
		if err := validateBranchName(rc.DefaultBranch); err != nil {
			return fmt.Errorf("configured default_branch for %q: %w", rc.Name, err)
		}
	}
	return nil
}

func (s *Syncer) repoMutex(repo string) *sync.Mutex {
	s.mu.Lock()
	defer s.mu.Unlock()
	mu, ok := s.repoLocks[repo]
	if !ok {
		mu = &sync.Mutex{}
		s.repoLocks[repo] = mu
	}
	return mu
}

func (s *Syncer) mirrorPath(repo string) (string, error) {
	sanitized, err := sanitizeRepoName(repo)
	if err != nil {
		return "", err
	}
	return filepath.Join(s.cacheDir, sanitized+".git"), nil
}

func sanitizeRepoName(repo string) (string, error) {
	if repo == "" || repo == "." || repo == ".." {
		return "", fmt.Errorf("invalid repo name %q", repo)
	}
	if strings.Contains(repo, "..") || strings.Contains(repo, "\x00") {
		return "", fmt.Errorf("invalid repo name %q", repo)
	}
	return url.PathEscape(repo), nil
}

func ensureBareMirror(ctx context.Context, mirror string) error {
	if _, err := os.Stat(filepath.Join(mirror, "HEAD")); err == nil {
		return nil
	} else if !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("stat bare mirror %q: %w", mirror, err)
	}

	if err := os.MkdirAll(filepath.Dir(mirror), 0o700); err != nil {
		return fmt.Errorf("create mirror cache directory: %w", err)
	}
	if _, err := runGit(ctx, false, "init", "--bare", mirror); err != nil {
		return fmt.Errorf("init bare mirror: %w", err)
	}
	return nil
}

func resolveDefaultBranch(ctx context.Context, rc RepoConfig) (string, error) {
	if rc.DefaultBranch != "" {
		return rc.DefaultBranch, nil
	}

	allowHTTPSCreds := strings.HasPrefix(rc.GitURL, "https://")
	out, err := runGit(ctx, allowHTTPSCreds, "ls-remote", "--symref", "--", rc.GitURL, "HEAD")
	if err != nil {
		return "", fmt.Errorf("detect default branch via ls-remote: %w", err)
	}
	branch, err := parseSymrefHeadBranch(string(out))
	if err != nil {
		return "", fmt.Errorf("repo %q: %w", rc.Name, err)
	}
	if err := validateBranchName(branch); err != nil {
		return "", fmt.Errorf("detected default branch for %q: %w", rc.Name, err)
	}
	return branch, nil
}

func fetchAndResolveSHA(ctx context.Context, mirror, gitURL, branch string) (string, error) {
	refspec := fmt.Sprintf("refs/heads/%s:refs/heads/%s", branch, branch)
	allowHTTPSCreds := strings.HasPrefix(gitURL, "https://")
	if _, err := runGit(ctx, allowHTTPSCreds, "--git-dir="+mirror, "fetch", "--depth=1", "--force", "--", gitURL, refspec); err != nil {
		return "", fmt.Errorf("fetch default branch %q: %w", branch, err)
	}

	out, err := runGit(ctx, false, "--git-dir="+mirror, "rev-parse", "refs/heads/"+branch)
	if err != nil {
		return "", fmt.Errorf("resolve commit SHA for %q: %w", branch, err)
	}
	sha := strings.TrimSpace(string(out))
	if sha == "" {
		return "", fmt.Errorf("empty commit SHA for branch %q", branch)
	}
	return sha, nil
}

func readAndValidateCatalog(ctx context.Context, mirror, sha string) ([]byte, error) {
	raw, err := runGit(ctx, false, "--git-dir="+mirror, "show", sha+":"+catalogFilePath)
	if err != nil {
		return nil, fmt.Errorf("read %s at %s: %w", catalogFilePath, sha, err)
	}

	cat, err := catalog.Parse(raw)
	if err != nil {
		return nil, fmt.Errorf("parse %s at %s: %w", catalogFilePath, sha, err)
	}

	skillNames := make([]string, 0, len(cat.Skills))
	for name := range cat.Skills {
		skillNames = append(skillNames, name)
	}
	slices.Sort(skillNames)

	for _, name := range skillNames {
		skill := cat.Skills[name]
		spec := sha + ":" + skill.SkillPath
		if _, err := runGit(ctx, false, "--git-dir="+mirror, "cat-file", "-e", spec); err != nil {
			return nil, fmt.Errorf("skill %q missing %q at %s: %w", name, skill.SkillPath, sha, err)
		}
	}
	return raw, nil
}

func parseSymrefHeadBranch(out string) (string, error) {
	const prefix = "ref: refs/heads/"
	const suffix = "\tHEAD"
	for _, line := range strings.Split(out, "\n") {
		line = strings.TrimRight(line, "\r")
		if strings.HasPrefix(line, prefix) && strings.HasSuffix(line, suffix) {
			branch := strings.TrimSuffix(strings.TrimPrefix(line, prefix), suffix)
			if branch != "" {
				return branch, nil
			}
		}
	}
	return "", errors.New("remote has no symbolic HEAD; set default_branch explicitly in broker.toml")
}

func validateBranchName(branch string) error {
	if branch == "" {
		return errors.New("branch name must not be empty")
	}
	if strings.HasPrefix(branch, "-") || strings.HasPrefix(branch, "/") || strings.HasPrefix(branch, ".") {
		return fmt.Errorf("invalid branch name %q", branch)
	}
	if strings.HasSuffix(branch, "/") || strings.HasSuffix(branch, ".") {
		return fmt.Errorf("invalid branch name %q", branch)
	}
	if strings.Contains(branch, "..") || strings.Contains(branch, "@{") || strings.ContainsAny(branch, " \t\r\n~^:?*[\\") {
		return fmt.Errorf("invalid branch name %q", branch)
	}
	return nil
}

func runGit(ctx context.Context, allowHTTPSCreds bool, args ...string) ([]byte, error) {
	cmd := exec.CommandContext(ctx, "git", args...)
	cmd.Env = sanitizedGitEnv(allowHTTPSCreds)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		detail := strings.TrimSpace(stderr.String())
		if detail == "" {
			detail = strings.TrimSpace(stdout.String())
		}
		return nil, fmt.Errorf("git %s: %w (output: %s)", strings.Join(args, " "), err, detail)
	}
	return stdout.Bytes(), nil
}

func sanitizedGitEnv(allowHTTPSCreds bool) []string {
	var env []string
	for _, kv := range os.Environ() {
		if strings.HasPrefix(kv, "GIT_") {
			continue
		}
		env = append(env, kv)
	}
	env = append(env,
		"GIT_CONFIG_NOSYSTEM=1",
		"GIT_TERMINAL_PROMPT=0",
	)
	if !allowHTTPSCreds {
		env = append(env, "GIT_CONFIG_GLOBAL=/dev/null")
	}
	return env
}
