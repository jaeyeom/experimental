// Package worker manages isolated execution state for agent-pool consumer jobs,
// including detached git worktrees pinned strictly to verified catalog SHAs.
package worker

import (
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
)

// ErrNotDefaultBranchAncestor is returned when a requested catalogSha is not
// an ancestor of the repository's default branch.
var ErrNotDefaultBranchAncestor = errors.New("catalogSha is not an ancestor of default branch")

var (
	repoNameRe   = regexp.MustCompile(`^[A-Za-z0-9._-]+/[A-Za-z0-9._-]+$`)
	jobIDRe      = regexp.MustCompile(`^[A-Za-z0-9._-]+$`)
	catalogShaRe = regexp.MustCompile(`^[0-9a-f]{40}$`)
)

// RepoRemote describes git remote configuration for a repository.
type RepoRemote struct {
	Name          string `toml:"name"`           // Repository slug, e.g. "org/core-api".
	GitURL        string `toml:"git_url"`        // Optional; defaults to "https://github.com/<name>.git".
	DefaultBranch string `toml:"default_branch"` // Optional fallback if broker omits defaultBranch.
}

// Worktree represents an ephemeral detached git worktree pinned to a verified
// catalog commit SHA.
type Worktree struct {
	Path       string
	Repo       string
	CatalogSha string
	Cleanup    func() error
}

// WorktreeManager maintains per-repository bare git mirrors and creates
// ephemeral detached worktrees for consumer jobs.
type WorktreeManager struct {
	stateRoot string
	remotes   map[string]RepoRemote
	logger    *slog.Logger

	mu        sync.Mutex
	repoLocks map[string]*sync.Mutex
}

// NewWorktreeManager creates a WorktreeManager rooted at stateRoot.
func NewWorktreeManager(stateRoot string, remotes map[string]RepoRemote, logger *slog.Logger) *WorktreeManager {
	copied := make(map[string]RepoRemote, len(remotes))
	for k, v := range remotes {
		copied[k] = v
	}
	if logger == nil {
		logger = slog.New(slog.NewTextHandler(io.Discard, nil))
	}
	return &WorktreeManager{
		stateRoot: stateRoot,
		remotes:   copied,
		logger:    logger,
		repoLocks: make(map[string]*sync.Mutex),
	}
}

// Prepare fetches the repository's default branch into the bare mirror,
// verifies that catalogSha is a 40-char hex commit on the default branch's
// ancestry, and checks out a fresh detached worktree pinned to catalogSha.
func (m *WorktreeManager) Prepare(ctx context.Context, jobID, repo, defaultBranch, catalogSha string) (*Worktree, error) {
	if err := validateInputs(jobID, repo, defaultBranch, catalogSha); err != nil {
		return nil, err
	}

	gitURL := m.resolveGitURL(repo)
	if strings.HasPrefix(gitURL, "-") {
		return nil, fmt.Errorf("invalid git URL %q: must not start with '-'", gitURL)
	}

	repoMu := m.repoMutex(repo)
	repoMu.Lock()
	defer repoMu.Unlock()

	mirror := m.mirrorPath(repo)
	if err := m.ensureBareMirror(ctx, mirror, gitURL); err != nil {
		return nil, err
	}

	branch, err := m.resolveDefaultBranch(ctx, repo, defaultBranch, gitURL)
	if err != nil {
		return nil, err
	}

	if err := m.fetchDefaultBranch(ctx, mirror, gitURL, branch); err != nil {
		return nil, err
	}

	if err := m.verifyDefaultBranchAncestor(ctx, mirror, catalogSha, branch); err != nil {
		return nil, err
	}

	worktreePath := m.worktreePath(jobID)
	if err := m.addDetachedWorktree(ctx, mirror, worktreePath, catalogSha); err != nil {
		return nil, err
	}

	var once sync.Once
	var cleanupErr error
	cleanup := func() error {
		once.Do(func() {
			cleanupErr = m.removeWorktree(repo, mirror, worktreePath)
		})
		return cleanupErr
	}

	m.logger.Info("prepared detached worktree",
		"job_id", jobID,
		"repo", repo,
		"branch", branch,
		"catalog_sha", catalogSha,
		"path", worktreePath,
	)

	return &Worktree{
		Path:       worktreePath,
		Repo:       repo,
		CatalogSha: catalogSha,
		Cleanup:    cleanup,
	}, nil
}

// PruneOrphans removes leftover directories under <stateRoot>/worktrees/ and
// runs `git worktree prune` on each bare mirror under <stateRoot>/repos/.
func (m *WorktreeManager) PruneOrphans(ctx context.Context) error {
	var errs []error

	if err := m.removeOrphanWorktreeDirs(); err != nil {
		errs = append(errs, err)
	}
	if err := m.pruneMirrorWorktrees(ctx); err != nil {
		errs = append(errs, err)
	}
	return errors.Join(errs...)
}

func (m *WorktreeManager) repoMutex(repo string) *sync.Mutex {
	m.mu.Lock()
	defer m.mu.Unlock()
	mu, ok := m.repoLocks[repo]
	if !ok {
		mu = &sync.Mutex{}
		m.repoLocks[repo] = mu
	}
	return mu
}

func (m *WorktreeManager) mirrorPath(repo string) string {
	return filepath.Join(m.stateRoot, "repos", filepath.FromSlash(repo)+".git")
}

func (m *WorktreeManager) worktreePath(jobID string) string {
	return filepath.Join(m.stateRoot, "worktrees", jobID)
}

func (m *WorktreeManager) resolveGitURL(repo string) string {
	if r, ok := m.remotes[repo]; ok && r.GitURL != "" {
		return r.GitURL
	}
	return fmt.Sprintf("https://github.com/%s.git", repo)
}

func (m *WorktreeManager) ensureBareMirror(ctx context.Context, mirror, gitURL string) error {
	if _, err := os.Stat(filepath.Join(mirror, "HEAD")); err != nil {
		if !errors.Is(err, os.ErrNotExist) {
			return fmt.Errorf("stat bare mirror %q: %w", mirror, err)
		}
		if err := os.MkdirAll(filepath.Dir(mirror), 0o700); err != nil {
			return fmt.Errorf("create mirror parent dir: %w", err)
		}
		if _, err := runGit(ctx, false, "init", "--bare", mirror); err != nil {
			return fmt.Errorf("init bare mirror: %w", err)
		}
	}

	if _, err := runGit(ctx, false, "--git-dir="+mirror, "remote", "set-url", "origin", gitURL); err != nil {
		if _, addErr := runGit(ctx, false, "--git-dir="+mirror, "remote", "add", "origin", gitURL); addErr != nil {
			return fmt.Errorf("configure origin remote: %w", addErr)
		}
	}
	return nil
}

func (m *WorktreeManager) resolveDefaultBranch(ctx context.Context, repo, defaultBranch, gitURL string) (string, error) {
	if defaultBranch != "" {
		return defaultBranch, nil
	}
	if r, ok := m.remotes[repo]; ok && r.DefaultBranch != "" {
		if err := validateBranchName(r.DefaultBranch); err != nil {
			return "", fmt.Errorf("configured default_branch for %q: %w", repo, err)
		}
		return r.DefaultBranch, nil
	}

	allowHTTPSCreds := strings.HasPrefix(gitURL, "https://")
	out, err := runGit(ctx, allowHTTPSCreds, "ls-remote", "--symref", "--", gitURL, "HEAD")
	if err != nil {
		return "", fmt.Errorf("detect default branch via ls-remote: %w", err)
	}
	branch, err := parseSymrefHeadBranch(out)
	if err != nil {
		return "", err
	}
	if err := validateBranchName(branch); err != nil {
		return "", fmt.Errorf("detected default branch: %w", err)
	}
	return branch, nil
}

func (m *WorktreeManager) fetchDefaultBranch(ctx context.Context, mirror, gitURL, branch string) error {
	refspec := fmt.Sprintf("+refs/heads/%s:refs/remotes/origin/%s", branch, branch)
	allowHTTPSCreds := strings.HasPrefix(gitURL, "https://")
	if _, err := runGit(ctx, allowHTTPSCreds, "--git-dir="+mirror, "fetch", "--prune", "--", gitURL, refspec); err != nil {
		return fmt.Errorf("fetch default branch %q: %w", branch, err)
	}
	return nil
}

func (m *WorktreeManager) verifyDefaultBranchAncestor(ctx context.Context, mirror, catalogSha, branch string) error {
	remoteRef := "refs/remotes/origin/" + branch
	if _, err := runGit(ctx, false, "--git-dir="+mirror, "merge-base", "--is-ancestor", catalogSha, remoteRef); err != nil {
		return fmt.Errorf("%w: commit %s is not an ancestor of %s: %w", ErrNotDefaultBranchAncestor, catalogSha, remoteRef, err)
	}
	return nil
}

func (m *WorktreeManager) addDetachedWorktree(ctx context.Context, mirror, worktreePath, catalogSha string) error {
	if err := os.MkdirAll(filepath.Dir(worktreePath), 0o700); err != nil {
		return fmt.Errorf("create worktrees directory: %w", err)
	}
	if _, err := runGit(ctx, false, "--git-dir="+mirror, "worktree", "add", "--detach", worktreePath, catalogSha); err != nil {
		return fmt.Errorf("add detached worktree: %w", err)
	}
	return nil
}

func (m *WorktreeManager) removeWorktree(repo, mirror, worktreePath string) error {
	repoMu := m.repoMutex(repo)
	repoMu.Lock()
	defer repoMu.Unlock()

	ctx := context.Background()
	if _, err := runGit(ctx, false, "--git-dir="+mirror, "worktree", "remove", "--force", worktreePath); err == nil {
		return nil
	}

	var errs []error
	if err := os.RemoveAll(worktreePath); err != nil {
		errs = append(errs, fmt.Errorf("remove worktree dir %q: %w", worktreePath, err))
	}
	if _, err := runGit(ctx, false, "--git-dir="+mirror, "worktree", "prune"); err != nil {
		errs = append(errs, fmt.Errorf("prune worktrees for %q: %w", mirror, err))
	}
	return errors.Join(errs...)
}

func (m *WorktreeManager) removeOrphanWorktreeDirs() error {
	worktreesDir := filepath.Join(m.stateRoot, "worktrees")
	entries, err := os.ReadDir(worktreesDir)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return nil
		}
		return fmt.Errorf("read worktrees dir %q: %w", worktreesDir, err)
	}

	var errs []error
	for _, entry := range entries {
		p := filepath.Join(worktreesDir, entry.Name())
		if err := os.RemoveAll(p); err != nil {
			errs = append(errs, fmt.Errorf("remove orphan worktree %q: %w", p, err))
		}
	}
	return errors.Join(errs...)
}

func (m *WorktreeManager) pruneMirrorWorktrees(ctx context.Context) error {
	reposDir := filepath.Join(m.stateRoot, "repos")
	if _, err := os.Stat(reposDir); err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return nil
		}
		return fmt.Errorf("stat repos dir %q: %w", reposDir, err)
	}

	var errs []error
	walkErr := filepath.WalkDir(reposDir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			errs = append(errs, fmt.Errorf("walk repos dir %q: %w", path, err))
			return nil
		}
		if !d.IsDir() || !strings.HasSuffix(d.Name(), ".git") {
			return nil
		}
		if _, pruneErr := runGit(ctx, false, "--git-dir="+path, "worktree", "prune"); pruneErr != nil {
			errs = append(errs, fmt.Errorf("prune mirror %q: %w", path, pruneErr))
		}
		return fs.SkipDir
	})
	if walkErr != nil {
		errs = append(errs, fmt.Errorf("walk repos dir: %w", walkErr))
	}
	return errors.Join(errs...)
}

func validateInputs(jobID, repo, defaultBranch, catalogSha string) error {
	if err := validateRepo(repo); err != nil {
		return err
	}
	if err := validateJobID(jobID); err != nil {
		return err
	}
	if err := validateCatalogSha(catalogSha); err != nil {
		return err
	}
	if defaultBranch != "" {
		if err := validateBranchName(defaultBranch); err != nil {
			return err
		}
	}
	return nil
}

func validateRepo(repo string) error {
	if !repoNameRe.MatchString(repo) {
		return fmt.Errorf("invalid repo %q: must match %s", repo, repoNameRe.String())
	}
	owner, name, _ := strings.Cut(repo, "/")
	if owner == "." || owner == ".." || name == "." || name == ".." {
		return fmt.Errorf("invalid repo %q: path traversal segments are not allowed", repo)
	}
	return nil
}

func validateJobID(jobID string) error {
	if !jobIDRe.MatchString(jobID) || jobID == "." || jobID == ".." {
		return fmt.Errorf("invalid jobID %q: must match %s", jobID, jobIDRe.String())
	}
	return nil
}

func validateCatalogSha(sha string) error {
	if !catalogShaRe.MatchString(sha) {
		return fmt.Errorf("invalid catalogSha %q: must be a 40-char lowercase hex SHA", sha)
	}
	return nil
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
	return "", errors.New("could not determine remote default branch from ls-remote --symref HEAD")
}

func runGit(ctx context.Context, allowHTTPSCreds bool, args ...string) (string, error) {
	cmd := exec.CommandContext(ctx, "git", args...)
	cmd.Env = sanitizedGitEnv(allowHTTPSCreds)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return string(out), fmt.Errorf("git %s: %w (output: %s)", strings.Join(args, " "), err, strings.TrimSpace(string(out)))
	}
	return string(out), nil
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
