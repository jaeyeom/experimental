package scan

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os/exec"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
)

const gitTimeout = 30 * time.Second

// WaiveCompareGH is the optional GitHub REST API surface used to classify
// stale waives when no local git checkout of the repository is available.
type WaiveCompareGH interface {
	CompareCommits(ctx context.Context, owner, repo, base, head string) (status, mergeBaseSHA string, err error)
	CompareDiff(ctx context.Context, owner, repo, base, head string) ([]byte, error)
}

func classifyStaleWaive(ctx context.Context, deps Deps, cfg config.Config, owner, name string, pr *PR, oldHead string, forcePushedAfter bool, commits []gh.PRCommit) (string, error) {
	if forcePushedAfter {
		return WaiveClassAuthorChanges, nil
	}
	var gitErr error
	if repoDir := findRepoDir(pr.Repo, deps, cfg, pr.agentCWDs); repoDir != "" {
		if err := ensureWaiveObjects(ctx, repoDir, pr.Base, pr.Number, oldHead, pr.HeadSHA); err != nil {
			gitErr = err
		} else {
			class, err := classifyGitRepoWithFetch(ctx, repoDir, pr.Base, oldHead, pr.HeadSHA)
			if err == nil {
				return class, nil
			}
			gitErr = err
		}
	}
	if cmp, ok := deps.GH.(WaiveCompareGH); ok {
		class, err := classifyWithGH(ctx, cmp, owner, name, pr.Base, oldHead, pr.HeadSHA, commits)
		if err != nil {
			return WaiveClassAuthorChanges, err
		}
		return class, nil
	}
	if gitErr != nil {
		return WaiveClassAuthorChanges, gitErr
	}
	return WaiveClassAuthorChanges, errors.New("no local git checkout found and GitHub compare unavailable")
}

func findRepoDir(repo string, deps Deps, cfg config.Config, agentCWDs []string) string {
	if dir := deps.RepoDirs[repo]; dir != "" {
		return dir
	}
	if deps.RepoResolver != nil {
		if dir := deps.RepoResolver(repo); dir != "" {
			return dir
		}
	}
	var candidates []string
	candidates = append(candidates, agentCWDs...)
	if dir := cfg.OpenTabCwd(""); dir != "" {
		candidates = append(candidates, dir)
	}
	for _, dir := range candidates {
		if dir == "" {
			continue
		}
		got, err := GitHubRepoFromCWD(dir)
		if err == nil && got == repo {
			return dir
		}
	}
	return ""
}

// ClassifyGitRepo classifies whether moving from oldHead to newHead on a PR
// targeting baseBranch in repoDir is "base-only" or "author changes".
//
// A transition is "base-only" only when oldHead is an ancestor of newHead,
// every commit on --first-parent oldHead..newHead is a 2-parent merge whose
// second parent is an ancestor of baseBranch, and git diff
// <old merge-base>..<oldHead> is byte-identical to git diff
// <new merge-base>..<newHead>.
func ClassifyGitRepo(ctx context.Context, repoDir, baseBranch, oldHead, newHead string) (string, error) {
	oldSHA, err := revParseCommit(ctx, repoDir, oldHead)
	if err != nil {
		return WaiveClassAuthorChanges, fmt.Errorf("resolve old head %s: %w", oldHead, err)
	}
	newSHA, err := revParseCommit(ctx, repoDir, newHead)
	if err != nil {
		return WaiveClassAuthorChanges, fmt.Errorf("resolve new head %s: %w", newHead, err)
	}
	baseCommit, err := resolveBaseCommit(ctx, repoDir, baseBranch)
	if err != nil {
		return WaiveClassAuthorChanges, fmt.Errorf("resolve base %s: %w", baseBranch, err)
	}
	if oldSHA == newSHA {
		return WaiveClassAuthorChanges, fmt.Errorf("old and new heads are identical (%s)", oldSHA)
	}
	anc, err := isAncestor(ctx, repoDir, oldSHA, newSHA)
	if err != nil {
		return WaiveClassAuthorChanges, err
	}
	if !anc {
		return WaiveClassAuthorChanges, nil
	}
	ok, err := allMergesFromBase(ctx, repoDir, baseCommit, oldSHA, newSHA)
	if err != nil || !ok {
		return WaiveClassAuthorChanges, err
	}
	return compareMergeBaseDiffs(ctx, repoDir, baseCommit, oldSHA, newSHA)
}

func classifyGitRepoWithFetch(ctx context.Context, repoDir, baseBranch, oldHead, newHead string) (string, error) {
	class, err := ClassifyGitRepo(ctx, repoDir, baseBranch, oldHead, newHead)
	if err != nil || class == WaiveClassBaseOnly {
		return class, err
	}
	// If local origin/<base> was behind the merged-in base commit, refresh
	// origin/<base> once before concluding author changes.
	if hasOriginRemote(ctx, repoDir) {
		if fetchErr := fetchBaseBranch(ctx, repoDir, baseBranch); fetchErr == nil {
			return ClassifyGitRepo(ctx, repoDir, baseBranch, oldHead, newHead)
		}
	}
	return class, nil
}

func allMergesFromBase(ctx context.Context, repoDir, baseCommit, oldSHA, newSHA string) (bool, error) {
	out, err := runGit(ctx, repoDir, "rev-list", "--first-parent", "--parents", oldSHA+".."+newSHA)
	if err != nil {
		return false, err
	}
	lines := strings.Split(strings.TrimSpace(string(out)), "\n")
	if len(lines) == 0 || (len(lines) == 1 && lines[0] == "") {
		return false, nil
	}
	var lastFirstParent string
	for _, line := range lines {
		fields := strings.Fields(line)
		if len(fields) < 2 {
			return false, nil
		}
		parents := fields[1:]
		if len(parents) != 2 {
			return false, nil
		}
		lastFirstParent = parents[0]
		fromBase, err := isAncestor(ctx, repoDir, parents[1], baseCommit)
		if err != nil {
			return false, err
		}
		if !fromBase {
			return false, nil
		}
	}
	return lastFirstParent == oldSHA, nil
}

func compareMergeBaseDiffs(ctx context.Context, repoDir, baseCommit, oldSHA, newSHA string) (string, error) {
	oldMB, err := gitMergeBase(ctx, repoDir, baseCommit, oldSHA)
	if err != nil {
		return WaiveClassAuthorChanges, err
	}
	newMB, err := gitMergeBase(ctx, repoDir, baseCommit, newSHA)
	if err != nil {
		return WaiveClassAuthorChanges, err
	}
	oldDiff, err := runGit(ctx, repoDir, "diff", "--no-ext-diff", "--no-color", oldMB+".."+oldSHA)
	if err != nil {
		return WaiveClassAuthorChanges, err
	}
	newDiff, err := runGit(ctx, repoDir, "diff", "--no-ext-diff", "--no-color", newMB+".."+newSHA)
	if err != nil {
		return WaiveClassAuthorChanges, err
	}
	if !bytes.Equal(oldDiff, newDiff) {
		return WaiveClassAuthorChanges, nil
	}
	return WaiveClassBaseOnly, nil
}

func ensureWaiveObjects(ctx context.Context, repoDir, baseBranch string, prNumber int, oldHead, newHead string) error {
	if hasCommit(ctx, repoDir, oldHead) && hasCommit(ctx, repoDir, newHead) {
		if _, err := resolveBaseCommit(ctx, repoDir, baseBranch); err == nil {
			return nil
		}
	}
	if !hasOriginRemote(ctx, repoDir) {
		return errors.New("missing commits and no origin remote")
	}
	_, err := runGit(ctx, repoDir, "fetch", "--no-tags", "--quiet", "origin",
		fmt.Sprintf("refs/heads/%s:refs/remotes/origin/%s", baseBranch, baseBranch),
		fmt.Sprintf("refs/pull/%d/head:refs/remotes/origin/pr/%d", prNumber, prNumber),
	)
	return err
}

func fetchBaseBranch(ctx context.Context, repoDir, baseBranch string) error {
	_, err := runGit(ctx, repoDir, "fetch", "--no-tags", "--quiet", "origin",
		fmt.Sprintf("refs/heads/%s:refs/remotes/origin/%s", baseBranch, baseBranch),
	)
	return err
}

func hasOriginRemote(ctx context.Context, repoDir string) bool {
	_, err := runGit(ctx, repoDir, "remote", "get-url", "origin")
	return err == nil
}

func hasCommit(ctx context.Context, repoDir, rev string) bool {
	_, err := runGit(ctx, repoDir, "cat-file", "-e", rev+"^{commit}")
	return err == nil
}

func resolveBaseCommit(ctx context.Context, repoDir, baseBranch string) (string, error) {
	for _, ref := range []string{
		"refs/remotes/origin/" + baseBranch,
		"refs/heads/" + baseBranch,
		baseBranch,
	} {
		if sha, err := revParseCommit(ctx, repoDir, ref); err == nil {
			return sha, nil
		}
	}
	return "", fmt.Errorf("base branch %q not found", baseBranch)
}

func revParseCommit(ctx context.Context, repoDir, rev string) (string, error) {
	out, err := runGit(ctx, repoDir, "rev-parse", "--verify", "--quiet", rev+"^{commit}")
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(out)), nil
}

func isAncestor(ctx context.Context, repoDir, ancestor, descendant string) (bool, error) {
	gctx, cancel := context.WithTimeout(ctx, gitTimeout)
	defer cancel()
	cmd := exec.CommandContext(gctx, "git", "-C", repoDir, "merge-base", "--is-ancestor", ancestor, descendant) //nolint:gosec // fixed git binary with arg list
	err := cmd.Run()
	if err == nil {
		return true, nil
	}
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) && exitErr.ExitCode() == 1 {
		return false, nil
	}
	return false, fmt.Errorf("git merge-base --is-ancestor: %w", err)
}

func gitMergeBase(ctx context.Context, repoDir, a, b string) (string, error) {
	out, err := runGit(ctx, repoDir, "merge-base", a, b)
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(out)), nil
}

func runGit(ctx context.Context, repoDir string, args ...string) ([]byte, error) {
	gctx, cancel := context.WithTimeout(ctx, gitTimeout)
	defer cancel()
	cmd := exec.CommandContext(gctx, "git", append([]string{"-C", repoDir}, args...)...) //nolint:gosec // fixed git binary with arg list
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg != "" {
			return nil, fmt.Errorf("git %s: %s: %w", args[0], msg, err)
		}
		return nil, fmt.Errorf("git %s: %w", args[0], err)
	}
	return stdout.Bytes(), nil
}

func classifyWithGH(ctx context.Context, cmp WaiveCompareGH, owner, repo, baseBranch, oldHead, newHead string, commits []gh.PRCommit) (string, error) {
	byOID := make(map[string]gh.PRCommit, len(commits))
	for _, c := range commits {
		byOID[c.OID] = c
	}
	ok, err := verifyGHFirstParentMerges(ctx, cmp, owner, repo, baseBranch, oldHead, newHead, byOID)
	if err != nil || !ok {
		return WaiveClassAuthorChanges, err
	}
	_, oldMB, err := cmp.CompareCommits(ctx, owner, repo, baseBranch, oldHead)
	if err != nil {
		return WaiveClassAuthorChanges, fmt.Errorf("compare old head: %w", err)
	}
	_, newMB, err := cmp.CompareCommits(ctx, owner, repo, baseBranch, newHead)
	if err != nil {
		return WaiveClassAuthorChanges, fmt.Errorf("compare new head: %w", err)
	}
	if oldMB == "" || newMB == "" {
		return WaiveClassAuthorChanges, errors.New("missing merge-base commit from GitHub compare")
	}
	oldDiff, err := cmp.CompareDiff(ctx, owner, repo, oldMB, oldHead)
	if err != nil {
		return WaiveClassAuthorChanges, fmt.Errorf("compare old diff: %w", err)
	}
	newDiff, err := cmp.CompareDiff(ctx, owner, repo, newMB, newHead)
	if err != nil {
		return WaiveClassAuthorChanges, fmt.Errorf("compare new diff: %w", err)
	}
	if !bytes.Equal(oldDiff, newDiff) {
		return WaiveClassAuthorChanges, nil
	}
	return WaiveClassBaseOnly, nil
}

func verifyGHFirstParentMerges(ctx context.Context, cmp WaiveCompareGH, owner, repo, baseBranch, oldHead, newHead string, byOID map[string]gh.PRCommit) (bool, error) {
	cur := newHead
	seen := make(map[string]bool)
	steps := 0
	for cur != oldHead {
		if seen[cur] || steps > len(byOID)+1 {
			return false, nil
		}
		seen[cur] = true
		steps++
		c, ok := byOID[cur]
		if !ok || len(c.Parents) != 2 {
			return false, nil
		}
		status, mb, err := cmp.CompareCommits(ctx, owner, repo, c.Parents[1], baseBranch)
		if err != nil {
			return false, fmt.Errorf("compare parent %s: %w", c.Parents[1], err)
		}
		if !slices.Contains([]string{"ahead", "identical"}, status) {
			return false, nil
		}
		if mb != "" && mb != c.Parents[1] {
			return false, nil
		}
		cur = c.Parents[0]
	}
	return steps > 0, nil
}
