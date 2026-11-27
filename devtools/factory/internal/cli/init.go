package cli

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

const initGitTimeout = 30 * time.Second

var githubRepoPattern = regexp.MustCompile(`^[\w.-]+/[\w.-]+$`)

type initOptions struct {
	cwd    string
	kind   string
	base   string
	global bool
	force  bool
	dryRun bool
}

type repoInfo struct {
	root string
	kind string
	name string
	repo string
	base string
}

func newInitCmd(ctx context.Context) *cobra.Command {
	var opts initOptions
	cmd := &cobra.Command{
		Use:           "init",
		Short:         "Scaffold factory.toml for the current git repository",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 0 {
				return usageError{err: fmt.Errorf("init: expected no arguments, found %d", len(args))}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, _ []string) error {
			return runInit(ctx, cmd, opts)
		},
	}
	cmd.Flags().StringVar(&opts.cwd, "cwd", "", "directory inside the git repository to inspect (default: current directory)")
	cmd.Flags().StringVar(&opts.kind, "kind", "", "source kind to scaffold: github or jira (default: inferred from origin)")
	cmd.Flags().StringVar(&opts.base, "base", "", "default branch name (overrides origin/HEAD; fallback is main)")
	cmd.Flags().BoolVar(&opts.global, "global", false, "append to $XDG_CONFIG_HOME/factory/factory.toml instead of writing <git-root>/factory.toml")
	cmd.Flags().BoolVar(&opts.force, "force", false, "overwrite an existing <git-root>/factory.toml")
	cmd.Flags().BoolVar(&opts.dryRun, "dry-run", false, "print the target path and generated TOML without writing")
	return cmd
}

func runInit(ctx context.Context, cmd *cobra.Command, opts initOptions) error {
	if err := validateInitFlags(cmd, opts); err != nil {
		return err
	}
	env := processEnv()
	dir, err := resolveInitDir(opts.cwd, env)
	if err != nil {
		return err
	}
	exec := newCommandExec()
	if exec == nil {
		return errors.New("init: expected an executor, found nil")
	}
	info, err := inspectRepo(ctx, exec, dir, opts.kind, opts.base)
	if err != nil {
		return err
	}
	displayCwd, err := config.ContractHome(info.root, env)
	if err != nil {
		return fmt.Errorf("init: %w", err)
	}
	srcBlock := renderInitSource(info, displayCwd)
	fullContent := renderInitFile(srcBlock)
	return applyInit(cmd, opts, info, fullContent, srcBlock, env)
}

func validateInitFlags(cmd *cobra.Command, opts initOptions) error {
	if cmd.Flags().Changed("cwd") && strings.TrimSpace(opts.cwd) == "" {
		return usageError{err: errors.New("init: expected a non-empty --cwd directory, found empty")}
	}
	if cmd.Flags().Changed("base") && strings.TrimSpace(opts.base) == "" {
		return usageError{err: errors.New("init: expected a non-empty --base branch, found empty")}
	}
	switch opts.kind {
	case "", "github", "jira":
	default:
		return usageError{err: fmt.Errorf("init: invalid --kind %q: expected github or jira", opts.kind)}
	}
	if opts.global && opts.force {
		return usageError{err: errors.New("init: expected at most one of --global and --force, found both")}
	}
	return nil
}

func resolveInitDir(flagCwd string, env config.Env) (string, error) {
	if flagCwd == "" {
		dir, err := env.Getwd()
		if err != nil {
			return "", fmt.Errorf("init: current directory: %w", err)
		}
		return dir, nil
	}
	dir, err := config.ExpandHome(flagCwd, env)
	if err != nil {
		return "", fmt.Errorf("init: --cwd: %w", err)
	}
	return dir, nil
}

func inspectRepo(ctx context.Context, exec executor.Executor, dir, kind, baseFlag string) (repoInfo, error) {
	root, err := gitTopLevel(ctx, exec, dir)
	if err != nil {
		return repoInfo{}, err
	}
	base := strings.TrimSpace(baseFlag)
	if base == "" {
		base, err = gitDefaultBranch(ctx, exec, root)
		if err != nil {
			return repoInfo{}, err
		}
	}
	if kind == "jira" {
		return repoInfo{
			root: root,
			kind: "jira",
			name: filepath.Base(root),
			base: base,
		}, nil
	}
	remoteURL, err := gitOriginURL(ctx, exec, root)
	if err != nil {
		return repoInfo{}, err
	}
	owner, repo, ok := parseGitHubRemote(remoteURL)
	if !ok {
		return repoInfo{}, fmt.Errorf(
			"init: origin remote %q: expected a github.com remote (github.com[:/]owner/repo[.git]), found non-GitHub remote; pass --kind jira to scaffold a Jira source",
			remoteURL,
		)
	}
	return repoInfo{
		root: root,
		kind: "github",
		name: repo,
		repo: owner + "/" + repo,
		base: base,
	}, nil
}

func gitTopLevel(ctx context.Context, exec executor.Executor, dir string) (string, error) {
	res, err := executeInitGit(ctx, exec, "git rev-parse --show-toplevel", dir, "rev-parse", "--show-toplevel")
	if err != nil {
		return "", err
	}
	if res.ExitCode != 0 {
		detail := initGitDetail(res)
		return "", fmt.Errorf("init: %s is not a git repository: %s", dir, detail)
	}
	root := strings.TrimSpace(res.Output)
	if root == "" {
		return "", errors.New("init: git rev-parse --show-toplevel: expected a repository root path, found empty")
	}
	return filepath.Clean(root), nil
}

func gitOriginURL(ctx context.Context, exec executor.Executor, root string) (string, error) {
	res, err := executeInitGit(ctx, exec, "git remote get-url origin", root, "remote", "get-url", "origin")
	if err != nil {
		return "", err
	}
	if res.ExitCode != 0 {
		detail := initGitDetail(res)
		return "", fmt.Errorf("init: git remote get-url origin: %s; pass --kind jira to scaffold a Jira source", detail)
	}
	raw := strings.TrimSpace(res.Output)
	if raw == "" {
		return "", errors.New("init: git remote get-url origin: expected a remote URL, found empty")
	}
	return raw, nil
}

func gitDefaultBranch(ctx context.Context, exec executor.Executor, root string) (string, error) {
	res, err := executeInitGit(ctx, exec, "git symbolic-ref refs/remotes/origin/HEAD", root, "symbolic-ref", "refs/remotes/origin/HEAD")
	if err != nil {
		return "", err
	}
	if res.ExitCode != 0 {
		return "main", nil
	}
	ref := strings.TrimSpace(res.Output)
	branch, ok := strings.CutPrefix(ref, "refs/remotes/origin/")
	if !ok || branch == "" || strings.ContainsAny(branch, " \t\r\n") {
		return "", fmt.Errorf("init: git symbolic-ref refs/remotes/origin/HEAD: expected refs/remotes/origin/<branch>, found %q", ref)
	}
	return branch, nil
}

func executeInitGit(ctx context.Context, exec executor.Executor, label, dir string, args ...string) (*executor.ExecutionResult, error) {
	full := make([]string, 0, len(args)+2)
	full = append(full, "-C", dir)
	full = append(full, args...)
	res, err := exec.Execute(ctx, executor.ToolConfig{
		Command: "git",
		Args:    full,
		Timeout: initGitTimeout,
	})
	if err != nil {
		return nil, fmt.Errorf("init: %s: %w", label, err)
	}
	if res == nil {
		return nil, fmt.Errorf("init: %s: expected an execution result, found nil", label)
	}
	return res, nil
}

func initGitDetail(res *executor.ExecutionResult) string {
	if s := strings.TrimSpace(res.Stderr); s != "" {
		return s
	}
	if s := strings.TrimSpace(res.Output); s != "" {
		return s
	}
	return fmt.Sprintf("exit %d", res.ExitCode)
}

func parseGitHubRemote(raw string) (owner, repo string, ok bool) {
	raw = strings.TrimSpace(raw)
	if raw == "" {
		return "", "", false
	}
	if !strings.Contains(raw, "://") {
		hostPart, pathPart, found := strings.Cut(raw, ":")
		if !found {
			return "", "", false
		}
		if _, afterAt, hasAt := strings.Cut(hostPart, "@"); hasAt {
			hostPart = afterAt
		}
		if !strings.EqualFold(hostPart, "github.com") {
			return "", "", false
		}
		return parseOwnerRepo(pathPart)
	}
	u, err := url.Parse(raw)
	if err != nil || !strings.EqualFold(u.Hostname(), "github.com") {
		return "", "", false
	}
	return parseOwnerRepo(strings.TrimPrefix(u.Path, "/"))
}

func parseOwnerRepo(pathPart string) (owner, repo string, ok bool) {
	pathPart = strings.TrimSuffix(pathPart, "/")
	pathPart = strings.TrimSuffix(pathPart, ".git")
	if !githubRepoPattern.MatchString(pathPart) {
		return "", "", false
	}
	owner, repo, found := strings.Cut(pathPart, "/")
	if !found || owner == "" || repo == "" {
		return "", "", false
	}
	return owner, repo, true
}

func renderInitSource(info repoInfo, displayCwd string) string {
	if info.kind == "jira" {
		return fmt.Sprintf(`[[source]]
name             = %q
kind             = "jira"
site             = "https://example.atlassian.net" # TODO: replace with your Jira site URL
email            = "you@example.com"               # TODO: replace with your Jira email
token_file       = "~/.config/factory/jira-token"  # chmod 600; or set token_env = "JIRA_API_TOKEN"
jql              = "project = PRJ"                 # TODO: replace with your Jira JQL
ready_statuses   = ["To Do", "Backlog"]
claim_status     = "In Progress"
claim_assign     = true
require_labels   = ["agent-ready"]
blocks_link_type = "Blocks"
cwd              = %q
base             = %q
workspace        = "factory"
start_prompt     = "/implement-jira-ticket {id}"
`, info.name, displayCwd, info.base)
	}
	return fmt.Sprintf(`[[source]]
name           = %q
kind           = "github"
repo           = %q
cwd            = %q
base           = %q
require_labels = ["agent-ready"]
start_prompt   = "/gh-issue-resolver {url}"
`, info.name, info.repo, displayCwd, info.base)
}

func renderInitFile(srcBlock string) string {
	const header = `# Generated by factory init. See factory.example.toml and docs/usage.md.

# [defaults]
# max_in_flight      = 3
# min_start_interval = "10m"
# full_sync_every    = "24h"
# exclude_labels     = ["needs-human", "blocked-external"]
# skip_types         = ["Epic"]
# dry_run            = true # tick/start need --apply unless false

# [worker]
# spawner     = "agentbus" # "agentbus" (default), "subagent", or "herdr"
# agent       = "claude"   # coding agent (e.g. "agy", "claude", "codex", "grok")
# agent_args  = ["--model", "opus"]
# max_workers = 1
# worktree    = false

# [lease]
# gate      = "auto"  # auto (default), off, required, queue, or queue-required
# mode      = "off"   # off (default), prompt, or prompt+check
# claim_for = "45m"
# restore   = false

# [output]
# mode = "pr" # "pr" (default) or "merge"

# [claim]
# require_leaf       = true               # refuse skip_types and items with open children
# require_unblocked  = true               # refuse items with open BlockedBy blockers
# source_containment = true               # enforce FACTORY_ITEM_SOURCE and current git repo scope
# assign             = true               # take assignee ownership on claim
# ownership_rule     = "unassigned_or_me" # "unassigned_or_me" (default), "unassigned", or "any"
# mark_in_progress   = true               # mark issue in-progress on claim
# in_progress_rule   = "block"            # "block" (default) or "allow"
# create_label       = false              # GitHub: create claim label if missing
# comment            = true               # post claim comment on start
# nickname           = ""                 # optional machine nickname: [a-z0-9][a-z0-9-]{0,31}
# comment_template   = ""                 # placeholders: {agent}, {nickname}, {timestamp}, {id}, {source}

`
	return header + srcBlock
}

func applyInit(cmd *cobra.Command, opts initOptions, info repoInfo, fullContent, srcBlock string, env config.Env) error {
	var targetPath, finalContent, previewContent string
	var appended bool
	var err error
	if opts.global {
		targetPath, err = config.GlobalConfigPath(env)
		if err != nil {
			return fmt.Errorf("init: %w", err)
		}
		finalContent, appended, err = prepareGlobalInit(targetPath, fullContent, srcBlock, info, env)
		if err != nil {
			return err
		}
		previewContent = fullContent
		if appended {
			previewContent = srcBlock
		}
	} else {
		targetPath = filepath.Join(info.root, "factory.toml")
		if err := checkLocalTarget(targetPath, opts.force, env); err != nil {
			return err
		}
		finalContent = fullContent
		previewContent = fullContent
	}
	if info.kind == "github" {
		if err := validateGeneratedConfig(targetPath, finalContent, env); err != nil {
			return err
		}
	}
	if opts.dryRun {
		return writeDryRun(cmd, targetPath, previewContent, appended)
	}
	if opts.global {
		if err := os.MkdirAll(filepath.Dir(targetPath), 0o700); err != nil {
			return fmt.Errorf("init: create config dir: %w", err)
		}
	}
	if err := os.WriteFile(targetPath, []byte(finalContent), 0o600); err != nil {
		return fmt.Errorf("init: write %s: %w", targetPath, err)
	}
	return writeInitDone(cmd, targetPath, info, appended)
}

func checkLocalTarget(targetPath string, force bool, env config.Env) error {
	info, err := env.Stat(targetPath)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return nil
		}
		return fmt.Errorf("init: stat %s: %w", targetPath, err)
	}
	if info.IsDir() {
		return fmt.Errorf("init: %s is a directory", targetPath)
	}
	if !force {
		return fmt.Errorf("init: %s already exists; pass --force to overwrite", targetPath)
	}
	return nil
}

func prepareGlobalInit(targetPath, fullContent, srcBlock string, info repoInfo, env config.Env) (content string, appended bool, err error) {
	fi, err := env.Stat(targetPath)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return fullContent, false, nil
		}
		return "", false, fmt.Errorf("init: stat %s: %w", targetPath, err)
	}
	if fi.IsDir() {
		return "", false, fmt.Errorf("init: %s is a directory", targetPath)
	}
	existing, err := config.Load(targetPath, env)
	if err != nil {
		return "", false, fmt.Errorf("init: load existing %s: %w", targetPath, err)
	}
	for i := range existing.Sources {
		if existing.Sources[i].Name == info.name {
			return "", false, fmt.Errorf("init: %s already has a source named %q", targetPath, info.name)
		}
	}
	if key, ok := existing.SourceCoveringDir(info.root, env); ok {
		return "", false, fmt.Errorf("init: %s already has source %s covering %s", targetPath, key, info.root)
	}
	raw, err := env.ReadFile(targetPath)
	if err != nil {
		return "", false, fmt.Errorf("init: read %s: %w", targetPath, err)
	}
	return joinTOML(string(raw), srcBlock), true, nil
}

func joinTOML(existing, block string) string {
	switch {
	case existing == "":
		return block
	case strings.HasSuffix(existing, "\n\n"):
		return existing + block
	case strings.HasSuffix(existing, "\n"):
		return existing + "\n" + block
	default:
		return existing + "\n\n" + block
	}
}

func validateGeneratedConfig(targetPath, content string, env config.Env) error {
	checkEnv := env
	checkEnv.ReadFile = func(p string) ([]byte, error) {
		if p == targetPath {
			return []byte(content), nil
		}
		return env.ReadFile(p)
	}
	checkEnv.Stat = func(p string) (fs.FileInfo, error) {
		if p == targetPath {
			return virtualFileInfo{name: filepath.Base(targetPath), size: int64(len(content))}, nil
		}
		return env.Stat(p)
	}
	if _, err := config.Load(targetPath, checkEnv); err != nil {
		return fmt.Errorf("init: validate generated config: %w", err)
	}
	return nil
}

func writeDryRun(cmd *cobra.Command, targetPath, preview string, appended bool) error {
	action := "would write"
	if appended {
		action = "would append to"
	}
	if _, err := fmt.Fprintf(cmd.OutOrStdout(), "# dry-run: %s %s\n%s", action, targetPath, preview); err != nil {
		return fmt.Errorf("init: write output: %w", err)
	}
	return nil
}

func writeInitDone(cmd *cobra.Command, targetPath string, info repoInfo, appended bool) error {
	var msg string
	if appended {
		msg = fmt.Sprintf("appended %s:%s to %s\n", info.kind, info.name, targetPath)
	} else {
		msg = fmt.Sprintf("wrote %s\n", targetPath)
	}
	if _, err := fmt.Fprint(cmd.OutOrStdout(), msg); err != nil {
		return fmt.Errorf("init: write output: %w", err)
	}
	return nil
}

type virtualFileInfo struct {
	name string
	size int64
}

func (v virtualFileInfo) Name() string       { return v.name }
func (v virtualFileInfo) Size() int64        { return v.size }
func (v virtualFileInfo) Mode() fs.FileMode  { return 0o600 }
func (v virtualFileInfo) ModTime() time.Time { return time.Time{} }
func (v virtualFileInfo) IsDir() bool        { return false }
func (v virtualFileInfo) Sys() any           { return nil }
