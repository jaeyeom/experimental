package cli

import (
	"context"
	"fmt"
	"net/url"
	"strconv"
	"strings"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func startTarget(ctx context.Context, st *store.Store, cfg *config.Config, id string) (source.Item, config.Source, error) {
	it, err := resolveItem(ctx, st, id, "")
	if err != nil {
		return source.Item{}, config.Source{}, err
	}
	srcCfg, err := configFor(cfg, it.Source)
	if err != nil {
		return source.Item{}, config.Source{}, err
	}
	if err := checkKind(it.Source, srcCfg.Kind); err != nil {
		return source.Item{}, config.Source{}, err
	}
	return it, srcCfg, nil
}

func startTargetScoped(ctx context.Context, cmd *cobra.Command, st *store.Store, cfg *config.Config, rawID, sourceOnly string) (source.Item, config.Source, error) {
	if sourceOnly != "" {
		if _, err := configFor(cfg, sourceOnly); err != nil {
			return source.Item{}, config.Source{}, err
		}
	}
	dirScope, err := startDirScope(cmd, cfg, sourceOnly)
	if err != nil {
		return source.Item{}, config.Source{}, err
	}
	hintScope := firstNonEmpty(sourceOnly, dirScope, cfg.ItemSource)
	canonicalID, urlSource, err := normalizeStartID(rawID, cfg, hintScope)
	if err != nil {
		return source.Item{}, config.Source{}, err
	}
	lookupOnly := sourceOnly
	if lookupOnly == "" && urlSource != "" {
		lookupOnly = urlSource
	}
	it, err := resolveStartItem(ctx, st, canonicalID, lookupOnly, hintScope)
	if err != nil {
		return source.Item{}, config.Source{}, err
	}
	srcCfg, err := configFor(cfg, it.Source)
	if err != nil {
		return source.Item{}, config.Source{}, err
	}
	if err := checkKind(it.Source, srcCfg.Kind); err != nil {
		return source.Item{}, config.Source{}, err
	}
	if err := enforceSourceContainment(cfg, srcCfg, it, sourceOnly, dirScope); err != nil {
		return source.Item{}, config.Source{}, err
	}
	return it, srcCfg, nil
}

func startDirScope(cmd *cobra.Command, cfg *config.Config, sourceOnly string) (string, error) {
	if sourceOnly != "" || !cfg.Claim.SourceContainment {
		return "", nil
	}
	env := processEnv()
	dir, err := env.Getwd()
	if err != nil {
		return "", fmt.Errorf("start: current directory: %w", err)
	}
	if key, ok := cfg.SourceForDir(dir, env); ok {
		return key, nil
	}
	if !configFlagChanged(cmd) {
		root, inGit, err := config.GitRoot(dir, env)
		if err != nil {
			return "", fmt.Errorf("start: %w", err)
		}
		if inGit {
			return "", fmt.Errorf("start: git repository %s is not configured in %s; run `factory init` or pass --source", root, cfg.Path)
		}
	}
	return "", nil
}

func resolveStartItem(ctx context.Context, st *store.Store, id, only, hintScope string) (source.Item, error) {
	if only != "" {
		return resolveItem(ctx, st, id, only)
	}
	it, err := resolveItem(ctx, st, id, "")
	if err != nil && hintScope != "" && strings.Contains(err.Error(), "expected one source, found ") {
		return resolveItem(ctx, st, id, hintScope)
	}
	return it, err
}

func enforceSourceContainment(cfg *config.Config, srcCfg config.Source, it source.Item, sourceOnly, dirScope string) error {
	if !srcCfg.Claim.SourceContainment {
		return nil
	}
	if cfg.ItemSource != "" && it.Source != cfg.ItemSource {
		return fmt.Errorf("start %s: expected source %s (%s), found %s", it.ID, cfg.ItemSource, config.ItemSourceEnv, it.Source)
	}
	if sourceOnly == "" && dirScope != "" && it.Source != dirScope {
		return fmt.Errorf("start %s: expected source %s (current directory), found %s; pass --source %s to override", it.ID, dirScope, it.Source, it.Source)
	}
	return nil
}

// normalizeStartID accepts a canonical ID, a GitHub issue URL, a Jira issue URL,
// or a GitHub issue number shorthand (#42 or 42) and returns the canonical ID
// and an optional matched source key.
func normalizeStartID(rawID string, cfg *config.Config, hintScope string) (id, matchedSource string, err error) {
	trimmed := strings.TrimSpace(rawID)
	if strings.HasPrefix(trimmed, "http://") || strings.HasPrefix(trimmed, "https://") {
		return parseIssueURL(trimmed, cfg)
	}
	if num, ok := parseIssueNumber(trimmed); ok {
		return resolveIssueNumber(rawID, num, cfg, hintScope)
	}
	return trimmed, "", nil
}

func parseIssueURL(rawURL string, cfg *config.Config) (id, matchedSource string, err error) {
	u, err := url.Parse(rawURL)
	if err != nil || u.Host == "" {
		return "", "", fmt.Errorf("start %s: expected an issue id or URL, found invalid URL", rawURL)
	}
	parts := strings.Split(strings.Trim(u.Path, "/"), "/")
	// GitHub issue URL: https://github.com/<owner>/<repo>/issues/<number>
	if len(parts) >= 4 && parts[2] == "issues" {
		if num, ok := parsePositiveInt(parts[3]); ok {
			repo := parts[0] + "/" + parts[1]
			canon := fmt.Sprintf("%s#%d", repo, num)
			return canon, findGitHubSource(cfg, repo), nil
		}
	}
	// Jira issue URL: https://<site>/browse/<KEY>
	if len(parts) >= 2 && parts[len(parts)-2] == "browse" && parts[len(parts)-1] != "" {
		key := parts[len(parts)-1]
		return key, findJiraSourceByHost(cfg, u.Host), nil
	}
	return "", "", fmt.Errorf("start %s: expected a GitHub (/owner/repo/issues/N) or Jira (/browse/KEY) issue URL, found %q", rawURL, u.Path)
}

func parseIssueNumber(s string) (int, bool) {
	s = strings.TrimPrefix(s, "#")
	return parsePositiveInt(s)
}

func parsePositiveInt(s string) (int, bool) {
	if s == "" {
		return 0, false
	}
	for i := range len(s) {
		if s[i] < '0' || s[i] > '9' {
			return 0, false
		}
	}
	n, err := strconv.Atoi(s)
	if err != nil || n <= 0 {
		return 0, false
	}
	return n, true
}

func resolveIssueNumber(rawID string, num int, cfg *config.Config, hintScope string) (id, matchedSource string, err error) {
	if hintScope != "" {
		srcCfg, err := configFor(cfg, hintScope)
		if err != nil {
			return "", "", err
		}
		if srcCfg.Kind != "github" || srcCfg.Repo == "" {
			return "", "", fmt.Errorf("start %s: expected a GitHub source for issue number shorthand, found %s", rawID, hintScope)
		}
		return fmt.Sprintf("%s#%d", srcCfg.Repo, num), hintScope, nil
	}
	var ghSources []config.Source
	for i := range cfg.Sources {
		if cfg.Sources[i].Kind == "github" && cfg.Sources[i].Repo != "" {
			ghSources = append(ghSources, cfg.Sources[i])
		}
	}
	switch len(ghSources) {
	case 0:
		return rawID, "", nil
	case 1:
		s := ghSources[0]
		return fmt.Sprintf("%s#%d", s.Repo, num), s.Kind + ":" + s.Name, nil
	default:
		return "", "", fmt.Errorf("start %s: expected owner/repo#%d or --source, found shorthand with %d GitHub sources", rawID, num, len(ghSources))
	}
}

func findGitHubSource(cfg *config.Config, repo string) string {
	for i := range cfg.Sources {
		if cfg.Sources[i].Kind == "github" && strings.EqualFold(cfg.Sources[i].Repo, repo) {
			return cfg.Sources[i].Kind + ":" + cfg.Sources[i].Name
		}
	}
	return ""
}

func findJiraSourceByHost(cfg *config.Config, host string) string {
	for i := range cfg.Sources {
		if cfg.Sources[i].Kind != "jira" || cfg.Sources[i].Site == "" {
			continue
		}
		u, err := url.Parse(cfg.Sources[i].Site)
		if err == nil && strings.EqualFold(u.Host, host) {
			return cfg.Sources[i].Kind + ":" + cfg.Sources[i].Name
		}
	}
	return ""
}

func firstNonEmpty(values ...string) string {
	for _, v := range values {
		if v != "" {
			return v
		}
	}
	return ""
}
