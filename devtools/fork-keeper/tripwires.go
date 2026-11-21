package forkkeeper

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os/exec"
	"regexp"
	"sort"
	"strings"
)

// MovedTag describes an upstream tag whose target commit changed.
type MovedTag struct {
	Name string `json:"name"`
	Was  string `json:"was"`
	Now  string `json:"now"`
}

// RangeInfo is the JSON output produced by review-range.sh.
type RangeInfo struct {
	UpstreamOld            string     `json:"upstream_old"`              //nolint:tagliatelle // External review-range.sh snake_case schema.
	UpstreamNew            string     `json:"upstream_new"`              //nolint:tagliatelle // External review-range.sh snake_case schema.
	FirstRun               bool       `json:"first_run"`                 //nolint:tagliatelle // External review-range.sh snake_case schema.
	UpToDate               bool       `json:"up_to_date"`                //nolint:tagliatelle // External review-range.sh snake_case schema.
	Rewritten              bool       `json:"rewritten"`                 //nolint:tagliatelle // External review-range.sh snake_case schema.
	ReviewBase             *string    `json:"review_base"`               //nolint:tagliatelle // External review-range.sh snake_case schema.
	UserCommits            []string   `json:"user_commits"`              //nolint:tagliatelle // External review-range.sh snake_case schema.
	ForkHasMerges          bool       `json:"fork_has_merges"`           //nolint:tagliatelle // External review-range.sh snake_case schema.
	ForkBehindRecordedHead bool       `json:"fork_behind_recorded_head"` //nolint:tagliatelle // External review-range.sh snake_case schema.
	MovedTags              []MovedTag `json:"moved_tags"`                //nolint:tagliatelle // External review-range.sh snake_case schema.
	DeletedTags            []string   `json:"deleted_tags"`              //nolint:tagliatelle // External review-range.sh snake_case schema.
}

// TripwireHit represents a single deterministic tripwire finding.
type TripwireHit struct {
	Kind   string
	Detail string
}

// String formats the hit as "tripwire: <kind>: <detail>".
func (h TripwireHit) String() string {
	return fmt.Sprintf("tripwire: %s: %s", h.Kind, h.Detail)
}

const (
	ciOrInstallPattern = `^\.(github|circleci|buildkite|githooks|husky)/|^\.gitlab-ci|^\.travis\.yml$|(^|/)\.pre-commit-|(^|/)(GNUmakefile|[Mm]akefile|[^/]*\.mk|Jenkinsfile|[Jj]ustfile|Rakefile|Taskfile\.ya?ml)$|(^|/)(install|setup|bootstrap)[^/]*$`
	manifestPattern    = `(^|/)(package\.json|package-lock\.json|npm-shrinkwrap\.json|yarn\.lock|pnpm-lock\.yaml|bun\.lockb?|go\.mod|go\.sum|Cargo\.toml|Cargo\.lock|requirements[^/]*\.txt|pyproject\.toml|poetry\.lock|uv\.lock|Pipfile(\.lock)?|setup\.cfg|Gemfile(\.lock)?|[^/]*\.gemspec|composer\.(json|lock)|Eask|Cask|[^/]*-pkg\.el|MODULE\.bazel(\.lock)?|WORKSPACE(\.bazel)?|pom\.xml|build\.gradle(\.kts)?|Package\.swift|flake\.(nix|lock))$`
)

// CheckTripwires runs deterministic diff and history checks on an upstream
// sync candidate. It is read-only and never modifies git refs or working trees.
func CheckTripwires(ctx context.Context, gitDir string, r RangeInfo, candidateSHA string) ([]TripwireHit, error) {
	var hits []TripwireHit
	add := func(kind, detail string) {
		hits = append(hits, TripwireHit{Kind: kind, Detail: detail})
	}

	if r.FirstRun {
		add("range", "first_run")
	}
	if r.Rewritten {
		add("range", "rewritten")
	}
	if r.ForkHasMerges {
		add("range", "fork_has_merges")
	}
	if r.ForkBehindRecordedHead {
		add("range", "fork_behind_recorded_head")
	}
	for _, mt := range r.MovedTags {
		add("tags", "moved "+mt.Name)
	}
	for _, dt := range r.DeletedTags {
		add("tags", "deleted "+dt)
	}

	if r.ReviewBase == nil || *r.ReviewBase == "" {
		add("range", "no review base (unrelated histories)")
	} else {
		diffHits, err := checkDiffTripwires(ctx, gitDir, *r.ReviewBase, r.UpstreamNew)
		if err != nil {
			return nil, err
		}
		hits = append(hits, diffHits...)
	}

	stackHits, err := checkStackTripwires(ctx, gitDir, r.UpstreamNew, candidateSHA, r.UserCommits)
	if err != nil {
		return nil, err
	}
	hits = append(hits, stackHits...)

	return hits, nil
}

func checkDiffTripwires(ctx context.Context, gitDir, base, newSHA string) ([]TripwireHit, error) {
	binHits, err := checkBinaryTripwires(ctx, gitDir, base, newSHA)
	if err != nil {
		return nil, err
	}
	pathHits, err := checkPathTripwires(ctx, gitDir, base, newSHA)
	if err != nil {
		return nil, err
	}
	patchHits, err := checkPatchTripwires(ctx, gitDir, base, newSHA)
	if err != nil {
		return nil, err
	}
	hits := make([]TripwireHit, 0, len(binHits)+len(pathHits)+len(patchHits))
	hits = append(hits, binHits...)
	hits = append(hits, pathHits...)
	hits = append(hits, patchHits...)
	return hits, nil
}

func checkBinaryTripwires(ctx context.Context, gitDir, base, newSHA string) ([]TripwireHit, error) {
	numstat, err := gitOutput(ctx, gitDir, "diff", "--no-renames", "--numstat", base, newSHA)
	if err != nil {
		return nil, fmt.Errorf("git diff --numstat: %w", err)
	}
	var hits []TripwireHit
	for _, line := range splitNonEmptyLines(numstat) {
		parts := strings.SplitN(line, "\t", 3)
		if len(parts) == 3 && parts[0] == "-" {
			hits = append(hits, TripwireHit{Kind: "binary", Detail: parts[2]})
		}
	}
	return hits, nil
}

func checkPathTripwires(ctx context.Context, gitDir, base, newSHA string) ([]TripwireHit, error) {
	ciRe := regexp.MustCompile(ciOrInstallPattern)
	manifestRe := regexp.MustCompile(manifestPattern)
	nameOnly, err := gitOutput(ctx, gitDir, "diff", "--no-renames", "--name-only", base, newSHA)
	if err != nil {
		return nil, fmt.Errorf("git diff --name-only: %w", err)
	}
	var hits []TripwireHit
	for _, path := range splitNonEmptyLines(nameOnly) {
		if ciRe.MatchString(path) {
			hits = append(hits, TripwireHit{Kind: "ci-or-install", Detail: path})
		}
		if manifestRe.MatchString(path) {
			hits = append(hits, TripwireHit{Kind: "manifest", Detail: path})
		}
	}
	return hits, nil
}

func checkPatchTripwires(ctx context.Context, gitDir, base, newSHA string) ([]TripwireHit, error) {
	patch, err := gitOutput(ctx, gitDir, "diff", "--no-renames", "--no-ext-diff", "--unified=0", base, newSHA)
	if err != nil {
		return nil, fmt.Errorf("git diff --unified=0: %w", err)
	}
	blobRe := regexp.MustCompile(`[A-Za-z0-9+/]{80,}`)
	urlRe := regexp.MustCompile(`[A-Za-z][A-Za-z0-9+.-]*://[^/[:space:]"'<>()` + "`" + `]+`)
	scpRe := regexp.MustCompile(`[A-Za-z0-9._-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}:`)

	blobFiles := make(map[string]struct{})
	hosts := make(map[string]struct{})
	var curFile string
	for _, line := range strings.Split(patch, "\n") {
		if strings.HasPrefix(line, "+++ ") {
			curFile = ""
			if len(line) >= 6 {
				curFile = line[6:]
			}
			continue
		}
		if !strings.HasPrefix(line, "+") {
			continue
		}
		text := line[1:]
		if curFile != "" && blobRe.MatchString(text) {
			blobFiles[curFile] = struct{}{}
		}
		for _, m := range urlRe.FindAllString(text, -1) {
			if h := extractURLHost(m); h != "" {
				hosts[h] = struct{}{}
			}
		}
		for _, m := range scpRe.FindAllString(text, -1) {
			if h := extractSCPHost(m); h != "" {
				hosts[h] = struct{}{}
			}
		}
	}

	var hits []TripwireHit
	for _, path := range sortedKeys(blobFiles) {
		hits = append(hits, TripwireHit{Kind: "blob", Detail: path})
	}
	for _, host := range sortedKeys(hosts) {
		if !gitGrepFixed(ctx, gitDir, host, base) {
			hits = append(hits, TripwireHit{Kind: "url", Detail: host})
		}
	}
	return hits, nil
}

func extractURLHost(raw string) string {
	if idx := strings.Index(raw, "://"); idx >= 0 {
		raw = raw[idx+3:]
	}
	if idx := strings.LastIndex(raw, "@"); idx >= 0 {
		raw = raw[idx+1:]
	}
	if idx := strings.LastIndex(raw, ":"); idx >= 0 {
		// Strip trailing port digits if present.
		port := raw[idx+1:]
		allDigits := true
		for _, r := range port {
			if r < '0' || r > '9' {
				allDigits = false
				break
			}
		}
		if allDigits {
			raw = raw[:idx]
		}
	}
	return raw
}

func extractSCPHost(raw string) string {
	if idx := strings.LastIndex(raw, "@"); idx >= 0 {
		raw = raw[idx+1:]
	}
	return strings.TrimSuffix(raw, ":")
}

func checkStackTripwires(ctx context.Context, gitDir, newSHA, candSHA string, userCommits []string) ([]TripwireHit, error) {
	if !gitIsAncestor(ctx, gitDir, newSHA, candSHA) {
		return []TripwireHit{{
			Kind:   "stack",
			Detail: fmt.Sprintf("candidate %s is not based on upstream_new %s", candSHA, newSHA),
		}}, nil
	}

	var hits []TripwireHit
	merges, err := gitOutput(ctx, gitDir, "rev-list", "--merges", newSHA+".."+candSHA)
	if err != nil {
		return nil, fmt.Errorf("git rev-list --merges: %w", err)
	}
	if strings.TrimSpace(merges) != "" {
		hits = append(hits, TripwireHit{Kind: "stack", Detail: "candidate has merge commits"})
	}

	want, err := patchIDs(ctx, gitDir, userCommits)
	if err != nil {
		return nil, err
	}
	candRevOut, err := gitOutput(ctx, gitDir, "rev-list", "--reverse", newSHA+".."+candSHA)
	if err != nil {
		return nil, fmt.Errorf("git rev-list --reverse: %w", err)
	}
	got, err := patchIDs(ctx, gitDir, splitNonEmptyLines(candRevOut))
	if err != nil {
		return nil, err
	}
	if !slicesEqual(want, got) {
		hits = append(hits, TripwireHit{Kind: "stack", Detail: "candidate patches differ from the fork's own commits"})
	}
	return hits, nil
}

func patchIDs(ctx context.Context, gitDir string, commits []string) ([]string, error) {
	ids := make([]string, 0, len(commits))
	for _, c := range commits {
		//nolint:gosec // gitDir and commit SHA are passed directly to git without a shell.
		showCmd := exec.CommandContext(ctx, "git", "--git-dir="+gitDir, "show", "--no-ext-diff", c)
		var showOut bytes.Buffer
		var showErr bytes.Buffer
		showCmd.Stdout = &showOut
		showCmd.Stderr = &showErr
		if err := showCmd.Run(); err != nil {
			return nil, fmt.Errorf("git show %s: %w: %s", c, err, strings.TrimSpace(showErr.String()))
		}

		pidCmd := exec.CommandContext(ctx, "git", "patch-id", "--stable")
		pidCmd.Stdin = &showOut
		var pidOut bytes.Buffer
		var pidErr bytes.Buffer
		pidCmd.Stdout = &pidOut
		pidCmd.Stderr = &pidErr
		if err := pidCmd.Run(); err != nil {
			return nil, fmt.Errorf("git patch-id for %s: %w: %s", c, err, strings.TrimSpace(pidErr.String()))
		}
		fields := strings.Fields(pidOut.String())
		if len(fields) > 0 {
			ids = append(ids, fields[0])
		} else {
			ids = append(ids, "")
		}
	}
	return ids, nil
}

func gitOutput(ctx context.Context, gitDir string, args ...string) (string, error) {
	fullArgs := append([]string{"--git-dir=" + gitDir}, args...)
	cmd := exec.CommandContext(ctx, "git", fullArgs...)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("%w: %s", err, strings.TrimSpace(stderr.String()))
	}
	return stdout.String(), nil
}

func gitIsAncestor(ctx context.Context, gitDir, ancestor, descendant string) bool {
	//nolint:gosec // gitDir and revision arguments are passed directly to git without a shell.
	cmd := exec.CommandContext(ctx, "git", "--git-dir="+gitDir, "merge-base", "--is-ancestor", ancestor, descendant)
	return cmd.Run() == nil
}

func gitGrepFixed(ctx context.Context, gitDir, pattern, rev string) bool {
	//nolint:gosec // gitDir, pattern, and rev are passed directly to git without a shell.
	cmd := exec.CommandContext(ctx, "git", "--git-dir="+gitDir, "grep", "-q", "-F", "-e", pattern, rev, "--")
	err := cmd.Run()
	if err == nil {
		return true
	}
	var exitErr *exec.ExitError
	_ = errors.As(err, &exitErr)
	return false
}

func splitNonEmptyLines(s string) []string {
	var out []string
	for _, line := range strings.Split(s, "\n") {
		if line != "" {
			out = append(out, line)
		}
	}
	return out
}

func sortedKeys(m map[string]struct{}) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}

func slicesEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
