package forkkeeper

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"strings"
)

// MovedTag describes an upstream tag whose target commit changed.
type MovedTag struct {
	Name string `json:"name"`
	Was  string `json:"was"`
	Now  string `json:"now"`
}

// RangeInfo describes the upstream commits, fork commits, and tag changes
// computed from git refs for a single fork-keeper run.
type RangeInfo struct {
	UpstreamOld            string     `json:"upstream_old"`              //nolint:tagliatelle // External range/verdict snake_case schema.
	UpstreamNew            string     `json:"upstream_new"`              //nolint:tagliatelle // External range/verdict snake_case schema.
	FirstRun               bool       `json:"first_run"`                 //nolint:tagliatelle // External range/verdict snake_case schema.
	UpToDate               bool       `json:"up_to_date"`                //nolint:tagliatelle // External range/verdict snake_case schema.
	Rewritten              bool       `json:"rewritten"`                 //nolint:tagliatelle // External range/verdict snake_case schema.
	ReviewBase             *string    `json:"review_base"`               //nolint:tagliatelle // External range/verdict snake_case schema.
	UserCommits            []string   `json:"user_commits"`              //nolint:tagliatelle // External range/verdict snake_case schema.
	ForkHasMerges          bool       `json:"fork_has_merges"`           //nolint:tagliatelle // External range/verdict snake_case schema.
	ForkBehindRecordedHead bool       `json:"fork_behind_recorded_head"` //nolint:tagliatelle // External range/verdict snake_case schema.
	MovedTags              []MovedTag `json:"moved_tags"`                //nolint:tagliatelle // External range/verdict snake_case schema.
	DeletedTags            []string   `json:"deleted_tags"`              //nolint:tagliatelle // External range/verdict snake_case schema.
}

// ComputeRange calculates the review range, fork patch stack, and upstream tag
// deltas purely from git refs in gitDir. It is read-only and never modifies
// refs, the index, or the working tree.
func ComputeRange(ctx context.Context, gitDir, upstreamRemote, branch string) (RangeInfo, error) {
	if upstreamRemote == "" {
		upstreamRemote = "upstream"
	}
	if branch == "" {
		branch = "main"
	}
	if strings.HasPrefix(upstreamRemote, "-") {
		return RangeInfo{}, fmt.Errorf("invalid upstream remote %q", upstreamRemote)
	}
	if strings.HasPrefix(branch, "-") {
		return RangeInfo{}, fmt.Errorf("invalid branch %q", branch)
	}

	newSHA := gitRevParseOptional(ctx, gitDir, fmt.Sprintf("refs/remotes/%s/%s^{commit}", upstreamRemote, branch))
	if newSHA == "" {
		return RangeInfo{}, fmt.Errorf("missing refs/remotes/%s/%s (fetch %s first)", upstreamRemote, branch, upstreamRemote)
	}
	forkSHA := gitRevParseOptional(ctx, gitDir, fmt.Sprintf("refs/remotes/origin/%s^{commit}", branch))
	if forkSHA == "" {
		return RangeInfo{}, fmt.Errorf("missing refs/remotes/origin/%s (fetch origin first)", branch)
	}

	oldSHA, firstRun, err := resolveUpstreamOld(ctx, gitDir, forkSHA, newSHA)
	if err != nil {
		return RangeInfo{}, err
	}

	upToDate := gitIsAncestor(ctx, gitDir, newSHA, forkSHA)
	rewritten, forkBehindRecordedHead, reviewBase := resolveReviewBase(ctx, gitDir, oldSHA, newSHA, forkSHA, firstRun)

	userCommits, err := computeUserCommits(ctx, gitDir, oldSHA, newSHA, forkSHA, firstRun)
	if err != nil {
		return RangeInfo{}, err
	}

	mergesOut, err := gitOutput(ctx, gitDir, "rev-list", "--merges", newSHA+".."+forkSHA)
	if err != nil {
		return RangeInfo{}, fmt.Errorf("check fork merges: %w", err)
	}
	forkHasMerges := strings.TrimSpace(mergesOut) != ""

	movedTags, deletedTags, err := computeTagDeltas(ctx, gitDir)
	if err != nil {
		return RangeInfo{}, err
	}

	return RangeInfo{
		UpstreamOld:            oldSHA,
		UpstreamNew:            newSHA,
		FirstRun:               firstRun,
		UpToDate:               upToDate,
		Rewritten:              rewritten,
		ReviewBase:             reviewBase,
		UserCommits:            userCommits,
		ForkHasMerges:          forkHasMerges,
		ForkBehindRecordedHead: forkBehindRecordedHead,
		MovedTags:              movedTags,
		DeletedTags:            deletedTags,
	}, nil
}

func resolveUpstreamOld(ctx context.Context, gitDir, forkSHA, newSHA string) (string, bool, error) {
	if oldSHA := gitRevParseOptional(ctx, gitDir, "refs/fork-keeper/upstream-head^{commit}"); oldSHA != "" {
		return oldSHA, false, nil
	}
	mb := gitMergeBaseOptional(ctx, gitDir, forkSHA, newSHA)
	if mb == nil {
		return "", true, errors.New("fork and upstream share no history")
	}
	return *mb, true, nil
}

func resolveReviewBase(ctx context.Context, gitDir, oldSHA, newSHA, forkSHA string, firstRun bool) (rewritten, forkBehind bool, reviewBase *string) {
	reviewBase = strCopyPtr(oldSHA)
	if firstRun {
		return false, false, reviewBase
	}
	if !gitIsAncestor(ctx, gitDir, oldSHA, newSHA) {
		rewritten = true
		reviewBase = gitMergeBaseOptional(ctx, gitDir, oldSHA, newSHA)
	}
	// If the fork no longer contains the recorded upstream head (rolled back,
	// or a promotion that never landed), commits between the fork and the
	// recorded head were never accepted, so review everything upstream has
	// that the fork lacks.
	if !gitIsAncestor(ctx, gitDir, oldSHA, forkSHA) {
		forkBehind = true
		reviewBase = gitMergeBaseOptional(ctx, gitDir, forkSHA, newSHA)
	}
	return rewritten, forkBehind, reviewBase
}

func computeUserCommits(ctx context.Context, gitDir, oldSHA, newSHA, forkSHA string, firstRun bool) ([]string, error) {
	revArgs := []string{"rev-list", "--reverse", "--no-merges", "--right-only", "--cherry-pick", newSHA + "..." + forkSHA}
	if !firstRun {
		revArgs = append(revArgs, "^"+oldSHA)
	}
	userOut, err := gitOutput(ctx, gitDir, revArgs...)
	if err != nil {
		return nil, fmt.Errorf("compute user commits: %w", err)
	}
	userCommits := splitNonEmptyLines(userOut)
	if userCommits == nil {
		return []string{}, nil
	}
	return userCommits, nil
}

func computeTagDeltas(ctx context.Context, gitDir string) ([]MovedTag, []string, error) {
	mirrorTags, err := readRefMap(ctx, gitDir, "refs/fork-keeper/upstream-tags/", 3)
	if err != nil {
		return nil, nil, fmt.Errorf("read mirrored upstream tags: %w", err)
	}
	fetchedTags, err := readRefMap(ctx, gitDir, "refs/upstream-tags/", 2)
	if err != nil {
		return nil, nil, fmt.Errorf("read fetched upstream tags: %w", err)
	}

	movedTags := []MovedTag{}
	deletedTags := []string{}
	for name, was := range mirrorTags {
		now, ok := fetchedTags[name]
		if !ok {
			deletedTags = append(deletedTags, name)
			continue
		}
		if now != was {
			movedTags = append(movedTags, MovedTag{
				Name: name,
				Was:  was,
				Now:  now,
			})
		}
	}
	sort.Slice(movedTags, func(i, j int) bool { return movedTags[i].Name < movedTags[j].Name })
	sort.Strings(deletedTags)
	return movedTags, deletedTags, nil
}

// BuildUpstreamDiff returns the unified diff of upstream changes between
// ReviewBase and UpstreamNew. When ReviewBase is nil (unrelated histories), it
// returns an explanatory note instead of failing.
func BuildUpstreamDiff(ctx context.Context, gitDir string, r RangeInfo) (string, error) {
	if r.ReviewBase == nil || *r.ReviewBase == "" {
		return "(no review base: fork and upstream have unrelated histories)\n", nil
	}
	diff, err := gitOutput(ctx, gitDir, "diff", "--no-renames", "--no-ext-diff", *r.ReviewBase, r.UpstreamNew)
	if err != nil {
		return "", fmt.Errorf("git diff %s..%s: %w", *r.ReviewBase, r.UpstreamNew, err)
	}
	return diff, nil
}

// BuildUpstreamSummary returns a concise commit list and diffstat for the
// upstream change range.
func BuildUpstreamSummary(ctx context.Context, gitDir string, r RangeInfo) string {
	if r.ReviewBase == nil || *r.ReviewBase == "" {
		return "No common review base (unrelated histories)."
	}
	var b strings.Builder
	logOut, err := gitOutput(ctx, gitDir, "log", "--oneline", "--no-decorate", *r.ReviewBase+".."+r.UpstreamNew)
	if err == nil && strings.TrimSpace(logOut) != "" {
		b.WriteString("### Upstream commits\n\n```text\n")
		b.WriteString(strings.TrimRight(logOut, "\n"))
		b.WriteString("\n```\n")
	}
	statOut, err := gitOutput(ctx, gitDir, "diff", "--stat", "--no-renames", *r.ReviewBase, r.UpstreamNew)
	if err == nil && strings.TrimSpace(statOut) != "" {
		if b.Len() > 0 {
			b.WriteByte('\n')
		}
		b.WriteString("### Upstream diffstat\n\n```text\n")
		b.WriteString(strings.TrimRight(statOut, "\n"))
		b.WriteString("\n```\n")
	}
	if b.Len() == 0 {
		return "No tree differences between review_base and upstream_new."
	}
	return strings.TrimRight(b.String(), "\n")
}

func gitMergeBaseOptional(ctx context.Context, gitDir, a, b string) *string {
	out, err := gitOutput(ctx, gitDir, "merge-base", a, b)
	if err != nil {
		return nil
	}
	trimmed := strings.TrimSpace(out)
	if trimmed == "" {
		return nil
	}
	return &trimmed
}

func readRefMap(ctx context.Context, gitDir, prefix string, lstrip int) (map[string]string, error) {
	out, err := gitOutput(ctx, gitDir, "for-each-ref",
		fmt.Sprintf("--format=%%(refname:lstrip=%d)\t%%(objectname)", lstrip), prefix)
	if err != nil {
		return nil, err
	}
	refs := make(map[string]string)
	for _, line := range splitNonEmptyLines(out) {
		name, sha, ok := strings.Cut(line, "\t")
		if !ok || name == "" || sha == "" {
			continue
		}
		refs[name] = sha
	}
	return refs, nil
}

func strCopyPtr(s string) *string {
	v := s
	return &v
}
