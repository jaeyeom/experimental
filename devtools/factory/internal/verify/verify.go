// Package verify checks a worker's pull request and checkout.
// It does not fetch, and it does not change the pull request, the claim, or the worktree.
package verify

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// BaseMaxAge is how old the merge-base with origin/<base> may be.
// verify does not fetch, so the age is the age of the local origin/<base> commit.
const BaseMaxAge = 7 * 24 * time.Hour

// Input is one verify pass. Functions are injected so tests do not run git or gh.
type Input struct {
	ID       string
	Now      time.Time
	Cfg      config.Source
	Claim    store.Claim
	HasClaim bool
	PRs      []gh.PR
	View     func(context.Context, string, int) (gh.PRView, error)
	Status   func(context.Context, string) (string, error)
	Stash    func(context.Context, string) (int, error)
	Base     func(context.Context, string, string) (time.Time, error)
	Merge    func(context.Context, string, string, string, string) ([]string, error)
	ReadFile func(string) ([]byte, error)
}

// EffectiveOutputMode returns the output mode for a verify pass: the claim's
// recorded OutputMode if present, else the source's configured Output.Mode,
// defaulting to config.OutputModePR.
func EffectiveOutputMode(hasClaim bool, claim store.Claim, cfg config.Source) string {
	if hasClaim && claim.OutputMode != "" {
		return claim.OutputMode
	}
	if cfg.Output.Mode != "" {
		return cfg.Output.Mode
	}
	return config.OutputModePR
}

// Check reports every failed check. An error is a broken tool call or a bad
// configuration, not a failed check. Failures already found are still returned.
func Check(ctx context.Context, in Input) ([]string, error) {
	if err := validate(ctx, in); err != nil {
		return nil, err
	}
	var fails []string
	if EffectiveOutputMode(in.HasClaim, in.Claim, in.Cfg) == config.OutputModeMerge {
		if err := checkCheckout(ctx, in, &fails); err != nil {
			return fails, err
		}
		if err := checkMerge(ctx, in, &fails); err != nil {
			return fails, err
		}
		if err := checkStash(ctx, in, &fails); err != nil {
			return fails, err
		}
		return fails, nil
	}
	body, err := templateBody(in)
	if err != nil {
		return nil, err
	}
	if err := checkPull(ctx, in, body, &fails); err != nil {
		return fails, err
	}
	if err := checkCheckout(ctx, in, &fails); err != nil {
		return fails, err
	}
	if err := checkBase(ctx, in, &fails); err != nil {
		return fails, err
	}
	if err := checkStash(ctx, in, &fails); err != nil {
		return fails, err
	}
	return fails, nil
}

func validate(ctx context.Context, in Input) error {
	if ctx == nil {
		return errors.New("verify: expected a context, found nil")
	}
	if in.ID == "" {
		return errors.New("verify: expected a non-empty id, found empty")
	}
	if in.Now.IsZero() {
		return errors.New("verify: expected a clock time, found zero")
	}
	if in.Cfg.Cwd == "" {
		return errors.New("verify: expected a directory, found empty")
	}
	if in.Cfg.Base == "" {
		return errors.New("verify: expected a base branch, found empty")
	}
	mode := EffectiveOutputMode(in.HasClaim, in.Claim, in.Cfg)
	if err := config.ValidOutputMode(mode); err != nil {
		return fmt.Errorf("verify: output mode: %w", err)
	}
	if mode == config.OutputModePR && in.Cfg.PRTemplate == "" {
		return errors.New("verify: expected pr_template, found empty")
	}
	return nil
}

func templateBody(in Input) (string, error) {
	if in.ReadFile == nil {
		return "", errors.New("verify: expected a file reader, found nil")
	}
	path := in.Cfg.PRTemplate
	if !filepath.IsAbs(path) {
		path = filepath.Join(in.Cfg.Cwd, path)
	}
	raw, err := in.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("verify: read pr_template: %w", err)
	}
	sections, err := splitSections(string(raw))
	if err != nil {
		return "", fmt.Errorf("verify: pr_template: %w", err)
	}
	if len(sections) == 0 {
		return "", errors.New("verify: pr_template: expected a section heading, found none")
	}
	return string(raw), nil
}

func checkPull(ctx context.Context, in Input, template string, fails *[]string) error {
	switch len(in.PRs) {
	case 0:
		*fails = append(*fails, "pull request: expected one, found none")
		return nil
	case 1:
	default:
		*fails = append(*fails, fmt.Sprintf("pull request: expected one, found %d", len(in.PRs)))
		return nil
	}
	if in.View == nil {
		return errors.New("verify: expected a pull request view, found nil")
	}
	pr := in.PRs[0]
	view, err := in.View(ctx, pr.Repo, pr.Number)
	if err != nil {
		return fmt.Errorf("verify %s: %w", in.ID, err)
	}
	if !view.Draft {
		*fails = append(*fails, "draft: expected a draft, found a ready pull request")
	}
	if len(view.Labels) > 0 {
		*fails = append(*fails, "labels: expected none, found "+strings.Join(view.Labels, ", "))
	}
	if len(view.Reviewers) > 0 {
		*fails = append(*fails, "reviewers: expected none, found "+strings.Join(view.Reviewers, ", "))
	}
	return checkSections(template, view.Body, fails)
}

func checkSections(template, body string, fails *[]string) error {
	want, err := splitSections(template)
	if err != nil {
		return fmt.Errorf("verify: pr_template: %w", err)
	}
	got, err := splitSections(body)
	if err != nil {
		*fails = append(*fails, "body: "+err.Error())
		return nil
	}
	byHeading := map[string]section{}
	for _, part := range got {
		byHeading[part.heading] = part
	}
	for _, part := range want {
		found, ok := byHeading[part.heading]
		if !ok {
			*fails = append(*fails, fmt.Sprintf("section %q: expected the section, found none", part.display))
			continue
		}
		if !hasContent(found.lines) {
			*fails = append(*fails, fmt.Sprintf("section %q: expected content, found none", part.display))
		}
	}
	return nil
}

func checkoutDir(in Input) string {
	if in.HasClaim && in.Claim.State.Active() && in.Claim.Worktree != "" {
		return in.Claim.Worktree
	}
	return in.Cfg.Cwd
}

func checkCheckout(ctx context.Context, in Input, fails *[]string) error {
	if in.Status == nil {
		return errors.New("verify: expected a git status function, found nil")
	}
	out, err := in.Status(ctx, checkoutDir(in))
	if err != nil {
		return fmt.Errorf("verify %s: %w", in.ID, err)
	}
	text := strings.TrimSpace(out)
	if text != "" {
		*fails = append(*fails, "checkout: expected a clean status, found "+text)
	}
	return nil
}

func checkBase(ctx context.Context, in Input, fails *[]string) error {
	if in.Base == nil {
		return errors.New("verify: expected a merge-base function, found nil")
	}
	when, err := in.Base(ctx, checkoutDir(in), in.Cfg.Base)
	if errors.Is(err, ErrNotAncestor) {
		*fails = append(*fails, fmt.Sprintf("base: expected the merge-base to be an ancestor of origin/%s, found it is not", in.Cfg.Base))
		return nil
	}
	if err != nil {
		return fmt.Errorf("verify %s: %w", in.ID, err)
	}
	if in.Now.Sub(when) > BaseMaxAge {
		*fails = append(*fails, fmt.Sprintf("base: expected a merge-base within %s, found %s", BaseMaxAge, when.UTC().Format(time.RFC3339)))
	}
	return nil
}

func checkStash(ctx context.Context, in Input, fails *[]string) error {
	if !in.HasClaim || in.Claim.StashCount == nil {
		*fails = append(*fails, "stash: start did not record a stash count")
		return nil
	}
	if in.Stash == nil {
		return errors.New("verify: expected a stash count function, found nil")
	}
	got, err := in.Stash(ctx, checkoutDir(in))
	if err != nil {
		return fmt.Errorf("verify %s: %w", in.ID, err)
	}
	if got != *in.Claim.StashCount {
		*fails = append(*fails, fmt.Sprintf("stash: expected %d, found %d", *in.Claim.StashCount, got))
	}
	return nil
}

func checkMerge(ctx context.Context, in Input, fails *[]string) error {
	if in.Merge == nil {
		return errors.New("verify: expected a merge check function, found nil")
	}
	got, err := in.Merge(ctx, checkoutDir(in), in.Cfg.Base, in.ID, in.Cfg.PRTitlePattern)
	if err != nil {
		return fmt.Errorf("verify %s: %w", in.ID, err)
	}
	*fails = append(*fails, got...)
	return nil
}
