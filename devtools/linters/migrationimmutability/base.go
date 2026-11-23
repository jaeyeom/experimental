package migrationimmutability

import (
	"fmt"
	"strings"
)

// resolveBase returns the base ref and its merge-base with HEAD. Resolution
// order: explicit base; origin/$GITHUB_BASE_REF; the current branch's
// upstream when it names a different branch; origin/HEAD. It never fetches.
func resolveBase(top, explicit string, getenv func(string) string) (ref, mergeBase string, err error) {
	ref, err = pickBaseRef(top, explicit, getenv)
	if err != nil {
		return "", "", err
	}
	if _, ok, err := gitOK(top, "rev-parse", "--verify", "--quiet", ref+"^{commit}"); err != nil {
		return "", "", err
	} else if !ok {
		return "", "", fmt.Errorf("base ref %s does not exist%s", ref, shallowHint(top))
	}
	mergeBase, ok, err := gitOK(top, "merge-base", ref, "HEAD")
	if err != nil {
		return "", "", err
	}
	if !ok || mergeBase == "" {
		return "", "", fmt.Errorf("HEAD shares no history with %s; pass --base with a ref it does share history with%s",
			ref, shallowHint(top))
	}
	return ref, mergeBase, nil
}

func pickBaseRef(top, explicit string, getenv func(string) string) (string, error) {
	if explicit != "" {
		return explicit, nil
	}
	if b := getenv("GITHUB_BASE_REF"); b != "" {
		return "origin/" + b, nil
	}
	if up, err := differentNameUpstream(top); err != nil || up != "" {
		return up, err
	}
	head, ok, err := gitOK(top, "symbolic-ref", "--quiet", "--short", "refs/remotes/origin/HEAD")
	if err != nil {
		return "", err
	}
	if ok && head != "" {
		return head, nil
	}
	return "", fmt.Errorf("cannot determine the base branch: no GITHUB_BASE_REF, no upstream naming a different branch, "+
		"and no origin/HEAD (try `git remote set-head origin --auto`); pass --base%s", shallowHint(top))
}

// differentNameUpstream returns the current branch's upstream ref when it
// tracks a branch with a different name (feat -> origin/main). A branch
// tracking its own remote copy (feat -> origin/feat) returns "": using it
// would treat pushed but unmerged migrations as already merged.
func differentNameUpstream(top string) (string, error) {
	branch, ok, err := gitOK(top, "symbolic-ref", "--quiet", "--short", "HEAD")
	if err != nil || !ok {
		return "", err
	}
	merge, ok, err := gitOK(top, "config", "--get", "branch."+branch+".merge")
	if err != nil || !ok {
		return "", err
	}
	if strings.TrimPrefix(merge, "refs/heads/") == branch {
		return "", nil
	}
	up, ok, err := gitOK(top, "rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}")
	if err != nil || !ok {
		return "", err
	}
	return up, nil
}

func shallowHint(top string) string {
	out, ok, err := gitOK(top, "rev-parse", "--is-shallow-repository")
	if err != nil || !ok || out != "true" {
		return ""
	}
	return "\nthe repository is a shallow clone; fetch full history (GitHub Actions: actions/checkout with fetch-depth: 0) " +
		"or fetch the base ref"
}
