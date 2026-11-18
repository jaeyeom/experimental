package approve

import (
	"fmt"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

// loadStat is how one kind of load went for one pull request.
type loadStat struct {
	// used is true when the pull request asked for the data.
	used bool
	// calls counts the reads that went to GitHub, or ran a command.
	calls int
	// d is the time those reads took.
	d time.Duration
	// note says why an unused load was skipped.
	note string
}

// String is "skipped", "cache hit" or the time spent.
func (s loadStat) String() string {
	switch {
	case !s.used && s.note != "":
		return "skipped (" + s.note + ")"
	case !s.used:
		return "skipped"
	case s.calls == 0:
		return "cache hit"
	default:
		return s.d.String()
	}
}

// prState is what one pull request has loaded so far and how long that took.
// Each load happens at most once per pull request.
type prState struct {
	start       time.Time
	pullRequest time.Duration
	codeowners  loadStat
	teams       loadStat
	patches     loadStat
	agent       loadStat

	ownership     *github.Ownership
	ownershipDone bool
	patchFiles    []github.PullFile
	patchErr      string
	patchesDone   bool
}

// state returns the state of the pull request being decided.
func (r *Runner) state() *prState {
	if r.cur == nil {
		r.cur = &prState{start: r.now()}
	}
	return r.cur
}

// now is the injectable clock.
func (r *Runner) now() time.Time {
	if r.Now == nil {
		return time.Now()
	}
	return r.Now()
}

// measure runs call and adds its duration to stat as one read.
func (r *Runner) measure(stat *loadStat, call func()) {
	start := r.now()
	call()
	stat.d += r.now().Sub(start)
	stat.calls++
	stat.used = true
}

// ownershipFor loads the pull request's ownership the first time and returns
// it after that.
func (r *Runner) ownershipFor(pr github.ApprovalPullRequest, viewer string) *github.Ownership {
	st := r.state()
	if !st.ownershipDone {
		st.ownership = r.loadOwnership(pr, viewer)
		st.ownershipDone = true
	}
	return st.ownership
}

// patchCapReason returns why pr is too large to fetch patches for, or "".
func (r *Runner) patchCapReason(pr github.ApprovalPullRequest) string {
	maxFiles, maxAdded := r.Config.PatchMaxFiles, r.Config.PatchMaxAddedLines
	if maxFiles <= 0 {
		maxFiles = config.DefaultPatchMaxFiles
	}
	if maxAdded <= 0 {
		maxAdded = config.DefaultPatchMaxAddedLines
	}
	switch {
	case pr.ChangedFiles > maxFiles:
		return fmt.Sprintf("too large to check: %d files (patch_max_files %d)", pr.ChangedFiles, maxFiles)
	case pr.Additions > maxAdded:
		return fmt.Sprintf("too large to check: %d added lines (patch_max_added_lines %d)", pr.Additions, maxAdded)
	}
	return ""
}

// patchesFor fetches the pull request's REST patches the first time. A pull
// request over the size cap fetches none and reports why. A transport error
// is returned as the reason too. The conditions report a missing patch
// themselves.
func (r *Runner) patchesFor(pr github.ApprovalPullRequest) ([]github.PullFile, string) {
	st := r.state()
	if st.patchesDone {
		return st.patchFiles, st.patchErr
	}
	st.patchesDone = true
	if reason := r.patchCapReason(pr); reason != "" {
		st.patchErr = reason
		st.patches.note = "too large"
		return nil, reason
	}
	r.measure(&st.patches, func() {
		files, err := r.Client.PullRequestFilePatches(pr.Ref)
		if err != nil {
			st.patchErr = err.Error()
			return
		}
		st.patchFiles = files
	})
	return st.patchFiles, st.patchErr
}

// loads returns the loaders the rules of pr use for the data only some
// conditions need.
func (r *Runner) loads(pr github.ApprovalPullRequest, viewer string) *lazyLoads {
	return &lazyLoads{
		ownership: func() *github.Ownership { return r.ownershipFor(pr, viewer) },
		patches:   func() ([]github.PullFile, string) { return r.patchesFor(pr) },
	}
}

// timedAgent wraps agent to time its runs. A run answered from the decision
// cache counts as a cache hit.
func (r *Runner) timedAgent(agent AgentFunc) AgentFunc {
	if agent == nil {
		return nil
	}
	stat := &r.state().agent
	return func(rule config.ApproveRule) AgentResult {
		start := r.now()
		got := agent(rule)
		stat.used = true
		if got.Cache != CacheHit {
			stat.calls++
			stat.d += r.now().Sub(start)
		}
		return got
	}
}

// timingLine is the --explain line with the time of the whole pull request
// and of each load.
func (r *Runner) timingLine() string {
	st := r.state()
	parts := []string{
		"total " + r.now().Sub(st.start).String(),
		"pull request " + st.pullRequest.String(),
		"CODEOWNERS " + st.codeowners.String(),
		"teams " + st.teams.String(),
		"patches " + st.patches.String(),
		"agent command " + st.agent.String(),
	}
	return "timing: " + strings.Join(parts, "; ")
}
