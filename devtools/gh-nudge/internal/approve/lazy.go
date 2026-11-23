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
	end         time.Time
	pullRequest time.Duration
	batchIndex  int
	batchCount  int
	codeowners  loadStat
	teams       loadStat
	patches     loadStat
	agent       loadStat

	cacheStatus         string
	cacheDirty          bool
	hasCachedOwnership  bool
	cachedOwnership     *github.Ownership
	cachedCodeownersSHA string
	cachedTeamsHash     string
	loadedCodeownersSHA string
	loadedTeamsHash     string

	ownership     *github.Ownership
	ownershipDone bool
	patchFiles    []github.PullFile
	patchErr      string
	patchesDone   bool
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
func (r *Runner) ownershipFor(pr github.ApprovalPullRequest, viewer string, st *prState, pool *loadPool) *github.Ownership {
	if st.ownershipDone {
		return st.ownership
	}
	if st.hasCachedOwnership && st.cachedOwnership != nil {
		_, blobSHA, err := r.codeownersOf(pr, st, pool)
		if err == nil {
			if blobSHA == st.cachedCodeownersSHA {
				set := r.viewerTeamsOf(pr.Ref.Owner, viewer, st, pool)
				if set.err == nil && set.hash == st.cachedTeamsHash {
					st.ownership = st.cachedOwnership
					st.ownershipDone = true
					st.loadedCodeownersSHA = blobSHA
					st.loadedTeamsHash = set.hash
					st.codeowners.used = true
					st.codeowners.calls = 0
					st.teams.used = true
					st.teams.calls = 0
					return st.ownership
				}
				if set.err == nil && set.hash != st.cachedTeamsHash {
					st.cacheStatus = "miss (teams changed)"
				}
			} else {
				st.cacheStatus = "miss (CODEOWNERS changed)"
			}
		}
	}
	st.ownership = r.loadOwnership(pr, viewer, st, pool)
	st.ownershipDone = true
	st.cacheDirty = true
	return st.ownership
}

// tooLarge starts the reason of a pull request over the patch size cap.
const tooLarge = "too large to check"

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
		return fmt.Sprintf(tooLarge+": %d files (patch_max_files %d)", pr.ChangedFiles, maxFiles)
	case pr.Additions > maxAdded:
		return fmt.Sprintf(tooLarge+": %d added lines (patch_max_added_lines %d)", pr.Additions, maxAdded)
	}
	return ""
}

// patchesFor fetches the pull request's REST patches the first time. A pull
// request over the size cap fetches none and reports why. A transport error
// is returned as the reason too. The conditions report a missing patch
// themselves.
func (r *Runner) patchesFor(pr github.ApprovalPullRequest, st *prState, pool *loadPool) ([]github.PullFile, string) {
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
		var files []github.PullFile
		err := pool.callWithRetry(func() error {
			var callErr error
			files, callErr = r.Client.PullRequestFilePatches(pr.Ref)
			return callErr //nolint:wrapcheck // caller records patch error string
		})
		if err != nil {
			st.patchErr = err.Error()
			return
		}
		st.patchFiles = files
		st.cacheDirty = true
	})
	return st.patchFiles, st.patchErr
}

// loads returns the loaders the rules of pr use for the data only some
// conditions need.
func (r *Runner) loads(pr github.ApprovalPullRequest, viewer string, st *prState, pool *loadPool) *lazyLoads {
	return &lazyLoads{
		ownership: func() *github.Ownership { return r.ownershipFor(pr, viewer, st, pool) },
		patches:   func() ([]github.PullFile, string) { return r.patchesFor(pr, st, pool) },
		generated: func() ([]string, string) { return r.generatedListFor(pr, pool) },
	}
}

// timedAgent wraps agent to time its runs. A run answered from the decision
// cache counts as a cache hit.
func (r *Runner) timedAgent(agent AgentFunc, st *prState) AgentFunc {
	if agent == nil {
		return nil
	}
	stat := &st.agent
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
func (r *Runner) timingLine(st *prState) string {
	end := st.end
	if end.IsZero() {
		end = r.now()
	}
	prTiming := "pull request " + st.pullRequest.String()
	if st.batchIndex > 0 {
		prTiming += fmt.Sprintf(" (batch %d/%d)", st.batchIndex, st.batchCount)
	}
	parts := []string{
		"total " + end.Sub(st.start).String(),
		prTiming,
		"CODEOWNERS " + st.codeowners.String(),
		"teams " + st.teams.String(),
		"patches " + st.patches.String(),
		"agent command " + st.agent.String(),
	}
	if st.cacheStatus != "" {
		parts = append(parts, "cache: "+st.cacheStatus)
	}
	return "timing: " + strings.Join(parts, "; ")
}
