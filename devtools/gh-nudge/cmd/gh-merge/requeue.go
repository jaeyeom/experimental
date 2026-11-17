package main

import (
	"cmp"
	"fmt"
	"io"
	"regexp"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

// requeueDecision is the RCA gate's decision for the latest removal. Its keys
// are snake_case as specified in issue #894.
//
//nolint:tagliatelle // requeue JSON keys are snake_case by design
type requeueDecision struct {
	Allowed           bool   `json:"allowed"`
	Reason            string `json:"reason"`
	Used              int    `json:"used"`
	Limit             int    `json:"limit"`
	Signature         string `json:"signature"`
	PreviousSignature string `json:"previous_signature"`
}

func (d *requeueDecision) refuse(reason string) *requeueDecision {
	d.Allowed = false
	d.Reason = reason
	return d
}

// sameFailure refuses a flake or infra requeue when the latest removal fails
// the same way as the previous removal that had a flake or infra RCA. It
// fetches evidence only when such a previous removal exists.
func sameFailure(client removalAPI, cfg *config.Config, pr github.QueuePullRequest, latest github.QueueEvent, d *requeueDecision) error {
	previous, found, err := previousFlakeRemoval(cfg.Settings.RCAMarker, pr, latest)
	if err != nil || !found {
		return err
	}
	if d.Signature, err = removalSignature(client, cfg, pr, latest); err != nil {
		return err
	}
	if d.PreviousSignature, err = removalSignature(client, cfg, pr, previous); err != nil {
		return err
	}
	if d.Signature != "" && d.Signature == d.PreviousSignature {
		d.refuse(fmt.Sprintf("same failure as the previous ejection (%s); likely our-code", d.Signature))
	}
	return nil
}

// previousFlakeRemoval returns the latest removal before latest whose RCA
// class is flake or infra.
func previousFlakeRemoval(marker string, pr github.QueuePullRequest, latest github.QueueEvent) (github.QueueEvent, bool, error) {
	sorted := append([]github.QueueEvent(nil), pr.Events...)
	sortQueueEvents(sorted)
	for i := len(sorted) - 1; i >= 0; i-- {
		event := sorted[i]
		if event.Kind != github.QueueEventRemoved || event.ID == latest.ID || !event.CreatedAt.Before(latest.CreatedAt) {
			continue
		}
		class, matched, err := rcaClass(marker, event.ID, pr.Comments)
		if err != nil {
			return github.QueueEvent{}, false, err
		}
		if matched && (class == "flake" || class == "infra") {
			return event, true, nil
		}
	}
	return github.QueueEvent{}, false, nil
}

// removalSignature loads the evidence for event, with failing tests, and
// returns its failure signature. A removal without a commit has none.
func removalSignature(client removalAPI, cfg *config.Config, pr github.QueuePullRequest, event github.QueueEvent) (string, error) {
	if event.CommitSHA == "" {
		return "", nil
	}
	fetch := func() (github.FailureEvidence, error) {
		return client.FailureEvidence(pr.Ref.Owner, pr.Ref.Repo, event.CommitSHA)
	}
	found, err := evidenceOut{cfg: cfg, w: io.Discard, details: true}.event(pr, event, fetch)
	if err != nil {
		return "", err
	}
	return failureSignature(cfg, found)
}

// failureSignature names a failure by its job, its first failing test, and
// its first failure line after normalizeFailureLine. Without a failing test or
// a failure line it is empty, which never matches.
func failureSignature(cfg *config.Config, found eventEvidence) (string, error) {
	evidence, report := found.evidence, found.report
	lines, err := failureLines(cfg, report.Log)
	if err != nil {
		return "", err
	}
	var test, line string
	if len(report.Tests) > 0 {
		test = report.Tests[0]
	}
	if len(lines) > 0 {
		line = normalizeFailureLine(lines[0])
	}
	if test == "" && line == "" {
		return "", nil
	}
	var parts []string
	if job := cmp.Or(report.JobName, evidence.JobName, evidence.Name); job != "" {
		parts = append(parts, "job="+job)
	}
	if test != "" {
		parts = append(parts, "test="+test)
	}
	if line != "" {
		parts = append(parts, "line="+line)
	}
	return strings.Join(parts, "; "), nil
}

var (
	timestampPattern = regexp.MustCompile(`\d{4}-\d{2}-\d{2}(?:[T ]\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|[+-]\d{2}:?\d{2})?)?|\b\d{1,2}:\d{2}:\d{2}(?:\.\d+)?\b`)
	// pathPattern matches an absolute, relative, or home path that starts a
	// word. A Bazel target such as //pkg:target and a URL are not paths.
	pathPattern  = regexp.MustCompile(`(^|[\s(\[='"])(?:~|\.{1,2})?/[^/\s:'"()\[\]][^\s:'"()\[\]]*`)
	hexPattern   = regexp.MustCompile(`\b(?:0[xX][0-9a-fA-F]+|[0-9a-fA-F]{6,})\b`)
	digitPattern = regexp.MustCompile(`\d+`)
	hexDigit     = regexp.MustCompile(`\d`)
	hexLetter    = regexp.MustCompile(`[a-fA-F]`)
)

// normalizeFailureLine masks what changes between runs of the same failure:
// timestamps, paths, hex identifiers, and digits. It also collapses spaces.
func normalizeFailureLine(line string) string {
	line = timestampPattern.ReplaceAllString(line, "<ts>")
	line = pathPattern.ReplaceAllString(line, "${1}<path>")
	line = hexPattern.ReplaceAllStringFunc(line, func(word string) string {
		if strings.HasPrefix(strings.ToLower(word), "0x") || (hexDigit.MatchString(word) && hexLetter.MatchString(word)) {
			return "<hex>"
		}
		return word
	})
	line = digitPattern.ReplaceAllString(line, "<n>")
	return strings.Join(strings.Fields(line), " ")
}
