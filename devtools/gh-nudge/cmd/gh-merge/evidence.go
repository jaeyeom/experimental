package main

import (
	"cmp"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
	"time"
	"unicode/utf8"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

// The JSON excerpt keeps at most maxExcerptLines lines and maxExcerptBytes
// bytes, counting one newline per line.
const (
	maxExcerptLines = 50
	maxExcerptBytes = 8192
)

var errEvidenceUsage = errors.New("evidence usage")

type evidenceAPI interface {
	LoadPullRequest(ref string) (github.QueuePullRequest, error)
	EjectionEvidence(owner, repo, sha string, ejectedAt time.Time) (github.FailureEvidence, error)
	landedFirstAPI
}

func evidenceCommandHelp() string {
	return "Print the failure evidence for a removal from the merge queue, the latest by default (--json for a JSON object)"
}

func evidenceUsage() string {
	return "evidence <owner/repo#n|url|number> [--event <id>] [--json]"
}

// evidenceJSON is the gh-merge evidence --json object. Its keys are
// snake_case as specified in issue #891.
//
//nolint:tagliatelle // evidence JSON keys are snake_case by design
type evidenceJSON struct {
	PullRequest  string           `json:"pull_request"`
	EventID      string           `json:"event_id"`
	Reason       string           `json:"reason"`
	Commit       string           `json:"commit"`
	Check        evidenceCheck    `json:"check"`
	Workflow     evidenceWorkflow `json:"workflow"`
	Job          evidenceJob      `json:"job"`
	FailingTests []string         `json:"failing_tests"`
	Excerpt      evidenceExcerpt  `json:"excerpt"`
	Siblings     []evidenceJob    `json:"siblings"`
	Infra        bool             `json:"infra"`
	FlakeHints   []flakeHint      `json:"flake_hints"`
	RCAContext   []rcaContext     `json:"rca_context"`
}

//nolint:tagliatelle // evidence JSON keys are snake_case by design
type evidenceCheck struct {
	Name        string `json:"name,omitempty"`
	URL         string `json:"url,omitempty"`
	CheckRunURL string `json:"check_run_url,omitempty"`
}

type evidenceWorkflow struct {
	ID   string `json:"id,omitempty"`
	Name string `json:"name,omitempty"`
}

type evidenceJob struct {
	ID      string `json:"id,omitempty"`
	Name    string `json:"name,omitempty"`
	Outcome string `json:"outcome,omitempty"`
	URL     string `json:"url,omitempty"`
}

//nolint:tagliatelle // evidence JSON keys are snake_case by design
type evidenceExcerpt struct {
	Lines     []string `json:"lines"`
	Truncated bool     `json:"truncated"`
	LogURL    string   `json:"log_url,omitempty"`
}

type parsedEvidence struct {
	ref    string
	event  string
	asJSON bool
}

func parseEvidenceArgs(args []string) (parsedEvidence, error) {
	parsed := parsedEvidence{asJSON: jsonOutput}
	var refs []string
	for i := 0; i < len(args); i++ {
		arg := args[i]
		switch {
		case arg == "--json":
			parsed.asJSON = true
		case arg == "--event":
			if i+1 >= len(args) {
				return parsedEvidence{}, errEvidenceUsage
			}
			i++
			parsed.event = args[i]
		case strings.HasPrefix(arg, "--event="):
			parsed.event = strings.TrimPrefix(arg, "--event=")
		case strings.HasPrefix(arg, "-"):
			return parsedEvidence{}, errEvidenceUsage
		default:
			refs = append(refs, arg)
		}
	}
	if len(refs) != 1 {
		return parsedEvidence{}, errEvidenceUsage
	}
	parsed.ref = refs[0]
	return parsed, nil
}

func evidenceCommand(client evidenceAPI, cfg *config.Config, args []string) error {
	parsed, err := parseEvidenceArgs(args)
	if err != nil {
		return err
	}
	ref, err := resolvePullRef(parsed.ref)
	if err != nil {
		return err
	}
	pr, err := client.LoadPullRequest(ref)
	if err != nil {
		return fmt.Errorf("load pull request %s: %w", ref, err)
	}
	cfg = cfg.ForRepository(pr.Ref.Owner, pr.Ref.Repo)
	event, err := removalEvent(pr.Events, parsed.event)
	if err != nil {
		return err
	}
	fetch := func() (github.FailureEvidence, error) {
		return client.EjectionEvidence(pr.Ref.Owner, pr.Ref.Repo, event.CommitSHA, event.CreatedAt)
	}
	others, err := otherRCAs(cfg.Settings.RCAMarker, event.ID, pr.Comments)
	if err != nil {
		return err
	}
	if !parsed.asJSON {
		if _, err := printEventEvidence(client, evidenceOut{cfg: cfg, w: os.Stdout}, pr, event, fetch); err != nil {
			return err
		}
		printOtherRCAs(os.Stdout, others)
		return nil
	}
	found, err := evidenceOut{cfg: cfg, w: io.Discard, details: true}.event(pr, event, fetch)
	if err != nil {
		return err
	}
	result, err := evidenceResult(cfg, pr, event, found)
	if err != nil {
		return err
	}
	result.RCAContext = others
	encoded, err := json.MarshalIndent(result, "", "  ")
	if err != nil {
		return fmt.Errorf("encode evidence: %w", err)
	}
	fmt.Printf("%s\n", encoded)
	return nil
}

// removalEvent returns the removal event with id, or the latest one when id
// is empty.
func removalEvent(events []github.QueueEvent, id string) (github.QueueEvent, error) {
	if id == "" {
		event, ok := latestRemoval(events)
		if !ok {
			return github.QueueEvent{}, errors.New("pull request has no removal event")
		}
		return event, nil
	}
	for _, event := range events {
		if event.ID == id && event.Kind == github.QueueEventRemoved {
			return event, nil
		}
	}
	return github.QueueEvent{}, fmt.Errorf("pull request has no removal event %s", id)
}

func evidenceResult(cfg *config.Config, pr github.QueuePullRequest, event github.QueueEvent, found eventEvidence) (evidenceJSON, error) {
	evidence, report := found.evidence, found.report
	result := evidenceJSON{
		PullRequest: pr.Ref.Raw,
		EventID:     event.ID,
		Reason:      event.Reason,
		Commit:      event.CommitSHA,
		Check: evidenceCheck{
			Name:        evidence.Name,
			URL:         evidence.TargetURL,
			CheckRunURL: evidence.CheckRunURL,
		},
		Workflow: evidenceWorkflow{
			ID:   cmp.Or(report.WorkflowID, evidence.WorkflowID),
			Name: cmp.Or(report.Workflow, evidence.Workflow),
		},
		Job: evidenceJob{
			ID:   cmp.Or(report.JobID, evidence.JobID),
			Name: cmp.Or(report.JobName, evidence.JobName),
			URL:  cmp.Or(report.JobURL, evidence.JobURL),
		},
		FailingTests: nonNil(report.Tests),
		Infra:        report.Infra,
		FlakeHints:   found.hints,
	}
	if result.FlakeHints == nil {
		result.FlakeHints = []flakeHint{}
	}
	lines, err := failureLines(cfg, report.Log)
	if err != nil {
		return evidenceJSON{}, err
	}
	result.Excerpt = boundExcerpt(lines)
	result.Excerpt.LogURL = cmp.Or(result.Job.URL, evidence.TargetURL)
	siblings := evidence.Siblings
	if report.JobID != "" {
		siblings = report.Siblings
	}
	result.Siblings = liveSiblings(siblings, event.CreatedAt)
	return result, nil
}

// liveSiblings drops jobs cancelled after the ejection. Ejecting a merge
// group cancels its remaining jobs, which say nothing about the failure.
func liveSiblings(jobs []github.JobOutcome, ejectedAt time.Time) []evidenceJob {
	out := []evidenceJob{}
	for _, job := range jobs {
		outcome := strings.ToLower(job.Outcome)
		cancelled := outcome == "canceled" || outcome == "cancelled"
		if cancelled && !job.CompletedAt.IsZero() && job.CompletedAt.After(ejectedAt) {
			continue
		}
		out = append(out, evidenceJob{ID: job.ID, Name: job.Name, Outcome: job.Outcome, URL: job.URL})
	}
	return out
}

// boundExcerpt keeps the first lines within maxExcerptLines and
// maxExcerptBytes. A first line longer than the byte limit is cut.
func boundExcerpt(lines []string) evidenceExcerpt {
	excerpt := evidenceExcerpt{Lines: []string{}}
	size := 0
	for _, line := range lines {
		if len(excerpt.Lines) == maxExcerptLines {
			excerpt.Truncated = true
			break
		}
		if size+len(line)+1 > maxExcerptBytes {
			excerpt.Truncated = true
			if len(excerpt.Lines) == 0 {
				excerpt.Lines = append(excerpt.Lines, cutUTF8(line, maxExcerptBytes-1))
			}
			break
		}
		excerpt.Lines = append(excerpt.Lines, line)
		size += len(line) + 1
	}
	return excerpt
}

// cutUTF8 returns the longest prefix of s within n bytes that does not split
// a rune.
func cutUTF8(s string, n int) string {
	if len(s) <= n {
		return s
	}
	for n > 0 && !utf8.RuneStart(s[n]) {
		n--
	}
	return s[:n]
}

func nonNil(items []string) []string {
	if items == nil {
		return []string{}
	}
	return items
}
