package main

import (
	"cmp"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/rcamarker"
)

var errRCAUsage = errors.New("rca usage")

type rcaAPI interface {
	LoadPullRequest(ref string) (github.QueuePullRequest, error)
	CommentOnPullRequest(owner, repo string, number int, body string) error
	removalAPI
}

func rcaCommandHelp() string {
	return "Print the failing step's log tail for the latest removal, then post an RCA comment (owner/repo#n, a URL, or a bare number)"
}

func rcaUsage() string {
	return "rca <owner/repo#n|url|number> --class our-code|flake|infra --file body.md"
}

func postRCA(client rcaAPI, cfg *config.Config, args []string) error {
	parsed, err := parseRCAArgs(args)
	if err != nil {
		return err
	}
	if !validRCAClass(parsed.class) {
		return errors.New("rca class must be our-code, flake, or infra")
	}
	fileBody, err := os.ReadFile(parsed.file)
	if err != nil {
		return fmt.Errorf("read rca file: %w", err)
	}
	if strings.TrimSpace(string(fileBody)) == "" {
		return errors.New("rca file is empty")
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
	if cfg.Settings.RCAMarker == "" {
		return errors.New("rca_marker is empty")
	}
	event, ok := latestRemoval(pr.Events)
	if !ok {
		return errors.New("pull request has no removal event")
	}
	found, err := rcaEvidence(client, cfg, pr, event)
	if err != nil {
		return err
	}
	marker := rcamarker.Render(cfg.Settings.RCAMarker, event.ID, parsed.class, rcaKeys(event, found)...)
	body := rcaComment(string(fileBody), marker)
	if err := client.CommentOnPullRequest(pr.Ref.Owner, pr.Ref.Repo, pr.Ref.Number, body); err != nil {
		return fmt.Errorf("comment on %s: %w", pr.Ref.Raw, err)
	}
	return printRCAResult(pr.Ref.Raw, cfg)
}

type rcaJSONResult struct {
	PullRequest  string `json:"pullRequest"`
	Config       string `json:"config"`
	SettingsFrom string `json:"settingsFrom"`
}

func printRCAResult(ref string, cfg *config.Config) error {
	if !jsonOutput {
		fmt.Printf("Commented on %s.\n", ref)
		return nil
	}
	encoded, err := json.MarshalIndent(rcaJSONResult{
		PullRequest:  ref,
		Config:       cfg.Path,
		SettingsFrom: settingsSource(cfg),
	}, "", "  ")
	if err != nil {
		return fmt.Errorf("encode rca result: %w", err)
	}
	fmt.Printf("%s\n", encoded)
	return nil
}

type parsedRCA struct {
	ref   string
	class string
	file  string
}

func parseRCAArgs(args []string) (parsedRCA, error) {
	var parsed parsedRCA
	var refs []string
	for i := 0; i < len(args); i++ {
		arg := args[i]
		switch {
		case arg == "--class" || arg == "--file":
			if i+1 >= len(args) {
				return parsedRCA{}, errRCAUsage
			}
			i++
			if arg == "--class" {
				parsed.class = args[i]
			} else {
				parsed.file = args[i]
			}
		case strings.HasPrefix(arg, "--class="):
			parsed.class = strings.TrimPrefix(arg, "--class=")
		case strings.HasPrefix(arg, "--file="):
			parsed.file = strings.TrimPrefix(arg, "--file=")
		case strings.HasPrefix(arg, "-"):
			return parsedRCA{}, errRCAUsage
		default:
			refs = append(refs, arg)
		}
	}
	if len(refs) != 1 {
		return parsedRCA{}, errRCAUsage
	}
	parsed.ref = refs[0]
	return parsed, nil
}

func validRCAClass(class string) bool {
	return class == "our-code" || class == "flake" || class == "infra"
}

func latestRemoval(events []github.QueueEvent) (github.QueueEvent, bool) {
	sorted := append([]github.QueueEvent(nil), events...)
	sortQueueEvents(sorted)
	var found github.QueueEvent
	ok := false
	for _, event := range sorted {
		if event.Kind != github.QueueEventRemoved {
			continue
		}
		found = event
		ok = true
	}
	return found, ok
}

// rcaEvidence loads the evidence for event, with failing tests. It prints
// the evidence unless the output is JSON.
func rcaEvidence(client removalAPI, cfg *config.Config, pr github.QueuePullRequest, event github.QueueEvent) (eventEvidence, error) {
	fetch := func() (github.FailureEvidence, error) {
		return client.FailureEvidence(pr.Ref.Owner, pr.Ref.Repo, event.CommitSHA)
	}
	if jsonOutput {
		return evidenceOut{cfg: cfg, w: io.Discard, details: true}.event(pr, event, fetch)
	}
	return printEventEvidence(client, evidenceOut{cfg: cfg, w: os.Stdout, details: true}, pr, event, fetch)
}

// rcaKeys are the marker keys the evidence has: the merge-group commit, the
// failed job, and the first failing test. It never adds tool=.
func rcaKeys(event github.QueueEvent, found eventEvidence) []rcamarker.Key {
	evidence, report := found.evidence, found.report
	keys := []rcamarker.Key{
		{Name: rcamarker.KeyCommit, Value: event.CommitSHA},
		{Name: rcamarker.KeyJob, Value: cmp.Or(report.JobName, evidence.JobName, report.JobID, evidence.JobID)},
	}
	if len(report.Tests) > 0 {
		keys = append(keys, rcamarker.Key{Name: rcamarker.KeyTest, Value: report.Tests[0]})
	}
	return keys
}

// rcaContext is an RCA marker another tool wrote. It is shown as context and
// never counted by the requeue gate.
type rcaContext struct {
	Tool       string `json:"tool"`
	Class      string `json:"class"`
	Commit     string `json:"commit,omitempty"`
	Job        string `json:"job,omitempty"`
	Test       string `json:"test,omitempty"`
	Supersedes string `json:"supersedes,omitempty"`
}

// otherRCAs returns the markers other tools wrote for eventID, oldest first.
func otherRCAs(marker, eventID string, comments []string) ([]rcaContext, error) {
	others, err := rcamarker.Others(marker, eventID, comments)
	if err != nil {
		return nil, fmt.Errorf("rca_marker: %w", err)
	}
	out := []rcaContext{}
	for _, m := range others {
		out = append(out, rcaContext{
			Tool:       m.Tool(),
			Class:      m.Class,
			Commit:     m.Keys[rcamarker.KeyCommit],
			Job:        m.Keys[rcamarker.KeyJob],
			Test:       m.Keys[rcamarker.KeyTest],
			Supersedes: m.Keys[rcamarker.KeySupersedes],
		})
	}
	return out, nil
}

func printOtherRCAs(w io.Writer, others []rcaContext) {
	for _, other := range others {
		fmt.Fprintf(w, "RCA from %s: %s (context, not counted)\n", other.Tool, other.Class)
	}
}

func rcaComment(fileBody, markerLine string) string {
	return strings.TrimRight(fileBody, "\n") + "\n" + markerLine
}
