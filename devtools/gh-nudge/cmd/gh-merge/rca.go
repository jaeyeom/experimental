package main

import (
	"cmp"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/rcamarker"
)

var errRCAUsage = errors.New("rca usage")

type rcaAPI interface {
	LoadPullRequest(ref string) (github.QueuePullRequest, error)
	CommentOnPullRequest(owner, repo string, number int, body string) error
	EditPullRequestComment(owner, repo string, id int64, body string) error
	removalAPI
}

func rcaCommandHelp() string {
	return "Print the failing step's log tail for the latest removal, then post an RCA comment (owner/repo#n, a URL, or a bare number); --check reports the existing RCA"
}

func rcaUsage() string {
	return "rca <owner/repo#n|url|number> --class our-code|flake|infra --file body.md [--update|--supersedes]\n" +
		"       rca --check <owner/repo#n|url|number> [--event <id>] [--json]"
}

func postRCA(client rcaAPI, cfg *config.Config, args []string) error {
	parsed, err := parseRCAArgs(args)
	if err != nil {
		return err
	}
	if parsed.check {
		return checkRCA(client, cfg, parsed)
	}
	fileBody, err := readRCAFile(parsed)
	if err != nil {
		return err
	}
	pr, cfg, err := loadRCAPullRequest(client, cfg, parsed.ref)
	if err != nil {
		return err
	}
	event, ok := latestRemoval(pr.Events)
	if !ok {
		return errors.New("pull request has no removal event")
	}
	existing, err := ourRCAs(cfg.Settings.RCAMarker, event.ID, pr)
	if err != nil {
		return err
	}
	if err := allowRCA(cfg.Settings.RCAMarker, event.ID, pr, parsed, existing); err != nil {
		return err
	}
	found, err := rcaEvidence(client, cfg, pr, event)
	if err != nil {
		return err
	}
	keys := append(rcaKeys(event, found), correctionKeys(parsed, existing)...)
	marker := rcamarker.Render(cfg.Settings.RCAMarker, event.ID, parsed.class, keys...)
	body := rcaComment(fileBody, marker)
	if parsed.update {
		last := existing[len(existing)-1]
		if err := client.EditPullRequestComment(pr.Ref.Owner, pr.Ref.Repo, last.id, body); err != nil {
			return fmt.Errorf("update rca comment on %s: %w", pr.Ref.Raw, err)
		}
		return printRCAResult(pr.Ref.Raw, cfg, "Updated the RCA comment on")
	}
	if err := client.CommentOnPullRequest(pr.Ref.Owner, pr.Ref.Repo, pr.Ref.Number, body); err != nil {
		return fmt.Errorf("comment on %s: %w", pr.Ref.Raw, err)
	}
	return printRCAResult(pr.Ref.Raw, cfg, "Commented on")
}

func readRCAFile(parsed parsedRCA) (string, error) {
	if !validRCAClass(parsed.class) {
		return "", errors.New("rca class must be our-code, flake, or infra")
	}
	fileBody, err := os.ReadFile(parsed.file)
	if err != nil {
		return "", fmt.Errorf("read rca file: %w", err)
	}
	if strings.TrimSpace(string(fileBody)) == "" {
		return "", errors.New("rca file is empty")
	}
	return string(fileBody), nil
}

// correctionKeys is the supersedes= key of a correction. --supersedes names
// our latest RCA comment; --update keeps the key the edited marker has.
func correctionKeys(parsed parsedRCA, existing []markedComment) []rcamarker.Key {
	if !parsed.update && !parsed.supersedes {
		return nil
	}
	last := existing[len(existing)-1]
	supersedes := last.marker.Keys[rcamarker.KeySupersedes]
	if parsed.supersedes {
		supersedes = strconv.FormatInt(last.id, 10)
	}
	return []rcamarker.Key{{Name: rcamarker.KeySupersedes, Value: supersedes}}
}

func loadRCAPullRequest(client rcaAPI, cfg *config.Config, rawRef string) (github.QueuePullRequest, *config.Config, error) {
	ref, err := resolvePullRef(rawRef)
	if err != nil {
		return github.QueuePullRequest{}, nil, err
	}
	pr, err := client.LoadPullRequest(ref)
	if err != nil {
		return github.QueuePullRequest{}, nil, fmt.Errorf("load pull request %s: %w", ref, err)
	}
	cfg = cfg.ForRepository(pr.Ref.Owner, pr.Ref.Repo)
	if cfg.Settings.RCAMarker == "" {
		return github.QueuePullRequest{}, nil, errors.New("rca_marker is empty")
	}
	return pr, cfg, nil
}

// allowRCA refuses a second RCA for the event unless it corrects the first
// one, and refuses a correction when there is nothing to correct.
func allowRCA(marker, eventID string, pr github.QueuePullRequest, parsed parsedRCA, existing []markedComment) error {
	if parsed.update || parsed.supersedes {
		if len(existing) == 0 {
			return fmt.Errorf("removal %s has no RCA comment to correct; post one without --update or --supersedes", eventID)
		}
		if existing[len(existing)-1].id == 0 {
			return fmt.Errorf("removal %s: the RCA comment id is unknown", eventID)
		}
		return nil
	}
	if len(existing) == 0 {
		return nil
	}
	class, _, err := rcaClass(marker, eventID, pr.Comments)
	if err != nil {
		return err
	}
	return fmt.Errorf("removal %s already has an RCA (%s); use --update to edit it or --supersedes to post a correction", eventID, class)
}

// markedComment is a pull request comment that carries a gh-merge marker.
type markedComment struct {
	id     int64
	marker rcamarker.Marker
}

// ourRCAs returns the comments with a gh-merge marker for eventID, oldest
// first. Markers from other tools are left out.
func ourRCAs(marker, eventID string, pr github.QueuePullRequest) ([]markedComment, error) {
	var out []markedComment
	for i, comment := range pr.Comments {
		found, err := rcamarker.Find(marker, eventID, []string{comment})
		if err != nil {
			return nil, fmt.Errorf("rca_marker: %w", err)
		}
		if len(found) == 0 || !found[0].Ours() {
			continue
		}
		var id int64
		if i < len(pr.CommentIDs) {
			id = pr.CommentIDs[i]
		}
		out = append(out, markedComment{id: id, marker: found[0]})
	}
	return out, nil
}

// rcaCheckJSON is the gh-merge rca --check --json object.
type rcaCheckJSON struct {
	PullRequest string       `json:"pullRequest"`
	EventID     string       `json:"eventId"`
	Exists      bool         `json:"exists"`
	Class       string       `json:"class,omitempty"`
	Superseded  bool         `json:"superseded"`
	RCAContext  []rcaContext `json:"rcaContext"`
}

// checkRCA prints whether the removal has a gh-merge RCA, its class, and
// whether a correction superseded an earlier one. It never posts.
func checkRCA(client rcaAPI, cfg *config.Config, parsed parsedRCA) error {
	pr, cfg, err := loadRCAPullRequest(client, cfg, parsed.ref)
	if err != nil {
		return err
	}
	event, err := removalEvent(pr.Events, parsed.event)
	if err != nil {
		return err
	}
	marker := cfg.Settings.RCAMarker
	result := rcaCheckJSON{PullRequest: pr.Ref.Raw, EventID: event.ID}
	result.Class, result.Exists, err = rcaClass(marker, event.ID, pr.Comments)
	if err != nil {
		return err
	}
	ours, err := ourRCAs(marker, event.ID, pr)
	if err != nil {
		return err
	}
	for _, c := range ours {
		if _, ok := c.marker.Keys[rcamarker.KeySupersedes]; ok {
			result.Superseded = true
		}
	}
	if result.RCAContext, err = otherRCAs(marker, event.ID, pr.Comments); err != nil {
		return err
	}
	if parsed.asJSON {
		encoded, err := json.MarshalIndent(result, "", "  ")
		if err != nil {
			return fmt.Errorf("encode rca check: %w", err)
		}
		fmt.Printf("%s\n", encoded)
		return nil
	}
	status := cmp.Or(result.Class, "none")
	if result.Superseded {
		status += " (superseded)"
	}
	fmt.Printf("RCA for %s: %s\n", event.ID, status)
	printOtherRCAs(os.Stdout, result.RCAContext)
	return nil
}

type rcaJSONResult struct {
	PullRequest  string `json:"pullRequest"`
	Config       string `json:"config"`
	SettingsFrom string `json:"settingsFrom"`
}

func printRCAResult(ref string, cfg *config.Config, done string) error {
	if !jsonOutput {
		fmt.Printf("%s %s.\n", done, ref)
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
	ref        string
	class      string
	file       string
	update     bool
	supersedes bool
	check      bool
	event      string
	asJSON     bool
}

func parseRCAArgs(args []string) (parsedRCA, error) {
	parsed := parsedRCA{asJSON: jsonOutput}
	var refs []string
	for i := 0; i < len(args); i++ {
		arg := args[i]
		switch {
		case arg == "--class" || arg == "--file" || arg == "--event":
			if i+1 >= len(args) {
				return parsedRCA{}, errRCAUsage
			}
			i++
			parsed.setValue(arg, args[i])
		case strings.HasPrefix(arg, "--class=") || strings.HasPrefix(arg, "--file=") || strings.HasPrefix(arg, "--event="):
			name, value, _ := strings.Cut(arg, "=")
			parsed.setValue(name, value)
		case parsed.setSwitch(arg):
		case strings.HasPrefix(arg, "-"):
			return parsedRCA{}, errRCAUsage
		default:
			refs = append(refs, arg)
		}
	}
	if len(refs) != 1 || !parsed.consistent() {
		return parsedRCA{}, errRCAUsage
	}
	parsed.ref = refs[0]
	return parsed, nil
}

// setSwitch sets the boolean flag arg names and reports whether it was one.
func (p *parsedRCA) setSwitch(arg string) bool {
	switch arg {
	case "--update":
		p.update = true
	case "--supersedes":
		p.supersedes = true
	case "--check":
		p.check = true
	case "--json":
		p.asJSON = true
	default:
		return false
	}
	return true
}

func (p *parsedRCA) setValue(flagName, value string) {
	switch flagName {
	case "--class":
		p.class = value
	case "--file":
		p.file = value
	case "--event":
		p.event = value
	}
}

// consistent reports whether the flags fit together: --check takes only
// --event and --json, and --update and --supersedes exclude each other.
func (p parsedRCA) consistent() bool {
	if p.check {
		return p.class == "" && p.file == "" && !p.update && !p.supersedes
	}
	return p.event == "" && (!p.update || !p.supersedes)
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
