package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
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
	if !jsonOutput {
		if err := printRemovalEvidence(client, cfg, pr); err != nil {
			return err
		}
	}
	body := rcaComment(string(fileBody), rcaMarkerLine(cfg.Settings.RCAMarker, event.ID, parsed.class))
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

func rcaMarkerLine(marker, eventID, class string) string {
	line := strings.ReplaceAll(marker, "{event_id}", eventID)
	return strings.ReplaceAll(line, "{class}", class)
}

func rcaComment(fileBody, markerLine string) string {
	return strings.TrimRight(fileBody, "\n") + "\n" + markerLine
}
