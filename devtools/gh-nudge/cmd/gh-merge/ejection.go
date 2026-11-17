package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"log/slog"
	"os"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

var errEjectionUsage = errors.New("handle-ejection usage")

// gateRefusedExit is the handle-ejection exit code when the queue preflight
// or the RCA gate refuses the requeue.
const gateRefusedExit = 2

type ejectionAPI interface {
	queueAPI
	rcaAPI
}

func handleEjectionCommandHelp() string {
	return "After an ejection: post the RCA if the removal has none, then requeue once if the gate allows it. Idempotent per removal; exits 2 when the gate refuses"
}

func handleEjectionUsage() string {
	return "handle-ejection <owner/repo#n|url|number> --class our-code|flake|infra|manual --file body.md [--event <id>] [--when-green] [--dry-run] [--json]"
}

// ejectionJSON is the gh-merge handle-ejection --json object.
type ejectionJSON struct {
	PullRequest string         `json:"pullRequest"`
	EventID     string         `json:"eventId"`
	Class       string         `json:"class"`
	DryRun      bool           `json:"dryRun"`
	Steps       []ejectionStep `json:"steps"`
}

// ejectionStep is one handle-ejection step: event, rca, or queue. Queue is
// the queue JSON result when the queue step wrote one.
type ejectionStep struct {
	Name   string           `json:"name"`
	Action string           `json:"action"`
	Detail string           `json:"detail,omitempty"`
	Queue  *queueJSONResult `json:"queue,omitempty"`
}

func (r *ejectionJSON) add(step ejectionStep) {
	r.Steps = append(r.Steps, step)
	if !jsonOutput && step.Detail != "" {
		fmt.Printf("%s: %s\n", step.Name, step.Detail)
	}
}

type parsedEjection struct {
	ref       string
	class     string
	file      string
	event     string
	whenGreen bool
	dryRun    bool
	asJSON    bool
}

// handleEjectionCommand runs handleEjection and exits with usage on a usage
// error, or with gateRefusedExit when the gate refused.
func handleEjectionCommand(client ejectionAPI, cfg *config.Config, args []string) error {
	err := handleEjection(client, cfg, args)
	exitOnUsage(err, errEjectionUsage, handleEjectionUsage())
	if errors.Is(err, errQueueRefused) {
		slog.Error("Requeue refused", "command", "handle-ejection", "error", err)
		os.Exit(gateRefusedExit)
	}
	return err
}

// handleEjection resolves the removal, posts the RCA when the removal has
// none, and requeues once when the gate allows it.
func handleEjection(client ejectionAPI, cfg *config.Config, args []string) error {
	parsed, err := parseEjectionArgs(args)
	if err != nil {
		return err
	}
	fileBody, err := readRCAFile(parsedRCA{class: parsed.class, file: parsed.file})
	if err != nil {
		return err
	}
	prevDry, prevGreen, prevJSON := dryRun, whenGreen, jsonOutput
	dryRun, whenGreen, jsonOutput = parsed.dryRun, parsed.whenGreen, parsed.asJSON
	defer func() { dryRun, whenGreen, jsonOutput = prevDry, prevGreen, prevJSON }()

	report := &ejectionJSON{Class: parsed.class, DryRun: parsed.dryRun, Steps: []ejectionStep{}}
	if !parsed.asJSON {
		return runEjection(client, cfg, parsed, fileBody, report)
	}
	// Step output goes to stderr so stdout holds only the JSON object.
	stdout := os.Stdout
	os.Stdout = os.Stderr
	err = runEjection(client, cfg, parsed, fileBody, report)
	os.Stdout = stdout
	encoded, encodeErr := json.MarshalIndent(report, "", "  ")
	if encodeErr != nil {
		return fmt.Errorf("encode handle-ejection result: %w", encodeErr)
	}
	fmt.Printf("%s\n", encoded)
	return err
}

func runEjection(client ejectionAPI, cfg *config.Config, parsed parsedEjection, fileBody string, report *ejectionJSON) error {
	pr, cfg, err := loadRCAPullRequest(client, cfg, parsed.ref)
	if err != nil {
		return err
	}
	report.PullRequest = pr.Ref.Raw
	event, err := removalEvent(pr.Events, parsed.event)
	if err != nil {
		return err
	}
	report.EventID = event.ID
	report.add(ejectionStep{
		Name:   "event",
		Action: "resolved",
		Detail: fmt.Sprintf("removal %s (%s) at %s", event.ID, event.Reason, event.CreatedAt.Format(time.RFC3339)),
	})
	if err := checkManualRCA(parsed.class, event); err != nil {
		return err
	}
	pr, err = ejectionRCA(client, cfg, pr, event, parsed.class, fileBody, report)
	if err != nil {
		return err
	}
	return ejectionQueue(client, cfg, pr, event, parsed.class, report)
}

// ejectionRCA posts the RCA unless the removal has one with the same class,
// and refuses a different class. The returned pull request carries the RCA,
// so the gate sees it in a dry run too.
func ejectionRCA(client ejectionAPI, cfg *config.Config, pr github.QueuePullRequest, event github.QueueEvent, class, fileBody string, report *ejectionJSON) (github.QueuePullRequest, error) {
	existing, exists, err := rcaClass(cfg.Settings.RCAMarker, event.ID, pr.Comments)
	if err != nil {
		return pr, err
	}
	if exists && existing == class {
		report.add(ejectionStep{Name: "rca", Action: "kept", Detail: fmt.Sprintf("removal %s already has a %s RCA", event.ID, class)})
		return pr, nil
	}
	if exists {
		detail := fmt.Sprintf("removal %s already has a %s RCA, not %s; to correct it, run gh-merge rca --supersedes --class %s --file <body>", event.ID, existing, class, class)
		report.Steps = append(report.Steps, ejectionStep{Name: "rca", Action: "refused", Detail: detail})
		return pr, errors.New(detail)
	}
	body, err := rcaBody(client, cfg, pr, event, class, fileBody)
	if err != nil {
		return pr, err
	}
	step := ejectionStep{Name: "rca", Action: "would-post", Detail: fmt.Sprintf("would post a %s RCA for %s", class, event.ID)}
	if !dryRun {
		if err := client.CommentOnPullRequest(pr.Ref.Owner, pr.Ref.Repo, pr.Ref.Number, body); err != nil {
			return pr, fmt.Errorf("comment on %s: %w", pr.Ref.Raw, err)
		}
		step = ejectionStep{Name: "rca", Action: "posted", Detail: fmt.Sprintf("posted a %s RCA for %s", class, event.ID)}
	}
	report.add(step)
	pr.Comments = append(slices.Clip(pr.Comments), body)
	return pr, nil
}

// ejectionQueue requeues once, through the queue preflight and RCA gate,
// unless the pull request was already requeued after the removal.
func ejectionQueue(client ejectionAPI, cfg *config.Config, pr github.QueuePullRequest, event github.QueueEvent, class string, report *ejectionJSON) error {
	if why, done := requeuedSince(pr, event); done {
		report.add(ejectionStep{Name: "queue", Action: "already-queued", Detail: fmt.Sprintf("already requeued after %s (%s); nothing to do", event.ID, why)})
		return nil
	}
	if latest, _ := latestQueueEvent(pr.Events); latest.ID != event.ID {
		return fmt.Errorf("removal %s is not the latest queue event (%s %s)", event.ID, latest.Kind, latest.ID)
	}
	var result *queueJSONResult
	queueResultSink = func(r queueJSONResult) { result = &r }
	defer func() { queueResultSink = nil }()
	err := queueLoaded(client, cfg, pr, includeDownstack, false)
	step := ejectionStep{Name: "queue", Queue: result}
	switch {
	case errors.Is(err, errQueueRefused):
		step.Action = "refused"
		step.Detail = err.Error()
		if class == "our-code" {
			step.Detail += "; push a fix, then run handle-ejection again"
		}
	case err != nil:
		step.Action = "error"
		step.Detail = err.Error()
	case result != nil:
		step.Action = result.Result
	case dryRun:
		step.Action = "would-enqueue"
	default:
		step.Action = "enqueued"
	}
	report.add(step)
	return err
}

// requeuedSince reports whether the pull request merged, is in the merge
// queue, or was added to it after the removal, and which.
func requeuedSince(pr github.QueuePullRequest, removal github.QueueEvent) (string, bool) {
	switch {
	case pr.Merged:
		return "merged", true
	case pr.InMergeQueue:
		return "in the merge queue", true
	}
	for _, event := range pr.Events {
		if event.Kind == github.QueueEventAdded && event.CreatedAt.After(removal.CreatedAt) {
			return "added " + event.CreatedAt.Format(time.RFC3339), true
		}
	}
	return "", false
}

func parseEjectionArgs(args []string) (parsedEjection, error) {
	parsed := parsedEjection{dryRun: dryRun, whenGreen: whenGreen, asJSON: jsonOutput}
	var refs []string
	for i := 0; i < len(args); i++ {
		arg := args[i]
		name, value, hasValue := strings.Cut(arg, "=")
		switch {
		case name == "--class" || name == "--file" || name == "--event":
			if !hasValue {
				if i+1 >= len(args) {
					return parsedEjection{}, errEjectionUsage
				}
				i++
				value = args[i]
			}
			parsed.setValue(name, value)
		case arg == "--when-green":
			parsed.whenGreen = true
		case arg == "--dry-run":
			parsed.dryRun = true
		case arg == "--json":
			parsed.asJSON = true
		case strings.HasPrefix(arg, "-"):
			return parsedEjection{}, errEjectionUsage
		default:
			refs = append(refs, arg)
		}
	}
	if len(refs) != 1 || parsed.class == "" || parsed.file == "" {
		return parsedEjection{}, errEjectionUsage
	}
	parsed.ref = refs[0]
	return parsed, nil
}

func (p *parsedEjection) setValue(flagName, value string) {
	switch flagName {
	case "--class":
		p.class = value
	case "--file":
		p.file = value
	case "--event":
		p.event = value
	}
}
