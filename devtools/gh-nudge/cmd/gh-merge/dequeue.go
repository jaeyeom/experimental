package main

import (
	"errors"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

var errDequeueUsage = errors.New("dequeue usage")

const (
	dequeueSettleTimeout = 30 * time.Second
	dequeueInitialDelay  = time.Second
	dequeueMaxDelay      = 8 * time.Second
)

var (
	dequeueNow   = time.Now
	dequeueSleep = time.Sleep
)

type dequeueAPI interface {
	LoadPullRequest(ref string) (github.QueuePullRequest, error)
	DequeuePullRequest(nodeID string) error
}

func dryRunHelp() string {
	return "Dry run mode (don't actually merge, enqueue, or dequeue)"
}

func dequeueCommandHelp() string {
	return "Remove one pull request from the merge queue (owner/repo#n, a URL, or a bare number)"
}

func dequeueUsage() string {
	return "dequeue <owner/repo#n|url|number>"
}

func dequeueCommand(client dequeueAPI, args []string) error {
	ref, err := parseDequeueArgs(args)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Usage: %s %s\n", os.Args[0], dequeueUsage())
		os.Exit(1)
	}
	return dequeuePullRequest(client, ref)
}

func parseDequeueArgs(args []string) (string, error) {
	var refs []string
	for _, arg := range args {
		switch arg {
		case "-dry-run", "--dry-run":
			dryRun = true
		default:
			if strings.HasPrefix(arg, "-") {
				return "", errDequeueUsage
			}
			refs = append(refs, arg)
		}
	}
	if len(refs) != 1 {
		return "", errDequeueUsage
	}
	return refs[0], nil
}

func dequeuePullRequest(client dequeueAPI, ref string) error {
	resolved, err := resolvePullRef(ref)
	if err != nil {
		return err
	}
	pr, err := client.LoadPullRequest(resolved)
	if err != nil {
		return fmt.Errorf("load pull request %s: %w", resolved, err)
	}
	if !pr.InMergeQueue {
		return errors.New("pull request is not in the merge queue")
	}
	if dryRun {
		fmt.Printf("Would dequeue %s.\n", pr.Ref.Raw)
		return nil
	}
	if err := client.DequeuePullRequest(pr.NodeID); err != nil {
		return fmt.Errorf("dequeue %s: %w", pr.Ref.Raw, err)
	}
	seen := waitForRemoval(client, resolved, pr.Events)
	fmt.Printf("Dequeued %s.\n", pr.Ref.Raw)
	if !seen {
		fmt.Printf("Warning: the timeline for %s does not show the removal yet; queue checks may be stale\n", pr.Ref.Raw)
	}
	return nil
}

// waitForRemoval polls the timeline until it shows a removal that was not in
// before, so a queue run right after dequeue sees it. GitHub can lag behind
// the mutation. It reports false if the removal is not seen in time.
func waitForRemoval(client dequeueAPI, ref string, before []github.QueueEvent) bool {
	known := make(map[string]bool, len(before))
	for _, event := range before {
		known[event.ID] = true
	}
	deadline := dequeueNow().Add(dequeueSettleTimeout)
	delay := dequeueInitialDelay
	for {
		now := dequeueNow()
		if !now.Before(deadline) {
			return false
		}
		dequeueSleep(min(delay, deadline.Sub(now)))
		delay = min(delay*2, dequeueMaxDelay)
		pr, err := client.LoadPullRequest(ref)
		if err != nil {
			continue
		}
		event, ok := latestQueueEvent(pr.Events)
		if ok && event.Kind == github.QueueEventRemoved && !known[event.ID] {
			return true
		}
	}
}
