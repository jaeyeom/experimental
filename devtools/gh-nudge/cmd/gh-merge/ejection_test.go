package main

import (
	"encoding/json"
	"errors"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

func runHandleEjection(t *testing.T, client *fakeQueue, args ...string) (string, error) {
	t.Helper()
	withMergeFlags(t, false, false, false)
	var err error
	output := captureStdout(t, func() {
		err = handleEjection(client, rcaConfig(), append([]string{"acme/widgets#12"}, args...))
	})
	return output, err
}

func TestHandleEjectionFreshFlakePostsAndEnqueues(t *testing.T) {
	path := writeRCAFile(t, "Network blip.\n")
	client := &fakeQueue{pr: rcaPR()}
	if _, err := runHandleEjection(t, client, "--class", "flake", "--file", path); err != nil {
		t.Fatalf("handleEjection() error = %v", err)
	}
	if client.commentCalls != 1 || client.comments[0] != "Network blip.\nRCA EVT1 class=flake" {
		t.Fatalf("comments = %q", client.comments)
	}
	if client.enqueueCalls != 1 {
		t.Fatalf("enqueueCalls = %d, want 1", client.enqueueCalls)
	}
}

func TestHandleEjectionRerunIsNoOp(t *testing.T) {
	path := writeRCAFile(t, "Network blip.\n")
	added := github.QueueEvent{ID: "ADD2", Kind: github.QueueEventAdded, CreatedAt: time.Date(2026, 1, 2, 1, 0, 0, 0, time.UTC)}
	tests := map[string]func(*github.QueuePullRequest){
		"added after the removal": func(pr *github.QueuePullRequest) { pr.Events = append(pr.Events, added) },
		"in the merge queue":      func(pr *github.QueuePullRequest) { pr.InMergeQueue = true },
	}
	for name, requeued := range tests {
		t.Run(name, func(t *testing.T) {
			pr := rcaPR("Network blip.\nRCA EVT1 class=flake")
			requeued(&pr)
			client := &fakeQueue{pr: pr}
			output, err := runHandleEjection(t, client, "--class", "flake", "--file", path)
			if err != nil {
				t.Fatalf("handleEjection() error = %v", err)
			}
			if client.commentCalls != 0 || client.enqueueCalls != 0 {
				t.Fatalf("comments = %d, enqueues = %d, want none", client.commentCalls, client.enqueueCalls)
			}
			if !strings.Contains(output, "already requeued") {
				t.Fatalf("output = %q", output)
			}
		})
	}
}

func TestHandleEjectionOurCodeSameHeadPostsWithoutEnqueue(t *testing.T) {
	path := writeRCAFile(t, "Broke the parser.\n")
	pr := rcaPR()
	pr.HeadPushedAt = pr.Events[0].CreatedAt.Add(-time.Hour)
	client := &fakeQueue{pr: pr}
	output, err := runHandleEjection(t, client, "--class", "our-code", "--file", path)
	if !errors.Is(err, errQueueRefused) {
		t.Fatalf("handleEjection() error = %v, want errQueueRefused", err)
	}
	if client.commentCalls != 1 || !strings.HasSuffix(client.comments[0], "RCA EVT1 class=our-code") {
		t.Fatalf("comments = %q", client.comments)
	}
	if client.enqueueCalls != 0 {
		t.Fatalf("enqueueCalls = %d, want 0", client.enqueueCalls)
	}
	if !strings.Contains(output, "push a fix") {
		t.Fatalf("output = %q, want a push-a-fix message", output)
	}
}

func TestHandleEjectionClassMismatchRefuses(t *testing.T) {
	path := writeRCAFile(t, "It was a flake.\n")
	client := &fakeQueue{pr: rcaPR("RCA EVT1 class=our-code")}
	_, err := runHandleEjection(t, client, "--class", "flake", "--file", path)
	if err == nil || !strings.Contains(err.Error(), "rca --supersedes") || errors.Is(err, errQueueRefused) {
		t.Fatalf("handleEjection() error = %v, want a refusal suggesting rca --supersedes", err)
	}
	if client.commentCalls != 0 || client.enqueueCalls != 0 {
		t.Fatalf("comments = %d, enqueues = %d, want none", client.commentCalls, client.enqueueCalls)
	}
}

func TestHandleEjectionManualIsOutsideTheCap(t *testing.T) {
	base := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	pr := readyPR()
	pr.Events = []github.QueueEvent{
		{ID: "R1", Kind: github.QueueEventRemoved, CreatedAt: base, Reason: "failed_checks"},
		{ID: "A1", Kind: github.QueueEventAdded, CreatedAt: base.Add(time.Minute)},
		{ID: "MAN", Kind: github.QueueEventRemoved, CreatedAt: base.Add(2 * time.Minute), Reason: "manual"},
	}
	pr.Comments = []string{"RCA R1 class=flake"}
	limit := 0
	cfg := rcaConfig()
	cfg.Settings.MaxRequeues = &limit
	path := writeRCAFile(t, "Dequeued by hand.\n")
	client := &fakeQueue{pr: pr}
	withMergeFlags(t, false, false, false)
	captureStdout(t, func() {
		if err := handleEjection(client, cfg, []string{"acme/widgets#12", "--class", "manual", "--file", path}); err != nil {
			t.Fatalf("handleEjection() error = %v", err)
		}
	})
	if client.commentCalls != 1 || !strings.HasSuffix(client.comments[0], "RCA MAN class=manual") {
		t.Fatalf("comments = %q", client.comments)
	}
	if client.enqueueCalls != 1 {
		t.Fatalf("enqueueCalls = %d, want 1", client.enqueueCalls)
	}
}

func TestHandleEjectionDryRunJSON(t *testing.T) {
	t.Cleanup(func() { jsonOutput = false })
	path := writeRCAFile(t, "Network blip.\n")
	client := &fakeQueue{pr: rcaPR()}
	output, err := runHandleEjection(t, client, "--class", "flake", "--file", path, "--dry-run", "--json")
	if err != nil {
		t.Fatalf("handleEjection() error = %v", err)
	}
	if client.commentCalls != 0 || client.enqueueCalls != 0 {
		t.Fatalf("comments = %d, enqueues = %d, want none", client.commentCalls, client.enqueueCalls)
	}
	var got ejectionJSON
	if err := json.Unmarshal([]byte(output), &got); err != nil {
		t.Fatalf("decode: %v\n%s", err, output)
	}
	if got.EventID != "EVT1" || got.Class != "flake" || !got.DryRun {
		t.Fatalf("json = %+v", got)
	}
	actions := map[string]string{}
	for _, step := range got.Steps {
		actions[step.Name] = step.Action
	}
	want := map[string]string{"event": "resolved", "rca": "would-post", "queue": "would-enqueue"}
	for name, action := range want {
		if actions[name] != action {
			t.Fatalf("steps = %+v, want %s=%s", got.Steps, name, action)
		}
	}
	if queue := got.Steps[len(got.Steps)-1]; queue.Queue == nil || queue.Queue.Requeue == nil || !queue.Queue.Requeue.Allowed {
		t.Fatalf("queue step = %+v, want the requeue decision", queue)
	}
}

func TestHandleEjectionUsage(t *testing.T) {
	for _, args := range [][]string{
		{"--class", "flake"},
		{"--file", "body.md"},
		{"--class", "flake", "--file", "body.md", "--bogus"},
	} {
		_, err := runHandleEjection(t, &fakeQueue{pr: rcaPR()}, args...)
		if !errors.Is(err, errEjectionUsage) {
			t.Fatalf("args %q error = %v, want usage", args, err)
		}
	}
}
