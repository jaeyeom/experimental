package main

import (
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

func TestRCAWritesEvidenceKeys(t *testing.T) {
	withEvidenceCircleAPI(t, evidenceTestFailure())
	path := writeRCAFile(t, "Broke the parser.\n")
	client := &fakeQueue{pr: removedPR(), evidence: circleEvidence()}
	captureStdout(t, func() {
		if err := postRCA(client, rcaConfig(), []string{"acme/widgets#12", "--class", "our-code", "--file", path}); err != nil {
			t.Fatalf("postRCA() error = %v", err)
		}
	})
	want := "\nRCA EVT1 class=our-code commit=deadbeef job=test-amd64 test=pkg/parse.TestParse"
	if !strings.HasSuffix(client.comments[0], want) {
		t.Fatalf("comment = %q, want suffix %q", client.comments[0], want)
	}

	pr := removedPR()
	pr.Events[0].CommitSHA = ""
	client = &fakeQueue{pr: pr}
	captureStdout(t, func() {
		if err := postRCA(client, rcaConfig(), []string{"acme/widgets#12", "--class", "flake", "--file", path}); err != nil {
			t.Fatalf("postRCA() error = %v", err)
		}
	})
	if !strings.HasSuffix(client.comments[0], "\nRCA EVT1 class=flake") {
		t.Fatalf("comment without evidence = %q", client.comments[0])
	}
}

func TestQueueGateSupersedes(t *testing.T) {
	pr := removedPR()
	pr.Comments = []string{"RCA EVT1 class=our-code", "Corrected.\nRCA EVT1 class=flake supersedes=101"}
	client := &fakeQueue{pr: pr}
	captureStdout(t, func() {
		if err := queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false); err != nil {
			t.Fatalf("queuePullRequest() error = %v", err)
		}
	})
	if client.enqueueCalls != 1 {
		t.Fatalf("enqueueCalls = %d", client.enqueueCalls)
	}

	pr.Comments = []string{"RCA EVT1 class=our-code", "RCA EVT1 class=flake"}
	client = &fakeQueue{pr: pr}
	captureStdout(t, func() {
		err := queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false)
		if err == nil || !strings.Contains(err.Error(), "our-code") {
			t.Fatalf("queuePullRequest() error = %v, want our-code", err)
		}
	})
}

func TestQueueGateIgnoresOtherToolMarkers(t *testing.T) {
	pr := removedPR()
	pr.Comments = []string{"RCA EVT1 class=flake tool=bot"}
	client := &fakeQueue{pr: pr}
	captureStdout(t, func() {
		err := queuePullRequest(client, rcaConfig(), pr.Ref.Raw, false)
		if err == nil || !strings.Contains(err.Error(), "no RCA comment") {
			t.Fatalf("queuePullRequest() error = %v, want no RCA comment", err)
		}
	})

	base := time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC)
	events := []github.QueueEvent{
		{ID: "R1", Kind: github.QueueEventRemoved, CreatedAt: base},
		{ID: "A1", Kind: github.QueueEventAdded, CreatedAt: base.Add(time.Minute)},
	}
	got, err := earlierRequeues("RCA {event_id} class={class}", []string{"RCA R1 class=our-code tool=bot"}, events)
	if err != nil || got != 1 {
		t.Fatalf("earlierRequeues() = %d, %v, want 1", got, err)
	}
}

func otherToolPR() github.QueuePullRequest {
	pr := removedPR()
	pr.Comments = []string{
		"RCA EVT1 class=flake",
		"Bot analysis.\nRCA EVT1 class=infra tool=bot job=build test=TestX",
	}
	return pr
}

func TestEvidenceShowsOtherToolRCAs(t *testing.T) {
	cfg := rcaConfig()
	client := &fakeQueue{pr: otherToolPR()}
	output := captureStdout(t, func() {
		if err := evidenceCommand(client, cfg, []string{"acme/widgets#12", "--json"}); err != nil {
			t.Fatalf("evidenceCommand() error = %v", err)
		}
	})
	var got evidenceJSON
	if err := json.Unmarshal([]byte(output), &got); err != nil {
		t.Fatalf("decode: %v\n%s", err, output)
	}
	want := rcaContext{Tool: "bot", Class: "infra", Job: "build", Test: "TestX"}
	if len(got.RCAContext) != 1 || got.RCAContext[0] != want {
		t.Fatalf("rca_context = %+v, want [%+v]", got.RCAContext, want)
	}

	text := captureStdout(t, func() {
		if err := evidenceCommand(client, cfg, []string{"acme/widgets#12"}); err != nil {
			t.Fatalf("evidenceCommand() error = %v", err)
		}
	})
	if !strings.Contains(text, "RCA from bot: infra (context, not counted)\n") {
		t.Fatalf("text = %q", text)
	}
}

func TestWatchJSONShowsOtherToolRCAs(t *testing.T) {
	client := &fakeQueue{pr: otherToolPR()}
	output := captureStdout(t, func() {
		err := watchPullRequests(client, []string{"acme/widgets#12"}, watchOptions{timeout: 0, asJSON: true, cfg: rcaConfig()})
		assertWatchExit(t, err, 2)
	})
	var got []watchResult
	if err := json.Unmarshal([]byte(output), &got); err != nil {
		t.Fatalf("decode: %v\n%s", err, output)
	}
	if len(got) != 1 || len(got[0].RCAContext) != 1 || got[0].RCAContext[0].Tool != "bot" {
		t.Fatalf("watch = %+v", got)
	}
}
