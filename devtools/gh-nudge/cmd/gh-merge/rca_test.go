package main

import (
	"encoding/json"
	"errors"
	"reflect"
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

// rcaPR is a removed pull request without evidence, so rca writes a bare
// marker, with comments numbered from 101.
func rcaPR(comments ...string) github.QueuePullRequest {
	pr := removedPR()
	pr.Events[0].CommitSHA = ""
	pr.Comments = comments
	for i := range comments {
		pr.CommentIDs = append(pr.CommentIDs, int64(101+i))
	}
	return pr
}

func runRCA(t *testing.T, client *fakeQueue, args ...string) error {
	t.Helper()
	var err error
	captureStdout(t, func() {
		err = postRCA(client, rcaConfig(), append([]string{"acme/widgets#12"}, args...))
	})
	return err
}

func TestRCARefusesSecondRCAForEvent(t *testing.T) {
	path := writeRCAFile(t, "Again.\n")
	client := &fakeQueue{pr: rcaPR("First.\nRCA EVT1 class=flake")}
	err := runRCA(t, client, "--class", "infra", "--file", path)
	if err == nil || !strings.Contains(err.Error(), "already has an RCA (flake)") {
		t.Fatalf("postRCA() error = %v, want refusal naming flake", err)
	}
	if client.commentCalls != 0 || len(client.edited) != 0 {
		t.Fatalf("comments = %d, edited = %v", client.commentCalls, client.edited)
	}
}

func TestRCAIgnoresOtherToolMarkers(t *testing.T) {
	path := writeRCAFile(t, "Ours.\n")
	client := &fakeQueue{pr: rcaPR("Bot.\nRCA EVT1 class=flake tool=bot")}
	if err := runRCA(t, client, "--class", "infra", "--file", path); err != nil {
		t.Fatalf("postRCA() error = %v", err)
	}
	if client.commentCalls != 1 {
		t.Fatalf("commentCalls = %d, want 1", client.commentCalls)
	}
	err := runRCA(t, &fakeQueue{pr: rcaPR("Bot.\nRCA EVT1 class=flake tool=bot")}, "--update", "--class", "infra", "--file", path)
	if err == nil || !strings.Contains(err.Error(), "no RCA") {
		t.Fatalf("--update over another tool's marker error = %v", err)
	}
}

func TestRCAUpdateEditsOurComment(t *testing.T) {
	path := writeRCAFile(t, "Corrected.\n")
	client := &fakeQueue{pr: rcaPR("RCA EVT1 class=our-code", "Unrelated.", "Bot.\nRCA EVT1 class=infra tool=bot")}
	if err := runRCA(t, client, "--update", "--class", "flake", "--file", path); err != nil {
		t.Fatalf("postRCA(--update) error = %v", err)
	}
	if client.commentCalls != 0 {
		t.Fatalf("commentCalls = %d, want 0", client.commentCalls)
	}
	if got, want := client.edited[101], "Corrected.\nRCA EVT1 class=flake"; got != want || len(client.edited) != 1 {
		t.Fatalf("edited = %v, want {101: %q}", client.edited, want)
	}

	client = &fakeQueue{pr: rcaPR("RCA EVT1 class=our-code", "RCA EVT1 class=flake supersedes=101")}
	if err := runRCA(t, client, "--update", "--class", "infra", "--file", path); err != nil {
		t.Fatalf("postRCA(--update) error = %v", err)
	}
	if got, want := client.edited[102], "Corrected.\nRCA EVT1 class=infra supersedes=101"; got != want {
		t.Fatalf("edited = %v, want {102: %q}", client.edited, want)
	}

	err := runRCA(t, &fakeQueue{pr: rcaPR()}, "--update", "--class", "flake", "--file", path)
	if err == nil || !strings.Contains(err.Error(), "no RCA") {
		t.Fatalf("--update without an RCA error = %v", err)
	}
}

func TestRCASupersedesPostsAndWins(t *testing.T) {
	path := writeRCAFile(t, "It was a flake.\n")
	pr := rcaPR("RCA EVT1 class=our-code")
	client := &fakeQueue{pr: pr}
	if err := runRCA(t, client, "--supersedes", "--class", "flake", "--file", path); err != nil {
		t.Fatalf("postRCA(--supersedes) error = %v", err)
	}
	if len(client.comments) != 1 || client.comments[0] != "It was a flake.\nRCA EVT1 class=flake supersedes=101" {
		t.Fatalf("comments = %q", client.comments)
	}
	class, matched, err := rcaClass(rcaConfig().Settings.RCAMarker, "EVT1", append(pr.Comments, client.comments...))
	if err != nil || !matched || class != "flake" {
		t.Fatalf("rcaClass() = %q, %v, %v, want flake", class, matched, err)
	}

	err = runRCA(t, &fakeQueue{pr: rcaPR()}, "--supersedes", "--class", "flake", "--file", path)
	if err == nil || !strings.Contains(err.Error(), "no RCA") {
		t.Fatalf("--supersedes without an RCA error = %v", err)
	}
	err = runRCA(t, &fakeQueue{pr: pr}, "--update", "--supersedes", "--class", "flake", "--file", path)
	if !errors.Is(err, errRCAUsage) {
		t.Fatalf("--update --supersedes error = %v, want usage", err)
	}
}

func TestRCACheck(t *testing.T) {
	tests := []struct {
		name     string
		comments []string
		text     string
		want     rcaCheckJSON
	}{
		{
			name: "none",
			text: "RCA for EVT1: none\n",
			want: rcaCheckJSON{PullRequest: "acme/widgets#12", EventID: "EVT1", RCAContext: []rcaContext{}},
		},
		{
			name:     "flake",
			comments: []string{"RCA EVT1 class=flake"},
			text:     "RCA for EVT1: flake\n",
			want:     rcaCheckJSON{PullRequest: "acme/widgets#12", EventID: "EVT1", Exists: true, Class: "flake", RCAContext: []rcaContext{}},
		},
		{
			name:     "our-code wins without a supersede mark",
			comments: []string{"RCA EVT1 class=our-code", "RCA EVT1 class=flake"},
			text:     "RCA for EVT1: our-code\n",
			want:     rcaCheckJSON{PullRequest: "acme/widgets#12", EventID: "EVT1", Exists: true, Class: "our-code", RCAContext: []rcaContext{}},
		},
		{
			name:     "superseded",
			comments: []string{"RCA EVT1 class=our-code", "RCA EVT1 class=flake supersedes=101", "RCA EVT1 class=infra tool=bot"},
			text:     "RCA for EVT1: flake (superseded)\nRCA from bot: infra (context, not counted)\n",
			want: rcaCheckJSON{
				PullRequest: "acme/widgets#12", EventID: "EVT1", Exists: true, Class: "flake", Superseded: true,
				RCAContext: []rcaContext{{Tool: "bot", Class: "infra"}},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			client := &fakeQueue{pr: rcaPR(tt.comments...)}
			text := captureStdout(t, func() {
				if err := postRCA(client, rcaConfig(), []string{"--check", "acme/widgets#12"}); err != nil {
					t.Fatalf("rca --check error = %v", err)
				}
			})
			if text != tt.text {
				t.Fatalf("text = %q, want %q", text, tt.text)
			}
			output := captureStdout(t, func() {
				if err := postRCA(client, rcaConfig(), []string{"--check", "acme/widgets#12", "--event", "EVT1", "--json"}); err != nil {
					t.Fatalf("rca --check --json error = %v", err)
				}
			})
			var got rcaCheckJSON
			if err := json.Unmarshal([]byte(output), &got); err != nil {
				t.Fatalf("decode: %v\n%s", err, output)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("json = %+v, want %+v", got, tt.want)
			}
			if client.commentCalls != 0 || len(client.edited) != 0 {
				t.Fatal("--check posted or edited a comment")
			}
		})
	}
}

func TestRCACheckUnknownEvent(t *testing.T) {
	err := postRCA(&fakeQueue{pr: rcaPR()}, rcaConfig(), []string{"--check", "acme/widgets#12", "--event", "NOPE"})
	if err == nil {
		t.Fatal("rca --check --event NOPE succeeded")
	}
}
