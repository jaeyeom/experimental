package main

import (
	"bytes"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/argparser"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/prreview"
)

type stubRerequestClient struct {
	state github.RerequestPRState
}

func (s *stubRerequestClient) LoadRerequestPRState(_ github.PullRef) (github.RerequestPRState, error) {
	return s.state, nil
}

func (s *stubRerequestClient) RequestReviewers(_ github.PullRef, _ []string) error {
	return nil
}

func TestParseRerequestFlagsWithIncludeBots(t *testing.T) {
	args := []string{"--include-bots", "--json", "acme/widgets", "123"}
	detected, err := autoDetectArgs(args, true)
	if err != nil {
		t.Fatalf("autoDetectArgs() error = %v", err)
	}
	parser := argparser.NewArgParser(detected, argparser.WithBooleanFlags("apply", "json", "include-bots"))
	if err := validateRerequestOptions(parser); err != nil {
		t.Fatalf("validateRerequestOptions() error = %v", err)
	}
	repo, prNum, err := parseSubmitArgs(parser)
	if err != nil {
		t.Fatalf("parseSubmitArgs() error = %v", err)
	}
	if repo.Owner != "acme" || repo.Name != "widgets" || prNum != 123 {
		t.Fatalf("parsed = %+v #%d, want acme/widgets #123", repo, prNum)
	}
	if !parser.GetBoolOption("include-bots") || !parser.GetBoolOption("json") || parser.GetBoolOption("apply") {
		t.Fatalf("flags = include-bots:%v json:%v apply:%v", parser.GetBoolOption("include-bots"), parser.GetBoolOption("json"), parser.GetBoolOption("apply"))
	}
}

func TestBuildRerequestOptionsMissingConfig(t *testing.T) {
	t.Setenv("HOME", t.TempDir())

	parser := argparser.NewArgParser([]string{"acme/widgets", "123"}, argparser.WithBooleanFlags("apply", "json", "include-bots"))
	opts, err := buildRerequestOptions(parser)
	if err != nil {
		t.Fatalf("buildRerequestOptions() error = %v", err)
	}
	if len(opts.ExcludeReviewers) != 0 {
		t.Fatalf("ExcludeReviewers = %v, want empty", opts.ExcludeReviewers)
	}

	t1 := time.Date(2026, 11, 24, 10, 0, 0, 0, time.UTC)
	t2 := t1.Add(time.Hour)
	client := &stubRerequestClient{
		state: github.RerequestPRState{
			Number: 123,
			State:  "OPEN",
			Author: "author",
			Reviews: []github.RerequestReview{
				{Author: "alice", AuthorType: "User", State: "COMMENTED", SubmittedAt: t1},
				{Author: "copilot[bot]", AuthorType: "Bot", State: "COMMENTED", SubmittedAt: t1},
			},
			AuthorPushTimes: []time.Time{t2},
		},
	}

	var buf bytes.Buffer
	if err := prreview.RunRerequest(client, github.PullRef{Owner: "acme", Repo: "widgets", Number: 123}, opts, &buf); err != nil {
		t.Fatalf("RunRerequest() error = %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "#123 alice would rerequest:") {
		t.Errorf("missing alice rerequest in output:\n%s", out)
	}
	if !strings.Contains(out, "#123 copilot[bot] skip: skipped: bot reviewer copilot[bot]") {
		t.Errorf("missing bot skip in output:\n%s", out)
	}
}
