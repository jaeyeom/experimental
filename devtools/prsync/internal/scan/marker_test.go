package scan

import (
	"context"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
)

func TestSharedRCAMarker(t *testing.T) {
	const marker = "RCA {event_id} class={class}"
	for _, class := range []string{"our-code", "flake", "infra"} {
		t.Run(class, func(t *testing.T) {
			line := renderRCAMarker(marker, "EVT", class)
			want := "RCA EVT class=" + class
			if line != want {
				t.Fatalf("renderRCAMarker() = %q, want %q", line, want)
			}
			got, matched, err := parseRCAMarker(marker, "EVT", []string{line})
			if err != nil {
				t.Fatalf("parseRCAMarker() error = %v", err)
			}
			if !matched || got != class {
				t.Fatalf("parseRCAMarker() = %q, %v, want %q, true", got, matched, class)
			}
		})
	}
}

func TestMergeQueueFixOurCodeOnlyWhenHeadIsOlder(t *testing.T) {
	const marker = "RCA {event_id} class={class}"
	commentAt := time.Date(2026, 1, 3, 0, 0, 0, 0, time.UTC)
	older := commentAt.Add(-time.Hour)
	newer := commentAt.Add(time.Hour)
	evidence := gh.MarkerComments{
		HeadCommittedAt: older,
		Comments: []gh.MarkerComment{
			{URL: "https://example/old", Body: "RCA OLD class=our-code", CreatedAt: commentAt.Add(-time.Minute)},
			{URL: "https://example/flake", Body: "RCA MID class=flake", CreatedAt: commentAt.Add(-time.Second)},
			{URL: "https://example/new", Body: "note\nRCA EVT class=our-code", CreatedAt: commentAt},
		},
	}
	markerFn := func(string, string) (string, error) { return marker, nil }
	client := &scriptGH{markers: map[int]gh.MarkerComments{7: evidence}}
	actions, err := mergeQueueActions(context.Background(), client, markerFn, "acme", "widgets", 7)
	if err != nil {
		t.Fatalf("mergeQueueActions() error = %v", err)
	}
	if len(actions) != 1 || actions[0].Name != ActionMergeQueueFix || actions[0].EventID != "EVT" || actions[0].Class != "our-code" || actions[0].CommentURL != "https://example/new" {
		t.Fatalf("actions = %+v", actions)
	}

	evidence.HeadCommittedAt = newer
	client.markers[7] = evidence
	actions, err = mergeQueueActions(context.Background(), client, markerFn, "acme", "widgets", 7)
	if err != nil {
		t.Fatalf("newer head error = %v", err)
	}
	if actions != nil {
		t.Fatalf("newer head actions = %+v, want none", actions)
	}

	evidence.HeadCommittedAt = commentAt
	client.markers[7] = evidence
	actions, err = mergeQueueActions(context.Background(), client, markerFn, "acme", "widgets", 7)
	if err != nil {
		t.Fatalf("same time error = %v", err)
	}
	if actions != nil {
		t.Fatalf("same time actions = %+v, want none", actions)
	}

	evidence.HeadCommittedAt = older
	evidence.Comments = nil
	client.markers[7] = evidence
	actions, err = mergeQueueActions(context.Background(), client, markerFn, "acme", "widgets", 7)
	if err != nil {
		t.Fatalf("no comment error = %v", err)
	}
	if actions != nil {
		t.Fatalf("no comment actions = %+v, want none", actions)
	}

	evidence.HeadCommittedAt = older
	evidence.Comments = []gh.MarkerComment{
		{URL: "https://example/old", Body: "RCA OLD class=our-code", CreatedAt: commentAt.Add(-time.Minute)},
		{URL: "https://example/flake", Body: "RCA MID class=flake", CreatedAt: commentAt},
	}
	client.markers[7] = evidence
	actions, err = mergeQueueActions(context.Background(), client, markerFn, "acme", "widgets", 7)
	if err != nil {
		t.Fatalf("flake newest error = %v", err)
	}
	if actions != nil {
		t.Fatalf("flake newest actions = %+v, want none", actions)
	}

	evidence.Comments = []gh.MarkerComment{
		{URL: "https://example/infra", Body: "RCA MID class=infra", CreatedAt: commentAt},
	}
	client.markers[7] = evidence
	actions, err = mergeQueueActions(context.Background(), client, markerFn, "acme", "widgets", 7)
	if err != nil {
		t.Fatalf("infra error = %v", err)
	}
	if actions != nil {
		t.Fatalf("infra actions = %+v, want none", actions)
	}
}

func TestParseNewestFailureMarker(t *testing.T) {
	t.Parallel()
	t1 := time.Date(2026, 6, 1, 10, 0, 0, 0, time.UTC)
	t2 := t1.Add(time.Minute)
	tests := []struct {
		name        string
		comments    []gh.MarkerComment
		rcaTemplate string
		wantKind    string
		wantHead    string
		wantRun     string
		wantNil     bool
	}{
		{
			name:     "no markers",
			comments: []gh.MarkerComment{{Body: "plain comment", CreatedAt: t1}},
			wantNil:  true,
		},
		{
			name: "generic prsync-failure our-code",
			comments: []gh.MarkerComment{
				{URL: "https://example/1", Body: "details\n<!-- prsync-failure kind=our-code head=abc123 queue_run=qr-1 -->", CreatedAt: t1},
			},
			wantKind: FailureKindOurCode,
			wantHead: "abc123",
			wantRun:  "qr-1",
		},
		{
			name: "newest marker wins over older our-code",
			comments: []gh.MarkerComment{
				{URL: "https://example/1", Body: "<!-- prsync-failure kind=our-code head=abc123 queue_run=qr-1 -->", CreatedAt: t1},
				{URL: "https://example/2", Body: "<!-- merge-failure kind=infra-flake head_sha=abc123 run_id=qr-2 -->", CreatedAt: t2},
			},
			wantKind: FailureKindInfraFlake,
			wantHead: "abc123",
			wantRun:  "qr-2",
		},
		{
			name: "unknown kind preserved",
			comments: []gh.MarkerComment{
				{URL: "https://example/3", Body: "<!-- queue-failure kind=unknown head=def456 run=qr-3 -->", CreatedAt: t1},
			},
			wantKind: FailureKindUnknown,
			wantHead: "def456",
			wantRun:  "qr-3",
		},
		{
			name: "rca marker mapped to failure marker",
			comments: []gh.MarkerComment{
				{URL: "https://example/4", Body: "<!-- gh-merge-rca EVT-9 class=flake commit=deadbeef -->", CreatedAt: t1},
			},
			rcaTemplate: "<!-- gh-merge-rca {event_id} class={class} -->",
			wantKind:    FailureKindInfraFlake,
			wantHead:    "deadbeef",
			wantRun:     "EVT-9",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			got, err := ParseNewestFailureMarker(tc.comments, tc.rcaTemplate)
			if err != nil {
				t.Fatalf("ParseNewestFailureMarker() error = %v", err)
			}
			if tc.wantNil {
				if got != nil {
					t.Fatalf("got %+v, want nil", got)
				}
				return
			}
			if got == nil {
				t.Fatal("got nil, want marker")
				return
			}
			if got.Kind != tc.wantKind || got.HeadSHA != tc.wantHead || got.QueueRunID != tc.wantRun {
				t.Fatalf("got %+v, want kind=%q head=%q run=%q", got, tc.wantKind, tc.wantHead, tc.wantRun)
			}
		})
	}
}
