package parentclose

import (
	"errors"
	"reflect"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func TestEvaluateMirroredChildrenAllDone(t *testing.T) {
	t.Parallel()

	items := []source.Item{
		{Source: "jira:acme", ID: "EPIC-1", Title: "Ship v1", Done: false},
		{Source: "jira:acme", ID: "PRJ-3", Parent: "EPIC-1", Done: true},
		{Source: "jira:acme", ID: "PRJ-1", Parent: "EPIC-1", Done: true},
		{Source: "jira:acme", ID: "PRJ-2", Parent: "EPIC-1", Done: true},
	}
	got, err := Evaluate("EPIC-1", items, nil)
	if err != nil {
		t.Fatalf("Evaluate: %v", err)
	}
	want := Result{
		Source:       "jira:acme",
		ID:           "EPIC-1",
		Title:        "Ship v1",
		Eligible:     true,
		DoneChildren: []string{"PRJ-1", "PRJ-2", "PRJ-3"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("got = %+v, want %+v", got, want)
	}
}

func TestEvaluateFinishedHistoryChildren(t *testing.T) {
	t.Parallel()

	items := []source.Item{
		{Source: "jira:acme", ID: "EPIC-1", Title: "Ship v1", Done: false},
	}
	finished := []store.Finished{
		{Source: "jira:acme", ID: "PRJ-2", Parent: "EPIC-1"},
		{Source: "jira:acme", ID: "PRJ-1", Parent: "EPIC-1"},
		{Source: "jira:other", ID: "PRJ-OTHER", Parent: "EPIC-1"},
	}
	got, err := Evaluate("EPIC-1", items, finished)
	if err != nil {
		t.Fatalf("Evaluate: %v", err)
	}
	want := Result{
		Source:       "jira:acme",
		ID:           "EPIC-1",
		Title:        "Ship v1",
		Eligible:     true,
		DoneChildren: []string{"PRJ-1", "PRJ-2"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("got = %+v, want %+v", got, want)
	}
}

func TestEvaluateMixedDoneAndOpenChildren(t *testing.T) {
	t.Parallel()

	items := []source.Item{
		{Source: "jira:acme", ID: "EPIC-1", Title: "Ship v1", Done: false},
		{Source: "jira:acme", ID: "PRJ-1", Parent: "EPIC-1", Done: true},
		{Source: "jira:acme", ID: "PRJ-3", Parent: "EPIC-1", Done: false},
		{Source: "jira:acme", ID: "PRJ-2", Parent: "EPIC-1", Done: false},
	}
	got, err := Evaluate("EPIC-1", items, nil)
	if err != nil {
		t.Fatalf("Evaluate: %v", err)
	}
	want := Result{
		Source:       "jira:acme",
		ID:           "EPIC-1",
		Title:        "Ship v1",
		Eligible:     false,
		DoneChildren: []string{"PRJ-1"},
		OpenChildren: []string{"PRJ-2", "PRJ-3"},
		Reason:       "2 open children: PRJ-2, PRJ-3",
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("got = %+v, want %+v", got, want)
	}
}

func TestEvaluateSingleOpenChild(t *testing.T) {
	t.Parallel()

	items := []source.Item{
		{Source: "jira:acme", ID: "EPIC-1", Title: "Ship v1", Done: false},
		{Source: "jira:acme", ID: "PRJ-1", Parent: "EPIC-1", Done: true},
		{Source: "jira:acme", ID: "PRJ-2", Parent: "EPIC-1", Done: false},
	}
	got, err := Evaluate("EPIC-1", items, nil)
	if err != nil {
		t.Fatalf("Evaluate: %v", err)
	}
	if got.Eligible || got.Reason != "1 open child: PRJ-2" {
		t.Fatalf("got = %+v", got)
	}
}

func TestEvaluateLeafAndAlreadyDone(t *testing.T) {
	t.Parallel()

	items := []source.Item{
		{Source: "jira:acme", ID: "LEAF-1", Title: "Leaf", Done: false},
		{Source: "jira:acme", ID: "EPIC-DONE", Title: "Closed epic", Done: true},
		{Source: "jira:acme", ID: "PRJ-1", Parent: "EPIC-DONE", Done: true},
	}

	leaf, err := Evaluate("LEAF-1", items, nil)
	if err != nil {
		t.Fatalf("Evaluate leaf: %v", err)
	}
	if leaf.Eligible || leaf.Reason != "no children in local cache" {
		t.Fatalf("leaf = %+v", leaf)
	}

	done, err := Evaluate("EPIC-DONE", items, nil)
	if err != nil {
		t.Fatalf("Evaluate done: %v", err)
	}
	if done.Eligible || done.Reason != "already done" || !reflect.DeepEqual(done.DoneChildren, []string{"PRJ-1"}) {
		t.Fatalf("done = %+v", done)
	}
}

func TestEvaluateReopenedChildInItemsOverridesFinished(t *testing.T) {
	t.Parallel()

	items := []source.Item{
		{Source: "jira:acme", ID: "EPIC-1", Title: "Ship v1", Done: false},
		{Source: "jira:acme", ID: "PRJ-1", Parent: "EPIC-1", Done: false},
		{Source: "jira:acme", ID: "PRJ-2", Parent: "EPIC-1", Done: true},
	}
	finished := []store.Finished{
		{Source: "jira:acme", ID: "PRJ-1", Parent: "EPIC-1"},
		{Source: "jira:acme", ID: "PRJ-2", Parent: "EPIC-1"},
	}
	got, err := Evaluate("EPIC-1", items, finished)
	if err != nil {
		t.Fatalf("Evaluate: %v", err)
	}
	want := Result{
		Source:       "jira:acme",
		ID:           "EPIC-1",
		Title:        "Ship v1",
		Eligible:     false,
		DoneChildren: []string{"PRJ-2"},
		OpenChildren: []string{"PRJ-1"},
		Reason:       "1 open child: PRJ-1",
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("got = %+v, want %+v", got, want)
	}
}

func TestEvaluateMultiLevelHierarchy(t *testing.T) {
	t.Parallel()

	items := []source.Item{
		{Source: "jira:acme", ID: "EPIC-1", Title: "Epic", Done: false},
		{Source: "jira:acme", ID: "STORY-1", Title: "Story", Parent: "EPIC-1", Done: false},
		{Source: "jira:acme", ID: "SUB-1", Parent: "STORY-1", Done: true},
		{Source: "jira:acme", ID: "SUB-2", Parent: "STORY-1", Done: true},
	}
	found, err := Find(items, nil)
	if err != nil {
		t.Fatalf("Find: %v", err)
	}
	want := []Result{{
		Source:       "jira:acme",
		ID:           "STORY-1",
		Title:        "Story",
		Eligible:     true,
		DoneChildren: []string{"SUB-1", "SUB-2"},
	}}
	if !reflect.DeepEqual(found, want) {
		t.Fatalf("Find while story open = %+v, want %+v", found, want)
	}

	epic, err := Evaluate("EPIC-1", items, nil)
	if err != nil {
		t.Fatalf("Evaluate EPIC-1: %v", err)
	}
	if epic.Eligible || epic.Reason != "1 open child: STORY-1" {
		t.Fatalf("epic while story open = %+v", epic)
	}

	// Once Story is marked done, Epic becomes eligible.
	items[1].Done = true
	found, err = Find(items, nil)
	if err != nil {
		t.Fatalf("Find after story done: %v", err)
	}
	wantEpic := []Result{{
		Source:       "jira:acme",
		ID:           "EPIC-1",
		Title:        "Epic",
		Eligible:     true,
		DoneChildren: []string{"STORY-1"},
	}}
	if !reflect.DeepEqual(found, wantEpic) {
		t.Fatalf("Find after story done = %+v, want %+v", found, wantEpic)
	}
}

func TestEvaluateIgnoresSelfParentAndEmptyIDs(t *testing.T) {
	t.Parallel()

	items := []source.Item{
		{ID: "", Parent: "EPIC-1", Done: true},
		{ID: "EPIC-1", Parent: "EPIC-1", Done: false},
	}
	finished := []store.Finished{
		{ID: "", Parent: "EPIC-1"},
		{ID: "EPIC-1", Parent: "EPIC-1"},
	}
	got, err := Evaluate("EPIC-1", items, finished)
	if err != nil {
		t.Fatalf("Evaluate: %v", err)
	}
	if got.Eligible || got.Reason != "no children in local cache" {
		t.Fatalf("got = %+v", got)
	}
}

func TestEvaluateErrors(t *testing.T) {
	t.Parallel()

	if _, err := Evaluate("", nil, nil); err == nil {
		t.Fatal("expected error for empty id")
	}
	_, err := Evaluate("PRJ-MISSING", []source.Item{{ID: "PRJ-1"}}, nil)
	if !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("err = %v, want source.ErrNotFound", err)
	}
}

func TestFindSortedDeterministically(t *testing.T) {
	t.Parallel()

	items := []source.Item{
		{Source: "jira:acme", ID: "EPIC-2", Title: "Two", Done: false},
		{Source: "jira:acme", ID: "EPIC-1", Title: "One", Done: false},
		{Source: "jira:acme", ID: "LEAF-1", Title: "Leaf", Done: false},
	}
	finished := []store.Finished{
		{Source: "jira:acme", ID: "PRJ-20", Parent: "EPIC-2"},
		{Source: "jira:acme", ID: "PRJ-10", Parent: "EPIC-1"},
	}
	got, err := Find(items, finished)
	if err != nil {
		t.Fatalf("Find: %v", err)
	}
	if len(got) != 2 || got[0].ID != "EPIC-1" || got[1].ID != "EPIC-2" {
		t.Fatalf("got = %+v", got)
	}
}
