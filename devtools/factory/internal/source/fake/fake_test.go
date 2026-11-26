package fake_test

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/fake"
)

func TestNameAndMe(t *testing.T) {
	ctx := context.Background()
	s := &fake.Source{SourceName: "mem", Account: "alice"}

	if got := s.Name(); got != "mem" {
		t.Fatalf("Name() = %q, want mem", got)
	}
	got, err := s.Me(ctx)
	if err != nil {
		t.Fatalf("Me() error = %v", err)
	}
	if got != "alice" {
		t.Fatalf("Me() = %q, want alice", got)
	}
}

func TestSync(t *testing.T) {
	early := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	mid := time.Date(2026, 1, 2, 0, 0, 0, 0, time.UTC)
	late := time.Date(2026, 1, 3, 0, 0, 0, 0, time.UTC)
	s := &fake.Source{Items: map[string]source.Item{
		"late":  {ID: "late", Title: "L", Updated: late},
		"early": {ID: "early", Title: "E", Updated: early},
		"mid":   {ID: "mid", Title: "M", Updated: mid},
		"same":  {ID: "same", Title: "S", Updated: early},
		"zero":  {ID: "zero", Title: "Z"},
	}}

	got, err := s.Sync(context.Background(), mid)
	if err != nil {
		t.Fatalf("Sync(mid) error = %v", err)
	}
	if ids := itemIDs(got); !reflect.DeepEqual(ids, []string{"mid", "late"}) {
		t.Fatalf("Sync(mid) ids = %v", ids)
	}
	if got[0].Title != "M" {
		t.Fatalf("Sync dropped fields: %+v", got[0])
	}
	got[0].Title = "changed"
	if s.Items["mid"].Title != "M" {
		t.Fatal("Sync returned a live alias")
	}

	got, err = s.Sync(context.Background(), time.Time{})
	if err != nil {
		t.Fatalf("Sync(zero) error = %v", err)
	}
	if ids := itemIDs(got); !reflect.DeepEqual(ids, []string{"zero", "early", "same", "mid", "late"}) {
		t.Fatalf("Sync(zero) ids = %v", ids)
	}

	got, err = s.Sync(context.Background(), early)
	if err != nil {
		t.Fatalf("Sync(early) error = %v", err)
	}
	if ids := itemIDs(got); !reflect.DeepEqual(ids, []string{"early", "same", "mid", "late"}) {
		t.Fatalf("Sync(early) ids = %v", ids)
	}

	got, err = s.Sync(context.Background(), late.Add(time.Second))
	if err != nil {
		t.Fatalf("Sync(after) error = %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("Sync(after) = %+v, want none", got)
	}
}

func TestQueryFiltersSyncAndIDs(t *testing.T) {
	ctx := context.Background()
	s := &fake.Source{Items: map[string]source.Item{
		"B": {ID: "B", Assignee: "bob"},
		"A": {ID: "A", Assignee: "alice"},
		"C": {ID: "C", Assignee: "alice"},
	}}

	ids, err := s.IDs(ctx)
	if err != nil {
		t.Fatalf("IDs() error = %v", err)
	}
	if !reflect.DeepEqual(ids, []string{"A", "B", "C"}) {
		t.Fatalf("IDs() with no query = %v", ids)
	}

	s.Query = func(it source.Item) bool { return it.Assignee == "alice" }
	ids, err = s.IDs(ctx)
	if err != nil {
		t.Fatalf("IDs() error = %v", err)
	}
	if !reflect.DeepEqual(ids, []string{"A", "C"}) {
		t.Fatalf("IDs() = %v, want [A C]", ids)
	}
	got, err := s.Sync(ctx, time.Time{})
	if err != nil {
		t.Fatalf("Sync() error = %v", err)
	}
	if ids := itemIDs(got); !reflect.DeepEqual(ids, []string{"A", "C"}) {
		t.Fatalf("Sync() ids = %v, want [A C]", ids)
	}
	if _, err := s.Get(ctx, "B"); err != nil {
		t.Fatalf("Get(B) outside the query error = %v", err)
	}
}

func TestGet(t *testing.T) {
	ctx := context.Background()
	s := &fake.Source{Items: map[string]source.Item{
		"A": {
			ID:        "A",
			Title:     "alpha",
			Labels:    []string{"x"},
			BlockedBy: []source.BlockRef{{ID: "Z", LinkID: "L-Z-A"}},
		},
	}}

	got, err := s.Get(ctx, "A")
	if err != nil {
		t.Fatalf("Get(A) error = %v", err)
	}
	got.Labels[0] = "changed"
	got.BlockedBy[0].ID = "nope"
	again, err := s.Get(ctx, "A")
	if err != nil {
		t.Fatalf("Get(A) again error = %v", err)
	}
	if again.Labels[0] != "x" || again.BlockedBy[0].ID != "Z" {
		t.Fatalf("Get returned a live alias: %+v", again)
	}

	_, err = s.Get(ctx, "missing")
	if !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("Get(missing) error = %v, want ErrNotFound", err)
	}
}

func TestClaimRelease(t *testing.T) {
	ctx := context.Background()
	s := &fake.Source{
		Account: "alice",
		Items: map[string]source.Item{
			"A": {ID: "A", Title: "alpha", State: "To Do", Assignee: "bob"},
		},
	}

	if err := s.Claim(ctx, "missing"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("Claim(missing) error = %v, want ErrNotFound", err)
	}
	if err := s.Release(ctx, "missing"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("Release(missing) error = %v, want ErrNotFound", err)
	}

	err := s.Release(ctx, "A")
	if err == nil || errors.Is(err, source.ErrNotFound) || !strings.Contains(err.Error(), "expected") {
		t.Fatalf("Release(unclaimed) error = %v", err)
	}
	if s.Items["A"].State != "To Do" || s.Items["A"].Assignee != "bob" {
		t.Fatalf("unclaimed release mutated the item: %+v", s.Items["A"])
	}

	if err := s.Claim(ctx, "A"); err != nil {
		t.Fatalf("Claim: %v", err)
	}
	if err := s.Claim(ctx, "A"); err != nil {
		t.Fatalf("second Claim: %v", err)
	}
	got := s.Items["A"]
	if got.State != "In Progress" || got.Assignee != "alice" || got.Title != "alpha" {
		t.Fatalf("after Claim: %+v", got)
	}

	if err := s.Release(ctx, "A"); err != nil {
		t.Fatalf("Release: %v", err)
	}
	got = s.Items["A"]
	if got.State != "To Do" || got.Assignee != "bob" || got.Title != "alpha" {
		t.Fatalf("after Release: %+v", got)
	}
	err = s.Release(ctx, "A")
	if err == nil || !strings.Contains(err.Error(), "expected") {
		t.Fatalf("second Release error = %v", err)
	}
}

func TestFinish(t *testing.T) {
	ctx := context.Background()
	s := &fake.Source{Items: map[string]source.Item{
		"A": {ID: "A", State: "In Progress"},
	}}

	if err := s.Finish(ctx, "missing"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("Finish(missing) error = %v, want ErrNotFound", err)
	}
	if err := s.Finish(ctx, "A"); err != nil {
		t.Fatalf("Finish: %v", err)
	}
	got := s.Items["A"]
	if !got.Done || got.State != "In Progress" {
		t.Fatalf("after Finish: %+v", got)
	}
}

func TestClose(t *testing.T) {
	ctx := context.Background()
	s := &fake.Source{Items: map[string]source.Item{
		"A": {ID: "A", State: "In Progress"},
		"B": {ID: "B", State: "In Progress"},
	}}

	var nilCtx context.Context
	if err := s.Close(nilCtx, "A", "done"); err == nil || !strings.Contains(err.Error(), "found nil") {
		t.Fatalf("Close(nil) error = %v", err)
	}
	if err := s.Close(ctx, "missing", "done"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("Close(missing) error = %v, want ErrNotFound", err)
	}
	if err := s.Close(ctx, "A", "Completed all subtasks."); err != nil {
		t.Fatalf("Close(A): %v", err)
	}
	if got := s.Items["A"]; !got.Done || got.State != "In Progress" {
		t.Fatalf("after Close(A): %+v", got)
	}
	if !reflect.DeepEqual(s.Comments["A"], []string{"Completed all subtasks."}) {
		t.Fatalf("Comments[A] = %#v, want [Completed all subtasks.]", s.Comments["A"])
	}
	if err := s.Close(ctx, "B", "   \n  "); err != nil {
		t.Fatalf("Close(B): %v", err)
	}
	if !s.Items["B"].Done {
		t.Fatalf("after Close(B): %+v", s.Items["B"])
	}
	if len(s.Comments["B"]) != 0 {
		t.Fatalf("Comments[B] = %#v, want empty", s.Comments["B"])
	}
}

func TestLinkUnlink(t *testing.T) {
	ctx := context.Background()
	s := &fake.Source{Items: map[string]source.Item{
		"A": {ID: "A"},
		"B": {ID: "B"},
		"C": {ID: "C"},
	}}

	if err := s.Link(ctx, "missing", "B"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("Link missing blocker error = %v", err)
	}
	if err := s.Link(ctx, "A", "missing"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("Link missing blocked error = %v", err)
	}
	if s.Items["B"].BlockedBy != nil {
		t.Fatalf("failed Link mutated B: %+v", s.Items["B"])
	}

	if err := s.Link(ctx, "A", "B"); err != nil {
		t.Fatalf("Link A B: %v", err)
	}
	if err := s.Link(ctx, "C", "B"); err != nil {
		t.Fatalf("Link C B: %v", err)
	}
	want := []source.BlockRef{
		{ID: "A", LinkID: "L-A-B"},
		{ID: "C", LinkID: "L-C-B"},
	}
	if !reflect.DeepEqual(s.Items["B"].BlockedBy, want) {
		t.Fatalf("BlockedBy = %+v, want %+v", s.Items["B"].BlockedBy, want)
	}

	err := s.Link(ctx, "A", "B")
	if err == nil || !strings.Contains(err.Error(), "expected") {
		t.Fatalf("duplicate Link error = %v", err)
	}
	if !reflect.DeepEqual(s.Items["B"].BlockedBy, want) {
		t.Fatalf("duplicate Link mutated BlockedBy: %+v", s.Items["B"].BlockedBy)
	}

	if err := s.Unlink(ctx, "A", "B"); err != nil {
		t.Fatalf("Unlink A B: %v", err)
	}
	want = []source.BlockRef{{ID: "C", LinkID: "L-C-B"}}
	if !reflect.DeepEqual(s.Items["B"].BlockedBy, want) {
		t.Fatalf("after Unlink A: %+v", s.Items["B"].BlockedBy)
	}
	if err := s.Unlink(ctx, "C", "B"); err != nil {
		t.Fatalf("Unlink C B: %v", err)
	}
	if s.Items["B"].BlockedBy != nil {
		t.Fatalf("BlockedBy = %+v, want nil", s.Items["B"].BlockedBy)
	}

	if err := s.Unlink(ctx, "A", "B"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("Unlink missing link error = %v", err)
	}
	if err := s.Unlink(ctx, "A", "missing"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("Unlink missing item error = %v", err)
	}
}

func TestCreate(t *testing.T) {
	ctx := context.Background()
	s := &fake.Source{
		SourceName: "mem",
		Items: map[string]source.Item{
			"B": {ID: "B", Title: "blocked"},
		},
	}
	labels := []string{"x", "y"}
	id, err := s.Create(ctx, source.CreateRequest{
		Title:     "T",
		Body:      "the body",
		Parent:    "P",
		Labels:    labels,
		BlockedBy: []string{"EXT-1"},
		Blocks:    []string{"B"},
	})
	if err != nil {
		t.Fatalf("Create: %v", err)
	}
	if id != "FAKE-1" {
		t.Fatalf("id = %q, want FAKE-1", id)
	}
	labels[0] = "changed"
	got := s.Items["FAKE-1"]
	if got.Source != "mem" || got.Title != "T" || got.Parent != "P" || !got.Created.IsZero() || !got.Updated.IsZero() {
		t.Fatalf("item = %+v", got)
	}
	if !reflect.DeepEqual(got.Labels, []string{"x", "y"}) {
		t.Fatalf("labels = %+v", got.Labels)
	}
	wantRef := []source.BlockRef{{ID: "EXT-1", LinkID: "L-EXT-1-FAKE-1"}}
	if !reflect.DeepEqual(got.BlockedBy, wantRef) {
		t.Fatalf("blocked by = %+v", got.BlockedBy)
	}
	wantBlocks := []source.BlockRef{{ID: "FAKE-1", LinkID: "L-FAKE-1-B"}}
	if !reflect.DeepEqual(s.Items["B"].BlockedBy, wantBlocks) {
		t.Fatalf("B blocked by = %+v", s.Items["B"].BlockedBy)
	}

	id2, err := s.Create(ctx, source.CreateRequest{Title: "U"})
	if err != nil {
		t.Fatalf("second Create: %v", err)
	}
	if id2 != "FAKE-2" {
		t.Fatalf("second id = %q, want FAKE-2", id2)
	}
}

func TestCreateRejects(t *testing.T) {
	ctx := context.Background()
	tests := []struct {
		name         string
		items        map[string]source.Item
		req          source.CreateRequest
		wantNotFound bool
	}{
		{
			name:         "missing block target",
			req:          source.CreateRequest{Blocks: []string{"B"}},
			wantNotFound: true,
		},
		{
			name: "empty blocked-by",
			req:  source.CreateRequest{BlockedBy: []string{""}},
		},
		{
			name:  "duplicate blocks",
			items: map[string]source.Item{"B": {ID: "B"}},
			req:   source.CreateRequest{Blocks: []string{"B", "B"}},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			s := &fake.Source{Items: tc.items}
			if _, err := s.Create(ctx, tc.req); err == nil {
				t.Fatal("error = nil, want an error")
			} else if errors.Is(err, source.ErrNotFound) != tc.wantNotFound {
				t.Fatalf("errors.Is(err, ErrNotFound) = %v, want %v; err = %v", errors.Is(err, source.ErrNotFound), tc.wantNotFound, err)
			} else if !strings.Contains(err.Error(), "expected") {
				t.Fatalf("error %q does not say what was expected", err)
			}
			if _, err := s.Create(ctx, source.CreateRequest{Title: "ok"}); err != nil {
				t.Fatalf("retry Create: %v", err)
			}
			if _, ok := s.Items["FAKE-1"]; !ok {
				t.Fatal("rejected create consumed an id")
			}
			if _, ok := s.Items["FAKE-2"]; ok {
				t.Fatal("retry did not reuse FAKE-1")
			}
		})
	}
}

func TestFailOnLeavesStateAndRecordsCall(t *testing.T) {
	ctx := context.Background()
	boom := errors.New("boom")
	base := source.Item{ID: "A", Title: "t", State: "To Do", Updated: time.Unix(10, 0).UTC()}
	other := source.Item{ID: "B", Title: "b"}
	tests := []struct {
		method string
		call   func(*fake.Source) error
	}{
		{method: "Me", call: func(s *fake.Source) error {
			_, err := s.Me(ctx)
			return err //nolint:wrapcheck // propagate context cancellation
		}},
		{method: "Sync", call: func(s *fake.Source) error {
			_, err := s.Sync(ctx, time.Time{})
			return err //nolint:wrapcheck // propagate context cancellation
		}},
		{method: "IDs", call: func(s *fake.Source) error {
			_, err := s.IDs(ctx)
			return err //nolint:wrapcheck // propagate context cancellation
		}},
		{method: "Get", call: func(s *fake.Source) error {
			_, err := s.Get(ctx, "A")
			return err //nolint:wrapcheck // propagate context cancellation
		}},
		{method: "Claim", call: func(s *fake.Source) error { return s.Claim(ctx, "A") }},
		{method: "Release", call: func(s *fake.Source) error { return s.Release(ctx, "A") }},
		{method: "Finish", call: func(s *fake.Source) error { return s.Finish(ctx, "A") }},
		{method: "Close", call: func(s *fake.Source) error { return s.Close(ctx, "A", "done") }},
		{method: "Link", call: func(s *fake.Source) error { return s.Link(ctx, "A", "B") }},
		{method: "Unlink", call: func(s *fake.Source) error { return s.Unlink(ctx, "A", "B") }},
		{method: "Create", call: func(s *fake.Source) error {
			_, err := s.Create(ctx, source.CreateRequest{Title: "n", Blocks: []string{"B"}})
			return err //nolint:wrapcheck // propagate context cancellation
		}},
		{method: "SetPriority", call: func(s *fake.Source) error { return s.SetPriority(ctx, "A", 0) }},
	}
	for _, tc := range tests {
		t.Run(tc.method, func(t *testing.T) {
			s := &fake.Source{
				SourceName: "mem",
				Account:    "alice",
				Items: map[string]source.Item{
					"A": base,
					"B": other,
				},
				FailOn: map[string]error{tc.method: boom},
			}
			if err := tc.call(s); !errors.Is(err, boom) {
				t.Fatalf("error = %v, want boom", err)
			}
			if got := s.Items["A"]; !reflect.DeepEqual(got, base) {
				t.Fatalf("A changed: %+v", got)
			}
			if got := s.Items["B"]; !reflect.DeepEqual(got, other) {
				t.Fatalf("B changed: %+v", got)
			}
			if _, ok := s.Items["FAKE-1"]; ok {
				t.Fatal("FailOn stored FAKE-1")
			}
			if len(s.Calls) != 1 || s.Calls[0].Method != tc.method {
				t.Fatalf("Calls = %+v", s.Calls)
			}
		})
	}
}

func TestCallLog(t *testing.T) {
	ctx := context.Background()
	since := time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC)
	s := &fake.Source{
		SourceName: "mem",
		Account:    "alice",
		Items: map[string]source.Item{
			"A": {ID: "A", State: "To Do"},
			"B": {ID: "B"},
		},
	}

	if got := s.Name(); got != "mem" {
		t.Fatalf("Name() = %q", got)
	}
	if got, err := s.Me(ctx); err != nil || got != "alice" {
		t.Fatalf("Me() = %q, %v", got, err)
	}
	if _, err := s.Sync(ctx, since); err != nil {
		t.Fatalf("Sync: %v", err)
	}
	if _, err := s.Get(ctx, "A"); err != nil {
		t.Fatalf("Get: %v", err)
	}
	if err := s.Claim(ctx, "A"); err != nil {
		t.Fatalf("Claim: %v", err)
	}
	if err := s.Release(ctx, "A"); err != nil {
		t.Fatalf("Release: %v", err)
	}
	if err := s.Finish(ctx, "A"); err != nil {
		t.Fatalf("Finish: %v", err)
	}
	if err := s.Close(ctx, "A", "closed"); err != nil {
		t.Fatalf("Close: %v", err)
	}
	if err := s.Link(ctx, "A", "B"); err != nil {
		t.Fatalf("Link: %v", err)
	}
	if err := s.Unlink(ctx, "A", "B"); err != nil {
		t.Fatalf("Unlink: %v", err)
	}
	id, err := s.Create(ctx, source.CreateRequest{
		Title:     "T",
		Body:      "body",
		Parent:    "P",
		Labels:    []string{"x", "y"},
		Blocks:    []string{"B"},
		BlockedBy: []string{"A"},
	})
	if err != nil {
		t.Fatalf("Create: %v", err)
	}
	if id != "FAKE-1" {
		t.Fatalf("Create id = %q", id)
	}
	if err := s.SetPriority(ctx, "A", 0); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	want := []fake.Call{
		{Method: "Name"},
		{Method: "Me"},
		{Method: "Sync", Args: []string{"2026-01-02T03:04:05Z"}},
		{Method: "Get", Args: []string{"A"}},
		{Method: "Claim", Args: []string{"A"}},
		{Method: "Release", Args: []string{"A"}},
		{Method: "Finish", Args: []string{"A"}},
		{Method: "Close", Args: []string{"A", "closed"}},
		{Method: "Link", Args: []string{"A", "B"}},
		{Method: "Unlink", Args: []string{"A", "B"}},
		{Method: "Create", Args: []string{"T", "body", "P", "x,y", "B", "A", ""}},
		{Method: "SetPriority", Args: []string{"A", "0"}},
	}
	if !reflect.DeepEqual(s.Calls, want) {
		t.Fatalf("Calls = %#v\nwant %#v", s.Calls, want)
	}
}

func TestConcurrentAccess(t *testing.T) {
	s := &fake.Source{
		SourceName: "mem",
		Account:    "alice",
		Items: map[string]source.Item{
			"A": {ID: "A", Updated: time.Unix(1, 0).UTC()},
			"B": {ID: "B", Updated: time.Unix(2, 0).UTC()},
		},
	}
	ctx := context.Background()
	var wg sync.WaitGroup
	for range 32 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			_ = s.Name()
			_, _ = s.Me(ctx)
			_, _ = s.Sync(ctx, time.Time{})
			_, _ = s.Get(ctx, "A")
			_ = s.Claim(ctx, "A")
			_ = s.Release(ctx, "A")
			_ = s.Finish(ctx, "B")
			_ = s.Close(ctx, "B", "done")
			_ = s.Link(ctx, "A", "B")
			_ = s.Unlink(ctx, "A", "B")
			_, _ = s.Create(ctx, source.CreateRequest{Title: "t", Blocks: []string{"B"}})
			_ = s.SetPriority(ctx, "A", 1)
		}()
	}
	wg.Wait()

	if _, err := s.Get(ctx, "A"); err != nil {
		t.Fatalf("Get A after concurrent access: %v", err)
	}
	if _, err := s.Get(ctx, "B"); err != nil {
		t.Fatalf("Get B after concurrent access: %v", err)
	}
}

func itemIDs(items []source.Item) []string {
	out := make([]string, len(items))
	for i, it := range items {
		out[i] = it.ID
	}
	return out
}

func TestClaimWithOptionsAndRelease(t *testing.T) {
	boolPtr := func(v bool) *bool { return &v }
	s := &fake.Source{
		Account:    "alice",
		ClaimLabel: "in-progress",
		Items: map[string]source.Item{
			"A": {ID: "A", State: "To Do", Assignee: "bob", Labels: []string{"bug"}},
			"B": {ID: "B", State: "To Do", Assignee: "bob", Labels: []string{"bug"}},
		},
	}
	ctx := context.Background()

	if err := s.ClaimWithOptions(ctx, "A", source.ClaimOptions{
		Assign:          boolPtr(false),
		MarkInProgress:  boolPtr(true),
		RemoveAssignees: []string{"bob"},
	}); err != nil {
		t.Fatalf("ClaimWithOptions(A): %v", err)
	}
	gotA, _ := s.Get(ctx, "A")
	if gotA.State != "In Progress" || gotA.Assignee != "bob" || !reflect.DeepEqual(gotA.Labels, []string{"bug", "in-progress"}) {
		t.Fatalf("A after claim = %+v", gotA)
	}
	if !reflect.DeepEqual(s.LastClaimOptions["A"].RemoveAssignees, []string{"bob"}) {
		t.Fatalf("LastClaimOptions[A] = %+v", s.LastClaimOptions["A"])
	}
	if err := s.Release(ctx, "A"); err != nil {
		t.Fatalf("Release(A): %v", err)
	}
	gotA, _ = s.Get(ctx, "A")
	if gotA.State != "To Do" || gotA.Assignee != "bob" || !reflect.DeepEqual(gotA.Labels, []string{"bug"}) {
		t.Fatalf("A after release = %+v", gotA)
	}

	if err := s.ClaimWithOptions(ctx, "B", source.ClaimOptions{
		Assign:         boolPtr(true),
		MarkInProgress: boolPtr(false),
	}); err != nil {
		t.Fatalf("ClaimWithOptions(B): %v", err)
	}
	gotB, _ := s.Get(ctx, "B")
	if gotB.State != "To Do" || gotB.Assignee != "alice" || !reflect.DeepEqual(gotB.Labels, []string{"bug"}) {
		t.Fatalf("B after claim = %+v", gotB)
	}
}

func TestClaimComments(t *testing.T) {
	t1 := time.Date(2026, 11, 24, 1, 0, 0, 0, time.UTC)
	t2 := time.Date(2026, 11, 24, 2, 0, 0, 0, time.UTC)
	s := &fake.Source{
		Account: "alice",
		Now:     func() time.Time { return t2 },
		Items: map[string]source.Item{
			"A": {ID: "A"},
		},
		ClaimComments: map[string][]source.ClaimComment{
			"A": {
				{Author: "bob", CreatedAt: t1, Body: "Claimed by claude on mac1"},
				{Author: "bob", CreatedAt: t2.Add(time.Minute), Body: "Regular note"},
			},
		},
	}
	ctx := context.Background()

	got, err := s.LatestClaimComment(ctx, "A")
	if err != nil {
		t.Fatalf("LatestClaimComment(A): %v", err)
	}
	if got == nil || got.Author != "bob" || !got.CreatedAt.Equal(t1) {
		t.Fatalf("LatestClaimComment(A) = %+v", got)
	}

	if err := s.PostClaimComment(ctx, "A", "Claimed by claude on mac2"); err != nil {
		t.Fatalf("PostClaimComment(A): %v", err)
	}
	got, err = s.LatestClaimComment(ctx, "A")
	if err != nil {
		t.Fatalf("LatestClaimComment(A) after post: %v", err)
	}
	if got == nil || got.Author != "alice" || !got.CreatedAt.Equal(t2) || got.Body != "Claimed by claude on mac2" {
		t.Fatalf("LatestClaimComment(A) after post = %+v", got)
	}

	if err := s.PostClaimComment(ctx, "A", "   "); err == nil {
		t.Fatal("PostClaimComment(blank) = nil, want error")
	}
	if _, err := s.LatestClaimComment(ctx, "missing"); !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("LatestClaimComment(missing) = %v, want ErrNotFound", err)
	}
}
