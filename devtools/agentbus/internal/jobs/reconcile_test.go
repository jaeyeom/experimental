package jobs

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"
)

// lostJob puts one job of kind into unknown: claimed, then its heartbeat lost.
func lostJob(t *testing.T, s *Store, c *clock, kind string) Job {
	t.Helper()
	j := mustEnqueue(t, s, Args{Kind: kind, Subject: "o/" + kind + "#1", Head: "abc"})
	mustClaim(t, s, "r1")
	c.t = c.t.Add(time.Hour)
	if _, err := s.Expire(); err != nil {
		t.Fatal(err)
	}
	return show(t, s, j.ID)
}

type asks struct{ texts []string }

func (a *asks) ask(text string) { a.texts = append(a.texts, text) }

func TestReconcileSettlesWorkAlreadyDone(t *testing.T) {
	cfg := Config{Kinds: map[string]Kind{"merge": {Reconcile: []string{"check-merged"}}}}
	s, c, _ := newStore(t, cfg)
	j := lostJob(t, s, c, "merge")
	var seen Job
	hook := func(_ context.Context, argv []string, job Job) (Outcome, string, error) {
		if argv[0] != "check-merged" {
			t.Errorf("hook argv = %v", argv)
		}
		seen = job
		return OutcomeDone, "PR merged at abc", nil
	}
	var a asks
	Reconcile(context.Background(), s, hook, a.ask)
	if seen.ID != j.ID {
		t.Fatalf("hook saw %q, want %s", seen.ID, j.ID)
	}
	got := show(t, s, j.ID)
	if got.State != StateDone || !strings.Contains(got.Note, "PR merged") {
		t.Fatalf("job = %s %q, want done with the hook's note", got.State, got.Note)
	}
	if len(a.texts) != 0 {
		t.Fatalf("asked %v, want no decide item", a.texts)
	}
}

func TestReconcileSettlesWorkNotDoneAsFailed(t *testing.T) {
	cfg := Config{Kinds: map[string]Kind{"merge": {Reconcile: []string{"check-merged"}}}}
	s, c, _ := newStore(t, cfg)
	j := lostJob(t, s, c, "merge")
	hook := func(context.Context, []string, Job) (Outcome, string, error) { return OutcomeFailed, "not merged", nil }
	Reconcile(context.Background(), s, hook, func(string) {})
	if got := show(t, s, j.ID); got.State != StateFailed {
		t.Fatalf("job = %s, want failed (never resent)", got.State)
	}
}

func TestReconcileUnclearRaisesOneDecideItem(t *testing.T) {
	cfg := Config{Kinds: map[string]Kind{
		"merge":   {Reconcile: []string{"check-merged"}},
		"comment": {OnBlocked: OnBlockedPark, Reconcile: []string{"check-comment"}},
	}}
	s, c, _ := newStore(t, cfg)
	merge := lostJob(t, s, c, "merge")
	comment := lostJob(t, s, c, "comment")
	noHook := lostJob(t, s, c, "push")
	calls := 0
	hook := func(_ context.Context, argv []string, _ Job) (Outcome, string, error) {
		calls++
		if argv[0] == "check-merged" {
			return OutcomeUnclear, "head moved", nil
		}
		return "", "", errors.New("exit status 2")
	}
	var a asks
	Reconcile(context.Background(), s, hook, a.ask)
	if calls != 2 {
		t.Fatalf("hook ran %d times, want 2", calls)
	}
	if len(a.texts) != 3 {
		t.Fatalf("asked %d times, want 3: %v", len(a.texts), a.texts)
	}
	for i, id := range []string{merge.ID, comment.ID, noHook.ID} {
		if !strings.Contains(a.texts[i], id) || !strings.Contains(a.texts[i], "agentbus jobs done "+id) {
			t.Errorf("ask %d = %q, want it to name %s and how to settle it", i, a.texts[i], id)
		}
		got := show(t, s, id)
		if !got.Asked || (got.State != StateUnknown && got.State != StateParked) {
			t.Errorf("%s = %s asked=%v, want still unsettled and asked", id, got.State, got.Asked)
		}
	}
	// The next pass asks nothing again.
	Reconcile(context.Background(), s, hook, a.ask)
	if len(a.texts) != 3 || calls != 2 {
		t.Fatalf("second pass asked %d and ran %d, want no new work", len(a.texts), calls)
	}
}

func TestReconcileLeavesAJobSettledMeanwhile(t *testing.T) {
	cfg := Config{Kinds: map[string]Kind{"merge": {Reconcile: []string{"check-merged"}}}}
	s, c, _ := newStore(t, cfg)
	j := lostJob(t, s, c, "merge")
	hook := func(context.Context, []string, Job) (Outcome, string, error) {
		if _, err := s.Fail(Args{ID: j.ID, Note: "by hand"}); err != nil {
			t.Fatal(err)
		}
		return OutcomeDone, "", nil
	}
	Reconcile(context.Background(), s, hook, func(string) {})
	if got := show(t, s, j.ID); got.State != StateFailed {
		t.Fatalf("job = %s, want the user's failed to stand", got.State)
	}
}

func TestExecHook(t *testing.T) {
	j := Job{ID: "j-7", Kind: "merge", Subject: "o/r#1", Head: "abc"}
	run := ExecHook(5*time.Second, nil)
	for _, tc := range []struct {
		script string
		want   Outcome
		note   string
		err    bool
	}{
		{script: `test "$JOB_ID $JOB_KIND $JOB_SUBJECT $JOB_HEAD" = "j-7 merge o/r#1 abc" && echo "done merged"`, want: OutcomeDone, note: "merged"},
		{script: `echo failed`, want: OutcomeFailed},
		{script: `echo maybe`, want: OutcomeUnclear, note: "maybe"},
		{script: `echo done; exit 3`, err: true},
	} {
		got, note, err := run(context.Background(), []string{"sh", "-c", tc.script}, j)
		if (err != nil) != tc.err || got != tc.want || note != tc.note {
			t.Errorf("%q = %q %q %v, want %q %q err=%v", tc.script, got, note, err, tc.want, tc.note, tc.err)
		}
	}
}
