package ledger

import (
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestStoreAppendQueryDedupeAndDailyCap(t *testing.T) {
	t.Parallel()
	path := filepath.Join(t.TempDir(), "ledger.jsonl")
	s := &Store{Path: path}

	day1 := time.Date(2026, 6, 1, 10, 0, 0, 0, time.UTC)
	day2 := time.Date(2026, 6, 2, 9, 0, 0, 0, time.UTC)

	// Missing file loads empty.
	got, err := s.Load()
	if err != nil || len(got) != 0 {
		t.Fatalf("empty Load() = %+v, %v", got, err)
	}

	entries := []Entry{
		{PR: "acme/widgets#1", HeadSHA: "aaa11112222", Kind: "rebase", Time: day1, Outcome: OutcomeDispatched},
		{PR: "acme/widgets#1", HeadSHA: "aaa11112222", Kind: "ci-fix", Time: day1.Add(time.Minute), Outcome: OutcomeDryRun},
		{PR: "acme/widgets#2", HeadSHA: "bbb22223333", Kind: "queue-fix", Time: day2, Outcome: OutcomeTimeout, Detail: "timed out"},
		{PR: "acme/widgets#3", HeadSHA: "ccc33334444", Kind: "nit-reply", Time: day2.Add(time.Hour), Outcome: OutcomeDispatched},
	}
	for _, e := range entries {
		if err := s.Append(e); err != nil {
			t.Fatalf("Append(%+v) error = %v", e, err)
		}
	}

	// Dedupe checks.
	deduped, err := s.HasDispatched("acme/widgets#1", "aaa11112222", "rebase")
	if err != nil || !deduped {
		t.Fatalf("HasDispatched(rebase) = %v, %v, want true", deduped, err)
	}
	// dry-run does not count as dispatched.
	deduped, err = s.HasDispatched("acme/widgets#1", "aaa11112222", "ci-fix")
	if err != nil || deduped {
		t.Fatalf("HasDispatched(ci-fix dry-run) = %v, %v, want false", deduped, err)
	}
	// Different head SHA does not match.
	deduped, err = s.HasDispatched("acme/widgets#1", "newsha", "rebase")
	if err != nil || deduped {
		t.Fatalf("HasDispatched(new head) = %v, %v, want false", deduped, err)
	}

	// Daily cap counting.
	c1, err := s.CountToday(day1.Add(2 * time.Hour))
	if err != nil || c1 != 1 {
		t.Fatalf("CountToday(day1) = %d, %v, want 1", c1, err)
	}
	c2, err := s.CountToday(day2.Add(3 * time.Hour))
	if err != nil || c2 != 2 {
		t.Fatalf("CountToday(day2) = %d, %v, want 2", c2, err)
	}

	// Query filters.
	q, err := s.Query(Filter{PR: "acme/widgets#1", Kind: "rebase"})
	if err != nil || len(q) != 1 || q[0].HeadSHA != "aaa11112222" {
		t.Fatalf("Query(PR+Kind) = %+v, %v", q, err)
	}
	q, err = s.Query(Filter{Limit: 2})
	if err != nil || len(q) != 2 || q[0].PR != "acme/widgets#2" || q[1].PR != "acme/widgets#3" {
		t.Fatalf("Query(Limit=2) = %+v, %v", q, err)
	}

	// Human-readable output.
	lines := FormatLines(q)
	if !strings.Contains(lines, "acme/widgets#2") || !strings.Contains(lines, "bbb2222") || !strings.Contains(lines, "timed out") {
		t.Fatalf("FormatLines() = %q", lines)
	}
}

func TestWaiveRepostLedgerDedupeCapAndFormat(t *testing.T) {
	t.Parallel()
	path := filepath.Join(t.TempDir(), "ledger.jsonl")
	s := &Store{Path: path}
	day := time.Date(2026, 6, 1, 10, 0, 0, 0, time.UTC)

	if err := s.Append(Entry{
		PR:         "acme/widgets#12",
		HeadSHA:    "newhead1111111",
		OldHeadSHA: "oldhead0000000",
		NewHeadSHA: "newhead1111111",
		CommentID:  "987654",
		Kind:       KindWaiveRepost,
		Time:       day,
		Outcome:    OutcomeDispatched,
	}); err != nil {
		t.Fatalf("Append() error = %v", err)
	}
	// A failed attempt must neither dedupe nor count toward the cap.
	if err := s.Append(Entry{
		PR:         "acme/widgets#12",
		HeadSHA:    "newhead2222222",
		OldHeadSHA: "newhead1111111",
		NewHeadSHA: "newhead2222222",
		Kind:       KindWaiveRepost,
		Time:       day.Add(time.Minute),
		Outcome:    OutcomeFailed,
	}); err != nil {
		t.Fatalf("Append(failed) error = %v", err)
	}

	// Reload from disk to confirm restart persistence.
	reloaded := &Store{Path: path}
	deduped, err := reloaded.HasWaiveReposted("acme/widgets#12", "newhead1111111")
	if err != nil || !deduped {
		t.Fatalf("HasWaiveReposted(newhead1) = %v, %v, want true", deduped, err)
	}
	deduped, err = reloaded.HasWaiveReposted("acme/widgets#12", "newhead2222222")
	if err != nil || deduped {
		t.Fatalf("HasWaiveReposted(failed head) = %v, %v, want false", deduped, err)
	}
	count, err := reloaded.CountWaiveReposts("acme/widgets#12")
	if err != nil || count != 1 {
		t.Fatalf("CountWaiveReposts() = %d, %v, want 1", count, err)
	}
	// Waive re-posts do not consume the policy daily dispatch cap.
	today, err := reloaded.CountToday(day)
	if err != nil || today != 0 {
		t.Fatalf("CountToday() = %d, %v, want 0", today, err)
	}

	entries, err := reloaded.Query(Filter{PR: "acme/widgets#12", Kind: KindWaiveRepost})
	if err != nil || len(entries) != 2 {
		t.Fatalf("Query() = %+v, %v", entries, err)
	}
	lines := FormatLines(entries[:1])
	for _, want := range []string{"acme/widgets#12", KindWaiveRepost, "oldhead0000000", "newhead1111111", "987654"} {
		if !strings.Contains(lines, want) {
			t.Fatalf("FormatLines() = %q, missing %q", lines, want)
		}
	}
}
