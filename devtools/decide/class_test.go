package decide

import (
	"errors"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestParseClassAndPrefix(t *testing.T) {
	cases := []struct {
		input  string
		class  string
		prefix string
	}{
		{"decision", ClassDecision, "D"},
		{"D", ClassDecision, "D"},
		{"d", ClassDecision, "D"},
		{"approval", ClassApproval, "A"},
		{"A", ClassApproval, "A"},
		{"request", ClassRequest, "R"},
		{"R", ClassRequest, "R"},
		{"fact", ClassFact, "Q"},
		{"Q", ClassFact, "Q"},
		{"inbound", ClassInbound, "I"},
		{"I", ClassInbound, "I"},
		{"task", ClassTask, "T"},
		{"T", ClassTask, "T"},
		{"note", ClassNote, "N"},
		{"N", ClassNote, "N"},
	}
	for _, tc := range cases {
		got, err := ParseClass(tc.input)
		if err != nil {
			t.Errorf("ParseClass(%q) error = %v", tc.input, err)
			continue
		}
		if got != tc.class {
			t.Errorf("ParseClass(%q) = %q, want %q", tc.input, got, tc.class)
		}
		pfx, err := ClassPrefix(got)
		if err != nil || pfx != tc.prefix {
			t.Errorf("ClassPrefix(%q) = %q, %v, want %q", got, pfx, err, tc.prefix)
		}
	}
	if _, err := ParseClass("bogus"); err == nil {
		t.Fatal("ParseClass(bogus) succeeded, want error")
	}
	if _, err := ParseClass(""); err == nil {
		t.Fatal("ParseClass('') succeeded, want error")
	}
}

func TestParseIDAcceptsAnyPrefixOrBareNumber(t *testing.T) {
	for _, raw := range []string{"860", "0860", "D-860", "d-860", "A-860", "R-860", "Q-860", "I-860", "T-860", "N-860"} {
		n, ok := parseID(raw)
		if !ok || n != 860 {
			t.Errorf("parseID(%q) = (%d, %v), want (860, true)", raw, n, ok)
		}
	}
	for _, bad := range []string{"", "0", "-1", "X-860", "D-0", "D-abc", "DD-1"} {
		if n, ok := parseID(bad); ok {
			t.Errorf("parseID(%q) = (%d, true), want false", bad, n)
		}
	}
}

func TestLedgerClassesResolveRetagParentAndPrune(t *testing.T) {
	led, err := Open(filepath.Join(t.TempDir(), "decide"))
	if err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time { return now })

	// Each class files with its prefix on a shared counter.
	classes := []struct {
		class  string
		prefix string
		opts   []Option
		verb   string
	}{
		{ClassDecision, "D-", []Option{{Key: "a", Label: "Alpha", Recommended: true}, {Key: "b", Label: "Beta"}}, ""},
		{ClassApproval, "A-", []Option{{Key: "yes", Label: "Approve", Recommended: true}, {Key: "no", Label: "Reject"}, {Key: "edit", Label: "Edit"}}, "exact payload"},
		{ClassRequest, "R-", nil, ""},
		{ClassFact, "Q-", nil, ""},
		{ClassInbound, "I-", []Option{{Key: "handle", Label: "Handle", Recommended: true}}, ""},
		{ClassTask, "T-", nil, ""},
		{ClassNote, "N-", nil, ""},
	}
	for i, tc := range classes {
		it, err := led.Create(&Item{
			Question: "item " + tc.class,
			Verbatim: tc.verb,
			Owner:    "ada",
			Kind:     KindRelay,
			Class:    tc.class,
			Options:  tc.opts,
		})
		if err != nil {
			t.Fatalf("Create(%s) error = %v", tc.class, err)
		}
		if !strings.HasPrefix(it.ID, tc.prefix) {
			t.Fatalf("Create(%s).ID = %q, want prefix %q", tc.class, it.ID, tc.prefix)
		}
		if n, ok := parseID(it.ID); !ok || n != i+1 {
			t.Fatalf("Create(%s).ID = %q, want number %d", tc.class, it.ID, i+1)
		}
	}

	// Any prefix or bare number resolves to the same item (item 2 is A-002).
	for _, ref := range []string{"2", "002", "A-002", "D-002", "N-002", "a-2"} {
		got, err := led.Get(ref)
		if err != nil {
			t.Fatalf("Get(%q) error = %v", ref, err)
		}
		if got.ID != "A-002" {
			t.Fatalf("Get(%q).ID = %q, want A-002", ref, got.ID)
		}
	}

	// Parent must exist and is normalized to canonical ID.
	if _, err := led.Create(&Item{
		Question: "orphan approval",
		Verbatim: "payload",
		Owner:    "ada",
		Kind:     KindRelay,
		Class:    ClassApproval,
		Parent:   "D-999",
		Options:  []Option{{Key: "yes", Label: "Yes", Recommended: true}},
	}); !errors.Is(err, ErrNotFound) {
		t.Fatalf("Create with missing parent = %v, want ErrNotFound", err)
	}
	child, err := led.Create(&Item{
		Question: "linked approval",
		Verbatim: "payload",
		Owner:    "ada",
		Kind:     KindRelay,
		Class:    ClassApproval,
		Parent:   "1",
		Options:  []Option{{Key: "yes", Label: "Yes", Recommended: true}},
	})
	if err != nil {
		t.Fatalf("Create with parent=1 error = %v", err)
	}
	if child.Parent != "D-001" {
		t.Fatalf("child.Parent = %q, want D-001", child.Parent)
	}

	// Approval without verbatim or with bad option key is refused.
	if _, err := led.Create(&Item{
		Question: "no verbatim",
		Owner:    "ada",
		Kind:     KindRelay,
		Class:    ClassApproval,
		Options:  []Option{{Key: "yes", Label: "Yes", Recommended: true}},
	}); err == nil {
		t.Fatal("approval without verbatim succeeded, want error")
	}
	if _, err := led.Create(&Item{
		Question: "bad key",
		Verbatim: "payload",
		Owner:    "ada",
		Kind:     KindRelay,
		Class:    ClassApproval,
		Options:  []Option{{Key: "post", Label: "Post", Recommended: true}},
	}); err == nil {
		t.Fatal("approval with key 'post' succeeded, want error")
	}

	// Note with options is refused, Answer and Done are refused, Archive works.
	if _, err := led.Create(&Item{
		Question: "note with opt",
		Owner:    "ada",
		Kind:     KindRelay,
		Class:    ClassNote,
		Options:  []Option{{Key: "ok", Label: "OK", Recommended: true}},
	}); err == nil {
		t.Fatal("note with options succeeded, want error")
	}
	if _, err := led.Answer("N-007", "hi", ViaCLI); err == nil {
		t.Fatal("Answer on note succeeded, want error")
	}
	if _, err := led.Done("N-007", "closed"); err == nil {
		t.Fatal("Done on note succeeded, want error")
	}
	archived, err := led.Archive("N-007", now.Add(time.Minute))
	if err != nil || archived.State != StateDone {
		t.Fatalf("Archive(N-007) = %+v, %v", archived, err)
	}

	// Decision Done with >2 lines is refused, <=2 lines succeeds.
	if _, err := led.Done("D-001", "line 1\nline 2\nline 3"); err == nil {
		t.Fatal("decision Done with 3 lines succeeded, want error")
	}
	if _, err := led.DoneVia("D-001", "chose A because of B", ViaUI); err != nil {
		t.Fatalf("decision Done with 1 line error = %v", err)
	}

	// Retag changes prefix and records system event.
	retagged, err := led.Retag("R-003", ClassTask, "agent-1")
	if err != nil {
		t.Fatalf("Retag error = %v", err)
	}
	if retagged.ID != "T-003" || retagged.Class != ClassTask {
		t.Fatalf("Retag = %+v, want T-003 / task", retagged)
	}
	lastMsg := retagged.Messages[len(retagged.Messages)-1]
	if lastMsg.Author != "system" || !strings.Contains(lastMsg.Text, "request") || !strings.Contains(lastMsg.Text, "task") {
		t.Fatalf("Retag event = %+v", lastMsg)
	}

	// Close A-002 and prune before future cutoff: A-002 stays, D-001 and N-007 are pruned.
	if _, err := led.DoneVia("A-002", "approved and posted", ViaUI); err != nil {
		t.Fatalf("Done(A-002) error = %v", err)
	}
	pruned, err := led.PruneBefore(now.Add(time.Hour))
	if err != nil {
		t.Fatalf("PruneBefore error = %v", err)
	}
	if pruned != 2 {
		t.Fatalf("pruned = %d, want 2 (D-001 and N-007)", pruned)
	}
	if _, err := led.Get("A-002"); err != nil {
		t.Fatalf("approval A-002 was pruned: %v", err)
	}
	if _, err := led.Get("D-001"); !errors.Is(err, ErrNotFound) {
		t.Fatalf("D-001 still present after prune: %v", err)
	}
}
