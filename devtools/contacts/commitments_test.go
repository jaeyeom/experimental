package contacts

import (
	"strings"
	"testing"
)

func TestCommitments(t *testing.T) {
	s := newStore(t) // today is 2026-03-10
	ada := mustAdd(t, s, Person{Name: "Ada"})
	bob := mustAdd(t, s, Person{Name: "Bob"})
	mk := func(person, dir, text, due string) int64 {
		t.Helper()
		id, err := s.Commit(Commitment{PersonID: person, Direction: dir, Text: text, Due: due, Reporter: "ops"})
		if err != nil {
			t.Fatal(err)
		}
		return id
	}
	late := mk(ada, "mine", "send the draft", "2026-03-09")
	mk(ada, "theirs", "review the API", "2026-03-10")
	done := mk(bob, "theirs", "share the logs", "")

	if _, err := s.CloseCommitment(done, "done", "got them"); err != nil {
		t.Fatal(err)
	}
	if _, err := s.CloseCommitment(done, "dropped", ""); err == nil || !strings.Contains(err.Error(), "already done") {
		t.Errorf("closing a closed commitment = %v", err)
	}
	if _, err := s.CloseCommitment(late, "finished", ""); err == nil {
		t.Error("bad status accepted")
	}

	for _, tc := range []struct {
		f    CommitmentFilter
		want []int64
	}{
		{CommitmentFilter{}, []int64{1, 2, 3}},
		{CommitmentFilter{PersonID: bob}, []int64{3}},
		{CommitmentFilter{Open: true}, []int64{1, 2}},
		{CommitmentFilter{Overdue: true}, []int64{1}},
		{CommitmentFilter{PersonID: bob, Open: true}, nil},
	} {
		got, err := s.Commitments(tc.f)
		if err != nil {
			t.Fatal(err)
		}
		var ids []int64
		for _, c := range got {
			ids = append(ids, c.ID)
			if c.Overdue != (c.ID == late) {
				t.Errorf("commitment %d overdue = %v", c.ID, c.Overdue)
			}
		}
		if len(ids) != len(tc.want) || (len(ids) > 0 && ids[0] != tc.want[0]) {
			t.Errorf("%+v = %v, want %v", tc.f, ids, tc.want)
		}
	}
}

func TestValidateCommitment(t *testing.T) {
	ok := Commitment{PersonID: "a", Direction: "mine", Text: "t", Reporter: "r"}
	for name, mutate := range map[string]func(*Commitment){
		"direction": func(c *Commitment) { c.Direction = "both" },
		"text":      func(c *Commitment) { c.Text = "" },
		"reporter":  func(c *Commitment) { c.Reporter = "" },
		"due":       func(c *Commitment) { c.Due = "friday" },
		"link":      func(c *Commitment) { c.Link = "x" },
	} {
		c := ok
		mutate(&c)
		if err := ValidateCommitment(c); err == nil {
			t.Errorf("bad %s accepted", name)
		}
	}
}
