package contacts

import (
	"testing"
	"time"
)

func TestAccessLogFiltersNewestFirst(t *testing.T) {
	s := newStore(t)
	now := fixedNow
	s.now = func() time.Time { return now }
	for _, a := range []Access{
		{Caller: "ops", Command: "resolve", PersonID: "ada"},
		{Caller: "ops", Command: "history", PersonID: "bob", Reason: "prep"},
		{Caller: "inbox", Command: "history", PersonID: "ada", Reason: "reply"},
	} {
		if err := s.LogAccess(a.Caller, a.Command, a.PersonID, a.Reason); err != nil {
			t.Fatal(err)
		}
		now = now.Add(24 * time.Hour)
	}
	all, err := s.AccessLog(AccessFilter{})
	if err != nil {
		t.Fatal(err)
	}
	if len(all) != 3 || all[0].Caller != "inbox" || all[2].Command != "resolve" {
		t.Errorf("all = %+v", all)
	}
	for _, tc := range []struct {
		f    AccessFilter
		want int
	}{
		{AccessFilter{Caller: "ops"}, 2},
		{AccessFilter{PersonID: "ada"}, 2},
		{AccessFilter{Caller: "ops", PersonID: "ada"}, 1},
		{AccessFilter{Since: "2026-03-11T00:00:00Z"}, 2},
	} {
		got, err := s.AccessLog(tc.f)
		if err != nil {
			t.Fatal(err)
		}
		if len(got) != tc.want {
			t.Errorf("%+v: %d rows, want %d", tc.f, len(got), tc.want)
		}
	}
}
