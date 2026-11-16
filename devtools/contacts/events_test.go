package contacts

import (
	"reflect"
	"regexp"
	"strings"
	"testing"
)

func validEvent(person string) Event {
	return Event{
		PersonID: person, Date: "2026-03-01", Channel: "chat",
		Link: "https://chat.example/x/1", Summary: "agreed to split the API change", Reporter: "ops/inbox",
	}
}

func TestValidateEvent(t *testing.T) {
	for name, mutate := range map[string]func(*Event){
		"no channel":    func(e *Event) { e.Channel = "" },
		"no reporter":   func(e *Event) { e.Reporter = "" },
		"no link":       func(e *Event) { e.Link = "" },
		"relative link": func(e *Event) { e.Link = "/x/1" },
		"non-http link": func(e *Event) { e.Link = "ftp://host/x" },
		"not a url":     func(e *Event) { e.Link = "see chat" },
		"multi-line":    func(e *Event) { e.Summary = "one\ntwo" },
		"carriage ret":  func(e *Event) { e.Summary = "one\rtwo" },
		"empty summary": func(e *Event) { e.Summary = "  " },
		"over-long":     func(e *Event) { e.Summary = strings.Repeat("é", 201) },
		"bad date":      func(e *Event) { e.Date = "03/01/2026" },
	} {
		e := validEvent("ada")
		mutate(&e)
		if err := ValidateEvent(e, DefaultSummaryMax); err == nil {
			t.Errorf("%s: accepted", name)
		}
	}
	e := validEvent("ada")
	e.Summary = strings.Repeat("é", 200)
	if err := ValidateEvent(e, DefaultSummaryMax); err != nil {
		t.Errorf("200 characters refused: %v", err)
	}
	if err := ValidateEvent(e, 100); err == nil {
		t.Error("configured cap not applied")
	}
}

func TestReportDuplicateIsNoOp(t *testing.T) {
	s := newStore(t)
	ada := mustAdd(t, s, Person{Name: "Ada"})
	id, dup, err := s.Report(validEvent(ada), DefaultSummaryMax)
	if err != nil || dup {
		t.Fatalf("Report = %d, %v, %v", id, dup, err)
	}
	again := validEvent(ada)
	again.Date = "2026-03-02"
	id2, dup, err := s.Report(again, DefaultSummaryMax)
	if err != nil || !dup || id2 != id {
		t.Errorf("duplicate Report = %d, %v, %v; want %d, true", id2, dup, err, id)
	}
	if n := count(t, s, `SELECT count(*) FROM events`); n != 1 {
		t.Errorf("events = %d, want 1", n)
	}
}

// TestNoEventUpdatePath guards the append-only rule: the store offers no
// method that edits or deletes a single event.
func TestNoEventUpdatePath(t *testing.T) {
	forbidden := regexp.MustCompile(`(?i)^(update|edit|set|change|delete|remove|correct)event`)
	st := reflect.TypeOf(&Store{})
	for i := range st.NumMethod() {
		if name := st.Method(i).Name; forbidden.MatchString(name) {
			t.Errorf("Store.%s edits an event; events are append-only", name)
		}
	}
}
