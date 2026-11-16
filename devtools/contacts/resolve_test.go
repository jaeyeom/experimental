package contacts

import (
	"errors"
	"testing"
)

func TestResolvePrecedence(t *testing.T) {
	s := newStore(t)
	// Bob's name is another person's chat handle: the handle wins.
	ada := mustAdd(t, s, Person{Name: "Ada", Handles: []Handle{mustHandle(t, "chat:bob")}})
	mustAdd(t, s, Person{Name: "Bob"})
	if p, err := s.Resolve("bob"); err != nil || p.ID != ada {
		t.Errorf("Resolve(bob) = %q, %v; want %q", p.ID, err, ada)
	}
	if p, err := s.Resolve("ad"); err != nil || p.ID != ada {
		t.Errorf("prefix Resolve(ad) = %q, %v", p.ID, err)
	}
	if _, err := s.Find("ad"); !errors.Is(err, ErrNotFound) {
		t.Errorf("Find(ad) = %v; Find must not prefix-match", err)
	}
}

func TestDetailLocalTime(t *testing.T) {
	s := newStore(t)
	id := mustAdd(t, s, Person{Name: "Ada", TZ: "Asia/Seoul"})
	p, err := s.Get(id)
	if err != nil {
		t.Fatal(err)
	}
	// fixedNow is 09:30 UTC, 18:30 in Seoul.
	if got := s.Detail(p).LocalTime; got != "2026-03-10 18:30 +09:00" {
		t.Errorf("LocalTime = %q", got)
	}
}
