package leasecache

import (
	"testing"
	"time"
)

const (
	repo  = "local:/home/u/src/repo"
	other = "local:/home/u/src/other"
)

func TestAllowedWithinTheTTL(t *testing.T) {
	now := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	c := Cache{Dir: t.TempDir(), Now: func() time.Time { return now }}
	if c.Allowed(repo, "p1") {
		t.Fatal("empty cache allowed")
	}
	if err := c.Put(repo, "p1", now); err != nil {
		t.Fatal(err)
	}
	if !c.Allowed(repo, "p1") {
		t.Fatal("fresh entry not allowed")
	}
	if c.Allowed(repo, "p2") || c.Allowed(other, "p1") {
		t.Fatal("entry leaked to another pane or checkout")
	}
	now = now.Add(TTL - time.Second)
	if !c.Allowed(repo, "p1") {
		t.Fatal("entry expired early")
	}
	now = now.Add(time.Second)
	if c.Allowed(repo, "p1") {
		t.Fatal("entry outlived the TTL")
	}
}

func TestDropForgetsEveryPaneOfTheCheckout(t *testing.T) {
	now := time.Date(2026, 11, 11, 9, 0, 0, 0, time.UTC)
	c := Cache{Dir: t.TempDir(), Now: func() time.Time { return now }}
	for _, pane := range []string{"p1", "p2"} {
		if err := c.Put(repo, pane, now); err != nil {
			t.Fatal(err)
		}
	}
	if err := c.Put(other, "p1", now); err != nil {
		t.Fatal(err)
	}
	now = now.Add(time.Second)
	if err := c.Drop(repo); err != nil {
		t.Fatal(err)
	}
	if c.Allowed(repo, "p1") || c.Allowed(repo, "p2") {
		t.Fatal("drop kept an entry")
	}
	if !c.Allowed(other, "p1") {
		t.Fatal("drop forgot another checkout")
	}
	// A check that began before the drop and finished after it is stale.
	if err := c.Put(repo, "p1", now.Add(-time.Millisecond)); err != nil {
		t.Fatal(err)
	}
	if c.Allowed(repo, "p1") {
		t.Fatal("a check started before the drop was kept")
	}
	now = now.Add(time.Millisecond)
	if err := c.Put(repo, "p1", now); err != nil {
		t.Fatal(err)
	}
	if !c.Allowed(repo, "p1") {
		t.Fatal("a check after the drop was not kept")
	}
}

func TestEmptyCacheIsOff(t *testing.T) {
	var c Cache
	if err := c.Put(repo, "p1", time.Now()); err != nil {
		t.Fatal(err)
	}
	if c.Allowed(repo, "p1") {
		t.Fatal("cache without a dir allowed")
	}
	if err := c.Drop(repo); err != nil {
		t.Fatal(err)
	}
	c = Cache{Dir: t.TempDir()}
	if err := c.Put(repo, "", time.Now()); err != nil {
		t.Fatal(err)
	}
	if c.Allowed(repo, "") {
		t.Fatal("an unnamed pane was cached")
	}
}
