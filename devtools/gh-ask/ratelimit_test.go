package ghask

import (
	"testing"
	"time"
)

func TestLimiter(t *testing.T) {
	start := time.Date(2026, 11, 14, 10, 0, 0, 0, time.UTC)
	now := start
	l := newLimiter(2, time.Hour, func() time.Time { return now })

	if !l.Allow() {
		t.Fatal("call at 0m should be allowed")
	}
	now = start.Add(30 * time.Minute)
	if !l.Allow() {
		t.Fatal("call at 30m should be allowed")
	}
	now = start.Add(59 * time.Minute)
	if l.Allow() {
		t.Fatal("third call within the hour should be refused")
	}
	now = start.Add(60 * time.Minute)
	if !l.Allow() {
		t.Fatal("call at 60m should be allowed after the 0m call expires")
	}
	if l.Allow() {
		t.Fatal("call at 60m should be refused: the 30m and 60m calls fill the window")
	}
}
