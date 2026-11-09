package eventlog

import (
	"context"
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"
)

func TestReadFiltersWithoutStartingDaemon(t *testing.T) {
	dir := t.TempDir()
	writer := Open(dir)
	base := time.Date(2026, 11, 9, 1, 2, 3, 0, time.UTC)
	events := []Event{
		{Time: base, Event: EventSend, Actor: "cli", Name: "alpha", MsgID: "m1", Detail: "one"},
		{Time: base.Add(time.Minute), Event: EventDelivered, Actor: "daemon", Name: "beta", MsgID: "m2", Detail: "two"},
		{Time: base.Add(2 * time.Minute), Event: EventHeld, Actor: "daemon", Name: "alpha", MsgID: "m1", Reason: "offline", TargetState: "held"},
	}
	for _, event := range events {
		if err := writer.Append(event); err != nil {
			t.Fatalf("append %s: %v", event.Event, err)
		}
	}

	reader := Open(dir)
	other := Open(dir)
	got, err := reader.Read(Filter{})
	if err != nil {
		t.Fatal(err)
	}
	again, err := other.Read(Filter{})
	if err != nil {
		t.Fatal(err)
	}
	if strings.Join(got, "\n") != strings.Join(again, "\n") {
		t.Fatalf("readers differ:\n%s\n%s", strings.Join(got, "\n"), strings.Join(again, "\n"))
	}
	if len(got) != 3 {
		t.Fatalf("lines = %d, want 3", len(got))
	}
	for _, line := range got {
		var rec map[string]any
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("jq-readable line %s: %v", line, err)
		}
		ts, _ := rec["ts"].(string)
		if !strings.Contains(ts, ".000Z") && !strings.HasSuffix(ts, ".000Z") {
			t.Fatalf("ts = %q, want RFC3339 milliseconds", ts)
		}
	}

	alpha, err := reader.Read(Filter{Name: "alpha"})
	if err != nil {
		t.Fatal(err)
	}
	if len(alpha) != 2 {
		t.Fatalf("name filter = %d lines, want 2", len(alpha))
	}
	byMsg, err := reader.Read(Filter{MsgID: "m2"})
	if err != nil {
		t.Fatal(err)
	}
	if len(byMsg) != 1 || !strings.Contains(byMsg[0], `"event":"delivered"`) {
		t.Fatalf("msg filter = %v", byMsg)
	}
	held, err := reader.Read(Filter{Event: EventHeld})
	if err != nil {
		t.Fatal(err)
	}
	if len(held) != 1 || !strings.Contains(held[0], `"reason":"offline"`) || !strings.Contains(held[0], `"target_state":"held"`) {
		t.Fatalf("held line = %v", held)
	}
	since, err := reader.Read(Filter{Since: base.Add(90 * time.Second)})
	if err != nil {
		t.Fatal(err)
	}
	if len(since) != 1 || !strings.Contains(since[0], `"event":"held"`) {
		t.Fatalf("since filter = %v", since)
	}
}

func TestPruneRemovesFileOlderThanRetention(t *testing.T) {
	dir := t.TempDir()
	lg := Open(dir)
	lg.RetentionDays = 30
	now := time.Date(2026, 11, 9, 12, 0, 0, 0, time.UTC)
	oldDay := now.AddDate(0, 0, -31)
	keptDay := now.AddDate(0, 0, -30)

	lg.now = func() time.Time { return oldDay }
	if err := lg.Append(Event{Event: EventExpired, Actor: "daemon", Reason: "old"}); err != nil {
		t.Fatal(err)
	}
	lg.now = func() time.Time { return keptDay }
	if err := lg.Append(Event{Event: EventRetry, Actor: "daemon", Detail: "kept"}); err != nil {
		t.Fatal(err)
	}
	lg.now = func() time.Time { return now }
	if err := lg.Append(Event{Event: EventSend, Actor: "cli", Detail: "today"}); err != nil {
		t.Fatal(err)
	}

	if _, err := os.Stat(filepath.Join(dir, fileName(oldDay))); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("file older than retention = %v, want removed", err)
	}
	if _, err := os.Stat(filepath.Join(dir, fileName(keptDay))); err != nil {
		t.Fatalf("file at the retention edge: %v", err)
	}
	if _, err := os.Stat(filepath.Join(dir, fileName(now))); err != nil {
		t.Fatalf("today's file: %v", err)
	}
}

func TestFollowSeesALaterAppend(t *testing.T) {
	dir := t.TempDir()
	lg := Open(dir)
	if err := lg.Append(Event{Event: EventStart, Actor: "daemon", Detail: "listening"}); err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var mu sync.Mutex
	var got []string
	errc := make(chan error, 1)
	go func() {
		errc <- lg.Follow(ctx, Filter{}, func(line string) error {
			mu.Lock()
			got = append(got, line)
			n := len(got)
			mu.Unlock()
			if n == 2 {
				cancel()
			}
			return nil
		})
	}()
	deadline := time.Now().Add(2 * time.Second)
	for {
		mu.Lock()
		n := len(got)
		mu.Unlock()
		if n >= 1 {
			break
		}
		if time.Now().After(deadline) {
			t.Fatal("follow did not see the first line")
		}
		time.Sleep(5 * time.Millisecond)
	}
	if err := lg.Append(Event{Event: EventStop, Actor: "daemon", Detail: "shutdown"}); err != nil {
		t.Fatal(err)
	}
	select {
	case err := <-errc:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("follow did not stop")
	}
	mu.Lock()
	defer mu.Unlock()
	if len(got) != 2 {
		t.Fatalf("follow lines = %d, want 2", len(got))
	}
}

func TestAppendRejectsUnknownAndIncompleteEvents(t *testing.T) {
	lg := Open(t.TempDir())
	if err := lg.Append(Event{Event: "name_expired", Actor: "daemon", Detail: "no"}); err == nil {
		t.Fatal("unknown event was accepted")
	}
	if err := lg.Append(Event{Event: EventHeld, Actor: "daemon", Reason: "offline"}); err == nil {
		t.Fatal("held without a target state was accepted")
	}
}
