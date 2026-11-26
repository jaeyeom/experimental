package decide

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"
)

func TestPanelStoreValidationAndRoundTrip(t *testing.T) {
	dir := t.TempDir()
	store, err := OpenPanels(dir)
	if err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 11, 26, 12, 0, 0, 0, time.UTC)
	store.SetClock(func() time.Time { return now })

	// Bad IDs are refused.
	for _, badID := range []string{"", "BadID", "a_b", "a/b", "../x", "a.json"} {
		_, err := store.Set(Panel{
			ID:    badID,
			Title: "Test",
			Kind:  "kv",
			Data:  json.RawMessage(`{"k":"v"}`),
		})
		if err == nil {
			t.Fatalf("Set(%q) succeeded, want error", badID)
		}
	}

	// Bad JSON is refused.
	for _, badData := range []string{"", "null", "{bad", `{"a":1} trailing`} {
		_, err := store.Set(Panel{
			ID:    "ok-id",
			Title: "Test",
			Kind:  "kv",
			Data:  json.RawMessage(badData),
		})
		if err == nil {
			t.Fatalf("Set(data=%q) succeeded, want error", badData)
		}
	}

	// Bad staleAfter is refused.
	for _, badDur := range []string{"not-a-dur", "0s", "-5m"} {
		_, err := store.Set(Panel{
			ID:         "ok-id",
			Title:      "Test",
			Kind:       "kv",
			Data:       json.RawMessage(`{"ok":true}`),
			StaleAfter: badDur,
		})
		if err == nil {
			t.Fatalf("Set(staleAfter=%q) succeeded, want error", badDur)
		}
	}

	// Known kind validator hook works.
	RegisterPanelKind("test-strict-kind", func(data json.RawMessage) error {
		var v struct {
			Count int `json:"count"`
		}
		if err := json.Unmarshal(data, &v); err != nil || v.Count <= 0 {
			return fmt.Errorf("count must be > 0")
		}
		return nil
	})
	t.Cleanup(func() { RegisterPanelKind("test-strict-kind", nil) })

	if _, err := store.Set(Panel{
		ID:    "strict-1",
		Title: "Strict",
		Kind:  "test-strict-kind",
		Data:  json.RawMessage(`{"count":0}`),
	}); err == nil {
		t.Fatal("Set with invalid known kind data succeeded")
	}

	saved, err := store.Set(Panel{
		ID:         "ci-status",
		Title:      "CI Status",
		Kind:       "test-strict-kind",
		Data:       json.RawMessage(`{"count":3}`),
		StaleAfter: "5m",
		Source:     "github-actions",
	})
	if err != nil {
		t.Fatal(err)
	}
	if !saved.UpdatedAt.Equal(now) {
		t.Fatalf("UpdatedAt = %v, want %v", saved.UpdatedAt, now)
	}

	entries, err := store.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 1 {
		t.Fatalf("List len = %d, want 1", len(entries))
	}
	e := entries[0]
	if e.ID != "ci-status" || e.Title != "CI Status" || e.Kind != "test-strict-kind" ||
		e.StaleAfter != "5m" || e.Source != "github-actions" || e.Stale || e.Unreadable {
		t.Fatalf("entry = %+v", e)
	}
	var gotData struct {
		Count int `json:"count"`
	}
	if err := json.Unmarshal(e.Data, &gotData); err != nil || gotData.Count != 3 {
		t.Fatalf("data = %s, err=%v", e.Data, err)
	}

	// Remove deletes the panel; removing missing returns ErrNotFound.
	if err := store.Remove("ci-status"); err != nil {
		t.Fatal(err)
	}
	if err := store.Remove("ci-status"); !errors.Is(err, ErrNotFound) {
		t.Fatalf("Remove missing = %v, want ErrNotFound", err)
	}
	entries, err = store.List()
	if err != nil || len(entries) != 0 {
		t.Fatalf("List after remove = %+v, %v", entries, err)
	}
}

func TestPanelWriteIsAtomic(t *testing.T) {
	dir := t.TempDir()
	store, err := OpenPanels(dir)
	if err != nil {
		t.Fatal(err)
	}
	payload := json.RawMessage(`{"status":"ok","items":["a","b","c"]}`)
	if _, err := store.Set(Panel{ID: "build", Title: "Build", Kind: "kv", Data: payload}); err != nil {
		t.Fatal(err)
	}

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var wg sync.WaitGroup
	errs := make(chan error, 1)

	wg.Add(1)
	go func() {
		defer wg.Done()
		for i := range 200 {
			if ctx.Err() != nil {
				return
			}
			_, err := store.Set(Panel{
				ID:    "build",
				Title: fmt.Sprintf("Build %d", i),
				Kind:  "kv",
				Data:  json.RawMessage(fmt.Sprintf(`{"step":%d}`, i)),
			})
			if err != nil {
				select {
				case errs <- err:
				default:
				}
				return
			}
		}
	}()

	wg.Add(1)
	go func() {
		defer wg.Done()
		for range 300 {
			entries, err := store.List()
			if err != nil {
				select {
				case errs <- err:
				default:
				}
				return
			}
			if len(entries) != 1 || entries[0].Unreadable {
				select {
				case errs <- fmt.Errorf("reader saw torn panel: %+v", entries):
				default:
				}
				return
			}
		}
	}()

	wg.Wait()
	select {
	case err := <-errs:
		t.Fatal(err)
	default:
	}
}

func TestPanelListHandlesUnreadableAndInvalidFiles(t *testing.T) {
	dir := t.TempDir()
	panelsDir := PanelsDir(dir)
	if err := os.MkdirAll(panelsDir, 0o755); err != nil {
		t.Fatal(err)
	}

	// Valid panel.
	store, err := OpenPanels(dir)
	if err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 11, 26, 12, 0, 0, 0, time.UTC)
	store.SetClock(func() time.Time { return now })
	if _, err := store.Set(Panel{ID: "good", Title: "Good", Kind: "kv", Data: json.RawMessage(`{"ok":true}`)}); err != nil {
		t.Fatal(err)
	}

	// Bad JSON.
	if err := os.WriteFile(filepath.Join(panelsDir, "broken.json"), []byte("{not-json"), 0o600); err != nil {
		t.Fatal(err)
	}
	// Unknown field.
	if err := os.WriteFile(filepath.Join(panelsDir, "extra.json"), []byte(`{"id":"extra","title":"T","kind":"k","data":{},"updatedAt":"2026-11-26T12:00:00Z","bogus":1}`), 0o600); err != nil {
		t.Fatal(err)
	}
	// Missing required field.
	if err := os.WriteFile(filepath.Join(panelsDir, "missing.json"), []byte(`{"id":"missing","kind":"k","data":{},"updatedAt":"2026-11-26T12:00:00Z"}`), 0o600); err != nil {
		t.Fatal(err)
	}
	// ID mismatch / bad id filename.
	if err := os.WriteFile(filepath.Join(panelsDir, "bad_name.json"), []byte(`{"id":"bad_name","title":"T","kind":"k","data":{},"updatedAt":"2026-11-26T12:00:00Z"}`), 0o600); err != nil {
		t.Fatal(err)
	}
	// Temp/dot files must be ignored.
	if err := os.WriteFile(filepath.Join(panelsDir, ".tmp-write.json"), []byte("partial"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(panelsDir, "scratch.tmp"), []byte("partial"), 0o600); err != nil {
		t.Fatal(err)
	}

	entries, err := store.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) != 5 {
		t.Fatalf("entries len = %d, want 5: %+v", len(entries), entries)
	}
	byFile := map[string]PanelEntry{}
	for _, e := range entries {
		byFile[e.File] = e
	}
	if byFile["good.json"].Unreadable {
		t.Fatalf("good.json marked unreadable: %+v", byFile["good.json"])
	}
	for _, bad := range []string{"broken.json", "extra.json", "missing.json", "bad_name.json"} {
		e, ok := byFile[bad]
		if !ok || !e.Unreadable || e.Error == "" {
			t.Fatalf("%s = %+v, want unreadable with error", bad, e)
		}
	}
	if !strings.Contains(byFile["missing.json"].Error, "title") {
		t.Fatalf("missing.json error = %q, want mention of title", byFile["missing.json"].Error)
	}
}

func TestPanelMonitorIdleAndStalenessWake(t *testing.T) {
	dir := t.TempDir()
	now := time.Date(2026, 11, 26, 12, 0, 0, 0, time.UTC)
	store, err := OpenPanels(dir)
	if err != nil {
		t.Fatal(err)
	}
	store.SetClock(func() time.Time { return now })
	if _, err := store.Set(Panel{
		ID:         "build",
		Title:      "Build",
		Kind:       "kv",
		Data:       json.RawMessage(`{"ok":true}`),
		StaleAfter: "5m",
	}); err != nil {
		t.Fatal(err)
	}

	mon := NewPanelMonitor(dir, func() time.Time { return now })
	initial, err := mon.Start()
	if err != nil {
		t.Fatal(err)
	}
	if len(initial) != 1 || initial[0].Stale {
		t.Fatalf("initial = %+v", initial)
	}
	if want := now.Add(5 * time.Minute); !mon.NextStaleAt().Equal(want) {
		t.Fatalf("NextStaleAt = %v, want %v", mon.NextStaleAt(), want)
	}

	for range 10 {
		if _, ok := mon.Poll(); ok {
			t.Fatal("idle Poll() reported change")
		}
	}
	if mon.loads != 0 {
		t.Fatalf("idle Poll() read files %d times, want 0", mon.loads)
	}

	// Advance to updatedAt + staleAfter.
	now = now.Add(5 * time.Minute)
	updated, ok := mon.Poll()
	if !ok || len(updated) != 1 || !updated[0].Stale {
		t.Fatalf("stale Poll() = %+v, %v", updated, ok)
	}
	if !mon.NextStaleAt().IsZero() {
		t.Fatalf("NextStaleAt after becoming stale = %v, want zero", mon.NextStaleAt())
	}
	if _, ok := mon.Poll(); ok {
		t.Fatal("Poll() after stale reported another change")
	}
}

func TestWatchPanelsFSNotifyAndNoIdleEvents(t *testing.T) {
	dir := t.TempDir()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	events := make(chan []PanelEntry, 8)
	errCh := make(chan error, 1)
	go func() {
		errCh <- WatchPanels(ctx, dir, time.Now, func(panels []PanelEntry) {
			events <- panels
		})
	}()

	// Initial empty snapshot.
	select {
	case p := <-events:
		if len(p) != 0 {
			t.Fatalf("initial = %+v, want empty", p)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("timed out waiting for initial snapshot")
	}

	// No events while idle.
	select {
	case p := <-events:
		t.Fatalf("unexpected idle event: %+v", p)
	case <-time.After(100 * time.Millisecond):
	}

	// Write a panel file (creates panels/ directory too); triggers exactly one event.
	store, err := OpenPanels(dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := store.Set(Panel{
		ID:         "ci",
		Title:      "CI",
		Kind:       "kv",
		Data:       json.RawMessage(`{"pass":true}`),
		StaleAfter: "150ms",
	}); err != nil {
		t.Fatal(err)
	}

	select {
	case p := <-events:
		if len(p) != 1 || p[0].ID != "ci" || p[0].Stale {
			t.Fatalf("after set = %+v", p)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("timed out waiting for panel set event")
	}

	// Wait for the one-shot staleness wakeup at updatedAt + staleAfter.
	select {
	case p := <-events:
		if len(p) != 1 || !p[0].Stale {
			t.Fatalf("after stale wakeup = %+v", p)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("timed out waiting for staleness wakeup")
	}

	// No further events while idle after becoming stale.
	select {
	case p := <-events:
		t.Fatalf("unexpected event after stale: %+v", p)
	case <-time.After(150 * time.Millisecond):
	}

	cancel()
	if err := <-errCh; err != nil {
		t.Fatalf("WatchPanels error = %v", err)
	}
}
