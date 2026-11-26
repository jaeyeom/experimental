package decide

import (
	"bytes"
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"time"

	"github.com/fsnotify/fsnotify"
)

type panelFileStamp struct {
	name string
	info os.FileInfo
}

func samePanelStamps(a, b []panelFileStamp) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i].name != b[i].name {
			return false
		}
		if !sameFileInfo(a[i].info, b[i].info) {
			return false
		}
	}
	return true
}

func sameFileInfo(a, b os.FileInfo) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	return os.SameFile(a, b) && a.Size() == b.Size() && a.ModTime().Equal(b.ModTime())
}

// PanelMonitor tracks panels/ under a decide state directory using cheap
// directory/file stat metadata on Poll, and tracks the earliest upcoming
// staleness deadline (updatedAt + staleAfter).
type PanelMonitor struct {
	dir       string
	now       func() time.Time
	started   bool
	stamps    []panelFileStamp
	panels    []PanelEntry
	nextStale time.Time
	loads     int
}

// NewPanelMonitor returns a monitor for panels/ under stateDir.
func NewPanelMonitor(stateDir string, now func() time.Time) *PanelMonitor {
	if now == nil {
		now = time.Now
	}
	return &PanelMonitor{dir: stateDir, now: now}
}

func (m *PanelMonitor) clock() time.Time {
	if m.now == nil {
		return time.Now()
	}
	return m.now()
}

// Start loads the initial panel snapshot and begins tracking changes.
func (m *PanelMonitor) Start() ([]PanelEntry, error) {
	stamps := statPanelsDir(PanelsDir(m.dir))
	store, err := OpenPanels(m.dir)
	if err != nil {
		return nil, err
	}
	store.SetClock(m.clock)
	entries, err := store.List()
	if err != nil {
		return nil, err
	}
	m.stamps = stamps
	m.panels = entries
	m.nextStale = NextPanelStaleAt(entries)
	m.started = true
	return ClonePanelEntries(entries), nil
}

// NextStaleAt returns the earliest upcoming staleness deadline, or zero.
func (m *PanelMonitor) NextStaleAt() time.Time {
	return m.nextStale
}

// Poll returns updated panels if any panel file changed or a panel's staleness
// deadline arrived. Unchanged polls never read or parse panel files.
func (m *PanelMonitor) Poll() ([]PanelEntry, bool) {
	if !m.started {
		return nil, false
	}
	now := m.clock()
	stamps := statPanelsDir(PanelsDir(m.dir))
	if samePanelStamps(stamps, m.stamps) {
		if m.nextStale.IsZero() || now.Before(m.nextStale) {
			return nil, false
		}
		updated, changed := RecomputePanelsStale(m.panels, now)
		m.nextStale = NextPanelStaleAt(updated)
		if !changed {
			return nil, false
		}
		m.panels = updated
		return ClonePanelEntries(updated), true
	}

	store, err := OpenPanels(m.dir)
	if err != nil {
		return nil, false
	}
	store.SetClock(func() time.Time { return now })
	fresh, err := store.List()
	if err != nil {
		return nil, false
	}
	m.loads++
	m.stamps = stamps
	m.nextStale = NextPanelStaleAt(fresh)
	if panelsEqual(m.panels, fresh) {
		return nil, false
	}
	m.panels = fresh
	return ClonePanelEntries(fresh), true
}

func statPanelsDir(panelsDir string) []panelFileStamp {
	entries, err := os.ReadDir(panelsDir)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return nil
		}
		return nil
	}
	out := make([]panelFileStamp, 0, len(entries))
	for _, de := range entries {
		name := de.Name()
		if !isPanelFileName(name) || de.IsDir() {
			continue
		}
		info, err := de.Info()
		if err != nil {
			info = nil
		}
		out = append(out, panelFileStamp{name: name, info: info})
	}
	sort.Slice(out, func(i, j int) bool {
		return out[i].name < out[j].name
	})
	return out
}

func panelsEqual(a, b []PanelEntry) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if !panelEntryEqual(&a[i], &b[i]) {
			return false
		}
	}
	return true
}

func panelEntryEqual(a, b *PanelEntry) bool {
	if a.ID != b.ID || a.File != b.File || a.Title != b.Title || a.Kind != b.Kind ||
		a.StaleAfter != b.StaleAfter || a.Source != b.Source ||
		a.Stale != b.Stale || a.Unreadable != b.Unreadable || a.Error != b.Error {
		return false
	}
	if !bytes.Equal(a.Data, b.Data) {
		return false
	}
	if (a.UpdatedAt == nil) != (b.UpdatedAt == nil) {
		return false
	}
	if a.UpdatedAt != nil && !a.UpdatedAt.Equal(*b.UpdatedAt) {
		return false
	}
	return true
}

// IsRelevantPanelFSEvent reports whether ev in stateDir or stateDir/panels/
// may affect panel files. Temp/dot/non-.json files inside panels/ are ignored.
func IsRelevantPanelFSEvent(panelsDir string, ev fsnotify.Event) bool {
	if ev.Op == fsnotify.Chmod {
		return false
	}
	if ev.Name == panelsDir {
		return true
	}
	if filepath.Dir(ev.Name) == panelsDir {
		return isPanelFileName(filepath.Base(ev.Name))
	}
	return false
}

// WatchPanels watches panels/ under stateDir using fsnotify (and a single
// one-shot wakeup timer for the earliest updatedAt + staleAfter) with no
// periodic polling. It emits the initial snapshot and every subsequent change
// until ctx ends.
func WatchPanels(ctx context.Context, stateDir string, now func() time.Time, emit func([]PanelEntry)) error {
	if err := os.MkdirAll(stateDir, 0o755); err != nil {
		return err //nolint:wrapcheck // caller gets OS path error
	}
	fsw, err := fsnotify.NewWatcher()
	if err != nil {
		return err //nolint:wrapcheck // fsnotify names failure
	}
	defer fsw.Close()

	if err := fsw.Add(stateDir); err != nil {
		return err //nolint:wrapcheck // fsnotify names path error
	}
	panelsDir := PanelsDir(stateDir)
	panelsWatched := addDirIfExists(fsw, panelsDir)

	mon := NewPanelMonitor(stateDir, now)
	initial, err := mon.Start()
	if err != nil {
		return err
	}
	emit(initial)

	runPanelWatchLoop(ctx, fsw, panelsDir, &panelsWatched, mon, emit)
	return nil
}

func stopPanelTimer(timer **time.Timer) {
	if *timer == nil {
		return
	}
	if !(*timer).Stop() {
		select {
		case <-(*timer).C:
		default:
		}
	}
	*timer = nil
}

func handlePanelFSEvent(fsw *fsnotify.Watcher, panelsDir string, panelsWatched *bool, first fsnotify.Event, mon *PanelMonitor, emit func([]PanelEntry)) {
	relevant, removed := drainPanelFSEvents(panelsDir, first, fsw.Events)
	if removed {
		*panelsWatched = false
	}
	if !*panelsWatched && addDirIfExists(fsw, panelsDir) {
		*panelsWatched = true
		relevant = true
	}
	if !relevant {
		return
	}
	if updated, ok := mon.Poll(); ok {
		emit(updated)
	}
}

func runPanelWatchLoop(ctx context.Context, fsw *fsnotify.Watcher, panelsDir string, panelsWatched *bool, mon *PanelMonitor, emit func([]PanelEntry)) {
	var timer *time.Timer
	defer stopPanelTimer(&timer)

	for {
		stopPanelTimer(&timer)
		var timerC <-chan time.Time
		if next := mon.NextStaleAt(); !next.IsZero() {
			timer = time.NewTimer(max(next.Sub(mon.clock()), 0))
			timerC = timer.C
		}
		select {
		case <-ctx.Done():
			return
		case <-timerC:
			if updated, ok := mon.Poll(); ok {
				emit(updated)
			}
		case ev, ok := <-fsw.Events:
			if !ok {
				return
			}
			handlePanelFSEvent(fsw, panelsDir, panelsWatched, ev, mon, emit)
		case _, ok := <-fsw.Errors:
			if !ok {
				return
			}
		}
	}
}

func addDirIfExists(fsw *fsnotify.Watcher, dir string) bool {
	info, err := os.Stat(dir)
	if err != nil || !info.IsDir() {
		return false
	}
	return fsw.Add(dir) == nil
}

func drainPanelFSEvents(panelsDir string, first fsnotify.Event, ch <-chan fsnotify.Event) (relevant, panelsRemoved bool) {
	check := func(ev fsnotify.Event) {
		if ev.Name == panelsDir && ev.Has(fsnotify.Remove) {
			panelsRemoved = true
		}
		if IsRelevantPanelFSEvent(panelsDir, ev) {
			relevant = true
		}
	}
	check(first)
	for {
		select {
		case ev, ok := <-ch:
			if !ok {
				return relevant, panelsRemoved
			}
			check(ev)
		default:
			return relevant, panelsRemoved
		}
	}
}

// HasPanelsChanged is true when any element in a differs from b, ignoring order
// only if already sorted (List sorts by File).
func HasPanelsChanged(a, b []PanelEntry) bool {
	return !slices.EqualFunc(a, b, func(x, y PanelEntry) bool {
		return panelEntryEqual(&x, &y)
	})
}
