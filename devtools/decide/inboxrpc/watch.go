package inboxrpc

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"sync"
	"time"

	"github.com/fsnotify/fsnotify"
	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

const defaultWatchEvery = 2 * time.Second

// WatchEvent is one watch snapshot or delta. Status, Drafts,
// OrgSpecModTime, and Panels are set only when they are new.
type WatchEvent struct {
	Seq            int64
	Resync         bool
	Items          []decide.Item
	Changes        []decide.LedgerChange
	Status         *tui.StatusBoard
	Drafts         []decide.Draft
	DraftSeq       int64
	OrgSpecModTime *time.Time
	Panels         *[]decide.PanelEntry
}

// fileStamp records cheap os.Stat metadata for change detection without
// opening or locking the file.
type fileStamp struct {
	info os.FileInfo
}

func statFile(path string) fileStamp {
	info, err := os.Stat(path)
	if err != nil {
		return fileStamp{}
	}
	return fileStamp{info: info}
}

func sameFileStamp(a, b fileStamp) bool {
	if a.info == nil || b.info == nil {
		return a.info == nil && b.info == nil
	}
	return os.SameFile(a.info, b.info) &&
		a.info.Size() == b.info.Size() &&
		a.info.ModTime().Equal(b.info.ModTime())
}

// Watcher watches the hub's ledger, status board, drafts, and org spec
// for changes, independent of any transport. Start takes the first
// snapshot; Poll and Run report what changed since the last event.
type Watcher struct {
	Backend *tui.LocalBackend
	// OrgConfig reports the org spec to watch; nil watches none.
	OrgConfig func() (enabled bool, spec string, err error)
	// Every is the fallback poll interval of Run. Zero defaults to 2s.
	Every time.Duration

	wakeOnce sync.Once
	wake     chan struct{}

	mu           sync.Mutex
	started      bool
	lastSeq      int64
	changesStamp fileStamp
	ledgerStamp  fileStamp
	snoozed      map[string]time.Time
	lastStatus   tui.StatusBoard
	hasStatus    bool
	statusAt     time.Time
	statusStamp  fileStamp
	lastDraftSeq int64
	hasDrafts    bool
	draftsStamp  fileStamp
	orgSpecStamp fileStamp
	hasOrgSpec   bool
	panelMon     *decide.PanelMonitor

	// Counters for tests verifying that no-change polls skip file loads.
	ledgerPolls int
	statusPolls int
	draftsPolls int
}

func (w *Watcher) wakeCh() chan struct{} {
	w.wakeOnce.Do(func() { w.wake = make(chan struct{}, 1) })
	return w.wake
}

// Signal asks Run to poll now, such as after a local write.
func (w *Watcher) Signal() {
	select {
	case w.wakeCh() <- struct{}{}:
	default:
	}
}

func (w *Watcher) now() time.Time {
	if w.Backend != nil && w.Backend.Now != nil {
		return w.Backend.Now()
	}
	return time.Now()
}

func (w *Watcher) ledger() (*decide.Ledger, error) {
	led, err := decide.Open(w.Backend.Dir)
	if err != nil {
		return nil, fmt.Errorf("open decide ledger: %w", err)
	}
	if w.Backend.Now != nil {
		led.SetClock(w.Backend.Now)
	}
	return led, nil
}

// Start returns the changes since seq (a resync snapshot when seq is 0
// or has a gap), together with the current status, drafts, and org spec
// time, and begins tracking from there.
func (w *Watcher) Start(since int64) (WatchEvent, error) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if w.Backend == nil {
		return WatchEvent{}, fmt.Errorf("no backend")
	}
	led, err := w.ledger()
	if err != nil {
		return WatchEvent{}, err
	}
	// Stat before reading so a concurrent write is never missed on the next Poll.
	changesStamp := w.statState("changes.json")
	ledgerStamp := w.statState("ledger.json")
	seq, changes, items, resync, err := led.ChangesSince(since)
	if err != nil {
		return WatchEvent{}, fmt.Errorf("decide changes: %w", err)
	}
	w.changesStamp = changesStamp
	w.ledgerStamp = ledgerStamp
	w.recordSnoozes(resync, items, changes)
	ev := WatchEvent{Seq: seq, Resync: resync, Items: items, Changes: changes}

	statusStamp := w.statState("status.json")
	if board, err := w.Backend.Status(); err == nil {
		ev.Status = &board
		w.lastStatus = board
		w.hasStatus = true
	}
	w.statusAt = time.Now()
	w.statusStamp = statusStamp

	draftsStamp := w.statState("drafts.json")
	if dseq, drafts, err := w.listDrafts(); err == nil {
		ev.Drafts = drafts
		ev.DraftSeq = dseq
		w.lastDraftSeq = dseq
		w.hasDrafts = true
	}
	w.draftsStamp = draftsStamp

	if stamp := w.statOrgSpec(); stamp.info != nil {
		mod := stamp.info.ModTime()
		ev.OrgSpecModTime = &mod
		w.orgSpecStamp = stamp
		w.hasOrgSpec = true
	}
	w.panelMon = decide.NewPanelMonitor(w.Backend.Dir, w.Backend.Now)
	if panels, err := w.panelMon.Start(); err == nil {
		ev.Panels = &panels
	}
	w.lastSeq = seq
	w.started = true
	return ev, nil
}

// Run polls until ctx ends, on every filesystem notification, Signal,
// staleness wakeup, and fallback tick, and passes each change to emit.
// Nothing is emitted before Start.
func (w *Watcher) Run(ctx context.Context, emit func(WatchEvent)) {
	every := w.Every
	if every <= 0 {
		every = defaultWatchEvery
	}
	tick := time.NewTicker(every)
	defer tick.Stop()
	fsw, fsEvents, fsErrors, panelsWatched := w.openFSWatch()
	if fsw != nil {
		defer fsw.Close()
	}
	panelsDir := w.panelsDir()
	var staleTimer *time.Timer
	defer stopWatchTimer(&staleTimer)

	for {
		stopWatchTimer(&staleTimer)
		staleC := w.armStaleTimer(&staleTimer)
		select {
		case <-ctx.Done():
			return
		case <-w.wakeCh():
		case <-staleC:
		case <-tick.C:
		case ev, ok := <-fsEvents:
			if !ok {
				fsEvents = nil
				continue
			}
			if !w.handleFSWatchEvent(fsw, panelsDir, &panelsWatched, ev, fsEvents) {
				continue
			}
		case _, ok := <-fsErrors:
			if !ok {
				fsErrors = nil
			}
			continue
		}
		if ev, ok := w.Poll(); ok {
			emit(ev)
		}
	}
}

func stopWatchTimer(timer **time.Timer) {
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

func (w *Watcher) armStaleTimer(timer **time.Timer) <-chan time.Time {
	w.mu.Lock()
	var next time.Time
	if w.panelMon != nil {
		next = w.panelMon.NextStaleAt()
	}
	w.mu.Unlock()
	if next.IsZero() {
		return nil
	}
	*timer = time.NewTimer(max(next.Sub(w.now()), 0))
	return (*timer).C
}

func (w *Watcher) panelsDir() string {
	if w.Backend == nil || w.Backend.Dir == "" {
		return ""
	}
	return decide.PanelsDir(w.Backend.Dir)
}

func (w *Watcher) handleFSWatchEvent(fsw *fsnotify.Watcher, panelsDir string, panelsWatched *bool, first fsnotify.Event, ch <-chan fsnotify.Event) bool {
	relevant, removed := drainFSEvents(panelsDir, first, ch)
	if removed {
		*panelsWatched = false
	}
	if fsw != nil && panelsDir != "" && !*panelsWatched && addPanelsWatch(fsw, panelsDir) {
		*panelsWatched = true
		relevant = true
	}
	return relevant
}

func (w *Watcher) openFSWatch() (*fsnotify.Watcher, <-chan fsnotify.Event, <-chan error, bool) {
	if w.Backend == nil || w.Backend.Dir == "" {
		return nil, nil, nil, false
	}
	if err := os.MkdirAll(w.Backend.Dir, 0o755); err != nil {
		return nil, nil, nil, false
	}
	fsw, err := fsnotify.NewWatcher()
	if err != nil {
		return nil, nil, nil, false
	}
	if err := fsw.Add(w.Backend.Dir); err != nil {
		_ = fsw.Close()
		return nil, nil, nil, false
	}
	panelsWatched := addPanelsWatch(fsw, decide.PanelsDir(w.Backend.Dir))
	return fsw, fsw.Events, fsw.Errors, panelsWatched
}

func addPanelsWatch(fsw *fsnotify.Watcher, panelsDir string) bool {
	info, err := os.Stat(panelsDir)
	if err != nil || !info.IsDir() {
		return false
	}
	return fsw.Add(panelsDir) == nil
}

func relevantFSEvent(panelsDir string, ev fsnotify.Event) bool {
	if ev.Op == fsnotify.Chmod {
		return false
	}
	if panelsDir != "" && (ev.Name == panelsDir || filepath.Dir(ev.Name) == panelsDir) {
		return decide.IsRelevantPanelFSEvent(panelsDir, ev)
	}
	switch filepath.Base(ev.Name) {
	case "ledger.lock", "status.lock", "drafts.lock":
		return false
	}
	return true
}

func drainFSEvents(panelsDir string, first fsnotify.Event, ch <-chan fsnotify.Event) (relevant, panelsRemoved bool) {
	check := func(ev fsnotify.Event) {
		if panelsDir != "" && ev.Name == panelsDir && ev.Has(fsnotify.Remove) {
			panelsRemoved = true
		}
		if relevantFSEvent(panelsDir, ev) {
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

// Poll reports what changed since Start or the previous Poll.
func (w *Watcher) Poll() (WatchEvent, bool) {
	w.mu.Lock()
	defer w.mu.Unlock()
	if !w.started || w.Backend == nil {
		return WatchEvent{}, false
	}
	seq, changes, items, resync, ok := w.pollLedger()
	if !ok {
		return WatchEvent{}, false
	}
	status := w.pollStatus()
	drafts, draftSeq := w.pollDrafts()
	orgMod := w.pollOrgSpec()
	panels := w.pollPanels()
	if !resync && len(changes) == 0 && status == nil && drafts == nil && orgMod == nil && panels == nil {
		return WatchEvent{}, false
	}
	ev := WatchEvent{
		Seq:            seq,
		Resync:         resync,
		Items:          items,
		Changes:        changes,
		Status:         status,
		OrgSpecModTime: orgMod,
		Panels:         panels,
	}
	if drafts != nil {
		ev.Drafts = *drafts
		ev.DraftSeq = draftSeq
	}
	return ev, true
}

func (w *Watcher) pollPanels() *[]decide.PanelEntry {
	if w.panelMon == nil {
		return nil
	}
	panels, ok := w.panelMon.Poll()
	if !ok {
		return nil
	}
	return &panels
}

func (w *Watcher) pollLedger() (int64, []decide.LedgerChange, []decide.Item, bool, bool) {
	changesStamp := w.statState("changes.json")
	ledgerStamp := w.statState("ledger.json")
	if sameFileStamp(changesStamp, w.changesStamp) && sameFileStamp(ledgerStamp, w.ledgerStamp) && !w.snoozeDue() {
		return w.lastSeq, nil, nil, false, true
	}
	led, err := w.ledger()
	if err != nil {
		return 0, nil, nil, false, false
	}
	w.ledgerPolls++
	seq, changes, items, resync, err := led.ChangesSince(w.lastSeq)
	if err != nil {
		return 0, nil, nil, false, false
	}
	w.changesStamp = changesStamp
	w.ledgerStamp = ledgerStamp
	w.lastSeq = seq
	w.recordSnoozes(resync, items, changes)
	return seq, changes, items, resync, true
}

func (w *Watcher) recordSnoozes(resync bool, items []decide.Item, changes []decide.LedgerChange) {
	if resync {
		w.snoozed = nil
		for i := range items {
			if items[i].SnoozedUntil != nil {
				if w.snoozed == nil {
					w.snoozed = make(map[string]time.Time)
				}
				w.snoozed[items[i].ID] = *items[i].SnoozedUntil
			}
		}
		return
	}
	for i := range changes {
		ch := &changes[i]
		if ch.Deleted || ch.Item == nil || ch.Item.SnoozedUntil == nil {
			delete(w.snoozed, ch.ID)
			continue
		}
		if w.snoozed == nil {
			w.snoozed = make(map[string]time.Time)
		}
		w.snoozed[ch.ID] = *ch.Item.SnoozedUntil
	}
}

func (w *Watcher) snoozeDue() bool {
	if len(w.snoozed) == 0 {
		return false
	}
	now := w.now()
	for _, until := range w.snoozed {
		if !now.Before(until) {
			return true
		}
	}
	return false
}

func (w *Watcher) listDrafts() (int64, []decide.Draft, error) {
	ds, err := decide.OpenDraftStore(w.Backend.Dir)
	if err != nil {
		return 0, nil, fmt.Errorf("open decide draft store: %w", err)
	}
	if w.Backend.Now != nil {
		ds.SetClock(w.Backend.Now)
	}
	seq, drafts, err := ds.List()
	if err != nil {
		return 0, nil, fmt.Errorf("list decide drafts: %w", err)
	}
	return seq, drafts, nil
}

func (w *Watcher) statState(name string) fileStamp {
	return statFile(filepath.Join(w.Backend.Dir, name))
}

func (w *Watcher) pollStatus() *tui.StatusBoard {
	stamp := w.statState("status.json")
	if sameFileStamp(stamp, w.statusStamp) && (len(w.Backend.Checkouts) == 0 || time.Since(w.statusAt) < time.Second) {
		return nil
	}
	w.statusStamp = stamp
	w.statusAt = time.Now()
	w.statusPolls++
	board, err := w.Backend.Status()
	if err != nil {
		return nil
	}
	if w.hasStatus && reflect.DeepEqual(w.lastStatus, board) {
		return nil
	}
	w.lastStatus = board
	w.hasStatus = true
	return &board
}

func (w *Watcher) pollDrafts() (*[]decide.Draft, int64) {
	stamp := w.statState("drafts.json")
	if sameFileStamp(stamp, w.draftsStamp) {
		return nil, 0
	}
	w.draftsStamp = stamp
	w.draftsPolls++
	dseq, drafts, err := w.listDrafts()
	if err != nil {
		return nil, 0
	}
	if w.hasDrafts && dseq == w.lastDraftSeq {
		return nil, 0
	}
	w.lastDraftSeq = dseq
	w.hasDrafts = true
	return &drafts, dseq
}

func (w *Watcher) statOrgSpec() fileStamp {
	if w.OrgConfig == nil {
		return fileStamp{}
	}
	enabled, spec, err := w.OrgConfig()
	if err != nil || !enabled || spec == "" {
		return fileStamp{}
	}
	return statFile(spec)
}

func (w *Watcher) pollOrgSpec() *time.Time {
	stamp := w.statOrgSpec()
	if stamp.info == nil {
		return nil
	}
	if w.hasOrgSpec && sameFileStamp(stamp, w.orgSpecStamp) {
		return nil
	}
	w.orgSpecStamp = stamp
	w.hasOrgSpec = true
	mod := stamp.info.ModTime()
	return &mod
}
