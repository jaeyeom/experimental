package inboxrpc

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

const defaultWatchEvery = 100 * time.Millisecond

// WatchEvent is one watch snapshot or delta. Status, Drafts, and
// OrgSpecModTime are set only when they are new.
type WatchEvent struct {
	Seq            int64
	Resync         bool
	Items          []decide.Item
	Changes        []decide.LedgerChange
	Status         *tui.StatusBoard
	Drafts         []decide.Draft
	DraftSeq       int64
	OrgSpecModTime *time.Time
}

// Watcher watches the hub's ledger, status board, drafts, and org spec
// for changes, independent of any transport. Start takes the first
// snapshot; Poll and Run report what changed since the last event.
type Watcher struct {
	Backend *tui.LocalBackend
	// OrgConfig reports the org spec to watch; nil watches none.
	OrgConfig func() (enabled bool, spec string, err error)
	// Every is the poll interval of Run. Zero defaults to 100ms.
	Every time.Duration

	wakeOnce sync.Once
	wake     chan struct{}

	mu           sync.Mutex
	started      bool
	lastSeq      int64
	lastStatus   tui.StatusBoard
	hasStatus    bool
	statusAt     time.Time
	statusMod    time.Time
	lastDraftSeq int64
	hasDrafts    bool
	draftsAt     time.Time
	draftsMod    time.Time
	orgSpecMod   time.Time
	hasOrgSpec   bool
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
	seq, changes, items, resync, err := led.ChangesSince(since)
	if err != nil {
		return WatchEvent{}, fmt.Errorf("decide changes: %w", err)
	}
	ev := WatchEvent{Seq: seq, Resync: resync, Items: items, Changes: changes}
	if board, err := w.Backend.Status(); err == nil {
		ev.Status = &board
		w.lastStatus = board
		w.hasStatus = true
	}
	w.statusAt = time.Now()
	w.statusMod = w.statMod("status.json")
	if dseq, drafts, err := w.listDrafts(); err == nil {
		ev.Drafts = drafts
		ev.DraftSeq = dseq
		w.lastDraftSeq = dseq
		w.hasDrafts = true
	}
	w.draftsAt = time.Now()
	w.draftsMod = w.statMod("drafts.json")
	if mod := w.statOrgSpecMod(); !mod.IsZero() {
		ev.OrgSpecModTime = &mod
		w.orgSpecMod = mod
		w.hasOrgSpec = true
	}
	w.lastSeq = seq
	w.started = true
	return ev, nil
}

// Run polls until ctx ends, on every tick and every Signal, and passes
// each change to emit. Nothing is emitted before Start.
func (w *Watcher) Run(ctx context.Context, emit func(WatchEvent)) {
	every := w.Every
	if every <= 0 {
		every = defaultWatchEvery
	}
	tick := time.NewTicker(every)
	defer tick.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-w.wakeCh():
		case <-tick.C:
		}
		if ev, ok := w.Poll(); ok {
			emit(ev)
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
	led, err := w.ledger()
	if err != nil {
		return WatchEvent{}, false
	}
	seq, changes, items, resync, err := led.ChangesSince(w.lastSeq)
	if err != nil {
		return WatchEvent{}, false
	}
	status := w.pollStatus()
	drafts, draftSeq := w.pollDrafts()
	orgMod := w.pollOrgSpec()
	if !resync && len(changes) == 0 && status == nil && drafts == nil && orgMod == nil {
		return WatchEvent{}, false
	}
	w.lastSeq = seq
	ev := WatchEvent{
		Seq:            seq,
		Resync:         resync,
		Items:          items,
		Changes:        changes,
		Status:         status,
		OrgSpecModTime: orgMod,
	}
	if drafts != nil {
		ev.Drafts = *drafts
		ev.DraftSeq = draftSeq
	}
	return ev, true
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

func (w *Watcher) statMod(name string) time.Time {
	info, err := os.Stat(filepath.Join(w.Backend.Dir, name))
	if err != nil {
		return time.Time{}
	}
	return info.ModTime()
}

func (w *Watcher) pollStatus() *tui.StatusBoard {
	mod := w.statMod("status.json")
	if mod.Equal(w.statusMod) && time.Since(w.statusAt) < time.Second {
		return nil
	}
	w.statusMod = mod
	w.statusAt = time.Now()
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
	dmod := w.statMod("drafts.json")
	if dmod.Equal(w.draftsMod) && time.Since(w.draftsAt) < time.Second {
		return nil, 0
	}
	w.draftsMod = dmod
	w.draftsAt = time.Now()
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

func (w *Watcher) statOrgSpecMod() time.Time {
	if w.OrgConfig == nil {
		return time.Time{}
	}
	enabled, spec, err := w.OrgConfig()
	if err != nil || !enabled || spec == "" {
		return time.Time{}
	}
	info, err := os.Stat(spec)
	if err != nil {
		return time.Time{}
	}
	return info.ModTime()
}

func (w *Watcher) pollOrgSpec() *time.Time {
	mod := w.statOrgSpecMod()
	if mod.IsZero() {
		return nil
	}
	if w.hasOrgSpec && mod.Equal(w.orgSpecMod) {
		return nil
	}
	w.orgSpecMod = mod
	w.hasOrgSpec = true
	return &mod
}
