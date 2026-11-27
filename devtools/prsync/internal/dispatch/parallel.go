package dispatch

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"sync"
	"sync/atomic"
	"time"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/ledger"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/runlog"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// dispatchedSet tracks agent panes dispatched by prsync during a parallel run
// so the parallel gate can distinguish prsync-dispatched lease holders from
// external ones without data races.
type dispatchedSet struct {
	mu      sync.RWMutex
	targets []DispatchedTarget
}

func newDispatchedSet() *dispatchedSet {
	return &dispatchedSet{}
}

func (d *dispatchedSet) Add(machine, paneID string) {
	if paneID == "" {
		return
	}
	target := DispatchedTarget{Machine: machine, PaneID: paneID}
	d.mu.Lock()
	defer d.mu.Unlock()
	if !slices.Contains(d.targets, target) {
		d.targets = append(d.targets, target)
	}
}

func (d *dispatchedSet) ContainsHolder(_ string, holder *LeaseHolder) bool {
	if d == nil || holder == nil {
		return false
	}
	d.mu.RLock()
	defer d.mu.RUnlock()
	for _, t := range d.targets {
		if holderMatchesTarget(holder, t) {
			return true
		}
	}
	return false
}

// parallelPrecheck verifies before starting any parallel worker that
// gate_lease=required is active and the agentbus workdir lease is readable
// and guards the checkout(s).
func parallelPrecheck(ctx context.Context, h Herdr, cfg config.Config, cands []Candidate) error {
	if cfg.GateLease != "required" {
		return fmt.Errorf("%w: parallel dispatch (--parallel %d) requires gate_lease=required", ErrLeaseRequired, cfg.DispatchMaxParallel)
	}
	probes := probesFrom(ctx)
	if probes.Lease == nil {
		return fmt.Errorf("%w: no agentbus lease probe available", ErrLeaseRequired)
	}
	if err := gatePrecheck(ctx, h, cfg); err != nil {
		return err
	}
	if m := cfg.GateMachine(); m != "" {
		leaseKey := m + ":" + cfg.MachineCheckouts[m]
		all, err := probes.Lease.All(ctx)
		if err != nil {
			return fmt.Errorf("%w: %w", ErrLeaseRequired, err)
		}
		for _, st := range all {
			if st.Checkout == leaseKey {
				return nil
			}
		}
		return fmt.Errorf("%w: checkout %s is not guarded by agentbus", ErrLeaseRequired, leaseKey)
	}
	checkouts := parallelCheckouts(cfg, cands)
	if len(checkouts) == 0 {
		if hasPotentiallyEligible(cands, cfg) {
			return fmt.Errorf("%w: no local checkout resolved for agentbus lease check", ErrLeaseRequired)
		}
		return nil
	}
	for _, ch := range checkouts {
		guarded, err := probes.Lease.Guarded(ctx, ch)
		if err != nil {
			return fmt.Errorf("%w: %w", ErrLeaseRequired, err)
		}
		if !guarded {
			return fmt.Errorf("%w: checkout %s is not guarded by agentbus", ErrLeaseRequired, ch)
		}
	}
	return nil
}

func parallelCheckouts(cfg config.Config, cands []Candidate) []string {
	seen := make(map[string]struct{})
	var out []string
	add := func(dir string) {
		if dir == "" {
			return
		}
		if _, ok := seen[dir]; ok {
			return
		}
		seen[dir] = struct{}{}
		out = append(out, dir)
	}
	add(cfg.Checkout)
	for _, c := range cands {
		add(gateConfig(cfg, c.Repo).Checkout)
	}
	if len(out) == 0 {
		add(CheckoutOfRepos(cfg.Repos))
	}
	if len(out) == 0 {
		add(cfg.OpenTabCwd(config.LocalMachine))
	}
	return out
}

func hasPotentiallyEligible(cands []Candidate, cfg config.Config) bool {
	for _, c := range cands {
		if c.PR == nil {
			continue
		}
		if c.PR.OutOfScope != "" {
			continue
		}
		if c.PR.IsDraft && !cfg.IncludeDrafts {
			continue
		}
		return true
	}
	return false
}

type workerJob struct {
	idx       int
	cand      Candidate
	needsOpen bool
	stSnap    State
	prKey     string
	tabKey    string
	paneKey   string
}

type workerDone struct {
	idx         int
	cand        Candidate
	item        Item
	prKey       string
	tabKey      string
	paneKey     string
	gateStopErr error
	fatalErr    error
}

type parallelCoordinator struct {
	ctx         context.Context
	h           Herdr
	store       StateStore
	cfg         config.Config
	req         Request
	now         time.Time
	cands       []Candidate
	st          State
	matched     map[string]struct{}
	clock       Clock
	sleeper     Sleeper
	budget      *tabBudget
	openMu      sync.Mutex
	gateMu      sync.Mutex
	stopGate    atomic.Bool
	dispatched  *dispatchedSet
	results     []Item
	pending     []int
	inFlightPR  map[string]struct{}
	inFlightTab map[string]struct{}
	inFlightPan map[string]struct{}
	active      int
	maxInFlight int
	stopping    bool
	gateErr     error
	fatalErr    error
	settledTO   bool
	doneCh      chan workerDone
}

func dispatchLiveParallel(ctx context.Context, h Herdr, store StateStore, cfg config.Config, req Request, now time.Time, cands []Candidate, doc Document) (Document, error) {
	if err := parallelPrecheck(ctx, h, cfg, cands); err != nil {
		return doc, err
	}
	st, err := loadState(store)
	if err != nil {
		return doc, err
	}
	clock, sleeper := reqClockSleeper(req)
	ctx = WithRoundStart(ctx, now)
	runlog.FromContext(ctx).Info("dispatch_parallel_start", "max_parallel", cfg.DispatchMaxParallel, "candidates", len(cands))

	pc := &parallelCoordinator{
		ctx:         ctx,
		h:           h,
		store:       store,
		cfg:         cfg,
		req:         req,
		now:         now,
		cands:       cands,
		st:          st,
		matched:     MatchedTabs(req.Doc),
		clock:       clock,
		sleeper:     sleeper,
		budget:      newTabBudget(cfg),
		dispatched:  newDispatchedSet(),
		results:     make([]Item, len(cands)),
		pending:     make([]int, len(cands)),
		inFlightPR:  make(map[string]struct{}),
		inFlightTab: make(map[string]struct{}),
		inFlightPan: make(map[string]struct{}),
		doneCh:      make(chan workerDone, max(1, len(cands))),
	}
	for i := range cands {
		pc.pending[i] = i
	}
	pc.runLoop()

	doc.Results = pc.results
	summary := summarizeParallel(doc.Results, pc.maxInFlight)
	doc.Summary = &summary
	logParallelSummary(ctx, summary)

	switch {
	case pc.gateErr != nil:
		return doc, pc.gateErr
	case pc.fatalErr != nil:
		return doc, pc.fatalErr
	case pc.settledTO:
		return doc, ErrSettleTimeout
	default:
		return doc, nil
	}
}

func (pc *parallelCoordinator) runLoop() {
	var wg sync.WaitGroup
	defer wg.Wait()

	for len(pc.pending) > 0 || pc.active > 0 {
		if !pc.stopping && pc.ctx.Err() == nil && pc.active < pc.cfg.DispatchMaxParallel {
			pc.scheduleReady(&wg)
		}
		if (pc.stopping || pc.ctx.Err() != nil) && pc.active == 0 && len(pc.pending) > 0 {
			pc.drainRemaining()
			break
		}
		if pc.active == 0 {
			break
		}
		done := <-pc.doneCh
		pc.handleDone(done)
	}
}

func (pc *parallelCoordinator) scheduleReady(wg *sync.WaitGroup) {
	kept := pc.pending[:0]
	for p := 0; p < len(pc.pending); p++ {
		if pc.stopping || pc.ctx.Err() != nil || pc.active >= pc.cfg.DispatchMaxParallel {
			kept = append(kept, pc.pending[p:]...)
			break
		}
		idx := pc.pending[p]
		if pc.tryScheduleCandidate(idx, wg) {
			kept = append(kept, idx)
		}
	}
	pc.pending = kept
}

func (pc *parallelCoordinator) tryScheduleCandidate(idx int, wg *sync.WaitGroup) bool {
	c := pc.cands[idx]
	key := prKey(c.Repo, c.Number)
	if _, busy := pc.inFlightPR[key]; busy {
		return true
	}
	needsOpen, item := pc.planCandidate(c)
	if item.Action != "" {
		pc.recordStaticItem(idx, key, item)
		return false
	}
	tabKey, paneKey, busy := pc.targetKeys(c, needsOpen)
	if busy {
		return true
	}
	pc.launchWorker(idx, c, key, tabKey, paneKey, needsOpen, wg)
	return false
}

func (pc *parallelCoordinator) planCandidate(c Candidate) (bool, Item) {
	item := Evaluate(c, pc.cfg, pc.st, pc.req, pc.now)
	if item.Action != ActionSkippedNoTab {
		return false, item
	}
	pc.openMu.Lock()
	_, alt, ok := planOpen(c, pc.cfg, pc.st, pc.req, pc.now, item, pc.budget)
	pc.openMu.Unlock()
	if !ok {
		return false, alt
	}
	return true, Item{}
}

func (pc *parallelCoordinator) recordStaticItem(idx int, key string, item Item) {
	logResult(pc.ctx, item)
	pc.results[idx] = item
	if item.Action != ActionDispatchedUnresolved {
		return
	}
	pc.st.NoteUnresolvedScan(key)
	if err := saveEntry(pc.store, pc.st, key); err != nil && pc.fatalErr == nil {
		pc.fatalErr = err
		pc.stopping = true
	}
}

func (pc *parallelCoordinator) targetKeys(c Candidate, needsOpen bool) (tabKey, paneKey string, busy bool) {
	if needsOpen || c.PR == nil || c.PR.Tab == nil {
		return "", "", false
	}
	if c.PR.Tab.TabID != "" {
		tabKey = scan.TabKey(c.PR.Tab.Machine, c.PR.Tab.TabID)
		if _, exists := pc.inFlightTab[tabKey]; exists {
			return "", "", true
		}
	}
	if c.PR.Tab.PaneID != nil && *c.PR.Tab.PaneID != "" {
		paneKey = scan.TabKey(c.PR.Tab.Machine, *c.PR.Tab.PaneID)
		if _, exists := pc.inFlightPan[paneKey]; exists {
			return "", "", true
		}
	}
	return tabKey, paneKey, false
}

func (pc *parallelCoordinator) launchWorker(idx int, c Candidate, key, tabKey, paneKey string, needsOpen bool, wg *sync.WaitGroup) {
	pc.inFlightPR[key] = struct{}{}
	if tabKey != "" {
		pc.inFlightTab[tabKey] = struct{}{}
	}
	if paneKey != "" {
		pc.inFlightPan[paneKey] = struct{}{}
	}
	pc.active++
	if pc.active > pc.maxInFlight {
		pc.maxInFlight = pc.active
	}
	var stSnap State
	if needsOpen {
		if e, ok := pc.st[key]; ok {
			stSnap = State{key: e}
		} else {
			stSnap = State{}
		}
	}
	job := workerJob{
		idx:       idx,
		cand:      c,
		needsOpen: needsOpen,
		stSnap:    stSnap,
		prKey:     key,
		tabKey:    tabKey,
		paneKey:   paneKey,
	}
	wg.Add(1)
	go func(j workerJob) {
		defer wg.Done()
		pc.doneCh <- pc.runWorker(j)
	}(job)
}

func (pc *parallelCoordinator) runWorker(j workerJob) workerDone {
	c := j.cand
	openedTab := ""
	if j.needsOpen {
		pc.openMu.Lock()
		var item Item
		c, item, openedTab = openIfUntabbed(pc.ctx, pc.cfg, j.stSnap, pc.req, pc.now, c, Item{Action: ActionSkippedNoTab}, pc.budget)
		pc.openMu.Unlock()
		if item.Action != "" {
			logResult(pc.ctx, item)
			return workerDone{
				idx:     j.idx,
				cand:    c,
				item:    item,
				prKey:   j.prKey,
				tabKey:  j.tabKey,
				paneKey: j.paneKey,
			}
		}
	}
	if err := pc.ctx.Err(); err != nil {
		item := failItem(c, err)
		logResult(pc.ctx, item)
		return workerDone{
			idx:      j.idx,
			cand:     c,
			item:     item,
			prKey:    j.prKey,
			tabKey:   j.tabKey,
			paneKey:  j.paneKey,
			fatalErr: fmt.Errorf("dispatch: %w", err),
		}
	}

	gCfg := exemptNewPane(gateConfig(pc.cfg, c.Repo), c.PR.Tab, openedTab)
	pc.gateMu.Lock()
	if pc.stopGate.Load() {
		pc.gateMu.Unlock()
		item := Item{Repo: c.Repo, Number: c.Number, Action: ActionQueued}
		logResult(pc.ctx, item)
		return workerDone{
			idx:     j.idx,
			cand:    c,
			item:    item,
			prKey:   j.prKey,
			tabKey:  j.tabKey,
			paneKey: j.paneKey,
		}
	}
	res, err := waitWithPolicy(pc.ctx, pc.h, gCfg, pc.req.RunnerPane, pc.matched, pc.dispatched.ContainsHolder, pc.clock, pc.sleeper)
	if err != nil {
		pc.stopGate.Store(true)
	}
	pc.gateMu.Unlock()

	if errors.Is(err, ErrTimeout) {
		item := gateTimeoutItem(c, res.Busy, pc.req.Doc)
		logResult(pc.ctx, item)
		return workerDone{
			idx:         j.idx,
			cand:        c,
			item:        item,
			prKey:       j.prKey,
			tabKey:      j.tabKey,
			paneKey:     j.paneKey,
			gateStopErr: fmt.Errorf("%w: %s", ErrTimeout, item.Detail),
		}
	}
	if errors.Is(err, ErrDirtyTree) {
		item := Item{Repo: c.Repo, Number: c.Number, Action: ActionGateUnsafe, Detail: res.Reason}
		logResult(pc.ctx, item)
		return workerDone{
			idx:         j.idx,
			cand:        c,
			item:        item,
			prKey:       j.prKey,
			tabKey:      j.tabKey,
			paneKey:     j.paneKey,
			gateStopErr: fmt.Errorf("%w: %s", ErrDirtyTree, res.Reason),
		}
	}
	if err != nil {
		failed := failItem(c, err)
		logResult(pc.ctx, failed)
		return workerDone{
			idx:      j.idx,
			cand:     c,
			item:     failed,
			prKey:    j.prKey,
			tabKey:   j.tabKey,
			paneKey:  j.paneKey,
			fatalErr: err,
		}
	}

	pc.dispatched.Add(c.PR.Tab.Machine, *c.PR.Tab.PaneID)
	sendArgs := []any{"repo", c.Repo, "number", c.Number, "pane_id", *c.PR.Tab.PaneID}
	if c.PR.Tab.Machine != "" {
		sendArgs = append(sendArgs, "machine", c.PR.Tab.Machine)
	}
	runlog.FromContext(pc.ctx).Info("dispatch_send", sendArgs...)

	item := markOpenedTab(sendPrompt(pc.ctx, pc.h, pc.cfg, c, pc.req, pc.clock, pc.sleeper), openedTab)
	logResult(pc.ctx, item)
	return workerDone{
		idx:     j.idx,
		cand:    c,
		item:    item,
		prKey:   j.prKey,
		tabKey:  j.tabKey,
		paneKey: j.paneKey,
	}
}

func (pc *parallelCoordinator) handleDone(done workerDone) {
	pc.active--
	delete(pc.inFlightPR, done.prKey)
	if done.tabKey != "" {
		delete(pc.inFlightTab, done.tabKey)
	}
	if done.paneKey != "" {
		delete(pc.inFlightPan, done.paneKey)
	}
	pc.results[done.idx] = done.item
	pc.persistWorkerOutcome(done)
	pc.applyWorkerStop(done)
}

func (pc *parallelCoordinator) persistWorkerOutcome(done workerDone) {
	if recordsDispatch(done.item.Action) {
		recordDispatch(pc.st, done.cand, pc.req, pc.now)
		if err := saveEntry(pc.store, pc.st, done.prKey); err != nil && pc.fatalErr == nil {
			pc.fatalErr = err
			pc.stopping = true
		}
	}
	if err := appendLedgerEntry(pc.req.Ledger, done.cand, pc.req, done.item, pc.now); err != nil && pc.fatalErr == nil {
		pc.fatalErr = err
		pc.stopping = true
	}
}

func (pc *parallelCoordinator) applyWorkerStop(done workerDone) {
	if done.item.Action == ActionDispatchedTimeout {
		pc.settledTO = true
	}
	switch {
	case done.gateStopErr != nil:
		if pc.gateErr == nil {
			pc.gateErr = done.gateStopErr
		}
		pc.stopping = true
	case done.fatalErr != nil:
		pc.stopGate.Store(true)
		if pc.fatalErr == nil {
			pc.fatalErr = done.fatalErr
		}
		pc.stopping = true
	case done.item.Action == ActionFailed:
		pc.stopGate.Store(true)
		if pc.fatalErr == nil {
			if err := pc.ctx.Err(); err != nil {
				pc.fatalErr = fmt.Errorf("dispatch: %w", err)
			} else {
				pc.fatalErr = fmt.Errorf("%w: %s", ErrFailed, done.item.Detail)
			}
		}
		pc.stopping = true
	}
}

func (pc *parallelCoordinator) drainRemaining() {
	for _, idx := range pc.pending {
		c := pc.cands[idx]
		var item Item
		if pc.gateErr == nil && pc.fatalErr == nil && pc.ctx.Err() != nil {
			item = failItem(c, pc.ctx.Err())
			pc.fatalErr = fmt.Errorf("dispatch: %w", pc.ctx.Err())
		} else {
			item = Item{Repo: c.Repo, Number: c.Number, Action: ActionQueued}
		}
		logResult(pc.ctx, item)
		pc.results[idx] = item
	}
	pc.pending = nil
}

func summarizeParallel(results []Item, maxInFlight int) RunSummary {
	out := RunSummary{MaxInFlight: maxInFlight}
	for _, r := range results {
		switch r.Action {
		case ActionDispatched, ActionOpenedTabAndDispatched:
			out.Finished++
		case ActionDispatchedTimeout, ActionGateTimeout:
			out.TimedOut++
		case ActionDispatchedBlocked, ActionGateUnsafe:
			out.Blocked++
		case ActionFailed, ActionOpenTabFailed:
			out.Failed++
		case ActionQueued:
			out.Queued++
		default:
			out.Skipped++
		}
	}
	return out
}

func logParallelSummary(ctx context.Context, s RunSummary) {
	runlog.FromContext(ctx).Info("dispatch_parallel_summary",
		"max_in_flight", s.MaxInFlight,
		"in_flight", s.InFlight,
		"finished", s.Finished,
		"timed_out", s.TimedOut,
		"blocked", s.Blocked,
		"failed", s.Failed,
		"queued", s.Queued,
		"skipped", s.Skipped,
	)
	writeStatus(ctx, fmt.Sprintf(
		"parallel summary: max_in_flight=%d in_flight=%d finished=%d timed_out=%d blocked=%d failed=%d queued=%d skipped=%d",
		s.MaxInFlight, s.InFlight, s.Finished, s.TimedOut, s.Blocked, s.Failed, s.Queued, s.Skipped,
	))
}

func appendLedgerEntry(lgr *ledger.Store, c Candidate, req Request, item Item, now time.Time) error {
	if lgr == nil {
		return nil
	}
	outcome, ok := ledgerOutcome(item.Action)
	if !ok {
		return nil
	}
	var headSHA string
	if c.PR != nil {
		headSHA = c.PR.HeadSHA
	}
	kind := item.Kind
	if kind == "" {
		kind = candidateKind(c, req)
	}
	if err := lgr.Append(ledger.Entry{
		PR:      prKey(c.Repo, c.Number),
		HeadSHA: headSHA,
		Kind:    kind,
		Time:    now,
		Outcome: outcome,
		Detail:  item.Detail,
	}); err != nil {
		return fmt.Errorf("append ledger: %w", err)
	}
	return nil
}

func ledgerOutcome(action string) (string, bool) {
	switch action {
	case ActionDispatched, ActionOpenedTabAndDispatched:
		return ledger.OutcomeDispatched, true
	case ActionDispatchedTimeout:
		return ledger.OutcomeTimeout, true
	case ActionDispatchedBlocked:
		return ledger.OutcomeBlocked, true
	case ActionFailed, ActionOpenTabFailed:
		return ledger.OutcomeFailed, true
	default:
		return "", false
	}
}
