package jobs

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

const (
	// stateFile is the persisted queue in the daemon state directory.
	stateFile = "jobs.json"
	// holdFile is the pause file. While it exists no job is claimed.
	holdFile    = "jobs.hold"
	fileVersion = 1
)

// ErrNoJob is a job id the store does not have.
var ErrNoJob = errors.New("no such job")

// Args is the jobs part of a daemon request.
type Args struct {
	ID       string `json:"id,omitempty"`
	Kind     string `json:"kind,omitempty"`
	Subject  string `json:"subject,omitempty"`
	Head     string `json:"head,omitempty"`
	Needs    Needs  `json:"needs,omitzero"`
	Priority int    `json:"priority,omitempty"`
	// Runner is the claimer and holder: a runner's own name.
	Runner string `json:"runner,omitempty"`
	// Kinds and Machine narrow a claim. Empty Kinds is every kind. A job
	// that needs one machine is claimed only by a runner naming it.
	Kinds   []string `json:"kinds,omitempty"`
	Machine string   `json:"machine,omitempty"`
	// Exit is the runner's exit code on fail.
	Exit *int   `json:"exit,omitempty"`
	Note string `json:"note,omitempty"`
	// State, Kind, Subject, and All filter ls. Without State or All, ls
	// lists live jobs only.
	State string `json:"state,omitempty"`
	All   bool   `json:"all,omitempty"`
	// Items are an enqueued job's work items, or on park the items a
	// child job takes.
	Items []string `json:"items,omitempty"`
	// Decide, Saved, and Tab are a park's decide item, saved branch
	// ref, and the parking agent's pane, which the daemon sets.
	Decide string `json:"decide,omitempty"`
	Saved  string `json:"saved,omitempty"`
	Tab    string `json:"tab,omitempty"`
	// AskMsg is the agent bus message a park waits on a reply to,
	// instead of a decide item, and AskTo the name it was sent to,
	// which the daemon sets.
	AskMsg string `json:"askMsg,omitempty"`
	AskTo  string `json:"askTo,omitempty"`
	// Calls is the GitHub API calls a reserve takes from the budget.
	Calls int `json:"calls,omitempty"`
}

// Reply is the jobs part of a daemon response.
type Reply struct {
	Job  *Job  `json:"job,omitempty"`
	Jobs []Job `json:"jobs,omitempty"`
	// Child is the gated job a partial park made.
	Child *Job `json:"child,omitempty"`
	// Duplicate is an enqueue that matched a live job, returned in Job.
	Duplicate bool `json:"duplicate,omitempty"`
	// Held is a claim refused because jobs are on hold.
	Held     bool   `json:"held,omitempty"`
	HoldNote string `json:"holdNote,omitempty"`
	Stats    *Stats `json:"stats,omitempty"`
	// Reserved is a reserve the budget granted. Budget is the budget
	// after it, nil when there is none.
	Reserved bool    `json:"reserved,omitempty"`
	Budget   *Budget `json:"budget,omitempty"`
	// Status is the per-machine status view.
	Status *StatusView `json:"status,omitempty"`
}

// Stats counts jobs by state.
type Stats struct {
	States   map[State]int `json:"states"`
	Held     bool          `json:"held"`
	HoldNote string        `json:"holdNote,omitempty"`
	// Budget is the shared GitHub API budget, nil when there is none.
	Budget *Budget `json:"budget,omitempty"`
}

// EventLog is where a Store logs its events.
type EventLog interface {
	Append(e eventlog.Event) error
}

type file struct {
	Version int `json:"version"`
	// Seq is the last job number given. It keeps growing across restarts.
	Seq  uint64 `json:"seq"`
	Jobs []*Job `json:"jobs"`
	// Reported are the keys of the anomalies reported and still active.
	Reported []string `json:"anomalies,omitempty"`
	// Budget is the GitHub API budget's current window.
	Budget budgetState `json:"budget,omitzero"`
}

// Store is the job queue, persisted in the daemon state directory. All
// methods are safe for concurrent use; claims are serialized, so two
// claimers never get the same job.
type Store struct {
	mu     sync.Mutex
	dir    string
	doc    file
	log    EventLog
	now    func() time.Time
	cfg    Config
	hold   func() (bool, string)
	policy Policy
	leases Leaser
	// frees are the leases the change under way lets go of. mutate
	// releases them once the change is saved.
	frees []heldLease
	// runners are the runners' last beats, kept in memory only.
	runners map[string]runnerSeen
}

// Leaser is the checkout lease table as the job queue uses it.
type Leaser interface {
	// Take takes the checkout lease job j needs for runner on machine,
	// and returns the lease key. It never queues: an error leaves j
	// ready. It is called with the queue locked, so it must not call
	// back into the queue.
	Take(j Job, machine, runner string) (string, error)
	// Free releases the lease key that job id holds. It is called with
	// the queue unlocked.
	Free(key, id string)
}

// heldLease is a lease key and the job that holds it.
type heldLease struct{ key, id string }

// SetLeaser makes claims of a job that needs a checkout take its lease
// with the claim, and the job's end release it. Without one no lease is
// taken.
func (s *Store) SetLeaser(l Leaser) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.leases = l
}

// SetPolicy sets the user's gate policy for jobs enqueued from now on.
// Until it is set, every kind is ask.
func (s *Store) SetPolicy(p Policy) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.policy = p
}

// Open loads the store from dir. A missing file is an empty queue.
func Open(dir string, log EventLog, now func() time.Time, cfg Config) (*Store, error) {
	if now == nil {
		now = time.Now
	}
	doc := file{Version: fileVersion}
	data, err := os.ReadFile(filepath.Join(dir, stateFile))
	switch {
	case errors.Is(err, os.ErrNotExist):
	case err != nil:
		return nil, fmt.Errorf("read jobs: %w", err)
	default:
		if err := json.Unmarshal(data, &doc); err != nil {
			return nil, fmt.Errorf("decode jobs: %w", err)
		}
		if doc.Version != fileVersion {
			return nil, fmt.Errorf("jobs file version %d, want %d", doc.Version, fileVersion)
		}
	}
	return &Store{dir: dir, doc: doc, log: log, now: now, cfg: cfg}, nil
}

// Config is the store's jobs config.
func (s *Store) Config() Config { return s.cfg }

// SetExternalHold adds a hold check besides the pause file, such as the
// user's decide status. It returns whether jobs are held, and why.
func (s *Store) SetExternalHold(f func() (bool, string)) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.hold = f
}

// Hold creates the pause file with note. Claims stop until Unhold.
func (s *Store) Hold(note string) error {
	if err := os.MkdirAll(s.dir, 0o755); err != nil {
		return fmt.Errorf("create state dir: %w", err)
	}
	if err := os.WriteFile(filepath.Join(s.dir, holdFile), []byte(note+"\n"), 0o600); err != nil {
		return fmt.Errorf("write hold: %w", err)
	}
	return s.logEvent("user", "hold", note)
}

// Unhold removes the pause file. A missing file is not an error.
func (s *Store) Unhold() error {
	if err := os.Remove(filepath.Join(s.dir, holdFile)); err != nil && !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("remove hold: %w", err)
	}
	return s.logEvent("user", "unhold", "")
}

// HoldPath is the pause file's path.
func (s *Store) HoldPath() string { return filepath.Join(s.dir, holdFile) }

// Held reports whether claims are stopped, and why: the pause file, or
// the external check.
func (s *Store) Held() (bool, string) {
	data, err := os.ReadFile(s.HoldPath())
	if err == nil || !errors.Is(err, os.ErrNotExist) {
		note := strings.TrimSpace(string(data))
		if note == "" {
			note = "pause file " + s.HoldPath()
		}
		return true, note
	}
	s.mu.Lock()
	f := s.hold
	s.mu.Unlock()
	if f != nil {
		return f()
	}
	return false, ""
}

func (s *Store) logEvent(actor, detail, reason string) error {
	if reason == "" {
		reason = "-"
	}
	if err := s.log.Append(eventlog.Event{Event: eventlog.EventJob, Actor: actor, Detail: detail, Reason: reason}); err != nil {
		return fmt.Errorf("log job event: %w", err)
	}
	return nil
}

// Enqueue adds a job, or returns the live job with the same kind,
// subject, and head as a duplicate. The job starts queued and passes
// the gate the user's policy sets for its kind: auto goes to ready, ask
// waits in gated for the user, and off is refused. The producer never
// sets the gate.
func (s *Store) Enqueue(a Args) (Reply, error) {
	if err := checkKind(a.Kind); err != nil {
		return Reply{}, err
	}
	subject := strings.TrimSpace(a.Subject)
	if subject == "" {
		return Reply{}, errors.New("a job needs a subject, such as owner/repo#12 or ABC-123")
	}
	needs := a.Needs
	if err := needs.normalize(); err != nil {
		return Reply{}, err
	}
	key := a.Kind + " " + subject + " " + a.Head
	var r Reply
	err := s.mutate(func(now time.Time) ([]eventlog.Event, error) {
		gate := s.policy.Gate(a.Kind)
		if gate == GateOff {
			return nil, fmt.Errorf("kind %s is off in the jobs policy", a.Kind)
		}
		items, dup := s.unparked(a.Kind, subject, key, a.Items)
		if dup != nil {
			r = Reply{Job: clone(dup), Duplicate: true}
			return nil, nil
		}
		s.doc.Seq++
		k := s.cfg.Kind(a.Kind)
		j := &Job{
			ID: fmt.Sprintf("j-%d", s.doc.Seq), Kind: a.Kind, Subject: subject, Head: a.Head,
			Dedupe: key, Needs: needs, Priority: a.Priority, State: StateQueued,
			MaxAttempts: k.MaxAttempts, Created: now, Updated: now, Note: a.Note, Items: items,
			History: []Entry{{At: now, To: StateQueued, Actor: "producer", Note: a.Note}},
		}
		s.doc.Jobs = append(s.doc.Jobs, j)
		events := []eventlog.Event{jobEvent(j, "", "producer", a.Note)}
		to, note := StateReady, ""
		if gate == GateAsk {
			to, note = StateGated, "the jobs policy asks the user first"
		}
		ev, err := s.move(j, to, "policy", note, now)
		if err != nil {
			return nil, err
		}
		r = Reply{Job: clone(j)}
		return append(events, ev), nil
	})
	return r, err
}

// Claim gives runner the next ready job, or none. Jobs on hold, not
// before their notBefore, of other kinds or machines, or on a subject a
// blocked job holds are skipped. Higher priority goes first, then the
// oldest. A job whose kind's cost does not fit the GitHub budget is
// deferred to the budget's reset, and the next one is tried; the claim
// reserves the cost of the job it gives. A job that needs a checkout
// takes its lease, keyed to the runner's machine, in the same step: if
// the lease is not free the job stays ready and the next one is tried.
func (s *Store) Claim(a Args) (Reply, error) {
	if strings.TrimSpace(a.Runner) == "" {
		return Reply{}, errors.New("claim needs --runner")
	}
	if held, why := s.Held(); held {
		return Reply{Held: true, HoldNote: why}, nil
	}
	var (
		r     Reply
		taken heldLease
	)
	err := s.mutate(func(now time.Time) ([]eventlog.Event, error) {
		pick, key, events := s.pick(a, now)
		if pick == nil {
			return events, nil
		}
		s.spend(s.cfg.Kind(pick.Kind).Cost)
		if key != "" {
			taken = heldLease{key: key, id: pick.ID}
		}
		ev, err := s.move(pick, StateRunning, a.Runner, "", now)
		if err != nil {
			return nil, err
		}
		pick.Holder, pick.Heartbeat, pick.NotBefore, pick.Asked = a.Runner, now, time.Time{}, false
		pick.Lease = key
		pick.Attempts++
		r = Reply{Job: clone(pick)}
		return append(events, ev), nil
	})
	if err != nil && taken.key != "" && !s.holds(taken) {
		// The claim was not saved, so neither is the lease.
		s.leaser().Free(taken.key, taken.id)
	}
	return r, err
}

// holds reports whether the store has the job holding the lease.
func (s *Store) holds(l heldLease) bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	j := s.find(l.id)
	return j != nil && j.Lease == l.key
}

// pick is the job a claims and the lease key it took, if it needs one,
// and the events of the jobs it deferred for the budget. The caller
// holds s.mu.
func (s *Store) pick(a Args, now time.Time) (*Job, string, []eventlog.Event) {
	blocked := s.blockedSubjects()
	var cands []*Job
	for _, j := range s.doc.Jobs {
		if j.State != StateReady || now.Before(j.NotBefore) || blocked[j.Subject] {
			continue
		}
		if len(a.Kinds) > 0 && !slices.Contains(a.Kinds, j.Kind) {
			continue
		}
		if j.Needs.Machine != "any" && j.Needs.Machine != a.Machine {
			continue
		}
		cands = append(cands, j)
	}
	// Stable, so the oldest goes first among equal priorities.
	slices.SortStableFunc(cands, func(x, y *Job) int { return y.Priority - x.Priority })
	var deferred []eventlog.Event
	for _, j := range cands {
		if k := s.cfg.Kind(j.Kind); !s.budgetFits(k.Cost, k.BelowFloor, now) {
			deferred = append(deferred, s.deferJob(j))
			continue
		}
		if j.Needs.Checkout == "none" || s.leases == nil {
			return j, "", deferred
		}
		if key, err := s.leases.Take(*clone(j), a.Machine, a.Runner); err == nil {
			return j, key, deferred
		}
	}
	return nil, "", deferred
}

// blockedSubjects are the subjects with a gated or unknown job whose
// kind blocks. The caller holds s.mu.
func (s *Store) blockedSubjects() map[string]bool {
	out := map[string]bool{}
	for _, j := range s.doc.Jobs {
		if j.ParkedOn != "" {
			continue
		}
		if (j.State == StateGated || j.State == StateUnknown) && s.cfg.Kind(j.Kind).OnBlocked == OnBlockedBlock {
			out[j.Subject] = true
		}
	}
	return out
}

// Heartbeat records that the holder of a running job is still at it.
func (s *Store) Heartbeat(a Args) (Reply, error) {
	return s.update(a.ID, func(j *Job, now time.Time) ([]eventlog.Event, error) {
		if err := holds(j, a.Runner); err != nil {
			return nil, err
		}
		j.Heartbeat = now
		return nil, nil
	})
}

// Done finishes a job: a running job by its holder, or an unknown or
// parked job by anyone settling it.
func (s *Store) Done(a Args) (Reply, error) {
	return s.update(a.ID, func(j *Job, now time.Time) ([]eventlog.Event, error) {
		if err := finisher(j, a.Runner); err != nil {
			return nil, err
		}
		ev, err := s.move(j, StateDone, actor(a.Runner), a.Note, now)
		return []eventlog.Event{ev}, err
	})
}

// Fail reports a failed job. A running job with an exit code its kind
// marks retryable goes back to ready after the backoff while attempts
// remain, and is dead once they are used up. Any other fail is failed.
// An unknown or parked job is never retried.
func (s *Store) Fail(a Args) (Reply, error) {
	return s.update(a.ID, func(j *Job, now time.Time) ([]eventlog.Event, error) {
		if err := finisher(j, a.Runner); err != nil {
			return nil, err
		}
		k := s.cfg.Kind(j.Kind)
		j.Exit = a.Exit
		to := StateFailed
		if j.State == StateRunning && a.Exit != nil && slices.Contains(k.RetryExits, *a.Exit) {
			to = StateDead
			if j.Attempts < j.MaxAttempts {
				to = StateReady
			}
		}
		ev, err := s.move(j, to, actor(a.Runner), a.Note, now)
		if err != nil {
			return nil, err
		}
		if to == StateReady {
			j.Holder, j.Heartbeat = "", time.Time{}
			j.NotBefore = now.Add(backoff(k.Backoff, j.Attempts))
		}
		return []eventlog.Event{ev}, nil
	})
}

// Gate hands a running job to the user: its holder found it cannot go
// on without them, such as a dialog on the target pane. The job is
// gated, lets go of its lease, and the ask gate raises a new decide
// item naming the note.
func (s *Store) Gate(a Args) (Reply, error) {
	return s.update(a.ID, func(j *Job, now time.Time) ([]eventlog.Event, error) {
		if err := holds(j, a.Runner); err != nil {
			return nil, err
		}
		ev, err := s.move(j, StateGated, a.Runner, a.Note, now)
		if err != nil {
			return nil, err
		}
		j.Holder, j.Heartbeat, j.Decide = "", time.Time{}, ""
		return []eventlog.Event{ev}, nil
	})
}

// backoff is base doubled for each attempt after the first, up to an
// hour.
func backoff(base time.Duration, attempts int) time.Duration {
	d := base
	for i := 1; i < attempts && d < maxBackoff; i++ {
		d *= 2
	}
	return min(d, maxBackoff)
}

func actor(runner string) string {
	if runner == "" {
		return "user"
	}
	return runner
}

// holds checks that runner holds the running job j.
func holds(j *Job, runner string) error {
	if j.State != StateRunning {
		return fmt.Errorf("job %s is %s, not running", j.ID, j.State)
	}
	if runner == "" || j.Holder != runner {
		return fmt.Errorf("job %s is held by %s, not %q", j.ID, j.Holder, runner)
	}
	return nil
}

// finisher checks that a done or fail may close j: its holder while it
// runs, or anyone while it is unknown or parked.
func finisher(j *Job, runner string) error {
	switch j.State {
	case StateRunning:
		return holds(j, runner)
	case StateUnknown, StateParked:
		return nil
	default:
		return fmt.Errorf("job %s is %s; only a running, unknown, or parked job can be closed", j.ID, j.State)
	}
}

// Expire applies heartbeat loss to every running job whose heartbeat
// is older than the timeout. An idempotent job goes back to ready while
// attempts remain, else dead. An outward job goes to unknown, or is
// parked when its kind parks, and is never run again. It returns the
// jobs it changed.
func (s *Store) Expire() ([]Job, error) {
	var changed []Job
	err := s.mutate(func(now time.Time) ([]eventlog.Event, error) {
		var events []eventlog.Event
		for _, j := range s.doc.Jobs {
			if j.State != StateRunning || now.Sub(j.Heartbeat) <= s.cfg.Timeout() {
				continue
			}
			k := s.cfg.Kind(j.Kind)
			note := fmt.Sprintf("no heartbeat from %s since %s", j.Holder, j.Heartbeat.Format(time.RFC3339))
			to := StateUnknown
			switch {
			case k.Idempotent && j.Attempts < j.MaxAttempts:
				to = StateReady
			case k.Idempotent:
				to = StateDead
			case k.OnBlocked == OnBlockedPark:
				to = StateParked
				j.ParkedFrom = StateUnknown
			}
			ev, err := s.move(j, to, "daemon", note, now)
			if err != nil {
				return nil, err
			}
			if to == StateReady {
				j.Holder, j.Heartbeat = "", time.Time{}
			}
			events = append(events, ev)
			changed = append(changed, *clone(j))
		}
		return events, nil
	})
	if err != nil {
		return nil, err
	}
	return changed, nil
}

// Unsettled returns the lost outward jobs, unknown or parked from
// unknown, that no decide item was raised for yet.
func (s *Store) Unsettled() []Job {
	s.mu.Lock()
	defer s.mu.Unlock()
	var out []Job
	for _, j := range s.doc.Jobs {
		if j.unsettled() && !j.Asked {
			out = append(out, *clone(j))
		}
	}
	return out
}

// errSettled is a settle or ask of a job someone settled meanwhile.
var errSettled = errors.New("job is already settled")

// Settle closes an unsettled job as done or failed from a reconcile.
func (s *Store) Settle(id string, out Outcome, note string) error {
	to := StateDone
	if out == OutcomeFailed {
		to = StateFailed
	}
	_, err := s.update(id, func(j *Job, now time.Time) ([]eventlog.Event, error) {
		if !j.unsettled() {
			return nil, errSettled
		}
		ev, err := s.move(j, to, "reconcile", note, now)
		return []eventlog.Event{ev}, err
	})
	return err
}

// MarkAsked records that a decide item was raised for an unsettled job.
func (s *Store) MarkAsked(id string) error {
	_, err := s.update(id, func(j *Job, _ time.Time) ([]eventlog.Event, error) {
		if !j.unsettled() {
			return nil, errSettled
		}
		j.Asked = true
		return nil, nil
	})
	return err
}

// Gated returns the gated jobs.
func (s *Store) Gated() []Job {
	s.mu.Lock()
	defer s.mu.Unlock()
	var out []Job
	for _, j := range s.doc.Jobs {
		if j.State == StateGated {
			out = append(out, *clone(j))
		}
	}
	return out
}

// errNotGated is a gate change on a job that left gated meanwhile, or
// whose decide item is another.
var errNotGated = errors.New("job is not waiting on this decide item")

// SetDecide records the decide item that asks the user about a gated
// job that has none.
func (s *Store) SetDecide(id, item string) error {
	_, err := s.update(id, func(j *Job, _ time.Time) ([]eventlog.Event, error) {
		if j.State != StateGated || j.Decide != "" {
			return nil, errNotGated
		}
		j.Decide = item
		return nil, nil
	})
	return err
}

// Ungate moves a gated job on the user's answer in its decide item:
// approved to ready, rejected to failed with note.
func (s *Store) Ungate(id, item string, approved bool, note string) error {
	to := StateFailed
	if approved {
		to = StateReady
	}
	_, err := s.update(id, func(j *Job, now time.Time) ([]eventlog.Event, error) {
		if j.State != StateGated || j.Decide != item {
			return nil, errNotGated
		}
		ev, err := s.move(j, to, "decide", note, now)
		return []eventlog.Event{ev}, err
	})
	return err
}

// List returns jobs in creation order: live ones, or with a.All every
// job, or those in a.State; narrowed by a.Kind and a.Subject.
func (s *Store) List(a Args) (Reply, error) {
	var state State
	if a.State != "" {
		var err error
		if state, err = parseState(a.State); err != nil {
			return Reply{}, err
		}
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	out := []Job{}
	for _, j := range s.doc.Jobs {
		switch {
		case state != "" && j.State != state,
			state == "" && !a.All && j.State.Final(),
			a.Kind != "" && j.Kind != a.Kind,
			a.Subject != "" && j.Subject != a.Subject:
			continue
		}
		out = append(out, *clone(j))
	}
	return Reply{Jobs: out}, nil
}

// Show returns one job.
func (s *Store) Show(id string) (Reply, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	j := s.find(id)
	if j == nil {
		return Reply{}, fmt.Errorf("%w: %s", ErrNoJob, id)
	}
	return Reply{Job: clone(j)}, nil
}

// Stats counts jobs by state and reports the hold.
func (s *Store) Stats() (Reply, error) {
	held, why := s.Held()
	st := &Stats{States: map[State]int{}, Held: held, HoldNote: why}
	s.mu.Lock()
	defer s.mu.Unlock()
	st.Budget = s.budgetAt(s.now().UTC())
	for _, j := range s.doc.Jobs {
		st.States[j.State]++
	}
	return Reply{Stats: st}, nil
}

func (s *Store) find(id string) *Job {
	for _, j := range s.doc.Jobs {
		if j.ID == id {
			return j
		}
	}
	return nil
}

// move changes j's state, checking the state machine, and records it.
func (s *Store) move(j *Job, to State, who, note string, now time.Time) (eventlog.Event, error) {
	from := j.State
	if !CanTransition(from, to) {
		return eventlog.Event{}, fmt.Errorf("job %s cannot go from %s to %s", j.ID, from, to)
	}
	if to != StateParked {
		j.ParkedFrom = ""
	}
	// A job holds its lease while it runs, and while it is unknown: a
	// lost runner may have left the checkout half changed.
	if j.Lease != "" && to != StateRunning && to != StateUnknown {
		s.frees = append(s.frees, heldLease{key: j.Lease, id: j.ID})
		j.Lease = ""
	}
	j.State, j.Updated = to, now
	if note != "" {
		j.Note = note
	}
	j.History = append(j.History, Entry{At: now, From: from, To: to, Actor: who, Note: note})
	return jobEvent(j, from, who, note), nil
}

func jobEvent(j *Job, from State, who, note string) eventlog.Event {
	detail := fmt.Sprintf("%s %s %s: %s", j.ID, j.Kind, j.Subject, j.State)
	if from != "" {
		detail = fmt.Sprintf("%s %s %s: %s -> %s", j.ID, j.Kind, j.Subject, from, j.State)
	}
	return eventlog.Event{Event: eventlog.EventJob, Actor: who, Detail: detail, Reason: note}
}

// update runs change on one job.
func (s *Store) update(id string, change func(*Job, time.Time) ([]eventlog.Event, error)) (Reply, error) {
	var r Reply
	err := s.mutate(func(now time.Time) ([]eventlog.Event, error) {
		j := s.find(id)
		if j == nil {
			return nil, fmt.Errorf("%w: %s", ErrNoJob, id)
		}
		events, err := change(j, now)
		if err != nil {
			return nil, err
		}
		r = Reply{Job: clone(j)}
		return events, nil
	})
	return r, err
}

// mutate runs change under the lock, saves, then logs its events and,
// with the lock released, releases the leases the change let go of. A
// change that fails, or a save that fails, leaves the store as it was.
func (s *Store) mutate(change func(now time.Time) ([]eventlog.Event, error)) error {
	frees, err := s.mutateLocked(change)
	for _, f := range frees {
		s.leaser().Free(f.key, f.id)
	}
	return err
}

func (s *Store) leaser() Leaser {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.leases
}

// mutateLocked is mutate up to the lease releases, which it returns
// once the change is saved.
func (s *Store) mutateLocked(change func(now time.Time) ([]eventlog.Event, error)) ([]heldLease, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.frees = nil
	defer func() { s.frees = nil }()
	before, err := json.Marshal(s.doc)
	if err != nil {
		return nil, fmt.Errorf("encode jobs: %w", err)
	}
	restore := func() {
		var doc file
		if json.Unmarshal(before, &doc) == nil {
			s.doc = doc
		}
	}
	events, err := change(s.now().UTC())
	if err != nil {
		restore()
		return nil, err
	}
	after, err := json.Marshal(s.doc)
	if err != nil {
		restore()
		return nil, fmt.Errorf("encode jobs: %w", err)
	}
	if string(after) == string(before) {
		return nil, nil
	}
	if err := s.save(); err != nil {
		restore()
		return nil, err
	}
	frees := s.frees
	if s.leases == nil {
		frees = nil
	}
	for _, ev := range events {
		if err := s.log.Append(ev); err != nil {
			return frees, fmt.Errorf("log job event: %w", err)
		}
	}
	return frees, nil
}

// save writes the queue to a temp file and renames it into place, so a
// crash never leaves a half-written file. The caller holds s.mu.
func (s *Store) save() error {
	data, err := json.MarshalIndent(s.doc, "", "  ")
	if err != nil {
		return fmt.Errorf("encode jobs: %w", err)
	}
	data = append(data, '\n')
	if err := os.MkdirAll(s.dir, 0o755); err != nil {
		return fmt.Errorf("create state dir: %w", err)
	}
	tmp, err := os.CreateTemp(s.dir, ".jobs-*.tmp")
	if err != nil {
		return fmt.Errorf("write jobs: %w", err)
	}
	_, werr := tmp.Write(data)
	cerr := tmp.Close()
	if werr == nil && cerr == nil {
		if err := os.Rename(tmp.Name(), filepath.Join(s.dir, stateFile)); err != nil {
			_ = os.Remove(tmp.Name())
			return fmt.Errorf("replace jobs: %w", err)
		}
		return nil
	}
	_ = os.Remove(tmp.Name())
	return fmt.Errorf("write jobs: %w", errors.Join(werr, cerr))
}

func clone(j *Job) *Job {
	c := *j
	c.History = slices.Clone(j.History)
	c.Items = slices.Clone(j.Items)
	c.AskSeen = slices.Clone(j.AskSeen)
	if j.Exit != nil {
		e := *j.Exit
		c.Exit = &e
	}
	return &c
}
