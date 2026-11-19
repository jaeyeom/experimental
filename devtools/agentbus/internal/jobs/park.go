package jobs

import (
	"cmp"
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

// checkPark checks a park's arguments: the decide item, or the agent
// bus message and its addressed name, it waits on; the saved branch
// ref; and the resume note.
func checkPark(a Args) error {
	decide, ask := strings.TrimSpace(a.Decide) != "", strings.TrimSpace(a.AskMsg) != ""
	switch {
	case decide && ask:
		return errors.New("park waits on a decide item or an agent bus message, not both")
	case !decide && !ask:
		return errors.New("park needs the decide item or the agent bus message the job waits on")
	case ask && strings.TrimSpace(a.AskTo) == "":
		return fmt.Errorf("park cannot tell who message %s was sent to", a.AskMsg)
	case strings.TrimSpace(a.Note) == "":
		return errors.New("park needs a resume note")
	}
	return checkSaved(a.Saved)
}

// checkSaved checks that ref can name a branch. A stash is refused:
// parked work is always on a branch.
func checkSaved(ref string) error {
	if ref == "" {
		return errors.New("park needs the branch ref that holds the saved work")
	}
	bad := ref == "stash" || ref == "refs/stash" || ref == "@" ||
		strings.HasPrefix(ref, "-") || strings.HasPrefix(ref, "/") || strings.HasSuffix(ref, "/") ||
		strings.HasSuffix(ref, ".lock") || strings.HasSuffix(ref, ".") ||
		strings.Contains(ref, "..") || strings.Contains(ref, "//") || strings.Contains(ref, "@{") ||
		strings.ContainsAny(ref, " \t\n~^:?*[\\")
	if bad {
		return fmt.Errorf("saved ref %q: want a branch ref such as park/pr-12; never a stash", ref)
	}
	return nil
}

// Park takes a running job off its runner mid-run, by its holder,
// while the user answers decide item a.Decide, or a.AskTo replies to
// agent bus message a.AskMsg. The work is saved on
// branch a.Saved with resume note a.Note; the caller checks that the
// checkout is clean first, since the park releases its lease. Without
// a.Items the job is parked. With them, the handled part is done and a
// gated child job holds the parked items. Either waits until the user
// answers the item, or the addressed name replies.
func (s *Store) Park(a Args) (Reply, error) {
	if err := checkPark(a); err != nil {
		return Reply{}, err
	}
	var r Reply
	err := s.mutate(func(now time.Time) ([]eventlog.Event, error) {
		j := s.find(a.ID)
		if j == nil {
			return nil, fmt.Errorf("%w: %s", ErrNoJob, a.ID)
		}
		if err := holds(j, a.Runner); err != nil {
			return nil, err
		}
		if len(a.Items) == 0 {
			ev, err := s.move(j, StateParked, a.Runner, a.Note, now)
			if err != nil {
				return nil, err
			}
			j.Holder, j.Heartbeat = "", time.Time{}
			setParked(j, a, now)
			r = Reply{Job: clone(j)}
			return []eventlog.Event{ev}, nil
		}
		for _, it := range a.Items {
			if len(j.Items) > 0 && !slices.Contains(j.Items, it) {
				return nil, fmt.Errorf("job %s has no item %s", j.ID, it)
			}
		}
		note := fmt.Sprintf("parked %s on %s", strings.Join(a.Items, ", "), parkWhat(a))
		ev, err := s.move(j, StateDone, a.Runner, note, now)
		if err != nil {
			return nil, err
		}
		child, events, err := s.child(j, a, now)
		if err != nil {
			return nil, err
		}
		r = Reply{Job: clone(j), Child: clone(child)}
		return append([]eventlog.Event{ev}, events...), nil
	})
	return r, err
}

func setParked(j *Job, a Args, now time.Time) {
	j.ParkedOn, j.Saved, j.Resume, j.Tab, j.ParkedAt = cmp.Or(a.Decide, a.AskMsg), a.Saved, a.Note, a.Tab, now
	j.AskMsg, j.AskTo = a.AskMsg, a.AskTo
}

// parkWhat names what a park waits on.
func parkWhat(a Args) string {
	if a.AskMsg != "" {
		return "message " + a.AskMsg + " to " + a.AskTo
	}
	return "decide " + a.Decide
}

// child is the gated job that holds the items parked out of parent. A
// live child parked on the same item takes the new items instead. The
// caller holds s.mu.
func (s *Store) child(parent *Job, a Args, now time.Time) (*Job, []eventlog.Event, error) {
	key := parent.Kind + " " + parent.Subject + " parked on " + cmp.Or(a.Decide, a.AskMsg)
	for _, c := range s.doc.Jobs {
		if c.Dedupe == key && !c.State.Final() {
			for _, it := range a.Items {
				if !slices.Contains(c.Items, it) {
					c.Items = append(c.Items, it)
				}
			}
			c.Updated = now
			return c, nil, nil
		}
	}
	s.doc.Seq++
	note := fmt.Sprintf("parked out of %s on %s", parent.ID, parkWhat(a))
	c := &Job{
		ID: fmt.Sprintf("j-%d", s.doc.Seq), Kind: parent.Kind, Subject: parent.Subject, Head: parent.Head,
		Dedupe: key, Needs: parent.Needs, Priority: parent.Priority, State: StateQueued,
		MaxAttempts: s.cfg.Kind(parent.Kind).MaxAttempts, Created: now, Updated: now, Note: note,
		Items: slices.Clone(a.Items), Parent: parent.ID,
		History: []Entry{{At: now, To: StateQueued, Actor: a.Runner, Note: note}},
	}
	setParked(c, a, now)
	s.doc.Jobs = append(s.doc.Jobs, c)
	ev, err := s.move(c, StateGated, a.Runner, a.Note, now)
	if err != nil {
		return nil, nil, err
	}
	return c, []eventlog.Event{jobEvent(c, "", a.Runner, note), ev}, nil
}

// unparked is items without those a live job of kind on subject holds
// parked, and the live job an enqueue with key duplicates: one with the
// same key, one parked whole, or the one holding the first item when
// every item is held. The caller holds s.mu.
func (s *Store) unparked(kind, subject, key string, items []string) ([]string, *Job) {
	for _, j := range s.doc.Jobs {
		if j.Dedupe == key && !j.State.Final() {
			return nil, j
		}
	}
	held, whole := s.parkedHolders(kind, subject)
	if whole != nil {
		return nil, whole
	}
	var out []string
	for _, it := range items {
		if held[it] == nil {
			out = append(out, it)
		}
	}
	if len(items) > 0 && len(out) == 0 {
		return nil, held[items[0]]
	}
	return out, nil
}

// parkedHolders are the live parked jobs and children of kind on
// subject by the items they hold, and one parked whole with no items.
// The caller holds s.mu.
func (s *Store) parkedHolders(kind, subject string) (map[string]*Job, *Job) {
	held := map[string]*Job{}
	var whole *Job
	for _, j := range s.doc.Jobs {
		if j.State.Final() || j.Kind != kind || j.Subject != subject || (j.ParkedOn == "" && j.Parent == "") {
			continue
		}
		if len(j.Items) == 0 && j.ParkedOn != "" && whole == nil {
			whole = j
		}
		for _, it := range j.Items {
			held[it] = j
		}
	}
	return held, whole
}

// ParkedOnItems returns the jobs waiting on a decide item they parked
// on: parked jobs and gated children.
func (s *Store) ParkedOnItems() []Job {
	s.mu.Lock()
	defer s.mu.Unlock()
	var out []Job
	for _, j := range s.doc.Jobs {
		if j.parked() && !j.waitsOnBus() {
			out = append(out, *clone(j))
		}
	}
	return out
}

// Resume makes a job parked on decide item ready again: it waits its
// turn for a runner and its checkout like any ready job, and keeps its
// saved ref and tab for the runner, and answer for its resume prompt.
func (s *Store) Resume(id, item, note, answer string) error {
	_, err := s.update(id, func(j *Job, now time.Time) ([]eventlog.Event, error) {
		if j.ParkedOn != item || j.waitsOnBus() || !j.parked() {
			return nil, fmt.Errorf("job %s is %s, not parked on %s", j.ID, j.State, item)
		}
		return resumed(s, j, "decide", note, answer, now)
	})
	return err
}

// resumed makes parked job j ready with note, and keeps answer for its
// resume prompt. The caller holds s.mu.
func resumed(s *Store, j *Job, who, note, answer string, now time.Time) ([]eventlog.Event, error) {
	ev, err := s.move(j, StateReady, who, note, now)
	if err != nil {
		return nil, err
	}
	j.ParkedOn, j.AskMsg, j.AskTo, j.AskSeen, j.ParkedAt = "", "", "", nil, time.Time{}
	j.Answer = answer
	return []eventlog.Event{ev}, nil
}

// parked reports whether j waits on a park: parked whole, or a gated
// child.
func (j *Job) parked() bool {
	return j.ParkedOn != "" && (j.State == StateParked || j.State == StateGated)
}

// waitsOnBus reports whether j waits on a reply to its agent bus
// message rather than on a decide item.
func (j *Job) waitsOnBus() bool {
	return j.AskMsg != "" && j.ParkedOn == j.AskMsg
}

// ParkTarget is what a parked job waits on: "asked <name>" for an
// agent bus reply, "decision <item>" for a decide item, or empty.
func (j Job) ParkTarget() string {
	switch {
	case !j.parked():
		return ""
	case j.waitsOnBus():
		return "asked " + j.AskTo
	default:
		return "decision " + j.ParkedOn
	}
}

// Regate moves a job parked on an agent bus message onto decide item
// item: from then on only the user's verified answer there resumes it,
// and a reply on the bus is ignored. A manager that cannot answer
// regates the job onto the item it filed.
func (s *Store) Regate(id, item string) (Reply, error) {
	if strings.TrimSpace(item) == "" {
		return Reply{}, errors.New("regate needs the decide item the job waits on")
	}
	return s.update(id, func(j *Job, now time.Time) ([]eventlog.Event, error) {
		if !j.waitsOnBus() || !j.parked() {
			return nil, fmt.Errorf("job %s is %s, not parked on an agent bus message", j.ID, j.State)
		}
		j.ParkedOn, j.ParkedAt, j.Updated = item, now, now
		note := "regated from message " + j.AskMsg + " to " + j.AskTo + " onto decide " + item
		return []eventlog.Event{jobEvent(j, "", "regate", note)}, nil
	})
}
