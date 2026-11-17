package jobs

import (
	"errors"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

// DefaultBudgetWindow is how long one budget window lasts when the
// config names none: GitHub resets its core rate limit hourly.
const DefaultBudgetWindow = time.Hour

// BudgetConfig is the shared GitHub API budget: Limit calls per Window,
// and a Floor that only kinds allowed below it may spend into. A zero
// Limit is no budget.
type BudgetConfig struct {
	Limit  int
	Window time.Duration
	Floor  int
}

type budgetFile struct {
	Limit  int    `json:"limit"`
	Window string `json:"window"`
	Floor  int    `json:"floor"`
}

func budgetFromFile(f budgetFile) (BudgetConfig, error) {
	b := BudgetConfig{Limit: f.Limit, Window: DefaultBudgetWindow, Floor: f.Floor}
	if b.Limit <= 0 {
		return BudgetConfig{}, fmt.Errorf("budget: limit %d: want at least 1 call", f.Limit)
	}
	if b.Floor < 0 || b.Floor > b.Limit {
		return BudgetConfig{}, fmt.Errorf("budget: floor %d: want 0 to the limit %d", f.Floor, f.Limit)
	}
	if f.Window != "" {
		d, err := time.ParseDuration(f.Window)
		if err != nil || d <= 0 {
			return BudgetConfig{}, fmt.Errorf("budget: window %q: want a positive duration such as \"1h\"", f.Window)
		}
		b.Window = d
	}
	return b, nil
}

// Budget is the shared budget as stats and reserve report it. Reset is
// when the window ends; zero before the first reservation.
type Budget struct {
	Limit     int       `json:"limit"`
	Used      int       `json:"used"`
	Remaining int       `json:"remaining"`
	Floor     int       `json:"floor"`
	Reset     time.Time `json:"reset,omitzero"`
}

// budgetState is the budget window saved with the queue, so a daemon
// restart does not hand out the spent calls again.
type budgetState struct {
	Used  int       `json:"used"`
	Reset time.Time `json:"reset,omitzero"`
}

// budgetAt is the budget at now, or nil without one. A window past its
// reset reads as unspent. The caller holds s.mu.
func (s *Store) budgetAt(now time.Time) *Budget {
	c := s.cfg.Budget
	if c.Limit == 0 {
		return nil
	}
	st := s.doc.Budget
	if !st.Reset.IsZero() && !now.Before(st.Reset) {
		st = budgetState{}
	}
	return &Budget{Limit: c.Limit, Used: st.Used, Remaining: max(c.Limit-st.Used, 0), Floor: c.Floor, Reset: st.Reset}
}

// budgetFits reports whether calls fit the budget: what is left after
// them stays at or above the floor, or, for a kind allowed below the
// floor, at or above zero. It opens a new window when none is open.
// The caller holds s.mu.
func (s *Store) budgetFits(calls int, belowFloor bool, now time.Time) bool {
	c := s.cfg.Budget
	if c.Limit == 0 || calls == 0 {
		return true
	}
	if s.doc.Budget.Reset.IsZero() || !now.Before(s.doc.Budget.Reset) {
		s.doc.Budget = budgetState{Reset: now.Add(c.Window)}
	}
	floor := c.Floor
	if belowFloor {
		floor = 0
	}
	return c.Limit-s.doc.Budget.Used-calls >= floor
}

// spend takes calls that budgetFits allowed. The caller holds s.mu.
func (s *Store) spend(calls int) {
	if s.cfg.Budget.Limit > 0 {
		s.doc.Budget.Used += calls
	}
}

// deferJob makes a ready job whose cost does not fit wait for the
// budget's reset. The caller holds s.mu.
func (s *Store) deferJob(j *Job) eventlog.Event {
	j.NotBefore = s.doc.Budget.Reset
	detail := fmt.Sprintf("%s %s %s: deferred to %s", j.ID, j.Kind, j.Subject, j.NotBefore.Format(time.RFC3339))
	b := s.doc.Budget
	reason := fmt.Sprintf("cost %d does not fit the GitHub budget: %d of %d left, floor %d",
		s.cfg.Kind(j.Kind).Cost, s.cfg.Budget.Limit-b.Used, s.cfg.Budget.Limit, s.cfg.Budget.Floor)
	return eventlog.Event{Event: eventlog.EventJob, Actor: "budget", Detail: detail, Reason: reason}
}

// Reserve takes a.Calls GitHub API calls from the shared budget for a
// caller outside the queue, such as the scanner, before it makes them.
// With a.Kind the kind's floor exemption applies. A reservation that
// does not fit is refused, with the budget and its reset in the reply.
// Without a budget every reservation is granted.
func (s *Store) Reserve(a Args) (Reply, error) {
	if a.Calls <= 0 {
		return Reply{}, errors.New("reserve needs --calls of at least 1")
	}
	if a.Kind != "" {
		if err := checkKind(a.Kind); err != nil {
			return Reply{}, err
		}
	}
	belowFloor := a.Kind != "" && s.cfg.Kind(a.Kind).BelowFloor
	var r Reply
	err := s.mutate(func(now time.Time) ([]eventlog.Event, error) {
		r = Reply{Reserved: s.budgetFits(a.Calls, belowFloor, now)}
		if r.Reserved {
			s.spend(a.Calls)
		}
		r.Budget = s.budgetAt(now)
		return nil, nil
	})
	return r, err
}
