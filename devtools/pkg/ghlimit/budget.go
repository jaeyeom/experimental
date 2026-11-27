package ghlimit

import (
	"cmp"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"sync/atomic"
	"time"
)

// Wait reasons for the rolling per-minute budgets.
const (
	ReasonPointsBudget      = "points_budget"
	ReasonSearchBudget      = "search_budget"
	ReasonGraphQLTimeBudget = "graphql_time_budget"
)

// budgetWindow is the span the budgets are counted over.
const budgetWindow = time.Minute

// Duration history kept per query hash or subcommand, to predict a
// call's GraphQL time from the median of its recent runs.
const (
	durationSamples = 9
	durationMaxAge  = 24 * time.Hour
)

// budgetCall is one call in the window. GraphQL is its expected
// GraphQL time while it runs and its measured time after.
type budgetCall struct {
	ID      string `json:"id"`
	Start   int64  `json:"t"`
	Points  int    `json:"p,omitempty"`
	Search  int    `json:"s,omitempty"`
	GraphQL int64  `json:"g,omitempty"`
}

type durationHistory struct {
	Last    int64   `json:"last"`
	Samples []int64 `json:"d"`
}

// budgetState is the budget file shared by all of the user's processes.
type budgetState struct {
	Calls     []budgetCall                `json:"calls"`
	Durations map[string]*durationHistory `json:"durations,omitempty"`
}

var budgetSeq atomic.Int64

func (l *Limiter) budgetsOn() bool {
	return l.Config.PointsPerMin > 0 || l.Config.SearchPerMin > 0 || l.Config.GraphQLTimePerMin > 0
}

// tryReserve reserves a call in the budget window if it fits without
// sleeping, or when force is true. When a wait is needed, it returns
// id="" and the non-zero wait so the caller can release any slot before
// sleeping.
func (l *Limiter) tryReserve(ctx context.Context, info CallInfo, force bool) (string, wait, error) {
	if !l.budgetsOn() {
		return "", wait{}, nil
	}
	var (
		id     string
		d      time.Duration
		reason string
	)
	err := l.withBudget(ctx, func(s *budgetState, now time.Time) bool {
		s.prune(now)
		if !force {
			d, reason = s.waitFor(info, l.Config, now)
			if d > 0 {
				return false
			}
		}
		id = strconv.Itoa(os.Getpid()) + "-" + strconv.FormatInt(now.UnixNano(), 10) + "-" + strconv.FormatInt(budgetSeq.Add(1), 10)
		c := budgetCall{ID: id, Start: now.UnixNano(), Points: info.Points}
		if info.Kind == KindSearch {
			c.Search = 1
		}
		if info.GraphQL() {
			c.GraphQL = int64(s.expected(info))
		}
		s.Calls = append(s.Calls, c)
		return true
	})
	if err != nil {
		return "", wait{}, err
	}
	if id != "" {
		return id, wait{}, nil
	}
	return "", wait{reason, d}, nil
}

// finish records the measured GraphQL time, actual points, and search
// page units of reservation id, and updates the duration history.
func (l *Limiter) finish(ctx context.Context, id string, info CallInfo, took time.Duration, points, pages int) {
	if id == "" {
		return
	}
	searchUnits := 0
	if info.Kind == KindSearch {
		searchUnits = max(1, pages)
	}
	if !info.GraphQL() && points == info.Points && searchUnits <= 1 {
		return
	}
	_ = l.withBudget(ctx, func(s *budgetState, now time.Time) bool {
		for i := range s.Calls {
			if s.Calls[i].ID != id {
				continue
			}
			s.Calls[i].Points = points
			if info.Kind == KindSearch {
				s.Calls[i].Search = searchUnits
			}
			if info.GraphQL() {
				s.Calls[i].GraphQL = int64(took)
			}
		}
		if info.GraphQL() {
			s.recordDuration(info, now, took)
		}
		s.prune(now)
		return true
	})
}

func (s *budgetState) recordDuration(info CallInfo, now time.Time, took time.Duration) {
	if s.Durations == nil {
		s.Durations = map[string]*durationHistory{}
	}
	h := s.Durations[info.durationKey()]
	if h == nil {
		h = &durationHistory{}
		s.Durations[info.durationKey()] = h
	}
	h.Last = now.UnixNano()
	h.Samples = append(h.Samples, int64(took))
	if n := len(h.Samples); n > durationSamples {
		h.Samples = h.Samples[n-durationSamples:]
	}
}

// prune drops the calls that left the window and stale histories.
func (s *budgetState) prune(now time.Time) {
	cutoff := now.Add(-budgetWindow).UnixNano()
	s.Calls = slices.DeleteFunc(s.Calls, func(c budgetCall) bool { return c.Start <= cutoff })
	slices.SortStableFunc(s.Calls, func(a, b budgetCall) int { return cmp.Compare(a.Start, b.Start) })
	old := now.Add(-durationMaxAge).UnixNano()
	for k, h := range s.Durations {
		if h.Last < old {
			delete(s.Durations, k)
		}
	}
}

// expected is the median recent GraphQL time of calls like info, or 0
// when none ran recently.
func (s *budgetState) expected(info CallInfo) time.Duration {
	h := s.Durations[info.durationKey()]
	if h == nil || len(h.Samples) == 0 {
		return 0
	}
	sorted := slices.Clone(h.Samples)
	slices.Sort(sorted)
	return time.Duration(sorted[len(sorted)/2])
}

// waitFor is how long until a call classified as info fits both
// budgets, and the reason of the longer wait. A call into an empty
// window always fits. s must be pruned.
func (s *budgetState) waitFor(info CallInfo, cfg Config, now time.Time) (time.Duration, string) {
	var (
		longest time.Duration
		reason  string
	)
	consider := func(r string, limit, need int64, used func(budgetCall) int64) {
		if limit <= 0 {
			return
		}
		sum := sumOf(s.Calls, used)
		if sum == 0 || sum+need <= limit {
			return
		}
		for _, c := range s.Calls {
			sum -= used(c)
			if sum+need <= limit || sum == 0 {
				if d := time.Unix(0, c.Start).Add(budgetWindow).Sub(now); d > longest {
					longest, reason = d, r
				}
				return
			}
		}
	}
	consider(ReasonPointsBudget, int64(cfg.PointsPerMin), int64(info.Points), func(c budgetCall) int64 { return int64(c.Points) })
	if info.Kind == KindSearch {
		consider(ReasonSearchBudget, int64(cfg.SearchPerMin), 1, func(c budgetCall) int64 { return int64(c.Search) })
	}
	if info.GraphQL() {
		consider(ReasonGraphQLTimeBudget, int64(cfg.GraphQLTimePerMin), int64(s.expected(info)), func(c budgetCall) int64 { return c.GraphQL })
	}
	return longest, reason
}

func sumOf(calls []budgetCall, f func(budgetCall) int64) int64 {
	var sum int64
	for _, c := range calls {
		sum += f(c)
	}
	return sum
}

// withBudget runs fn on the budget state under budget.lock and writes
// the state back when fn returns true. A missing or unreadable state is
// empty.
func (l *Limiter) withBudget(ctx context.Context, fn func(*budgetState, time.Time) bool) error {
	return withLockedJSON(ctx, l, "budget.lock", "budget.json", fn)
}

// withLockedJSON runs fn on a JSON state file under a flock and writes
// the updated state atomically when fn returns true.
func withLockedJSON[T any](ctx context.Context, l *Limiter, lockName, stateFile string, fn func(*T, time.Time) bool) error {
	unlock, _, err := l.acquire(ctx, []string{lockName})
	if err != nil {
		return err
	}
	defer unlock()
	path := filepath.Join(l.Dir, stateFile)
	var s T
	if raw, err := os.ReadFile(path); err == nil {
		_ = json.Unmarshal(raw, &s)
	}
	if !fn(&s, l.now()) {
		return nil
	}
	raw, err := json.Marshal(&s)
	if err != nil {
		return fmt.Errorf("encode %s: %w", stateFile, err)
	}
	tmp, err := os.CreateTemp(l.Dir, "."+stateFile+"-*")
	if err != nil {
		return fmt.Errorf("write %s: %w", stateFile, err)
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.Write(raw); err != nil {
		_ = tmp.Close()
		return fmt.Errorf("write %s: %w", stateFile, err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("write %s: %w", stateFile, err)
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		return fmt.Errorf("write %s: %w", stateFile, err)
	}
	return nil
}
