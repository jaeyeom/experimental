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
	return l.Config.PointsPerMin > 0 || l.Config.GraphQLTimePerMin > 0
}

// reserve waits until a call classified as info fits the budgets, then
// records it in the window. Past MaxWait it records the call anyway.
// It returns the reservation id ("" when the budgets are off) and the
// waits.
func (l *Limiter) reserve(ctx context.Context, info CallInfo) (string, []wait, error) {
	if !l.budgetsOn() {
		return "", nil, nil
	}
	var waits []wait
	begin := l.now()
	for {
		force := l.now().Sub(begin) >= l.Config.MaxWait
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
			if info.GraphQL() {
				c.GraphQL = int64(s.expected(info))
			}
			s.Calls = append(s.Calls, c)
			return true
		})
		if err != nil {
			return "", waits, err
		}
		if id != "" {
			return id, waits, nil
		}
		if err := ctx.Err(); err != nil {
			return "", waits, fmt.Errorf("wait for gh budget: %w", err)
		}
		l.sleep(d)
		if n := len(waits); n > 0 && waits[n-1].reason == reason {
			waits[n-1].d += d
		} else {
			waits = append(waits, wait{reason, d})
		}
	}
}

// finish records the measured GraphQL time of reservation id and adds
// it to the history of its query.
func (l *Limiter) finish(ctx context.Context, id string, info CallInfo, took time.Duration) {
	if id == "" || !info.GraphQL() {
		return
	}
	_ = l.withBudget(ctx, func(s *budgetState, now time.Time) bool {
		for i := range s.Calls {
			if s.Calls[i].ID == id {
				s.Calls[i].GraphQL = int64(took)
			}
		}
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
		s.prune(now)
		return true
	})
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
	unlock, _, err := l.acquire(ctx, []string{"budget.lock"})
	if err != nil {
		return err
	}
	defer unlock()
	path := filepath.Join(l.Dir, "budget.json")
	var s budgetState
	if raw, err := os.ReadFile(path); err == nil {
		_ = json.Unmarshal(raw, &s)
	}
	if !fn(&s, l.now()) {
		return nil
	}
	raw, err := json.Marshal(&s)
	if err != nil {
		return fmt.Errorf("encode budget: %w", err)
	}
	tmp, err := os.CreateTemp(l.Dir, ".budget-*")
	if err != nil {
		return fmt.Errorf("write budget: %w", err)
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.Write(raw); err != nil {
		_ = tmp.Close()
		return fmt.Errorf("write budget: %w", err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("write budget: %w", err)
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		return fmt.Errorf("write budget: %w", err)
	}
	return nil
}
