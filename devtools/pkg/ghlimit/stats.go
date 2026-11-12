package ghlimit

import (
	"bufio"
	"cmp"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"slices"
	"strings"
	"text/tabwriter"
	"time"
)

// statsTop is how many rows each ranking keeps.
const statsTop = 10

// StatRow is one caller, query hash or wait reason in Stats.
type StatRow struct {
	Key string `json:"key"`
	// Fields are a query's top-level field names.
	Fields    string `json:"fields,omitempty"`
	Count     int    `json:"count"`
	Points    int    `json:"points"`
	GraphQLMS int64  `json:"graphqlMs"`
	WaitMS    int64  `json:"waitMs"`
}

// Stats summarises the wait log and the call log over a window.
type Stats struct {
	Window string `json:"window"`

	Calls       int   `json:"calls"`
	Points      int   `json:"points"`
	GraphQLMS   int64 `json:"graphqlMs"`
	RateLimited int   `json:"rateLimited"`
	CacheHits   int   `json:"cacheHits"`

	Waits  int   `json:"waits"`
	WaitMS int64 `json:"waitMs"`

	CallersByPoints      []StatRow `json:"callersByPoints"`
	CallersByGraphQLTime []StatRow `json:"callersByGraphqlTime"`
	// QueriesByTime and QueriesByCount rank gh api graphql query hashes
	// by summed duration and by number of calls.
	QueriesByTime  []StatRow `json:"queriesByTime"`
	QueriesByCount []StatRow `json:"queriesByCount"`
	WaitsByReason  []StatRow `json:"waitsByReason"`
	WaitsByCaller  []StatRow `json:"waitsByCaller"`
}

// ReadStats summarises the entries of the wait log at waitLog and the
// call log at callLog (and its rotated callLog.1) from the last window
// before now. Missing logs are empty; unreadable lines are skipped.
func ReadStats(waitLog, callLog string, now time.Time, window time.Duration) (Stats, error) {
	s := Stats{Window: window.String()}
	inWindow := func(t time.Time) bool { return now.Sub(t) <= window }
	byReason, byWaitCaller, err := s.readWaits(waitLog, inWindow)
	if err != nil {
		return s, err
	}
	callers, graphqlCallers, queries := map[string]*StatRow{}, map[string]*StatRow{}, map[string]*StatRow{}
	for _, path := range []string{callLog + ".1", callLog} {
		err := scanJSONL(path, func(raw []byte) {
			var e CallEntry
			if json.Unmarshal(raw, &e) == nil && inWindow(e.Time) {
				s.addCall(e, callers, graphqlCallers, queries)
			}
		})
		if err != nil {
			return s, err
		}
	}
	s.CallersByPoints = ranked(callers, func(r *StatRow) int64 { return int64(r.Points) })
	s.CallersByGraphQLTime = ranked(graphqlCallers, func(r *StatRow) int64 { return r.GraphQLMS })
	s.QueriesByTime = ranked(queries, func(r *StatRow) int64 { return r.GraphQLMS })
	s.QueriesByCount = ranked(queries, func(r *StatRow) int64 { return int64(r.Count) })
	s.WaitsByReason = ranked(byReason, func(r *StatRow) int64 { return r.WaitMS })
	s.WaitsByCaller = ranked(byWaitCaller, func(r *StatRow) int64 { return r.WaitMS })
	return s, nil
}

// readWaits adds the wait log entries in the window to s and returns
// them by reason and by caller.
func (s *Stats) readWaits(path string, inWindow func(time.Time) bool) (byReason, byCaller map[string]*StatRow, err error) {
	byReason, byCaller = map[string]*StatRow{}, map[string]*StatRow{}
	err = scanJSONL(path, func(raw []byte) {
		var e Entry
		if json.Unmarshal(raw, &e) != nil || !inWindow(e.Time) || e.Reason == ReasonCacheHit {
			return
		}
		s.Waits++
		s.WaitMS += e.WaitMS
		for _, r := range []*StatRow{row(byReason, e.Reason), row(byCaller, e.Caller)} {
			r.Count++
			r.WaitMS += e.WaitMS
		}
	})
	return byReason, byCaller, err
}

// addCall adds one call log entry to s and to the caller and query
// tallies.
func (s *Stats) addCall(e CallEntry, callers, graphqlCallers, queries map[string]*StatRow) {
	gql := e.GraphQLTime().Milliseconds()
	s.Calls++
	s.Points += e.Points
	s.GraphQLMS += gql
	if e.RateLimited {
		s.RateLimited++
	}
	c := row(callers, e.Caller)
	c.Count++
	c.Points += e.Points
	c.GraphQLMS += gql
	if e.CacheHit {
		s.CacheHits++
		return
	}
	if (CallInfo{Kind: e.Kind}).GraphQL() {
		g := row(graphqlCallers, e.Caller)
		g.Count++
		g.GraphQLMS += gql
	}
	if e.QueryHash != "" {
		q := row(queries, e.QueryHash)
		q.Fields = strings.Join(e.QueryFields, ",")
		q.Count++
		q.Points += e.Points
		q.GraphQLMS += gql
	}
}

func row(m map[string]*StatRow, key string) *StatRow {
	if m[key] == nil {
		m[key] = &StatRow{Key: key}
	}
	return m[key]
}

// ranked is the top statsTop rows of m, highest by first, ties by key.
func ranked(m map[string]*StatRow, by func(*StatRow) int64) []StatRow {
	rows := make([]StatRow, 0, len(m))
	for _, r := range m {
		rows = append(rows, *r)
	}
	slices.SortFunc(rows, func(a, b StatRow) int {
		if c := cmp.Compare(by(&b), by(&a)); c != 0 {
			return c
		}
		return cmp.Compare(a.Key, b.Key)
	})
	return rows[:min(len(rows), statsTop)]
}

// scanJSONL calls fn with each line of the file at path. A missing file
// has no lines.
func scanJSONL(path string, fn func([]byte)) error {
	f, err := os.Open(path)
	switch {
	case errors.Is(err, fs.ErrNotExist):
		return nil
	case err != nil:
		return fmt.Errorf("open log: %w", err)
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	scanner.Buffer(make([]byte, 0, 64<<10), 1<<20)
	for scanner.Scan() {
		fn(scanner.Bytes())
	}
	if err := scanner.Err(); err != nil {
		return fmt.Errorf("read log %s: %w", path, err)
	}
	return nil
}

func secs(ms int64) string { return fmt.Sprintf("%.1fs", float64(ms)/1000) }

// WriteText writes s for a person to read.
func (s Stats) WriteText(w io.Writer) error {
	tw := tabwriter.NewWriter(w, 0, 0, 2, ' ', 0)
	fmt.Fprintf(tw, "%d waits in the last %s, %s total\n", s.Waits, s.Window, secs(s.WaitMS))
	fmt.Fprintf(tw, "%d calls, %d points, %s GraphQL time, %d rate-limited\n", s.Calls, s.Points, secs(s.GraphQLMS), s.RateLimited)
	if s.CacheHits > 0 {
		fmt.Fprintf(tw, "%d cache hits (gh runs saved)\n", s.CacheHits)
	}
	section := func(title, header string, rows []StatRow, line func(StatRow) string) {
		if len(rows) == 0 {
			return
		}
		fmt.Fprintf(tw, "%s:\n  %s\n", title, header)
		for _, r := range rows {
			fmt.Fprintf(tw, "  %s\n", line(r))
		}
	}
	waitLine := func(r StatRow) string { return fmt.Sprintf("%s\t%d\t%s", r.Key, r.Count, secs(r.WaitMS)) }
	section("waits by reason", "reason\twaits\ttotal", s.WaitsByReason, waitLine)
	section("waits by caller", "caller\twaits\ttotal", s.WaitsByCaller, waitLine)
	section("top callers by points", "caller\tcalls\tpoints", s.CallersByPoints, func(r StatRow) string {
		return fmt.Sprintf("%s\t%d\t%d", r.Key, r.Count, r.Points)
	})
	section("top callers by GraphQL time", "caller\tcalls\tGraphQL time", s.CallersByGraphQLTime, func(r StatRow) string {
		return fmt.Sprintf("%s\t%d\t%s", r.Key, r.Count, secs(r.GraphQLMS))
	})
	queryLine := func(r StatRow) string {
		return fmt.Sprintf("%s\t%d\t%s\t%s", r.Key, r.Count, secs(r.GraphQLMS), r.Fields)
	}
	section("top queries by total time", "hash\tcalls\ttime\tfields", s.QueriesByTime, queryLine)
	section("top queries by count", "hash\tcalls\ttime\tfields", s.QueriesByCount, queryLine)
	if err := tw.Flush(); err != nil {
		return fmt.Errorf("write stats: %w", err)
	}
	return nil
}
