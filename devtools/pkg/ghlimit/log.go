package ghlimit

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"text/tabwriter"
	"time"
)

// Entry is one log line for a call that waited.
type Entry struct {
	Time time.Time `json:"time"`
	PID  int       `json:"pid"`
	// Caller is the name of the process that ran gh.
	Caller string `json:"caller"`
	// Args are the gh arguments with values redacted.
	Args string `json:"args"`
	// Reason names the waits, joined by "+".
	Reason string `json:"reason"`
	WaitMS int64  `json:"waitMs"`
}

// AppendLog appends e as one JSON line to path, creating its directory.
func AppendLog(path string, e Entry) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("create log dir: %w", err)
	}
	line, err := json.Marshal(e)
	if err != nil {
		return fmt.Errorf("encode log entry: %w", err)
	}
	f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o600)
	if err != nil {
		return fmt.Errorf("open log: %w", err)
	}
	if _, err := f.Write(append(line, '\n')); err != nil {
		_ = f.Close()
		return fmt.Errorf("write log: %w", err)
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("close log: %w", err)
	}
	return nil
}

type tally struct {
	count int
	wait  time.Duration
}

func add(m map[string]*tally, key string, d time.Duration) {
	if m[key] == nil {
		m[key] = &tally{}
	}
	m[key].count++
	m[key].wait += d
}

// WriteStats summarises the log entries at path from the last window
// before now: the count and total wait, the cache hits, then the waits
// by reason and by caller.
// A missing log is no entries. Unreadable lines are skipped.
func WriteStats(w io.Writer, path string, now time.Time, window time.Duration) error {
	byReason, byCaller := map[string]*tally{}, map[string]*tally{}
	var total tally
	hits := 0
	f, err := os.Open(path)
	switch {
	case errors.Is(err, fs.ErrNotExist):
	case err != nil:
		return fmt.Errorf("open log: %w", err)
	default:
		defer f.Close()
		scanner := bufio.NewScanner(f)
		for scanner.Scan() {
			var e Entry
			if json.Unmarshal(scanner.Bytes(), &e) != nil || now.Sub(e.Time) > window {
				continue
			}
			if e.Reason == ReasonCacheHit {
				hits++
				continue
			}
			d := time.Duration(e.WaitMS) * time.Millisecond
			total.count++
			total.wait += d
			add(byReason, e.Reason, d)
			add(byCaller, e.Caller, d)
		}
		if err := scanner.Err(); err != nil {
			return fmt.Errorf("read log: %w", err)
		}
	}
	tw := tabwriter.NewWriter(w, 0, 0, 2, ' ', 0)
	fmt.Fprintf(tw, "%d waits in the last %v, %.1fs total\n", total.count, window, total.wait.Seconds())
	if hits > 0 {
		fmt.Fprintf(tw, "%d cache hits (gh runs saved)\n", hits)
	}
	for _, group := range []struct {
		title string
		m     map[string]*tally
	}{{"by reason", byReason}, {"by caller", byCaller}} {
		if len(group.m) == 0 {
			continue
		}
		fmt.Fprintf(tw, "%s:\n", group.title)
		keys := make([]string, 0, len(group.m))
		for k := range group.m {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			fmt.Fprintf(tw, "  %s\t%d\t%.1fs\n", k, group.m[k].count, group.m[k].wait.Seconds())
		}
	}
	if err := tw.Flush(); err != nil {
		return fmt.Errorf("write stats: %w", err)
	}
	return nil
}
