package quotapace

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"time"
)

// Sent is the state check last notified for one profile and window.
type Sent struct {
	State string    `json:"state"`
	At    time.Time `json:"at"`
}

// CheckResult is what check found. Notify is true when a problem is new,
// changed state, or is due again after the repeat interval.
type CheckResult struct {
	Problems []string
	Notify   bool
	Code     int
	Next     map[string]Sent
}

// Check compares the snapshot with what was last sent. Green windows
// are forgotten, so a later problem notifies again. A zero repeat never
// repeats an unchanged state.
func Check(snap Snapshot, prev map[string]Sent, now time.Time, repeat time.Duration) CheckResult {
	res := CheckResult{Next: map[string]Sent{}}
	for _, ws := range snap.Windows() {
		res.Code = max(res.Code, ExitCode(ws.State))
		if ws.State == Green {
			continue
		}
		key := ws.Profile + "/" + ws.Window
		res.Problems = append(res.Problems, problemLine(ws, now))
		last, seen := prev[key]
		due := !seen || last.State != ws.State || (repeat > 0 && now.Sub(last.At) >= repeat)
		if due {
			res.Notify = true
			res.Next[key] = Sent{State: ws.State, At: now}
		} else {
			res.Next[key] = last
		}
	}
	if res.Notify {
		// Every problem goes out in one message, so all are sent now.
		for key, sent := range res.Next {
			res.Next[key] = Sent{State: sent.State, At: now}
		}
	}
	return res
}

func problemLine(ws WindowStatus, now time.Time) string {
	if ws.State == Missing || ws.Projection == nil {
		return ws.Profile + " " + ws.StatusLine(now)
	}
	pr := ws.Projection
	return fmt.Sprintf("%s %s %s: %.0f%% at %s (on track %.0f%%), projected %.0f%% at reset",
		ws.Profile, ws.Window, ws.State, pr.Percent, pr.At.UTC().Format(time.RFC3339), pr.OnTrack, pr.Projected)
}

func (s Store) checkPath() string { return filepath.Join(s.Dir, "check.json") }

// LoadSent reads what check last notified; a missing file is empty.
func (s Store) LoadSent() (map[string]Sent, error) {
	sent := map[string]Sent{}
	data, err := os.ReadFile(s.checkPath())
	if errors.Is(err, fs.ErrNotExist) {
		return sent, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read check state: %w", err)
	}
	if err := json.Unmarshal(data, &sent); err != nil {
		return nil, fmt.Errorf("decode check state: %w", err)
	}
	return sent, nil
}

// SaveSent replaces the check state.
func (s Store) SaveSent(sent map[string]Sent) error {
	return writeJSON(s.checkPath(), sent)
}
