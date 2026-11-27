package loop

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"time"

	"github.com/google/renameio/v2"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// BlockedStatePath returns the per-config blocked-tab timestamp state path in stateDir.
func BlockedStatePath(stateDir, configPath string) string {
	return filepath.Join(stateDir, "loop"+configSuffix(configPath)+".blocked.json")
}

// BlockedTracker persists the first-seen blocked timestamp per PR tab so
// tab_blocked judgment entries report how many minutes a tab has been blocked.
type BlockedTracker struct {
	Path  string
	Since map[string]time.Time
}

// LoadBlockedTracker reads persisted blocked-tab timestamps from path. A
// missing file returns an empty tracker.
func LoadBlockedTracker(path string) (*BlockedTracker, error) {
	tr := &BlockedTracker{
		Path:  path,
		Since: make(map[string]time.Time),
	}
	if path == "" {
		return tr, nil
	}
	data, err := os.ReadFile(path) //nolint:gosec // loop blocked-state path inside state dir
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return tr, nil
		}
		return tr, fmt.Errorf("read blocked state: %w", err)
	}
	if len(data) == 0 {
		return tr, nil
	}
	var loaded map[string]time.Time
	if err := json.Unmarshal(data, &loaded); err != nil {
		return tr, fmt.Errorf("decode blocked state: %w", err)
	}
	for k, v := range loaded {
		if !v.IsZero() {
			tr.Since[k] = v.UTC()
		}
	}
	return tr, nil
}

// Save atomically persists the blocked-tab timestamps to Path.
func (t *BlockedTracker) Save() error {
	if t == nil || t.Path == "" {
		return nil
	}
	if err := os.MkdirAll(filepath.Dir(t.Path), 0o700); err != nil {
		return fmt.Errorf("create blocked state dir: %w", err)
	}
	data, err := json.MarshalIndent(t.Since, "", "  ")
	if err != nil {
		return fmt.Errorf("encode blocked state: %w", err)
	}
	data = append(data, '\n')
	if err := renameio.WriteFile(t.Path, data, 0o600); err != nil {
		return fmt.Errorf("write blocked state: %w", err)
	}
	return nil
}

// Update refreshes blocked-tab timestamps from doc and res at now, clearing
// tabs that are no longer blocked and pruning disappeared PRs after a scan.
func (t *BlockedTracker) Update(doc scan.Document, res RoundResult, now time.Time) {
	if t == nil {
		return
	}
	if t.Since == nil {
		t.Since = make(map[string]time.Time)
	}
	now = now.UTC()
	seen := make(map[string]struct{}, len(doc.PRs))
	for _, pr := range doc.PRs {
		if !isOpenInScopePR(pr) || pr.Tab == nil {
			continue
		}
		key := blockedTabKey(pr)
		seen[key] = struct{}{}
		if isTabBlocked(pr, res.Dispatch.Results, res.CIFixInbox) {
			if _, ok := t.Since[key]; !ok {
				t.Since[key] = now
			}
		} else {
			delete(t.Since, key)
		}
	}
	if scan.Started(doc) {
		for key := range t.Since {
			if _, ok := seen[key]; !ok {
				delete(t.Since, key)
			}
		}
	}
}

// Minutes returns the whole minutes elapsed since pr's tab was first recorded
// as blocked, or 0 if newly observed.
func (t *BlockedTracker) Minutes(pr scan.PR, now time.Time) int {
	if t == nil {
		return 0
	}
	return blockedMinutesFromMap(t.Since, pr, now)
}

func blockedTabKey(pr scan.PR) string {
	if pr.Tab != nil && pr.Tab.TabID != "" {
		return scan.TabKey(pr.Tab.Machine, pr.Tab.TabID)
	}
	return fmt.Sprintf("%s#%d", pr.Repo, pr.Number)
}

func blockedMinutesFromMap(since map[string]time.Time, pr scan.PR, now time.Time) int {
	if len(since) == 0 {
		return 0
	}
	key := blockedTabKey(pr)
	ts, ok := since[key]
	if !ok {
		ts, ok = since[fmt.Sprintf("%s#%d", pr.Repo, pr.Number)]
	}
	if !ok || ts.IsZero() {
		return 0
	}
	mins := int(now.UTC().Sub(ts.UTC()) / time.Minute)
	if mins < 0 {
		return 0
	}
	return mins
}

func isTabBlocked(pr scan.PR, dispatchItems, inboxItems []dispatch.Item) bool {
	if pr.Tab == nil {
		return false
	}
	if pr.Tab.AgentStatus == "blocked" {
		return true
	}
	return hasPRItemAction(dispatchItems, pr.Repo, pr.Number, dispatch.ActionDispatchedBlocked) ||
		hasPRItemAction(inboxItems, pr.Repo, pr.Number, dispatch.ActionDispatchedBlocked)
}
