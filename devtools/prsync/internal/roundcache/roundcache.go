// Package roundcache keeps GitHub results between prsync rounds in one JSON
// file, so a round can skip calls whose answer cannot have changed.
// Entries expire after TTL (the history TTL for path history, the load TTL
// for review load) and are dropped on Save, as are entries whose key is
// not in the current format. The file is only a cache: a missing or
// corrupt file starts empty.
package roundcache

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/google/renameio/v2"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
)

// TTL is how long a status-check entry is reused.
const TTL = time.Hour

// HistoryTTL is how long a path-history entry is reused unless
// SetHistoryTTL sets another TTL. It suits entries keyed by the base
// branch head, where a moved head is already a miss and the TTL only
// bounds how far the history window can drift.
const HistoryTTL = 24 * time.Hour

// LoadTTL is how long a review-load count is reused unless SetLoadTTL sets
// another TTL.
const LoadTTL = time.Hour

// dayVersion prefixes a path-history version keyed by the history
// window's start day; other versions are base branch heads.
const dayVersion = "since:"

// Cache is safe for concurrent use.
type Cache struct {
	mu         sync.Mutex
	path       string
	now        func() time.Time
	historyTTL time.Duration
	// byDay is set when path history is keyed by the window's start day,
	// so head-keyed entries are of an old format, and the reverse.
	byDay   bool
	loadTTL time.Duration
	data    payload
	dirty   bool
}

type payload struct {
	History map[string]historyEntry `json:"history"`
	Rollups map[string]rollupEntry  `json:"rollups"`
	Rounds  map[string]int          `json:"rounds,omitempty"`
	Loads   map[string]loadEntry    `json:"loads,omitempty"`
	// HistoryBatch is the path-history request size that last worked.
	HistoryBatch int `json:"historyBatch,omitempty"`
}

type historyEntry struct {
	At      time.Time      `json:"at"`
	History gh.PathHistory `json:"value"`
}

type loadEntry struct {
	At    time.Time `json:"at"`
	Count int       `json:"count"`
}

type rollupEntry struct {
	At        time.Time        `json:"at"`
	Head      string           `json:"head"`
	UpdatedAt string           `json:"updatedAt"`
	Checks    []gh.StatusCheck `json:"checks"`
}

// Load reads path. A missing file is an empty cache. A corrupt file is an
// empty cache and an error for the caller to log.
func Load(path string, now func() time.Time) (*Cache, error) {
	c := &Cache{path: path, now: now, historyTTL: HistoryTTL, loadTTL: LoadTTL, data: emptyPayload()}
	raw, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return c, nil
	}
	if err != nil {
		return c, fmt.Errorf("read round cache: %w", err)
	}
	var data payload
	if err := json.Unmarshal(raw, &data); err != nil {
		return c, fmt.Errorf("decode round cache %s: %w", path, err)
	}
	if data.History != nil {
		c.data.History = data.History
	}
	if data.Rollups != nil {
		c.data.Rollups = data.Rollups
	}
	if data.Rounds != nil {
		c.data.Rounds = data.Rounds
	}
	if data.Loads != nil {
		c.data.Loads = data.Loads
	}
	c.data.HistoryBatch = data.HistoryBatch
	return c, nil
}

func emptyPayload() payload {
	return payload{
		History: map[string]historyEntry{},
		Rollups: map[string]rollupEntry{},
		Rounds:  map[string]int{},
		Loads:   map[string]loadEntry{},
	}
}

// SetHistoryTTL sets how long a path-history entry is reused; Save drops
// older entries. A ttl > 0 means history is keyed by the window's start
// day ("since:" versions), so Save drops head-keyed entries; a ttl <= 0
// means it is keyed by the base head, restores HistoryTTL, and Save drops
// day-keyed entries.
func (c *Cache) SetHistoryTTL(ttl time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.byDay = ttl > 0
	if ttl <= 0 {
		ttl = HistoryTTL
	}
	c.historyTTL = ttl
}

// SetLoadTTL sets how long a review-load count is reused; Save drops
// older entries. A ttl <= 0 restores LoadTTL.
func (c *Cache) SetLoadTTL(ttl time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if ttl <= 0 {
		ttl = LoadTTL
	}
	c.loadTTL = ttl
}

func historyKey(repo, ref, path, version string) string {
	return repo + "@" + ref + "\x00" + path + "\x00" + version
}

func rollupKey(repo string, number int) string {
	return repo + "#" + strconv.Itoa(number)
}

func loadKey(org, login string) string {
	return org + "/" + strings.ToLower(login)
}

// knownHistoryKey reports whether key is repo@ref, path, and a version of
// the current kind.
func (c *Cache) knownHistoryKey(key string) bool {
	parts := strings.Split(key, "\x00")
	if len(parts) != 3 || !strings.Contains(parts[0], "@") || parts[1] == "" || parts[2] == "" {
		return false
	}
	return strings.HasPrefix(parts[2], dayVersion) == c.byDay
}

func knownRollupKey(key string) bool {
	repo, number, ok := strings.Cut(key, "#")
	if !ok || !strings.Contains(repo, "/") {
		return false
	}
	_, err := strconv.Atoi(number)
	return err == nil
}

func knownLoadKey(key string) bool {
	org, login, ok := strings.Cut(key, "/")
	return ok && org != "" && login != "" && !strings.Contains(login, "/")
}

func (c *Cache) fresh(at time.Time, ttl time.Duration) bool {
	return c.now().Sub(at) < ttl
}

// PathHistory returns the stored history of path on ref for version (the
// caller's key, such as the base head or the window's start day) while it
// is fresh.
func (c *Cache) PathHistory(repo, ref, path, version string) (gh.PathHistory, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	entry, ok := c.data.History[historyKey(repo, ref, path, version)]
	if !ok || !c.fresh(entry.At, c.historyTTL) {
		return gh.PathHistory{}, false
	}
	return entry.History, true
}

// PutPathHistory stores history for PathHistory.
func (c *Cache) PutPathHistory(repo, ref, path, version string, history gh.PathHistory) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.data.History[historyKey(repo, ref, path, version)] = historyEntry{At: c.now(), History: history}
	c.dirty = true
}

// ReviewLoad returns the stored review-load count of login in org while it
// is fresh. Logins are case-insensitive.
func (c *Cache) ReviewLoad(org, login string) (int, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	entry, ok := c.data.Loads[loadKey(org, login)]
	if !ok || !c.fresh(entry.At, c.loadTTL) {
		return 0, false
	}
	return entry.Count, true
}

// PutReviewLoad stores n for ReviewLoad.
func (c *Cache) PutReviewLoad(org, login string, n int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.data.Loads[loadKey(org, login)] = loadEntry{At: c.now(), Count: n}
	c.dirty = true
}

// Rollup returns the stored status checks of a pull request when its head
// and updatedAt still match and the entry is fresh. Empty head or updatedAt
// is a miss.
func (c *Cache) Rollup(repo string, number int, head, updatedAt string) ([]gh.StatusCheck, bool) {
	if head == "" || updatedAt == "" {
		return nil, false
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	entry, ok := c.data.Rollups[rollupKey(repo, number)]
	if !ok || entry.Head != head || entry.UpdatedAt != updatedAt || !c.fresh(entry.At, TTL) {
		return nil, false
	}
	return entry.Checks, true
}

// PutRollup stores checks for Rollup.
func (c *Cache) PutRollup(repo string, number int, head, updatedAt string, checks []gh.StatusCheck) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.data.Rollups[rollupKey(repo, number)] = rollupEntry{At: c.now(), Head: head, UpdatedAt: updatedAt, Checks: checks}
	c.dirty = true
}

// NextRound counts one more round of job and returns the count, starting
// at 1. Counters do not expire.
func (c *Cache) NextRound(job string) int {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.data.Rounds[job]++
	c.dirty = true
	return c.data.Rounds[job]
}

// HistoryBatch is the path-history request size that last worked, or 0
// when none is stored. It does not expire.
func (c *Cache) HistoryBatch() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.data.HistoryBatch
}

// PutHistoryBatch stores n for HistoryBatch.
func (c *Cache) PutHistoryBatch(n int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.data.HistoryBatch == n {
		return
	}
	c.data.HistoryBatch = n
	c.dirty = true
}

// Save drops expired entries and those whose key is not in the current
// format, and writes the file atomically. A cache with no new entries
// since Load is not written.
func (c *Cache) Save() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	for key, entry := range c.data.History {
		if !c.fresh(entry.At, c.historyTTL) || !c.knownHistoryKey(key) {
			delete(c.data.History, key)
			c.dirty = true
		}
	}
	for key, entry := range c.data.Rollups {
		if !c.fresh(entry.At, TTL) || !knownRollupKey(key) {
			delete(c.data.Rollups, key)
			c.dirty = true
		}
	}
	for key, entry := range c.data.Loads {
		if !c.fresh(entry.At, c.loadTTL) || !knownLoadKey(key) {
			delete(c.data.Loads, key)
			c.dirty = true
		}
	}
	if !c.dirty {
		return nil
	}
	if err := os.MkdirAll(filepath.Dir(c.path), 0o700); err != nil {
		return fmt.Errorf("create round cache dir: %w", err)
	}
	data, err := json.Marshal(c.data)
	if err != nil {
		return fmt.Errorf("encode round cache: %w", err)
	}
	if err := renameio.WriteFile(c.path, append(data, '\n'), 0o600); err != nil {
		return fmt.Errorf("write round cache: %w", err)
	}
	c.dirty = false
	return nil
}
