// Package roundcache keeps GitHub results between prsync rounds in one JSON
// file, so a round can skip calls whose answer cannot have changed.
// Entries expire after TTL (HistoryTTL for path history). The file is only
// a cache: a missing or corrupt file starts empty.
package roundcache

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"sync"
	"time"

	"github.com/google/renameio/v2"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
)

// TTL is how long a status-check entry is reused.
const TTL = time.Hour

// HistoryTTL is how long a path-history entry is reused. The entry is keyed
// by the base branch head, so a moved head is already a miss; this only
// bounds how far the history window can drift.
const HistoryTTL = 24 * time.Hour

// Cache is safe for concurrent use.
type Cache struct {
	mu    sync.Mutex
	path  string
	now   func() time.Time
	data  payload
	dirty bool
}

type payload struct {
	History map[string]historyEntry `json:"history"`
	Rollups map[string]rollupEntry  `json:"rollups"`
	Rounds  map[string]int          `json:"rounds,omitempty"`
}

type historyEntry struct {
	At      time.Time      `json:"at"`
	History gh.PathHistory `json:"value"`
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
	c := &Cache{path: path, now: now, data: emptyPayload()}
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
	return c, nil
}

func emptyPayload() payload {
	return payload{History: map[string]historyEntry{}, Rollups: map[string]rollupEntry{}, Rounds: map[string]int{}}
}

func historyKey(repo, ref, path, head string) string {
	return repo + "@" + ref + "\x00" + path + "\x00" + head
}

func rollupKey(repo string, number int) string {
	return repo + "#" + strconv.Itoa(number)
}

func (c *Cache) fresh(at time.Time, ttl time.Duration) bool {
	return c.now().Sub(at) < ttl
}

// PathHistory returns the stored history of path on ref, whose head commit
// is head, while it is fresh.
func (c *Cache) PathHistory(repo, ref, path, head string) (gh.PathHistory, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	entry, ok := c.data.History[historyKey(repo, ref, path, head)]
	if !ok || !c.fresh(entry.At, HistoryTTL) {
		return gh.PathHistory{}, false
	}
	return entry.History, true
}

// PutPathHistory stores history for PathHistory.
func (c *Cache) PutPathHistory(repo, ref, path, head string, history gh.PathHistory) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.data.History[historyKey(repo, ref, path, head)] = historyEntry{At: c.now(), History: history}
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

// Save drops expired entries and writes the file atomically. A cache with
// no new entries since Load is not written.
func (c *Cache) Save() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	for key, entry := range c.data.History {
		if !c.fresh(entry.At, HistoryTTL) {
			delete(c.data.History, key)
			c.dirty = true
		}
	}
	for key, entry := range c.data.Rollups {
		if !c.fresh(entry.At, TTL) {
			delete(c.data.Rollups, key)
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
