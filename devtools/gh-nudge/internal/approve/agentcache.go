package approve

import (
	"crypto/sha256"
	"encoding/binary"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// Values of AgentResult.Cache.
const (
	// CacheHit means the decision came from the cache. The command did not run.
	CacheHit = "hit"
	// CacheMiss means the command ran because the cache had no entry.
	CacheMiss = "miss"
	// CacheBypass means the command ran without the cache, because of
	// --no-agent-cache or the rule's agent_cache = false.
	CacheBypass = "bypass"
)

// shortKeyLen is how much of the cache key is shown.
const shortKeyLen = 12

// agentCacheExt is the extension of cache entry files.
const agentCacheExt = ".json"

// AgentCache stores agent command decisions, one file per key in Dir.
type AgentCache struct {
	Dir string
	// TTL is how long an entry is reused. Older entries are removed.
	TTL time.Duration
	// MaxBytes bounds the total size of the entries. The oldest go first.
	MaxBytes int64
	Now      func() time.Time
}

// agentCacheEntry is one cached decision.
type agentCacheEntry struct {
	Decision string    `json:"decision"`
	Reason   string    `json:"reason"`
	Time     time.Time `json:"time"`
	// DurationMs is how long the original run took.
	DurationMs int64 `json:"durationMs"`
}

// agentCacheKey is the hex sha256 of the agent input: stdin, argv, and the
// rule name. Each part is length-prefixed, so parts cannot run together.
func agentCacheKey(stdin []byte, argv []string, rule string) string {
	h := sha256.New()
	write := func(b []byte) {
		var n [8]byte
		binary.BigEndian.PutUint64(n[:], uint64(len(b)))
		h.Write(n[:])
		h.Write(b)
	}
	write(stdin)
	write([]byte(rule))
	for _, arg := range argv {
		write([]byte(arg))
	}
	return hex.EncodeToString(h.Sum(nil))
}

func (c *AgentCache) path(key string) string {
	return filepath.Join(c.Dir, key+agentCacheExt)
}

func (c *AgentCache) expired(at time.Time) bool {
	return c.Now().Sub(at) > c.TTL
}

// get returns the entry for key. A missing, unreadable, or expired entry is
// a miss, and an unreadable or expired file is removed.
func (c *AgentCache) get(key string) (agentCacheEntry, bool) {
	path := c.path(key)
	data, err := os.ReadFile(path)
	if err != nil {
		return agentCacheEntry{}, false
	}
	var entry agentCacheEntry
	if err := json.Unmarshal(data, &entry); err != nil || c.expired(entry.Time) ||
		(entry.Decision != AgentApprove && entry.Decision != AgentDefer) {
		_ = os.Remove(path)
		return agentCacheEntry{}, false
	}
	return entry, true
}

// put writes entry atomically, then removes expired entries and evicts the
// oldest ones while the cache is over MaxBytes.
func (c *AgentCache) put(key string, entry agentCacheEntry) error {
	if err := os.MkdirAll(c.Dir, 0o700); err != nil {
		return fmt.Errorf("create agent cache: %w", err)
	}
	data, err := json.Marshal(entry)
	if err != nil {
		return fmt.Errorf("encode agent cache entry: %w", err)
	}
	if err := writeFileAtomic(c.path(key), data, entry.Time); err != nil {
		return err
	}
	return c.prune()
}

// writeFileAtomic writes data to a temporary file in path's directory with
// mode 0600 and modification time at, then renames it to path.
func writeFileAtomic(path string, data []byte, at time.Time) (err error) {
	tmp, err := os.CreateTemp(filepath.Dir(path), ".tmp-*")
	if err != nil {
		return fmt.Errorf("write agent cache: %w", err)
	}
	defer func() {
		if err != nil {
			_ = os.Remove(tmp.Name())
		}
	}()
	if _, err := tmp.Write(data); err != nil {
		_ = tmp.Close()
		return fmt.Errorf("write agent cache: %w", err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("write agent cache: %w", err)
	}
	if err := os.Chmod(tmp.Name(), 0o600); err != nil {
		return fmt.Errorf("write agent cache: %w", err)
	}
	if err := os.Chtimes(tmp.Name(), at, at); err != nil {
		return fmt.Errorf("write agent cache: %w", err)
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		return fmt.Errorf("write agent cache: %w", err)
	}
	return nil
}

// prune removes expired entries, then the oldest entries until the total
// size is at most MaxBytes. An entry's age is its file modification time,
// which put sets to the entry's time.
func (c *AgentCache) prune() error {
	dirEntries, err := os.ReadDir(c.Dir)
	if err != nil {
		return fmt.Errorf("read agent cache: %w", err)
	}
	type file struct {
		path string
		size int64
		at   time.Time
	}
	var (
		files []file
		total int64
		errs  []error
	)
	for _, de := range dirEntries {
		if de.IsDir() || !strings.HasSuffix(de.Name(), agentCacheExt) {
			continue
		}
		info, err := de.Info()
		if err != nil {
			if !errors.Is(err, fs.ErrNotExist) {
				errs = append(errs, err)
			}
			continue
		}
		f := file{path: filepath.Join(c.Dir, de.Name()), size: info.Size(), at: info.ModTime()}
		if c.expired(f.at) {
			errs = append(errs, removeIfExists(f.path))
			continue
		}
		files = append(files, f)
		total += f.size
	}
	sort.Slice(files, func(i, j int) bool {
		if !files[i].at.Equal(files[j].at) {
			return files[i].at.Before(files[j].at)
		}
		return files[i].path < files[j].path
	})
	for _, f := range files {
		if total <= c.MaxBytes {
			break
		}
		errs = append(errs, removeIfExists(f.path))
		total -= f.size
	}
	if err := errors.Join(errs...); err != nil {
		return fmt.Errorf("prune agent cache: %w", err)
	}
	return nil
}

func removeIfExists(path string) error {
	if err := os.Remove(path); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return fmt.Errorf("remove %s: %w", path, err)
	}
	return nil
}

// cacheNote says where an agent decision came from, for the table.
func cacheNote(a AgentResult) string {
	if a.Cache != CacheHit {
		return "fresh"
	}
	if a.CacheAge < time.Minute {
		return "cached"
	}
	return "cached " + shortAge(a.CacheAge) + " ago"
}

// shortAge formats d in its largest whole unit: 3m, 5h, 2d.
func shortAge(d time.Duration) string {
	switch {
	case d < time.Hour:
		return fmt.Sprintf("%dm", int(d/time.Minute))
	case d < 24*time.Hour:
		return fmt.Sprintf("%dh", int(d/time.Hour))
	default:
		return fmt.Sprintf("%dd", int(d/(24*time.Hour)))
	}
}
