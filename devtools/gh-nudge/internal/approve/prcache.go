package approve

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
)

// prCacheSchemaVersion is bumped whenever prCacheEntry's format or semantics change.
const prCacheSchemaVersion = 1

const prCacheExt = ".json"

// PRCache stores per-commit pull request facts (files, patches, and CODEOWNERS
// ownership) on disk, keyed by repository, PR number, and head SHA.
type PRCache struct {
	Dir string
	// TTL is how long an entry for a PR that is no longer a candidate is kept.
	// Zero disables age-based pruning.
	TTL time.Duration
	// SchemaVersion overrides prCacheSchemaVersion when non-zero (for tests).
	SchemaVersion int
	Now           func() time.Time
}

type prCacheEntry struct {
	SchemaVersion int       `json:"schemaVersion"`
	Owner         string    `json:"owner"`
	Repo          string    `json:"repo"`
	Number        int       `json:"number"`
	HeadSHA       string    `json:"headSha"`
	Time          time.Time `json:"time"`

	Files    []models.File `json:"files,omitempty"`
	FilesErr string        `json:"filesErr,omitempty"`
	HasFiles bool          `json:"hasFiles,omitempty"`

	Patches    []github.PullFile `json:"patches,omitempty"`
	PatchesErr string            `json:"patchesErr,omitempty"`
	HasPatches bool              `json:"hasPatches,omitempty"`

	Ownership         *github.Ownership `json:"ownership,omitempty"`
	CodeownersBlobSHA string            `json:"codeownersBlobSha,omitempty"`
	ViewerTeamsHash   string            `json:"viewerTeamsHash,omitempty"`
	HasOwnership      bool              `json:"hasOwnership,omitempty"`
}

func (c *PRCache) now() time.Time {
	if c.Now == nil {
		return time.Now()
	}
	return c.Now()
}

func (c *PRCache) schemaVersion() int {
	if c.SchemaVersion != 0 {
		return c.SchemaVersion
	}
	return prCacheSchemaVersion
}

func prCacheKey(ref github.PullRef) string {
	raw := strings.ToLower(ref.Owner+"/"+ref.Repo) + "#" + strconv.Itoa(ref.Number)
	sum := sha256.Sum256([]byte(raw))
	return hex.EncodeToString(sum[:])
}

func (c *PRCache) path(ref github.PullRef) string {
	return filepath.Join(c.Dir, prCacheKey(ref)+prCacheExt)
}

// get looks up cached per-commit facts for ref at headSHA. On a miss it returns
// a short reason suitable for --explain. Corrupt or expired entries are removed.
func (c *PRCache) get(ref github.PullRef, headSHA string) (prCacheEntry, string) {
	path := c.path(ref)
	data, err := os.ReadFile(path)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return prCacheEntry{}, "not cached"
		}
		_ = os.Remove(path)
		return prCacheEntry{}, "corrupt"
	}
	var entry prCacheEntry
	if err := json.Unmarshal(data, &entry); err != nil {
		_ = os.Remove(path)
		return prCacheEntry{}, "corrupt"
	}
	if entry.SchemaVersion != c.schemaVersion() {
		return prCacheEntry{}, "schema changed"
	}
	if headSHA == "" || entry.HeadSHA != headSHA {
		return prCacheEntry{}, "head changed"
	}
	if c.TTL > 0 && c.now().Sub(entry.Time) > c.TTL {
		_ = os.Remove(path)
		return prCacheEntry{}, "expired"
	}
	return entry, ""
}

// put writes entry atomically to the cache directory.
func (c *PRCache) put(entry prCacheEntry) error {
	if err := os.MkdirAll(c.Dir, 0o700); err != nil {
		return fmt.Errorf("create pr cache: %w", err)
	}
	entry.SchemaVersion = c.schemaVersion()
	entry.Time = c.now().UTC()
	data, err := json.Marshal(entry)
	if err != nil {
		return fmt.Errorf("encode pr cache entry: %w", err)
	}
	ref := github.PullRef{Owner: entry.Owner, Repo: entry.Repo, Number: entry.Number}
	if err := writeFileAtomic(c.path(ref), data, entry.Time); err != nil {
		return fmt.Errorf("write pr cache: %w", err)
	}
	return nil
}

// touch refreshes the modification time of an active candidate's cache file
// so prune only removes entries for PRs that are no longer candidates.
func (c *PRCache) touch(ref github.PullRef) {
	now := c.now()
	_ = os.Chtimes(c.path(ref), now, now)
}

// prune removes cache entries older than TTL for PRs not in activeKeys.
func (c *PRCache) prune(activeKeys map[string]bool) error {
	if c.TTL <= 0 {
		return nil
	}
	dirEntries, err := os.ReadDir(c.Dir)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return nil
		}
		return fmt.Errorf("read pr cache: %w", err)
	}
	now := c.now()
	var errs []error
	for _, de := range dirEntries {
		if de.IsDir() || !strings.HasSuffix(de.Name(), prCacheExt) {
			continue
		}
		key := strings.TrimSuffix(de.Name(), prCacheExt)
		if activeKeys[key] {
			continue
		}
		info, err := de.Info()
		if err != nil {
			if !errors.Is(err, fs.ErrNotExist) {
				errs = append(errs, err)
			}
			continue
		}
		if now.Sub(info.ModTime()) > c.TTL {
			errs = append(errs, removeIfExists(filepath.Join(c.Dir, de.Name())))
		}
	}
	if err := errors.Join(errs...); err != nil {
		return fmt.Errorf("prune pr cache: %w", err)
	}
	return nil
}

// hashViewerTeams returns a deterministic hash of the viewer's team slugs.
func hashViewerTeams(slugs map[string]bool, ok bool) string {
	if !ok {
		return "unavailable"
	}
	list := make([]string, 0, len(slugs))
	for s := range slugs {
		list = append(list, strings.ToLower(s))
	}
	sort.Strings(list)
	h := sha256.New()
	for _, s := range list {
		h.Write([]byte(s))
		h.Write([]byte{0})
	}
	return hex.EncodeToString(h.Sum(nil))
}
