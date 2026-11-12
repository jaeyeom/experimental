// Package roundcache keeps GitHub results between prsync rounds in one JSON
// file, so a round can skip calls whose answer cannot have changed.
// Entries expire after their TTL (the status-checks TTL for status checks,
// the history TTL for path history, the load TTL for review load, the
// members TTL for team members, the CODEOWNERS TTL for CODEOWNERS bodies,
// the threads TTL for review threads, the closed TTL for closed
// pull-request views, and KeepTTL for changed files, review snapshots and
// CODEOWNERS locations) and are dropped on Save, as are entries whose key
// is not in the current format.
// Merged pull-request views do not expire; PrunePRViews drops them. The
// file is only a cache: a missing or corrupt file starts empty.
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

// StatusChecksTTL is how long a settled status-check rollup is reused
// unless SetStatusChecksTTL sets another TTL.
const StatusChecksTTL = 6 * time.Hour

// HistoryTTL is how long a path-history entry is reused unless
// SetHistoryTTL sets another TTL. It suits entries keyed by the base
// branch head, where a moved head is already a miss and the TTL only
// bounds how far the history window can drift.
const HistoryTTL = 24 * time.Hour

// LoadTTL is how long a review-load count is reused unless SetLoadTTL sets
// another TTL.
const LoadTTL = time.Hour

// MembersTTL is how long a team's member list is reused unless
// SetMembersTTL sets another TTL.
const MembersTTL = 24 * time.Hour

// CodeownersTTL is how long a CODEOWNERS body is reused unless
// SetCodeownersTTL sets another TTL.
const CodeownersTTL = 6 * time.Hour

// ThreadsTTL is how long a pull request's review threads are reused
// unless SetThreadsTTL sets another TTL.
const ThreadsTTL = 6 * time.Hour

// PRViewClosedTTL is how long a closed (not merged) pull request's view is
// reused unless SetPRViewClosedTTL sets another TTL.
const PRViewClosedTTL = 24 * time.Hour

// KeepTTL is how long entries are kept whose key already pins the answer
// (changed files per head, review snapshots per head and updatedAt), and
// how long a CODEOWNERS location is kept as the first place to look.
const KeepTTL = 24 * time.Hour

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
	byDay           bool
	loadTTL         time.Duration
	membersTTL      time.Duration
	codeownersTTL   time.Duration
	threadsTTL      time.Duration
	statusChecksTTL time.Duration
	closedTTL       time.Duration
	data            payload
	dirty           bool
}

type payload struct {
	History map[string]historyEntry `json:"history"`
	Rollups map[string]rollupEntry  `json:"rollups"`
	Rounds  map[string]int          `json:"rounds,omitempty"`
	Loads   map[string]loadEntry    `json:"loads,omitempty"`
	// Members is keyed by org/slug, Files, Reviews, Threads and PRViews by
	// repo#number, and Codeowners by repo@ref.
	Members    map[string]membersEntry    `json:"members,omitempty"`
	Files      map[string]filesEntry      `json:"files,omitempty"`
	Codeowners map[string]codeownersEntry `json:"codeowners,omitempty"`
	Reviews    map[string]reviewsEntry    `json:"reviews,omitempty"`
	Threads    map[string]threadsEntry    `json:"threads,omitempty"`
	PRViews    map[string]prViewEntry     `json:"prViews,omitempty"`
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

type membersEntry struct {
	At      time.Time       `json:"at"`
	Members []gh.TeamMember `json:"members"`
}

type filesEntry struct {
	At    time.Time `json:"at"`
	Head  string    `json:"head"`
	Files []string  `json:"files"`
}

// codeownersEntry is the CODEOWNERS body of a ref and the path it was found
// at; an empty path means none of the locations exists.
type codeownersEntry struct {
	At   time.Time `json:"at"`
	Path string    `json:"path"`
	Body string    `json:"body"`
}

type reviewsEntry struct {
	At        time.Time         `json:"at"`
	Head      string            `json:"head"`
	UpdatedAt string            `json:"updatedAt"`
	Author    string            `json:"author"`
	Reviews   []gh.LatestReview `json:"reviews"`
}

type threadsEntry struct {
	At        time.Time   `json:"at"`
	Head      string      `json:"head"`
	UpdatedAt string      `json:"updatedAt"`
	Threads   []gh.Thread `json:"threads"`
}

// prViewEntry is a merged or closed pull request's view.
type prViewEntry struct {
	At   time.Time       `json:"at"`
	Item gh.PRSearchItem `json:"item"`
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
	c := &Cache{
		path: path, now: now, historyTTL: HistoryTTL, loadTTL: LoadTTL,
		membersTTL: MembersTTL, codeownersTTL: CodeownersTTL, threadsTTL: ThreadsTTL,
		statusChecksTTL: StatusChecksTTL, closedTTL: PRViewClosedTTL, data: emptyPayload(),
	}
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
	if data.Members != nil {
		c.data.Members = data.Members
	}
	if data.Files != nil {
		c.data.Files = data.Files
	}
	if data.Codeowners != nil {
		c.data.Codeowners = data.Codeowners
	}
	if data.Reviews != nil {
		c.data.Reviews = data.Reviews
	}
	if data.Threads != nil {
		c.data.Threads = data.Threads
	}
	if data.PRViews != nil {
		c.data.PRViews = data.PRViews
	}
	c.data.HistoryBatch = data.HistoryBatch
	return c, nil
}

func emptyPayload() payload {
	return payload{
		History:    map[string]historyEntry{},
		Rollups:    map[string]rollupEntry{},
		Rounds:     map[string]int{},
		Loads:      map[string]loadEntry{},
		Members:    map[string]membersEntry{},
		Files:      map[string]filesEntry{},
		Codeowners: map[string]codeownersEntry{},
		Reviews:    map[string]reviewsEntry{},
		Threads:    map[string]threadsEntry{},
		PRViews:    map[string]prViewEntry{},
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

// SetMembersTTL sets how long a team's member list is reused; Save drops
// older entries. A ttl <= 0 restores MembersTTL.
func (c *Cache) SetMembersTTL(ttl time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if ttl <= 0 {
		ttl = MembersTTL
	}
	c.membersTTL = ttl
}

// SetCodeownersTTL sets how long a CODEOWNERS body is reused. Its location
// is kept for the longer of ttl and KeepTTL. A ttl <= 0 restores
// CodeownersTTL.
func (c *Cache) SetCodeownersTTL(ttl time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if ttl <= 0 {
		ttl = CodeownersTTL
	}
	c.codeownersTTL = ttl
}

// SetThreadsTTL sets how long a pull request's review threads are reused;
// Save drops older entries. A ttl <= 0 restores ThreadsTTL.
func (c *Cache) SetThreadsTTL(ttl time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if ttl <= 0 {
		ttl = ThreadsTTL
	}
	c.threadsTTL = ttl
}

// SetStatusChecksTTL sets how long a settled status-check rollup is reused;
// Save drops older entries. A ttl <= 0 restores StatusChecksTTL. A caller
// that must fetch every round skips the cache instead of storing zero.
func (c *Cache) SetStatusChecksTTL(ttl time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if ttl <= 0 {
		ttl = StatusChecksTTL
	}
	c.statusChecksTTL = ttl
}

// SetPRViewClosedTTL sets how long a closed (not merged) pull request's
// view is reused; Save drops older entries. A ttl <= 0 restores
// PRViewClosedTTL.
func (c *Cache) SetPRViewClosedTTL(ttl time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if ttl <= 0 {
		ttl = PRViewClosedTTL
	}
	c.closedTTL = ttl
}

func historyKey(repo, ref, path, version string) string {
	return repo + "@" + ref + "\x00" + path + "\x00" + version
}

func codeownersKey(repo, ref string) string {
	return repo + "@" + ref
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

func knownCodeownersKey(key string) bool {
	repo, ref, ok := strings.Cut(key, "@")
	return ok && strings.Contains(repo, "/") && ref != ""
}

// knownLoadKey also fits team members, keyed by org/slug.
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

// TeamMembers returns the stored members of org/slug while they are fresh.
func (c *Cache) TeamMembers(org, slug string) ([]gh.TeamMember, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	entry, ok := c.data.Members[org+"/"+slug]
	if !ok || !c.fresh(entry.At, c.membersTTL) {
		return nil, false
	}
	return entry.Members, true
}

// PutTeamMembers stores members for TeamMembers.
func (c *Cache) PutTeamMembers(org, slug string, members []gh.TeamMember) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.data.Members[org+"/"+slug] = membersEntry{At: c.now(), Members: members}
	c.dirty = true
}

// ChangedFiles returns the stored changed files of a pull request while
// its head is unchanged, for at most KeepTTL. An empty head is a miss.
func (c *Cache) ChangedFiles(repo string, number int, head string) ([]string, bool) {
	if head == "" {
		return nil, false
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	entry, ok := c.data.Files[rollupKey(repo, number)]
	if !ok || entry.Head != head || !c.fresh(entry.At, KeepTTL) {
		return nil, false
	}
	return entry.Files, true
}

// PutChangedFiles stores files for ChangedFiles. An empty head is not
// stored.
func (c *Cache) PutChangedFiles(repo string, number int, head string, files []string) {
	if head == "" {
		return
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	c.data.Files[rollupKey(repo, number)] = filesEntry{At: c.now(), Head: head, Files: files}
	c.dirty = true
}

// ReviewSnapshot returns the stored author and latest reviews of a pull
// request while its head and updatedAt are unchanged, for at most KeepTTL.
// An empty head or updatedAt is a miss.
func (c *Cache) ReviewSnapshot(repo string, number int, head, updatedAt string) (string, []gh.LatestReview, bool) {
	if head == "" || updatedAt == "" {
		return "", nil, false
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	entry, ok := c.data.Reviews[rollupKey(repo, number)]
	if !ok || entry.Head != head || entry.UpdatedAt != updatedAt || !c.fresh(entry.At, KeepTTL) {
		return "", nil, false
	}
	return entry.Author, entry.Reviews, true
}

// PutReviewSnapshot stores a snapshot for ReviewSnapshot. An empty head or
// updatedAt is not stored.
func (c *Cache) PutReviewSnapshot(repo string, number int, head, updatedAt, author string, reviews []gh.LatestReview) {
	if head == "" || updatedAt == "" {
		return
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	c.data.Reviews[rollupKey(repo, number)] = reviewsEntry{
		At: c.now(), Head: head, UpdatedAt: updatedAt, Author: author, Reviews: reviews,
	}
	c.dirty = true
}

// ReviewThreads returns the stored review threads of a pull request while
// its head and updatedAt are unchanged and the entry is younger than the
// threads TTL. An empty head or updatedAt is a miss.
func (c *Cache) ReviewThreads(repo string, number int, head, updatedAt string) ([]gh.Thread, bool) {
	if head == "" || updatedAt == "" {
		return nil, false
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	entry, ok := c.data.Threads[rollupKey(repo, number)]
	if !ok || entry.Head != head || entry.UpdatedAt != updatedAt || !c.fresh(entry.At, c.threadsTTL) {
		return nil, false
	}
	return entry.Threads, true
}

// PutReviewThreads stores threads for ReviewThreads. An empty head or
// updatedAt is not stored.
func (c *Cache) PutReviewThreads(repo string, number int, head, updatedAt string, threads []gh.Thread) {
	if head == "" || updatedAt == "" {
		return
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	c.data.Threads[rollupKey(repo, number)] = threadsEntry{
		At: c.now(), Head: head, UpdatedAt: updatedAt, Threads: threads,
	}
	c.dirty = true
}

// PruneReviewThreads drops the stored review threads of repo's pull
// requests whose number is not in open.
func (c *Cache) PruneReviewThreads(repo string, open []int) {
	keep := make(map[string]struct{}, len(open))
	for _, number := range open {
		keep[rollupKey(repo, number)] = struct{}{}
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	for key := range c.data.Threads {
		if r, _, _ := strings.Cut(key, "#"); r != repo {
			continue
		}
		if _, ok := keep[key]; !ok {
			delete(c.data.Threads, key)
			c.dirty = true
		}
	}
}

// PRView returns the stored view of a pull request: a merged one until
// PrunePRViews drops it, a closed one while younger than the closed TTL.
func (c *Cache) PRView(repo string, number int) (gh.PRSearchItem, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	entry, ok := c.data.PRViews[rollupKey(repo, number)]
	if !ok || !c.prViewFresh(entry) {
		return gh.PRSearchItem{}, false
	}
	return entry.Item, true
}

// PutPRView stores item for PRView. Only a merged or closed state is
// stored, because an open pull request can change at any time.
func (c *Cache) PutPRView(repo string, number int, item gh.PRSearchItem) {
	if item.State != "merged" && item.State != "closed" {
		return
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	c.data.PRViews[rollupKey(repo, number)] = prViewEntry{At: c.now(), Item: item}
	c.dirty = true
}

// PrunePRViews drops the stored views of pull requests keep rejects.
func (c *Cache) PrunePRViews(keep func(repo string, number int) bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	for key := range c.data.PRViews {
		repo, n, _ := strings.Cut(key, "#")
		number, err := strconv.Atoi(n)
		if err == nil && keep(repo, number) {
			continue
		}
		delete(c.data.PRViews, key)
		c.dirty = true
	}
}

// prViewFresh reports whether a stored view may be reused. The caller
// holds mu.
func (c *Cache) prViewFresh(entry prViewEntry) bool {
	return entry.Item.State == "merged" || c.fresh(entry.At, c.closedTTL)
}

// Codeowners returns the stored CODEOWNERS body of repo at ref and the path
// it was found at (empty when none exists). ok is set while the body is
// fresh; past that, path is still returned for the longer of the
// CODEOWNERS TTL and KeepTTL, as the place to look first.
func (c *Cache) Codeowners(repo, ref string) (body, path string, ok bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	entry, found := c.data.Codeowners[codeownersKey(repo, ref)]
	if !found || !c.fresh(entry.At, c.codeownersKeep()) {
		return "", "", false
	}
	if !c.fresh(entry.At, c.codeownersTTL) {
		return "", entry.Path, false
	}
	return entry.Body, entry.Path, true
}

// PutCodeowners stores body, found at path, for Codeowners.
func (c *Cache) PutCodeowners(repo, ref, path, body string) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.data.Codeowners[codeownersKey(repo, ref)] = codeownersEntry{At: c.now(), Path: path, Body: body}
	c.dirty = true
}

func (c *Cache) codeownersKeep() time.Duration {
	return max(c.codeownersTTL, KeepTTL)
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
	if !ok || entry.Head != head || entry.UpdatedAt != updatedAt || !c.fresh(entry.At, c.statusChecksTTL) {
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
		if !c.fresh(entry.At, c.statusChecksTTL) || !knownRollupKey(key) {
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
	c.pruneThreads()
	c.prunePRViews()
	c.pruneTeamCover()
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

// pruneThreads drops expired review threads and those whose key is not in
// the current format. The caller holds mu.
func (c *Cache) pruneThreads() {
	for key, entry := range c.data.Threads {
		if !c.fresh(entry.At, c.threadsTTL) || !knownRollupKey(key) {
			delete(c.data.Threads, key)
			c.dirty = true
		}
	}
}

// prunePRViews drops expired closed pull-request views and those whose key
// is not in the current format. The caller holds mu.
func (c *Cache) prunePRViews() {
	for key, entry := range c.data.PRViews {
		if !c.prViewFresh(entry) || !knownRollupKey(key) {
			delete(c.data.PRViews, key)
			c.dirty = true
		}
	}
}

// pruneTeamCover drops expired team-cover entries and those whose key is
// not in the current format. The caller holds mu.
func (c *Cache) pruneTeamCover() {
	for key, entry := range c.data.Members {
		if !c.fresh(entry.At, c.membersTTL) || !knownLoadKey(key) {
			delete(c.data.Members, key)
			c.dirty = true
		}
	}
	for key, entry := range c.data.Files {
		if !c.fresh(entry.At, KeepTTL) || !knownRollupKey(key) {
			delete(c.data.Files, key)
			c.dirty = true
		}
	}
	for key, entry := range c.data.Reviews {
		if !c.fresh(entry.At, KeepTTL) || !knownRollupKey(key) {
			delete(c.data.Reviews, key)
			c.dirty = true
		}
	}
	for key, entry := range c.data.Codeowners {
		if !c.fresh(entry.At, c.codeownersKeep()) || !knownCodeownersKey(key) {
			delete(c.data.Codeowners, key)
			c.dirty = true
		}
	}
}
