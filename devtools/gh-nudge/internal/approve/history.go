package approve

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

// historyBatchReader is how large a log line history will read.
const historyLineBytes = 1024 * 1024

// HistoryOptions selects rows from the approve log and prints them.
type HistoryOptions struct {
	LogPath   string
	CachePath string
	// Days is how far back to list. Zero lists nothing older than Now.
	Days    int
	Action  string
	Rule    string
	JSON    bool
	NoState bool
	Now     func() time.Time
	Out     io.Writer
	Err     io.Writer
	// Lookup loads current state. It is not called for a cached MERGED
	// pull request, or when NoState is set.
	Lookup func([]github.PullRef) (map[string]github.PRSnapshot, error)
}

type loggedDecision struct {
	Time   time.Time `json:"time"`
	PR     string    `json:"pr"`
	Title  string    `json:"title"`
	Author string    `json:"author"`
	Action string    `json:"action"`
	Rule   *string   `json:"rule"`
	ref    github.PullRef
}

type cachedPR struct {
	State  string `json:"state"`
	Author string `json:"author,omitempty"`
	Title  string `json:"title,omitempty"`
}

type prStateFile struct {
	Pulls map[string]cachedPR `json:"pulls"`
}

type historyRow struct {
	Time   time.Time
	PR     string
	Author string
	State  string
	Action string
	Rule   string
	Title  string
	warn   bool
}

// PRStateCachePath is $XDG_CACHE_HOME/gh-nudge/pr-state.json, or
// ~/.cache/gh-nudge/pr-state.json when xdgCacheHome is empty.
func PRStateCachePath(home, xdgCacheHome string) string {
	base := xdgCacheHome
	if base == "" {
		base = filepath.Join(home, ".cache")
	}
	return filepath.Join(base, "gh-nudge", "pr-state.json")
}

// History prints decisions from the approve log.
func History(opts HistoryOptions) error {
	if err := validateHistory(opts); err != nil {
		return err
	}
	if opts.Now == nil {
		opts.Now = time.Now
	}
	if opts.Err == nil {
		opts.Err = io.Discard
	}
	rows, err := readHistory(opts)
	if err != nil {
		return err
	}
	cache, err := loadPRStateCache(opts.CachePath)
	if err != nil {
		return err
	}
	resolved, changed, err := resolveHistory(opts, rows, cache)
	if err != nil {
		return err
	}
	if changed {
		if err := writePRStateCache(opts.CachePath, cache); err != nil {
			return err
		}
	}
	if err := writeHistory(opts, resolved); err != nil {
		return err
	}
	return warnUnread(opts.Err, resolved)
}

func validateHistory(opts HistoryOptions) error {
	if opts.Days < 0 {
		return fmt.Errorf("--days: expected a non-negative integer, found %d", opts.Days)
	}
	switch opts.Action {
	case "approved", "handoff", "mark_viewed", "all":
	default:
		return fmt.Errorf("--action: expected approved, handoff, mark_viewed, or all, found %q", opts.Action)
	}
	if opts.Out == nil {
		return errors.New("history: expected an output writer")
	}
	return nil
}

func readHistory(opts HistoryOptions) ([]loggedDecision, error) {
	file, err := os.Open(opts.LogPath) //nolint:gosec // path is the approve log under the user config dir
	if errors.Is(err, os.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read approve log: %w", err)
	}
	defer file.Close() //nolint:errcheck
	cutoff := opts.Now().Add(-time.Duration(opts.Days) * 24 * time.Hour)
	scanner := bufio.NewScanner(file)
	scanner.Buffer(make([]byte, 0, 64*1024), historyLineBytes)
	var rows []loggedDecision
	line := 0
	for scanner.Scan() {
		line++
		text := strings.TrimSpace(scanner.Text())
		if text == "" {
			continue
		}
		row, err := parseHistoryLine(line, text)
		if err != nil {
			return nil, err
		}
		if row.Time.Before(cutoff) || !actionListed(row.Action, opts.Action) || !ruleListed(row.Rule, opts.Rule) {
			continue
		}
		rows = append(rows, row)
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("read approve log: %w", err)
	}
	return rows, nil
}

func parseHistoryLine(line int, text string) (loggedDecision, error) {
	var row loggedDecision
	if err := json.Unmarshal([]byte(text), &row); err != nil {
		return loggedDecision{}, fmt.Errorf("approve log line %d: expected a JSON object, found %q", line, textHead(text))
	}
	if row.PR == "" || row.Action == "" {
		return loggedDecision{}, fmt.Errorf("approve log line %d: expected pr and action, found %q", line, textHead(text))
	}
	ref, err := github.ParsePullRef(row.PR)
	if err != nil {
		return loggedDecision{}, fmt.Errorf("approve log line %d: %w", line, err)
	}
	row.ref = ref
	return row, nil
}

func actionListed(action, filter string) bool {
	if filter == "all" {
		return true
	}
	return action == filter
}

func ruleListed(rule *string, want string) bool {
	if want == "" {
		return true
	}
	return rule != nil && *rule == want
}

func resolveHistory(opts HistoryOptions, rows []loggedDecision, cache map[string]cachedPR) ([]historyRow, bool, error) {
	snaps := map[string]github.PRSnapshot{}
	if !opts.NoState {
		refs := refsToFetch(rows, cache)
		if len(refs) > 0 {
			if opts.Lookup == nil {
				return nil, false, errors.New("history: expected a pull request lookup")
			}
			var err error
			snaps, err = opts.Lookup(refs)
			if err != nil {
				return nil, false, err
			}
		}
	}
	out := make([]historyRow, 0, len(rows))
	changed := false
	warned := map[string]bool{}
	for _, row := range rows {
		resolved, grew := resolveRow(row, cache, snaps, opts.NoState, warned)
		if grew {
			changed = true
		}
		out = append(out, resolved)
	}
	return out, changed, nil
}

func refsToFetch(rows []loggedDecision, cache map[string]cachedPR) []github.PullRef {
	seen := map[string]bool{}
	var refs []github.PullRef
	for _, row := range rows {
		if cache[row.PR].State == "MERGED" || seen[row.PR] {
			continue
		}
		seen[row.PR] = true
		refs = append(refs, row.ref)
	}
	return refs
}

func resolveRow(row loggedDecision, cache map[string]cachedPR, snaps map[string]github.PRSnapshot, noState bool, warned map[string]bool) (historyRow, bool) {
	cached := cache[row.PR]
	snap, looked := snaps[row.PR]
	changed := rememberMerged(cache, row.PR, snap, looked && !noState)
	author := firstNonEmpty(row.Author, cached.Author)
	title := firstNonEmpty(row.Title, cached.Title)
	state := ""
	unread := false
	if !noState {
		state, unread = rowState(cached, snap, looked)
		if looked && !snap.Unreadable {
			author = firstNonEmpty(author, snap.Author)
			title = firstNonEmpty(title, snap.Title)
		}
		if unread {
			author = firstNonEmpty(author, "?")
			title = firstNonEmpty(title, "?")
		}
	}
	warn := unread && !warned[row.PR]
	if warn {
		warned[row.PR] = true
	}
	return historyRow{
		Time:   row.Time.UTC(),
		PR:     row.PR,
		Author: author,
		State:  state,
		Action: row.Action,
		Rule:   ruleText(row.Rule),
		Title:  title,
		warn:   warn,
	}, changed
}

func rememberMerged(cache map[string]cachedPR, key string, snap github.PRSnapshot, looked bool) bool {
	if !looked || snap.Unreadable || snap.State != "MERGED" {
		return false
	}
	cache[key] = cachedPR{State: "MERGED", Author: snap.Author, Title: snap.Title}
	return true
}

func rowState(cached cachedPR, snap github.PRSnapshot, looked bool) (string, bool) {
	if cached.State == "MERGED" {
		return "MERGED", false
	}
	if !looked || snap.Unreadable {
		return "?", true
	}
	return snap.State, false
}

func firstNonEmpty(values ...string) string {
	for _, value := range values {
		if value != "" {
			return value
		}
	}
	return ""
}

func ruleText(rule *string) string {
	if rule == nil {
		return ""
	}
	return *rule
}

func writeHistory(opts HistoryOptions, rows []historyRow) error {
	if opts.JSON {
		return writeHistoryJSON(opts.Out, rows, opts.NoState)
	}
	return writeHistoryTable(opts.Out, rows, opts.NoState)
}

type historyJSON struct {
	Time   string `json:"time"`
	PR     string `json:"pr"`
	Author string `json:"author"`
	State  string `json:"state,omitempty"`
	Action string `json:"action"`
	Rule   string `json:"rule"`
	Title  string `json:"title"`
}

func writeHistoryJSON(w io.Writer, rows []historyRow, noState bool) error {
	encoded := make([]historyJSON, len(rows))
	for i, row := range rows {
		item := historyJSON{
			Time:   row.Time.Format(time.RFC3339),
			PR:     row.PR,
			Author: row.Author,
			Action: row.Action,
			Rule:   row.Rule,
			Title:  row.Title,
		}
		if !noState {
			item.State = row.State
		}
		encoded[i] = item
	}
	body, err := json.Marshal(encoded)
	if err != nil {
		return fmt.Errorf("encode history: %w", err)
	}
	if _, err := fmt.Fprintf(w, "%s\n", body); err != nil {
		return fmt.Errorf("write history: %w", err)
	}
	return nil
}

func writeHistoryTable(w io.Writer, rows []historyRow, noState bool) error {
	header := []string{"TIME", "PR", "AUTHOR", "STATE", "ACTION", "RULE", "TITLE"}
	if noState {
		header = []string{"TIME", "PR", "AUTHOR", "ACTION", "RULE", "TITLE"}
	}
	width := make([]int, len(header)-1)
	for i, name := range header[:len(header)-1] {
		width[i] = len(name)
	}
	cells := make([][]string, len(rows))
	for i, row := range rows {
		cells[i] = historyCells(row, noState)
		for col := 0; col < len(width); col++ {
			if n := len(cells[i][col]); n > width[col] {
				width[col] = n
			}
		}
	}
	if err := writeHistoryLine(w, header, width); err != nil {
		return err
	}
	for _, row := range cells {
		if err := writeHistoryLine(w, row, width); err != nil {
			return err
		}
	}
	return nil
}

func historyCells(row historyRow, noState bool) []string {
	rule := row.Rule
	if rule == "" {
		rule = "-"
	}
	timeText := row.Time.Format(time.RFC3339)
	if noState {
		return []string{timeText, row.PR, row.Author, row.Action, rule, row.Title}
	}
	return []string{timeText, row.PR, row.Author, row.State, row.Action, rule, row.Title}
}

func writeHistoryLine(w io.Writer, cells []string, width []int) error {
	var b strings.Builder
	for i, cell := range cells {
		if i > 0 {
			b.WriteByte(' ')
		}
		if i < len(width) {
			fmt.Fprintf(&b, "%-*s", width[i], cell)
			continue
		}
		b.WriteString(cell)
	}
	b.WriteByte('\n')
	if _, err := io.WriteString(w, b.String()); err != nil {
		return fmt.Errorf("write history: %w", err)
	}
	return nil
}

func warnUnread(w io.Writer, rows []historyRow) error {
	for _, row := range rows {
		if !row.warn {
			continue
		}
		if _, err := fmt.Fprintf(w, "cannot read %s\n", row.PR); err != nil {
			return fmt.Errorf("write history warning: %w", err)
		}
	}
	return nil
}

func loadPRStateCache(path string) (map[string]cachedPR, error) {
	data, err := os.ReadFile(path) //nolint:gosec // path is the pr state cache under the user cache dir
	if errors.Is(err, os.ErrNotExist) {
		return map[string]cachedPR{}, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read pr state cache: %w", err)
	}
	var file prStateFile
	if err := json.Unmarshal(data, &file); err != nil {
		return nil, fmt.Errorf("pr state cache: expected a JSON object with pulls, found %q", textHead(string(data)))
	}
	if file.Pulls == nil {
		file.Pulls = map[string]cachedPR{}
	}
	for key, entry := range file.Pulls {
		if entry.State != "MERGED" {
			delete(file.Pulls, key)
		}
	}
	return file.Pulls, nil
}

func writePRStateCache(path string, cache map[string]cachedPR) error {
	body, err := json.Marshal(prStateFile{Pulls: cache})
	if err != nil {
		return fmt.Errorf("encode pr state cache: %w", err)
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("create pr state cache: %w", err)
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), ".pr-state-*")
	if err != nil {
		return fmt.Errorf("write pr state cache: %w", err)
	}
	tmpName := tmp.Name()
	cleanup := true
	defer func() {
		if cleanup {
			_ = os.Remove(tmpName)
		}
	}()
	if _, err := tmp.Write(append(body, '\n')); err != nil {
		_ = tmp.Close()
		return fmt.Errorf("write pr state cache: %w", err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("write pr state cache: %w", err)
	}
	if err := os.Chmod(tmpName, 0o600); err != nil {
		return fmt.Errorf("write pr state cache: %w", err)
	}
	if err := os.Rename(tmpName, path); err != nil {
		return fmt.Errorf("write pr state cache: %w", err)
	}
	cleanup = false
	return nil
}

func textHead(s string) string {
	s = strings.TrimSpace(s)
	if len(s) > 80 {
		return s[:80]
	}
	return s
}
