package secretwatch

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"sync"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/notify"
)

const unreadableSystemNote = "system journal unreadable (insufficient permissions); skipped"

// Finding describes a detected secret without ever storing or exposing the
// secret value.
type Finding struct {
	Time         time.Time   `json:"time"`
	Journal      JournalKind `json:"journal"`
	Unit         string      `json:"unit,omitempty"`
	Process      string      `json:"process,omitempty"`
	PID          string      `json:"pid,omitempty"`
	Rule         string      `json:"rule"`
	MaskedPrefix string      `json:"maskedPrefix"`
	Cursor       string      `json:"cursor,omitempty"`
	Notified     bool        `json:"notified,omitempty"`
	NotifyError  string      `json:"notifyError,omitempty"`
}

// JournalReport summarizes scanning one journal ("user" or "system").
type JournalReport struct {
	Journal  JournalKind `json:"journal"`
	Scanned  int         `json:"scanned"`
	Findings int         `json:"findings"`
	Skipped  bool        `json:"skipped,omitempty"`
	Note     string      `json:"note,omitempty"`
	Error    string      `json:"error,omitempty"`
}

// Report is the aggregate result of a check or scan pass.
type Report struct {
	Findings []Finding       `json:"findings"`
	Journals []JournalReport `json:"journals"`
}

// ExitCode returns 2 on operational or notifier errors, 1 if findings were
// detected, and 0 when clean.
func (r Report) ExitCode() int {
	for _, j := range r.Journals {
		if j.Error != "" {
			return 2
		}
	}
	for _, f := range r.Findings {
		if f.NotifyError != "" {
			return 2
		}
	}
	if len(r.Findings) > 0 {
		return 1
	}
	return 0
}

// CheckInput bundles dependencies and state for Check and Follow.
type CheckInput struct {
	Config  Config
	State   State
	Since   string
	Now     time.Time
	BaseEnv []string
	Stream  JournalStreamer
	Notify  notify.Runner
}

// ScanInput configures a one-shot historical Scan (no state or notify).
type ScanInput struct {
	Config Config
	Since  string
	Until  string
	Unit   string
	Stream JournalStreamer
}

// FollowInput configures a continuous Check --follow session.
type FollowInput struct {
	CheckInput
	OnFinding func(Finding)
	OnNote    func(JournalKind, string)
	OnSave    func(State) error
}

// Check performs one pass over user and system journals, notifies once per
// new finding (deduped by cursor), and returns the updated State.
func Check(ctx context.Context, in CheckInput) (Report, State, error) {
	rs, err := CompileRuleSet(in.Config)
	if err != nil {
		return Report{}, in.State, err
	}
	state := cloneState(in.State)
	rep := Report{Findings: []Finding{}}
	for _, kind := range []JournalKind{JournalUser, JournalSystem} {
		jr, findings := checkOneJournal(ctx, in, &state, rs, kind)
		rep.Journals = append(rep.Journals, jr)
		rep.Findings = append(rep.Findings, findings...)
	}
	return rep, state, nil
}

type checkJournalTracker struct {
	findings       []Finding
	lastCursor     string
	lastSafeCursor string
	notifyFailed   bool
}

func (t *checkJournalTracker) handleEntry(
	ctx context.Context,
	in CheckInput,
	state *State,
	rs *CompiledRuleSet,
	kind JournalKind,
	jr *JournalReport,
	entry Entry,
) {
	jr.Scanned++
	if entry.Cursor != "" {
		t.lastCursor = entry.Cursor
	}
	m, matched := rs.ScanMessage(entry.Unit, entry.Message)
	if !matched || (entry.Cursor != "" && state.HasSeenCursor(entry.Cursor)) {
		if !t.notifyFailed && entry.Cursor != "" {
			t.lastSafeCursor = entry.Cursor
		}
		return
	}
	f := newFinding(kind, entry, m)
	if notifyFinding(ctx, in, &f) {
		state.MarkCursorSeen(entry.Cursor, in.Now)
		if !t.notifyFailed && entry.Cursor != "" {
			t.lastSafeCursor = entry.Cursor
		}
	} else {
		t.notifyFailed = true
	}
	t.findings = append(t.findings, f)
	jr.Findings++
}

func checkOneJournal(
	ctx context.Context,
	in CheckInput,
	state *State,
	rs *CompiledRuleSet,
	kind JournalKind,
) (JournalReport, []Finding) {
	jr := JournalReport{Journal: kind}
	js := state.journal(kind)
	q := buildCheckQuery(kind, js, in.Since, in.Now, false)

	var tracker checkJournalTracker
	tailCursor, err := in.Stream(ctx, q, func(entry Entry) error {
		tracker.handleEntry(ctx, in, state, rs, kind, &jr, entry)
		return nil
	})
	if err != nil {
		if kind == JournalSystem && errors.Is(err, ErrJournalUnreadable) {
			jr.Skipped = true
			jr.Note = unreadableSystemNote
			return jr, tracker.findings
		}
		jr.Error = err.Error()
		return jr, tracker.findings
	}

	if !tracker.notifyFailed {
		if nextCursor := firstNonEmpty(tracker.lastCursor, tailCursor); nextCursor != "" {
			js.Cursor = nextCursor
		}
	} else if tracker.lastSafeCursor != "" {
		js.Cursor = tracker.lastSafeCursor
	}
	js.LastScan = in.Now
	state.setJournal(kind, js)
	return jr, tracker.findings
}

func buildCheckQuery(kind JournalKind, js JournalState, sinceOverride string, now time.Time, follow bool) Query {
	q := Query{Kind: kind, Follow: follow}
	switch {
	case sinceOverride != "":
		q.Since = sinceOverride
	case js.Cursor != "":
		q.AfterCursor = js.Cursor
	case !js.LastScan.IsZero():
		q.Since = FormatJournalTime(js.LastScan)
	default:
		q.Since = FormatJournalTime(now)
	}
	return q
}

func notifyFinding(ctx context.Context, in CheckInput, f *Finding) bool {
	if len(in.Config.Notify) == 0 || in.Notify == nil {
		return true
	}
	argv, env := BuildNotifyCommand(in.Config.Notify, in.BaseEnv, *f)
	if err := in.Notify(ctx, argv, env); err != nil {
		f.NotifyError = err.Error()
		return false
	}
	f.Notified = true
	return true
}

// Scan runs a one-shot read-only scan over a past journal window without
// reading or writing state or executing notify commands.
func Scan(ctx context.Context, in ScanInput) (Report, error) {
	rs, err := CompileRuleSet(in.Config)
	if err != nil {
		return Report{}, err
	}
	rep := Report{Findings: []Finding{}}
	seen := make(map[string]bool)
	for _, kind := range []JournalKind{JournalUser, JournalSystem} {
		jr, findings := scanOneJournal(ctx, in, rs, kind, seen)
		rep.Journals = append(rep.Journals, jr)
		rep.Findings = append(rep.Findings, findings...)
	}
	return rep, nil
}

func scanOneJournal(
	ctx context.Context,
	in ScanInput,
	rs *CompiledRuleSet,
	kind JournalKind,
	seen map[string]bool,
) (JournalReport, []Finding) {
	jr := JournalReport{Journal: kind}
	q := Query{
		Kind:  kind,
		Since: in.Since,
		Until: in.Until,
		Unit:  in.Unit,
	}
	var findings []Finding
	_, err := in.Stream(ctx, q, func(entry Entry) error {
		if in.Unit != "" && !unitMatches(entry.Unit, in.Unit) {
			return nil
		}
		jr.Scanned++
		m, matched := rs.ScanMessage(entry.Unit, entry.Message)
		if !matched {
			return nil
		}
		if entry.Cursor != "" {
			if seen[entry.Cursor] {
				return nil
			}
			seen[entry.Cursor] = true
		}
		findings = append(findings, newFinding(kind, entry, m))
		jr.Findings++
		return nil
	})
	if err != nil {
		if kind == JournalSystem && errors.Is(err, ErrJournalUnreadable) {
			jr.Skipped = true
			jr.Note = unreadableSystemNote
			return jr, findings
		}
		jr.Error = err.Error()
	}
	return jr, findings
}

// Follow streams user and system journals until ctx is cancelled or an
// operational error occurs, updating state and firing notifications as findings arrive.
func Follow(ctx context.Context, in FollowInput) (Report, State, error) {
	rs, err := CompileRuleSet(in.Config)
	if err != nil {
		return Report{}, in.State, err
	}
	followCtx, cancel := context.WithCancel(ctx)
	defer cancel()

	var mu sync.Mutex
	state := cloneState(in.State)
	rep := Report{Findings: []Finding{}}

	var wg sync.WaitGroup
	kinds := []JournalKind{JournalUser, JournalSystem}
	reports := make([]JournalReport, len(kinds))

	for i, kind := range kinds {
		wg.Add(1)
		go func(idx int, k JournalKind) {
			defer wg.Done()
			jr := followOneJournal(followCtx, in, rs, k, &mu, &state, &rep)
			mu.Lock()
			reports[idx] = jr
			hasErr := jr.Error != ""
			mu.Unlock()
			if hasErr {
				cancel()
			}
		}(i, kind)
	}
	wg.Wait()

	rep.Journals = reports
	return rep, state, nil
}

func followOneJournal(
	ctx context.Context,
	in FollowInput,
	rs *CompiledRuleSet,
	kind JournalKind,
	mu *sync.Mutex,
	state *State,
	rep *Report,
) JournalReport {
	jr := JournalReport{Journal: kind}
	mu.Lock()
	js := state.journal(kind)
	mu.Unlock()
	q := buildCheckQuery(kind, js, in.Since, in.Now, true)

	tailCursor, err := in.Stream(ctx, q, func(entry Entry) error {
		return handleFollowEntry(ctx, in, rs, kind, entry, &jr, mu, state, rep)
	})

	mu.Lock()
	defer mu.Unlock()
	if err != nil {
		if kind == JournalSystem && errors.Is(err, ErrJournalUnreadable) {
			jr.Skipped = true
			jr.Note = unreadableSystemNote
			if in.OnNote != nil {
				in.OnNote(kind, unreadableSystemNote)
			}
			return jr
		}
		if ctx.Err() == nil {
			jr.Error = err.Error()
			return jr
		}
	}
	updated := state.journal(kind)
	if updated.Cursor == "" && tailCursor != "" {
		updated.Cursor = tailCursor
	}
	updated.LastScan = time.Now().UTC()
	state.setJournal(kind, updated)
	return jr
}

func handleFollowEntry(
	ctx context.Context,
	in FollowInput,
	rs *CompiledRuleSet,
	kind JournalKind,
	entry Entry,
	jr *JournalReport,
	mu *sync.Mutex,
	state *State,
	rep *Report,
) error {
	mu.Lock()
	jr.Scanned++
	alreadySeen := entry.Cursor != "" && state.HasSeenCursor(entry.Cursor)
	mu.Unlock()

	m, matched := rs.ScanMessage(entry.Unit, entry.Message)
	if !matched || alreadySeen {
		if entry.Cursor != "" {
			mu.Lock()
			js := state.journal(kind)
			js.Cursor = entry.Cursor
			state.setJournal(kind, js)
			mu.Unlock()
		}
		return nil
	}

	f := newFinding(kind, entry, m)
	ok := notifyFinding(ctx, in.CheckInput, &f)

	mu.Lock()
	if ok {
		now := time.Now().UTC()
		if !in.Now.IsZero() {
			now = in.Now
		}
		state.MarkCursorSeen(entry.Cursor, now)
		if entry.Cursor != "" {
			js := state.journal(kind)
			js.Cursor = entry.Cursor
			state.setJournal(kind, js)
		}
	}
	rep.Findings = append(rep.Findings, f)
	jr.Findings++
	snap := cloneState(*state)
	mu.Unlock()

	if in.OnFinding != nil {
		in.OnFinding(f)
	}
	if in.OnSave != nil {
		if saveErr := in.OnSave(snap); saveErr != nil {
			return saveErr
		}
	}
	return nil
}

func newFinding(kind JournalKind, entry Entry, m Match) Finding {
	return Finding{
		Time:         entry.Timestamp.UTC(),
		Journal:      kind,
		Unit:         entry.Unit,
		Process:      entry.Process,
		PID:          entry.PID,
		Rule:         m.Rule,
		MaskedPrefix: m.MaskedPrefix,
		Cursor:       entry.Cursor,
	}
}

func unitMatches(entryUnit, want string) bool {
	if entryUnit == want {
		return true
	}
	return strings.TrimSuffix(entryUnit, ".service") == strings.TrimSuffix(want, ".service")
}

func cloneState(s State) State {
	out := State{
		Journals: make(map[JournalKind]JournalState, len(s.Journals)),
	}
	for k, v := range s.Journals {
		out.Journals[k] = v
	}
	if len(s.SeenCursors) > 0 {
		out.SeenCursors = make(map[string]time.Time, len(s.SeenCursors))
		for k, v := range s.SeenCursors {
			out.SeenCursors[k] = v
		}
	}
	return out
}

// FormatReport renders a Report as human-readable text or JSON.
func FormatReport(rep Report, asJSON bool) (string, error) {
	if rep.Findings == nil {
		rep.Findings = []Finding{}
	}
	if rep.Journals == nil {
		rep.Journals = []JournalReport{}
	}
	if asJSON {
		body, err := json.MarshalIndent(rep, "", "  ")
		if err != nil {
			return "", fmt.Errorf("encode report: %w", err)
		}
		return string(body) + "\n", nil
	}
	var b strings.Builder
	for _, f := range rep.Findings {
		fmt.Fprintln(&b, FormatFindingLine(f))
	}
	return b.String(), nil
}

// FormatFindingLine formats a single masked Finding as one line.
func FormatFindingLine(f Finding) string {
	ts := "-"
	if !f.Time.IsZero() {
		ts = f.Time.UTC().Format(time.RFC3339)
	}
	unit := f.Unit
	if unit == "" {
		unit = "-"
	}
	proc := f.Process
	if proc == "" {
		proc = "-"
	}
	pid := f.PID
	if pid == "" {
		pid = "-"
	}
	cursor := f.Cursor
	if cursor == "" {
		cursor = "-"
	}
	return fmt.Sprintf("%s %s unit=%s proc=%s pid=%s rule=%s prefix=%s cursor=%s",
		ts, f.Journal, unit, proc, pid, f.Rule, f.MaskedPrefix, cursor)
}
