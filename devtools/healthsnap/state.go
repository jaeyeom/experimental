package healthsnap

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/google/renameio/v2"
)

const stateFileName = "state.json"

// LoadState loads state.json from path. A missing file returns an empty State.
func LoadState(path string) (State, error) {
	data, err := os.ReadFile(path) //nolint:gosec // G304: user's own state path
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return newEmptyState(), nil
		}
		return State{}, fmt.Errorf("read state: %w", err)
	}
	var st State
	if err := json.Unmarshal(data, &st); err != nil {
		return State{}, fmt.Errorf("parse state: %w", err)
	}
	if st.Rules == nil {
		st.Rules = map[string]RuleState{}
	}
	if st.Checks == nil {
		st.Checks = map[string]CheckState{}
	}
	return st, nil
}

func newEmptyState() State {
	return State{
		Rules:  map[string]RuleState{},
		Checks: map[string]CheckState{},
	}
}

// SaveState atomically writes state.json with mode 0600.
func SaveState(path string, st State) error {
	if st.Rules == nil {
		st.Rules = map[string]RuleState{}
	}
	if st.Checks == nil {
		st.Checks = map[string]CheckState{}
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("create state dir: %w", err)
	}
	data, err := json.MarshalIndent(st, "", "  ")
	if err != nil {
		return fmt.Errorf("encode state: %w", err)
	}
	data = append(data, '\n')
	if err := renameio.WriteFile(path, data, 0o600); err != nil {
		return fmt.Errorf("write state: %w", err)
	}
	return nil
}

// SnapshotFiles lists the file paths written by WriteSnapshot.
type SnapshotFiles struct {
	JSONPath       string
	MarkdownPath   string
	LatestJSONPath string
	LatestMDPath   string
}

// WriteSnapshot writes a JSON snapshot (and optional Markdown rendering) into
// stateDir, updates latest.json/latest.md, and retains at most keep snapshots.
func WriteSnapshot(stateDir string, snap Snapshot, writeMarkdown bool, keep int) (SnapshotFiles, error) {
	snapDir := filepath.Join(stateDir, "snapshots")
	if err := os.MkdirAll(snapDir, 0o700); err != nil {
		return SnapshotFiles{}, fmt.Errorf("create snapshots dir: %w", err)
	}
	stem := uniqueSnapshotStem(snapDir, snap.Timestamp)
	jsonPath := filepath.Join(snapDir, stem+".json")
	latestJSON := filepath.Join(stateDir, "latest.json")

	raw, err := json.MarshalIndent(snap, "", "  ")
	if err != nil {
		return SnapshotFiles{}, fmt.Errorf("encode snapshot: %w", err)
	}
	raw = append(raw, '\n')
	if err := renameio.WriteFile(jsonPath, raw, 0o600); err != nil {
		return SnapshotFiles{}, fmt.Errorf("write snapshot: %w", err)
	}
	if err := renameio.WriteFile(latestJSON, raw, 0o600); err != nil {
		return SnapshotFiles{}, fmt.Errorf("write latest snapshot: %w", err)
	}

	files := SnapshotFiles{
		JSONPath:       jsonPath,
		LatestJSONPath: latestJSON,
	}
	if writeMarkdown {
		md := []byte(RenderMarkdown(snap))
		mdPath := filepath.Join(snapDir, stem+".md")
		latestMD := filepath.Join(stateDir, "latest.md")
		if err := renameio.WriteFile(mdPath, md, 0o600); err != nil {
			return SnapshotFiles{}, fmt.Errorf("write markdown snapshot: %w", err)
		}
		if err := renameio.WriteFile(latestMD, md, 0o600); err != nil {
			return SnapshotFiles{}, fmt.Errorf("write latest markdown: %w", err)
		}
		files.MarkdownPath = mdPath
		files.LatestMDPath = latestMD
	}

	if err := pruneSnapshots(snapDir, keep); err != nil {
		return files, err
	}
	return files, nil
}

func uniqueSnapshotStem(snapDir string, ts time.Time) string {
	base := ts.UTC().Format("20060102T150405.000000000Z")
	stem := base
	for i := 1; i < 1000; i++ {
		if _, err := os.Stat(filepath.Join(snapDir, stem+".json")); errors.Is(err, fs.ErrNotExist) {
			return stem
		}
		stem = fmt.Sprintf("%s-%03d", base, i)
	}
	return stem
}

func pruneSnapshots(snapDir string, keep int) error {
	if keep <= 0 {
		return nil
	}
	entries, err := os.ReadDir(snapDir)
	if err != nil {
		return fmt.Errorf("list snapshots: %w", err)
	}
	var jsonNames []string
	for _, e := range entries {
		if !e.IsDir() && strings.HasSuffix(e.Name(), ".json") {
			jsonNames = append(jsonNames, e.Name())
		}
	}
	if len(jsonNames) <= keep {
		return nil
	}
	sort.Strings(jsonNames)
	excess := len(jsonNames) - keep
	for _, name := range jsonNames[:excess] {
		_ = os.Remove(filepath.Join(snapDir, name))
		mdName := strings.TrimSuffix(name, ".json") + ".md"
		_ = os.Remove(filepath.Join(snapDir, mdName))
	}
	return nil
}

// LoadLatestSnapshot reads the newest snapshot from stateDir (latest.json or
// the newest file under snapshots/).
func LoadLatestSnapshot(stateDir string) (Snapshot, error) {
	latestPath := filepath.Join(stateDir, "latest.json")
	if data, err := os.ReadFile(latestPath); err == nil { //nolint:gosec // G304: user's own state dir
		var snap Snapshot
		if err := json.Unmarshal(data, &snap); err != nil {
			return Snapshot{}, fmt.Errorf("parse latest snapshot: %w", err)
		}
		return snap, nil
	} else if !errors.Is(err, fs.ErrNotExist) {
		return Snapshot{}, fmt.Errorf("read latest snapshot: %w", err)
	}

	snapDir := filepath.Join(stateDir, "snapshots")
	entries, err := os.ReadDir(snapDir)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return Snapshot{}, fmt.Errorf("no snapshots found in %s: %w", stateDir, fs.ErrNotExist)
		}
		return Snapshot{}, fmt.Errorf("read snapshots dir: %w", err)
	}
	var jsonNames []string
	for _, e := range entries {
		if !e.IsDir() && strings.HasSuffix(e.Name(), ".json") {
			jsonNames = append(jsonNames, e.Name())
		}
	}
	if len(jsonNames) == 0 {
		return Snapshot{}, fmt.Errorf("no snapshots found in %s: %w", stateDir, fs.ErrNotExist)
	}
	sort.Strings(jsonNames)
	data, err := os.ReadFile(filepath.Join(snapDir, jsonNames[len(jsonNames)-1])) //nolint:gosec // G304: user's own state dir
	if err != nil {
		return Snapshot{}, fmt.Errorf("read snapshot: %w", err)
	}
	var snap Snapshot
	if err := json.Unmarshal(data, &snap); err != nil {
		return Snapshot{}, fmt.Errorf("parse snapshot: %w", err)
	}
	return snap, nil
}

// RenderMarkdown formats a Snapshot as a human-readable Markdown summary.
func RenderMarkdown(snap Snapshot) string {
	var b strings.Builder
	fmt.Fprintf(&b, "# healthsnap snapshot (%s)\n\n", snap.Timestamp.UTC().Format(time.RFC3339))
	fmt.Fprintf(&b, "Overall state: **%s**\n\n", snap.State)
	fmt.Fprintln(&b, "| Check | Kind | State | Value | Duration | Error |")
	fmt.Fprintln(&b, "| --- | --- | --- | --- | --- | --- |")
	for _, c := range snap.Checks {
		val := c.Display
		if val == "" && c.Value != nil {
			val = formatScalar(c.Value)
		}
		if c.Cached {
			val += " (cached)"
		}
		fmt.Fprintf(&b, "| %s | %s | %s | %s | %dms | %s |\n",
			escapeMDCell(c.ID), c.Kind, c.State, escapeMDCell(val), c.DurationMs, escapeMDCell(c.Error))
	}
	if len(snap.Alerts) > 0 {
		fmt.Fprintln(&b, "\n## Alerts")
		for _, a := range snap.Alerts {
			fmt.Fprintf(&b, "- **%s** `%s` (%s -> %s): %s\n", a.Event, a.RuleID, a.Previous, a.State, a.Message)
		}
	}
	return b.String()
}

func escapeMDCell(s string) string {
	s = strings.ReplaceAll(s, "\n", " ")
	return strings.ReplaceAll(s, "|", `\|`)
}

// FormatSnapshotText formats a Snapshot for terminal output.
func FormatSnapshotText(snap Snapshot) string {
	var b strings.Builder
	fmt.Fprintf(&b, "healthsnap: %s (%s)\n", snap.State, snap.Timestamp.UTC().Format(time.RFC3339))
	for _, c := range snap.Checks {
		val := c.Display
		if val == "" && c.Value != nil {
			val = formatScalar(c.Value)
		}
		suffix := ""
		if c.Cached {
			suffix = " [cached]"
		}
		if c.Error != "" {
			fmt.Fprintf(&b, "  %-4s %-20s error: %s (%dms)%s\n", c.State, c.ID, c.Error, c.DurationMs, suffix)
		} else {
			fmt.Fprintf(&b, "  %-4s %-20s %s (%dms)%s\n", c.State, c.ID, val, c.DurationMs, suffix)
		}
	}
	for _, a := range snap.Alerts {
		fmt.Fprintf(&b, "  alert %s %s (%s -> %s) recipient=%s sent=%v\n", a.Event, a.RuleID, a.Previous, a.State, a.Recipient, a.Sent)
	}
	return b.String()
}
