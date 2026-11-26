package decide

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/google/renameio/v2"
)

const (
	// PanelsDirName is the directory under the decide state dir holding panel JSON files.
	PanelsDirName = "panels"

	panelFileExt = ".json"
)

var panelIDRegexp = regexp.MustCompile(`^[a-z0-9-]+$`)

// Panel is one dashboard panel stored in panels/<id>.json.
type Panel struct {
	ID         string          `json:"id"`
	Title      string          `json:"title"`
	Kind       string          `json:"kind"`
	Data       json.RawMessage `json:"data"`
	UpdatedAt  time.Time       `json:"updatedAt"`
	StaleAfter string          `json:"staleAfter,omitempty"`
	Source     string          `json:"source,omitempty"`
}

// PanelEntry is one loaded panel from panels/, either valid (with Stale
// evaluated against the read clock) or unreadable/invalid.
type PanelEntry struct {
	ID         string          `json:"id"`
	File       string          `json:"file,omitempty"`
	Title      string          `json:"title,omitempty"`
	Kind       string          `json:"kind,omitempty"`
	Data       json.RawMessage `json:"data,omitempty"`
	UpdatedAt  *time.Time      `json:"updatedAt,omitempty"`
	StaleAfter string          `json:"staleAfter,omitempty"`
	Source     string          `json:"source,omitempty"`
	Stale      bool            `json:"stale"`
	Unreadable bool            `json:"unreadable,omitempty"`
	Error      string          `json:"error,omitempty"`
}

// PanelKindValidator validates the data payload for a known panel kind.
type PanelKindValidator func(data json.RawMessage) error

var (
	panelKindMu         sync.RWMutex
	panelKindValidators = map[string]PanelKindValidator{}
)

// RegisterPanelKind registers a data validator for a known panel kind.
// Passing nil removes any validator for kind.
func RegisterPanelKind(kind string, validate PanelKindValidator) {
	panelKindMu.Lock()
	defer panelKindMu.Unlock()
	if validate == nil {
		delete(panelKindValidators, kind)
		return
	}
	panelKindValidators[kind] = validate
}

// PanelsDir returns the panels directory under stateDir.
func PanelsDir(stateDir string) string {
	return filepath.Join(stateDir, PanelsDirName)
}

// ValidPanelID reports whether id matches [a-z0-9-]+.
func ValidPanelID(id string) error {
	if id == "" || !panelIDRegexp.MatchString(id) {
		return fmt.Errorf("decide panel id %q must match [a-z0-9-]+", id)
	}
	return nil
}

// ValidatePanelData checks that data is valid non-null JSON and, when kind has
// a registered validator, runs that validator.
func ValidatePanelData(kind string, data json.RawMessage) error {
	trimmed := bytes.TrimSpace(data)
	if len(trimmed) == 0 || string(trimmed) == "null" {
		return fmt.Errorf("panel data is required")
	}
	if !json.Valid(trimmed) {
		return fmt.Errorf("panel data must be valid JSON")
	}
	panelKindMu.RLock()
	v := panelKindValidators[kind]
	panelKindMu.RUnlock()
	if v != nil {
		if err := v(trimmed); err != nil {
			return fmt.Errorf("panel kind %q data: %w", kind, err)
		}
	}
	return nil
}

// PanelStore manages dashboard panel JSON files under <stateDir>/panels/.
type PanelStore struct {
	dir string
	now func() time.Time
}

// OpenPanels opens the panel store under stateDir.
func OpenPanels(stateDir string) (*PanelStore, error) {
	if stateDir == "" {
		return nil, fmt.Errorf("decide directory is empty")
	}
	return &PanelStore{dir: stateDir, now: time.Now}, nil
}

// SetClock replaces the clock used for UpdatedAt and staleness checks.
func (s *PanelStore) SetClock(now func() time.Time) {
	if now == nil {
		s.now = time.Now
		return
	}
	s.now = now
}

// Dir returns the panels/ directory path.
func (s *PanelStore) Dir() string {
	return PanelsDir(s.dir)
}

func (s *PanelStore) clock() time.Time {
	if s.now == nil {
		return time.Now()
	}
	return s.now()
}

// Set validates p, stamps UpdatedAt with the store clock, and writes
// panels/<id>.json atomically via a temp file in the same directory.
func (s *PanelStore) Set(p Panel) (Panel, error) {
	if err := validatePanelInput(&p); err != nil {
		return Panel{}, err
	}
	p.UpdatedAt = s.clock().UTC()

	panelsDir := s.Dir()
	if err := os.MkdirAll(panelsDir, 0o755); err != nil {
		return Panel{}, fmt.Errorf("create panels directory: %w", err)
	}
	raw, err := json.MarshalIndent(p, "", "  ")
	if err != nil {
		return Panel{}, fmt.Errorf("encode decide panel: %w", err)
	}
	raw = append(raw, '\n')
	target := filepath.Join(panelsDir, p.ID+panelFileExt)
	if err := renameio.WriteFile(target, raw, 0o600, renameio.WithTempDir(panelsDir)); err != nil {
		return Panel{}, fmt.Errorf("write decide panel: %w", err)
	}
	return p, nil
}

// Remove deletes panels/<id>.json.
func (s *PanelStore) Remove(id string) error {
	if err := ValidPanelID(id); err != nil {
		return err
	}
	target := filepath.Join(s.Dir(), id+panelFileExt)
	if err := os.Remove(target); err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return fmt.Errorf("decide panel %q: %w", id, ErrNotFound)
		}
		return fmt.Errorf("remove decide panel %q: %w", id, err)
	}
	return nil
}

// List reads all *.json files in panels/ (ignoring dotfiles and non-.json
// files). Unreadable or invalid files are returned with Unreadable=true and
// never fail the whole list.
func (s *PanelStore) List() ([]PanelEntry, error) {
	panelsDir := s.Dir()
	entries, err := os.ReadDir(panelsDir)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return []PanelEntry{}, nil
		}
		return nil, fmt.Errorf("read panels directory: %w", err)
	}
	now := s.clock()
	out := make([]PanelEntry, 0, len(entries))
	for _, de := range entries {
		name := de.Name()
		if !isPanelFileName(name) || de.IsDir() {
			continue
		}
		stem := strings.TrimSuffix(name, panelFileExt)
		path := filepath.Join(panelsDir, name)
		out = append(out, readPanelEntry(path, name, stem, now))
	}
	sort.Slice(out, func(i, j int) bool {
		return out[i].File < out[j].File
	})
	return out, nil
}

func isPanelFileName(name string) bool {
	return !strings.HasPrefix(name, ".") && strings.HasSuffix(name, panelFileExt)
}

func validatePanelInput(p *Panel) error {
	if err := ValidPanelID(p.ID); err != nil {
		return err
	}
	p.Title = strings.TrimSpace(p.Title)
	if p.Title == "" {
		return fmt.Errorf("decide panel title is required")
	}
	p.Kind = strings.TrimSpace(p.Kind)
	if p.Kind == "" {
		return fmt.Errorf("decide panel kind is required")
	}
	if err := ValidatePanelData(p.Kind, p.Data); err != nil {
		return err
	}
	trimmedData := bytes.TrimSpace(p.Data)
	p.Data = append(json.RawMessage(nil), trimmedData...)
	if err := validateStaleAfter(p.StaleAfter); err != nil {
		return err
	}
	return nil
}

func validateStaleAfter(raw string) error {
	if strings.TrimSpace(raw) == "" {
		return nil
	}
	d, err := time.ParseDuration(raw)
	if err != nil || d <= 0 {
		return fmt.Errorf("decide panel staleAfter %q must be a positive duration", raw)
	}
	return nil
}

func readPanelEntry(path, file, stem string, now time.Time) PanelEntry {
	raw, err := os.ReadFile(path) //nolint:gosec // operator-local state file
	if err != nil {
		return PanelEntry{ID: stem, File: file, Unreadable: true, Error: err.Error()}
	}
	p, err := decodePanel(raw, stem)
	if err != nil {
		return PanelEntry{ID: stem, File: file, Unreadable: true, Error: err.Error()}
	}
	ts := p.UpdatedAt.UTC()
	return PanelEntry{
		ID:         p.ID,
		File:       file,
		Title:      p.Title,
		Kind:       p.Kind,
		Data:       append(json.RawMessage(nil), p.Data...),
		UpdatedAt:  &ts,
		StaleAfter: p.StaleAfter,
		Source:     p.Source,
		Stale:      panelIsStale(ts, p.StaleAfter, now),
	}
}

func decodePanel(raw []byte, stem string) (Panel, error) {
	if err := ValidPanelID(stem); err != nil {
		return Panel{}, err
	}
	dec := json.NewDecoder(bytes.NewReader(raw))
	dec.DisallowUnknownFields()
	var p Panel
	if err := dec.Decode(&p); err != nil {
		return Panel{}, fmt.Errorf("parse JSON: %w", err)
	}
	if _, err := dec.Token(); !errors.Is(err, io.EOF) {
		return Panel{}, fmt.Errorf("unexpected trailing data after JSON object")
	}
	if err := validateLoadedPanel(&p, stem); err != nil {
		return Panel{}, err
	}
	return p, nil
}

func validateLoadedPanel(p *Panel, stem string) error {
	if p.ID == "" {
		return fmt.Errorf("missing required field %q", "id")
	}
	if err := ValidPanelID(p.ID); err != nil {
		return err
	}
	if p.ID != stem {
		return fmt.Errorf("panel id %q does not match file %q", p.ID, stem+panelFileExt)
	}
	if strings.TrimSpace(p.Title) == "" {
		return fmt.Errorf("missing required field %q", "title")
	}
	if strings.TrimSpace(p.Kind) == "" {
		return fmt.Errorf("missing required field %q", "kind")
	}
	if p.UpdatedAt.IsZero() {
		return fmt.Errorf("missing required field %q", "updatedAt")
	}
	if err := ValidatePanelData(p.Kind, p.Data); err != nil {
		return err
	}
	p.Data = append(json.RawMessage(nil), bytes.TrimSpace(p.Data)...)
	return validateStaleAfter(p.StaleAfter)
}

func panelIsStale(updatedAt time.Time, staleAfter string, now time.Time) bool {
	if strings.TrimSpace(staleAfter) == "" || updatedAt.IsZero() {
		return false
	}
	d, err := time.ParseDuration(staleAfter)
	if err != nil || d <= 0 {
		return false
	}
	return !now.Before(updatedAt.Add(d))
}

// PanelAge formats a non-negative duration since updatedAt as "now", "5m",
// "2h", or "1d".
func PanelAge(d time.Duration) string {
	if d < 0 {
		d = 0
	}
	switch {
	case d < time.Minute:
		return "now"
	case d < time.Hour:
		return fmt.Sprintf("%dm", int(d/time.Minute))
	case d < 24*time.Hour:
		return fmt.Sprintf("%dh", int(d/time.Hour))
	default:
		return fmt.Sprintf("%dd", int(d/(24*time.Hour)))
	}
}

// NextPanelStaleAt returns the earliest updatedAt + staleAfter among currently
// non-stale readable panels, or zero if none have a pending staleness deadline.
func NextPanelStaleAt(entries []PanelEntry) time.Time {
	var earliest time.Time
	for i := range entries {
		e := &entries[i]
		if e.Unreadable || e.Stale || e.UpdatedAt == nil || strings.TrimSpace(e.StaleAfter) == "" {
			continue
		}
		d, err := time.ParseDuration(e.StaleAfter)
		if err != nil || d <= 0 {
			continue
		}
		at := e.UpdatedAt.Add(d)
		if earliest.IsZero() || at.Before(earliest) {
			earliest = at
		}
	}
	return earliest
}

// RecomputePanelsStale updates Stale on entries against now and reports
// whether any entry's Stale value changed.
func RecomputePanelsStale(entries []PanelEntry, now time.Time) ([]PanelEntry, bool) {
	if len(entries) == 0 {
		return nil, false
	}
	var changed bool
	out := ClonePanelEntries(entries)
	for i := range out {
		e := &out[i]
		if e.Unreadable || e.UpdatedAt == nil {
			continue
		}
		stale := panelIsStale(*e.UpdatedAt, e.StaleAfter, now)
		if stale != e.Stale {
			e.Stale = stale
			changed = true
		}
	}
	return out, changed
}

// ClonePanelEntries returns a deep copy of entries.
func ClonePanelEntries(entries []PanelEntry) []PanelEntry {
	if entries == nil {
		return nil
	}
	out := make([]PanelEntry, len(entries))
	for i, e := range entries {
		if e.Data != nil {
			e.Data = append(json.RawMessage(nil), e.Data...)
		}
		if e.UpdatedAt != nil {
			t := *e.UpdatedAt
			e.UpdatedAt = &t
		}
		out[i] = e
	}
	return out
}
