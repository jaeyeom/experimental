package memstore

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/pelletier/go-toml/v2"
)

// RoutineManifest describes a declarative routine loaded from a TOML file.
type RoutineManifest struct {
	ID          string         `toml:"id"`
	Name        string         `toml:"name"`
	Vault       string         `toml:"vault"`
	Profile     string         `toml:"profile"`
	Residency   string         `toml:"residency"`
	Schedule    string         `toml:"schedule"`
	Timezone    string         `toml:"timezone"`
	Timeout     string         `toml:"timeout"`
	Idempotency string         `toml:"idempotency"`
	Enabled     bool           `toml:"enabled"`
	Handler     RoutineHandler `toml:"handler"`
}

// RoutineHandler configures how a routine executes its command.
type RoutineHandler struct {
	Command      []string `toml:"command"`
	OutputFormat string   `toml:"output_format"`
}

type rawRoutineManifest struct {
	ID          string         `toml:"id"`
	Name        string         `toml:"name"`
	Vault       string         `toml:"vault"`
	Profile     string         `toml:"profile"`
	Residency   string         `toml:"residency"`
	Schedule    string         `toml:"schedule"`
	Timezone    string         `toml:"timezone"`
	Timeout     string         `toml:"timeout"`
	Idempotency string         `toml:"idempotency"`
	Enabled     *bool          `toml:"enabled"`
	Handler     RoutineHandler `toml:"handler"`
}

// TimeoutDuration returns the parsed Timeout duration.
func (m RoutineManifest) TimeoutDuration() (time.Duration, error) {
	d, err := time.ParseDuration(m.Timeout)
	if err != nil {
		return 0, fmt.Errorf("parse timeout %q: %w", m.Timeout, err)
	}
	return d, nil
}

// ParseRoutineManifest parses and validates a single TOML routine manifest.
func ParseRoutineManifest(data []byte) (RoutineManifest, error) {
	var raw rawRoutineManifest
	if err := toml.Unmarshal(data, &raw); err != nil {
		return RoutineManifest{}, fmt.Errorf("parse routine manifest: %w", err)
	}

	enabled := true
	if raw.Enabled != nil {
		enabled = *raw.Enabled
	}

	m := RoutineManifest{
		ID:          raw.ID,
		Name:        raw.Name,
		Vault:       raw.Vault,
		Profile:     raw.Profile,
		Residency:   raw.Residency,
		Schedule:    raw.Schedule,
		Timezone:    raw.Timezone,
		Timeout:     raw.Timeout,
		Idempotency: raw.Idempotency,
		Enabled:     enabled,
		Handler:     raw.Handler,
	}

	applyRoutineDefaults(&m)
	if err := validateRoutineManifest(m); err != nil {
		return RoutineManifest{}, err
	}
	return m, nil
}

// LoadRoutineManifest reads, parses, and validates a single TOML manifest file.
func LoadRoutineManifest(path string) (RoutineManifest, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return RoutineManifest{}, fmt.Errorf("read routine manifest %s: %w", path, err)
	}
	m, err := ParseRoutineManifest(data)
	if err != nil {
		return RoutineManifest{}, fmt.Errorf("manifest %s: %w", path, err)
	}
	return m, nil
}

// LoadRoutineManifests loads all *.toml routine manifests from dir, sorted by
// filename. A missing directory returns an empty slice without error.
func LoadRoutineManifests(dir string) ([]RoutineManifest, error) {
	entries, err := os.ReadDir(dir)
	if errors.Is(err, fs.ErrNotExist) {
		return []RoutineManifest{}, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read routines dir %s: %w", dir, err)
	}

	var names []string
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".toml") {
			continue
		}
		names = append(names, e.Name())
	}
	slices.Sort(names)

	manifests := make([]RoutineManifest, 0, len(names))
	seenIDs := make(map[string]string, len(names))
	for _, name := range names {
		p := filepath.Join(dir, name)
		m, err := LoadRoutineManifest(p)
		if err != nil {
			return nil, err
		}
		if prev, dup := seenIDs[m.ID]; dup {
			return nil, fmt.Errorf("duplicate routine id %q in %s and %s", m.ID, prev, name)
		}
		seenIDs[m.ID] = name
		manifests = append(manifests, m)
	}
	return manifests, nil
}

func applyRoutineDefaults(m *RoutineManifest) {
	if m.Name == "" {
		m.Name = m.ID
	}
	if m.Residency == "" {
		m.Residency = "local"
	}
	if m.Timezone == "" {
		m.Timezone = "UTC"
	}
	if m.Timeout == "" {
		m.Timeout = "2m"
	}
	if m.Idempotency == "" {
		m.Idempotency = "date"
	}
	if m.Handler.OutputFormat == "" {
		m.Handler.OutputFormat = "none"
	}
}

func validateRoutineManifest(m RoutineManifest) error {
	if !isValidRoutineID(m.ID) {
		return fmt.Errorf("invalid routine id %q: must be non-empty slug [a-z0-9][a-z0-9_-]*", m.ID)
	}
	if m.Vault == "" {
		return fmt.Errorf("routine %q: vault is required", m.ID)
	}
	if m.Profile == "" {
		return fmt.Errorf("routine %q: profile is required", m.ID)
	}
	if m.Residency != "local" && m.Residency != "gcp" {
		return fmt.Errorf("routine %q: invalid residency %q (want local or gcp)", m.ID, m.Residency)
	}
	if m.Idempotency != "date" && m.Idempotency != "date-hour" && m.Idempotency != "run" {
		return fmt.Errorf("routine %q: invalid idempotency %q (want date, date-hour, or run)", m.ID, m.Idempotency)
	}
	if err := validateRoutineTiming(m); err != nil {
		return err
	}
	return validateRoutineHandler(m.ID, m.Handler)
}

func validateRoutineTiming(m RoutineManifest) error {
	if _, err := loadScheduleLocation(m.Timezone); err != nil {
		return fmt.Errorf("routine %q: %w", m.ID, err)
	}
	if _, err := ParseSchedule(m.Schedule); err != nil {
		return fmt.Errorf("routine %q: invalid schedule: %w", m.ID, err)
	}
	d, err := time.ParseDuration(m.Timeout)
	if err != nil || d <= 0 {
		return fmt.Errorf("routine %q: invalid timeout %q: must be positive duration", m.ID, m.Timeout)
	}
	return nil
}

func validateRoutineHandler(routineID string, h RoutineHandler) error {
	if len(h.Command) == 0 || h.Command[0] == "" {
		return fmt.Errorf("routine %q: handler.command must be non-empty", routineID)
	}
	if h.OutputFormat != "json" && h.OutputFormat != "none" {
		return fmt.Errorf("routine %q: invalid handler.output_format %q (want json or none)", routineID, h.OutputFormat)
	}
	return nil
}

func isValidRoutineID(id string) bool {
	if id == "" {
		return false
	}
	for i := range len(id) {
		c := id[i]
		isLower := c >= 'a' && c <= 'z'
		isDigit := c >= '0' && c <= '9'
		if i == 0 {
			if !isLower && !isDigit {
				return false
			}
			continue
		}
		if !isLower && !isDigit && c != '-' && c != '_' {
			return false
		}
	}
	return true
}
