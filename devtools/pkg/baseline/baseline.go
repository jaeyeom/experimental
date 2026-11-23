// Package baseline implements the standard checker baseline JSON schema,
// loader, canonical writer, and path/glob matcher.
package baseline

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
)

// CurrentVersion is the supported baseline schema version.
const CurrentVersion = 1

// File represents a checker baseline JSON document.
type File struct {
	Version int              `json:"version"`
	Checker string           `json:"checker"`
	Entries map[string]Entry `json:"entries"`
}

// Entry represents a single path or glob baseline entry.
type Entry struct {
	Limits map[string]int64 `json:"limits,omitempty"`
	Reason string           `json:"reason,omitempty"`
	Issue  string           `json:"issue,omitempty"`
}

// canonicalFile mirrors File with fields ordered lexicographically by JSON key
// so encoding/json emits deterministic, sorted output.
type canonicalFile struct {
	Checker string                    `json:"checker"`
	Entries map[string]canonicalEntry `json:"entries"`
	Version int                       `json:"version"`
}

type canonicalEntry struct {
	Issue  string           `json:"issue,omitempty"`
	Limits map[string]int64 `json:"limits,omitempty"`
	Reason string           `json:"reason,omitempty"`
}

// Load reads and validates a baseline JSON file from path.
// If path does not exist, Load returns an initialized empty *File with
// Version set to CurrentVersion and a non-nil Entries map.
func Load(path string) (*File, error) {
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return &File{
			Version: CurrentVersion,
			Entries: map[string]Entry{},
		}, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read baseline %s: %w", path, err)
	}

	f, err := decodeFile(data)
	if err != nil {
		return nil, fmt.Errorf("decode baseline %s: %w", path, err)
	}
	if err := validateFile(f); err != nil {
		return nil, fmt.Errorf("validate baseline %s: %w", path, err)
	}
	return f, nil
}

func decodeFile(data []byte) (*File, error) {
	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()

	var f File
	if err := dec.Decode(&f); err != nil {
		return nil, fmt.Errorf("parse json: %w", err)
	}
	var extra any
	if err := dec.Decode(&extra); !errors.Is(err, io.EOF) {
		return nil, errors.New("unexpected trailing JSON data")
	}
	if f.Entries == nil {
		f.Entries = map[string]Entry{}
	}
	return &f, nil
}

func validateFile(f *File) error {
	if f.Version != CurrentVersion {
		return fmt.Errorf("unsupported version %d (expected %d)", f.Version, CurrentVersion)
	}
	for k := range f.Entries {
		norm, err := NormalizeTarget(k)
		if err != nil {
			return fmt.Errorf("invalid target key %q: %w", k, err)
		}
		if norm != k {
			return fmt.Errorf("unnormalized target key %q (expected %q)", k, norm)
		}
	}
	return nil
}

// Marshal serializes f as canonical pretty-printed JSON with 2-space
// indentation, lexicographically sorted keys, and a trailing newline.
func Marshal(f *File) ([]byte, error) {
	if f == nil {
		return nil, errors.New("baseline file is nil")
	}
	entries := make(map[string]canonicalEntry, len(f.Entries))
	for k, e := range f.Entries {
		entries[k] = canonicalEntry{
			Issue:  e.Issue,
			Limits: e.Limits,
			Reason: e.Reason,
		}
	}
	cf := canonicalFile{
		Checker: f.Checker,
		Entries: entries,
		Version: f.Version,
	}

	var buf bytes.Buffer
	enc := json.NewEncoder(&buf)
	enc.SetEscapeHTML(false)
	enc.SetIndent("", "  ")
	if err := enc.Encode(cf); err != nil {
		return nil, fmt.Errorf("encode baseline json: %w", err)
	}
	return buf.Bytes(), nil
}

// Save writes f in canonical JSON format to path, creating parent directories
// if needed.
func Save(path string, f *File) error {
	data, err := Marshal(f)
	if err != nil {
		return err
	}
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("create baseline directory %s: %w", dir, err)
	}
	if err := os.WriteFile(path, data, 0o644); err != nil {
		return fmt.Errorf("write baseline %s: %w", path, err)
	}
	return nil
}
