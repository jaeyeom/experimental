package outbox

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
)

// ErrNotFound is returned when no record matches the requested id.
var ErrNotFound = errors.New("no such record")

// Store persists confirmation records.
type Store interface {
	Create(r Record) (Record, error)
	Save(r Record) error
	Get(id int64) (Record, error)
	List() ([]Record, error)
}

// WriteFileAtomic replaces path safely using a temporary file in the same directory.
func WriteFileAtomic(path string, data []byte, pattern string) error {
	dir := filepath.Dir(path)
	tmp, err := os.CreateTemp(dir, pattern)
	if err != nil {
		return fmt.Errorf("create temp file for %s: %w", path, err)
	}
	_, werr := tmp.Write(data)
	serr := tmp.Sync()
	cerr := tmp.Close()
	if err := errors.Join(werr, serr, cerr); err != nil {
		return errors.Join(fmt.Errorf("write %s: %w", path, err), os.Remove(tmp.Name()))
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		return errors.Join(fmt.Errorf("rename %s: %w", path, err), os.Remove(tmp.Name()))
	}
	return nil
}

// TypedFileStore stores one indented JSON file per record inside Dir.
type TypedFileStore[T any] struct {
	Dir   string
	IDOf  func(T) int64
	SetID func(*T, int64)
}

// NewTypedFileStore returns a TypedFileStore rooted at dir.
func NewTypedFileStore[T any](dir string, idOf func(T) int64, setID func(*T, int64)) *TypedFileStore[T] {
	return &TypedFileStore[T]{Dir: dir, IDOf: idOf, SetID: setID}
}

// NewFileStore returns a TypedFileStore for canonical outbox Records.
func NewFileStore(dir string) *TypedFileStore[Record] {
	return NewTypedFileStore(
		dir,
		func(r Record) int64 { return r.ID },
		func(r *Record, id int64) { r.ID = id },
	)
}

func (s *TypedFileStore[T]) path(id int64) string {
	return filepath.Join(s.Dir, strconv.FormatInt(id, 10)+".json")
}

// Lock acquires an exclusive non-blocking lock on the store directory.
func (s *TypedFileStore[T]) Lock() (func(), error) {
	return LockDir(s.Dir)
}

// Create stores r under a new monotonic id and returns it.
func (s *TypedFileStore[T]) Create(r T) (T, error) {
	var zero T
	if err := os.MkdirAll(s.Dir, 0o700); err != nil {
		return zero, fmt.Errorf("create data dir: %w", err)
	}
	ids, err := s.ids()
	if err != nil {
		return zero, err
	}
	next := int64(1)
	if len(ids) > 0 {
		next = ids[len(ids)-1] + 1
	}
	for ; ; next++ {
		f, err := os.OpenFile(s.path(next), os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0o600)
		if errors.Is(err, fs.ErrExist) {
			continue
		}
		if err != nil {
			return zero, fmt.Errorf("create record: %w", err)
		}
		if err := f.Close(); err != nil {
			return zero, fmt.Errorf("create record: %w", err)
		}
		s.SetID(&r, next)
		return r, s.Save(r)
	}
}

// Save replaces r's JSON file atomically.
func (s *TypedFileStore[T]) Save(r T) error {
	id := s.IDOf(r)
	data, err := json.MarshalIndent(r, "", "  ")
	if err != nil {
		return fmt.Errorf("encode record %d: %w", id, err)
	}
	if err := WriteFileAtomic(s.path(id), append(data, '\n'), ".draft-*"); err != nil {
		return fmt.Errorf("save record %d: %w", id, err)
	}
	return nil
}

// Get reads one record by id.
func (s *TypedFileStore[T]) Get(id int64) (T, error) {
	var r T
	data, err := os.ReadFile(s.path(id))
	if errors.Is(err, fs.ErrNotExist) {
		return r, fmt.Errorf("record %d: %w", id, ErrNotFound)
	}
	if err != nil {
		return r, fmt.Errorf("read record %d: %w", id, err)
	}
	if err := json.Unmarshal(data, &r); err != nil {
		return r, fmt.Errorf("parse record %d: %w", id, err)
	}
	return r, nil
}

// List returns every record, oldest first. Empty placeholder files held by
// a concurrent Create are skipped.
func (s *TypedFileStore[T]) List() ([]T, error) {
	ids, err := s.ids()
	if err != nil {
		return nil, err
	}
	var out []T
	for _, id := range ids {
		r, err := s.Get(id)
		if err != nil {
			if info, serr := os.Stat(s.path(id)); serr == nil && info.Size() == 0 {
				continue
			}
			return nil, err
		}
		out = append(out, r)
	}
	return out, nil
}

func (s *TypedFileStore[T]) ids() ([]int64, error) {
	entries, err := os.ReadDir(s.Dir)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read data dir: %w", err)
	}
	var ids []int64
	for _, e := range entries {
		name, ok := strings.CutSuffix(e.Name(), ".json")
		if !ok {
			continue
		}
		if id, err := strconv.ParseInt(name, 10, 64); err == nil {
			ids = append(ids, id)
		}
	}
	slices.Sort(ids)
	return ids, nil
}
