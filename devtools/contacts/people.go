package contacts

import (
	"database/sql"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"time"
)

// Handle is one account of a person in one system.
type Handle struct {
	// Kind is chat, code, tracker, email, or other:<label>.
	Kind  string `json:"kind"`
	Value string `json:"value"`
}

func (h Handle) String() string { return h.Kind + ":" + h.Value }

// ParseHandle parses kind:value. Free-form kinds are written
// other:<label>:value.
func ParseHandle(s string) (Handle, error) {
	kind, value, ok := strings.Cut(s, ":")
	if kind == "other" && ok {
		var label string
		label, value, ok = strings.Cut(value, ":")
		if label == "" {
			return Handle{}, fmt.Errorf("handle %q: other needs a label, as other:<label>:value", s)
		}
		kind = "other:" + label
	}
	if !ok || value == "" {
		return Handle{}, fmt.Errorf("handle %q: want kind:value", s)
	}
	if !validKind(kind) {
		return Handle{}, fmt.Errorf("handle %q: kind must be chat, code, tracker, email or other:<label>", s)
	}
	return Handle{Kind: kind, Value: value}, nil
}

func validKind(kind string) bool {
	switch kind {
	case "chat", "code", "tracker", "email":
		return true
	}
	label, ok := strings.CutPrefix(kind, "other:")
	return ok && label != ""
}

// singleAccountKinds are the handle kinds a person has at most one of.
// Merging two people who each have a different handle of one of these
// kinds is a conflict the user must resolve first.
var singleAccountKinds = []string{"chat", "code", "tracker"}

// Person is one row of people plus its aliases and handles.
type Person struct {
	ID      string   `json:"id"`
	Name    string   `json:"name"`
	Aliases []string `json:"aliases"`
	Handles []Handle `json:"handles"`
	Team    string   `json:"team"`
	Role    string   `json:"role"`
	TZ      string   `json:"tz"`
	Channel string   `json:"channel"`
}

// Candidate is one of several people a query matched.
type Candidate struct {
	ID   string `json:"id"`
	Name string `json:"name"`
	Team string `json:"team"`
}

// ErrNotFound means no person matched.
var ErrNotFound = errors.New("no such person")

// AmbiguousError means a query matched more than one person.
type AmbiguousError struct {
	Query      string
	Candidates []Candidate
}

func (e *AmbiguousError) Error() string {
	var b strings.Builder
	fmt.Fprintf(&b, "%q matches %d people:", e.Query, len(e.Candidates))
	for _, c := range e.Candidates {
		fmt.Fprintf(&b, "\n  %s\t%s\t%s", c.ID, c.Name, c.Team)
	}
	return b.String()
}

// HandleTakenError means a handle already belongs to another person.
type HandleTakenError struct {
	Handle Handle
	Owner  string
}

func (e *HandleTakenError) Error() string {
	return fmt.Sprintf("handle %s already belongs to %s", e.Handle, e.Owner)
}

// querier is satisfied by *sql.DB and *sql.Tx.
type querier interface {
	Exec(query string, args ...any) (sql.Result, error)
	Query(query string, args ...any) (*sql.Rows, error)
	QueryRow(query string, args ...any) *sql.Row
}

// inTx runs f in one immediate transaction.
func (s *Store) inTx(f func(tx *sql.Tx) error) error {
	tx, err := s.db.Begin()
	if err != nil {
		return fmt.Errorf("begin: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit
	if err := f(tx); err != nil {
		return err
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("commit: %w", err)
	}
	return nil
}

// NotesPath is where the user's notes for person id live. The file may not
// exist.
func (s *Store) NotesPath(id string) string {
	return filepath.Join(s.dir, "people", id+".md")
}

var nonSlug = regexp.MustCompile(`[^a-z0-9]+`)

// Add stores a new person and returns its id, a slug of the name made
// unique with a numeric suffix. The id never changes afterwards.
func (s *Store) Add(p Person) (string, error) {
	p.Name = strings.TrimSpace(p.Name)
	if p.Name == "" {
		return "", errors.New("name is empty")
	}
	if err := checkTZ(p.TZ); err != nil {
		return "", err
	}
	base := strings.Trim(nonSlug.ReplaceAllString(strings.ToLower(p.Name), "-"), "-")
	if base == "" {
		base = "person"
	}
	var id string
	err := s.inTx(func(tx *sql.Tx) error {
		id = base
		for n := 2; ; n++ {
			var exists int
			if err := tx.QueryRow(`SELECT count(*) FROM people WHERE id = ?`, id).Scan(&exists); err != nil {
				return fmt.Errorf("add: %w", err)
			}
			if exists == 0 {
				break
			}
			id = fmt.Sprintf("%s-%d", base, n)
		}
		if _, err := tx.Exec(`INSERT INTO people (id, name, team, role, tz, channel, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)`,
			id, p.Name, p.Team, p.Role, p.TZ, p.Channel, s.timestamp()); err != nil {
			return fmt.Errorf("add: %w", err)
		}
		if err := addAliases(tx, id, p.Aliases); err != nil {
			return err
		}
		return addHandles(tx, id, p.Handles)
	})
	if err != nil {
		return "", err
	}
	return id, nil
}

// Patch is a change to a person. A nil field is left alone.
type Patch struct {
	Team, Role, TZ, Channel   *string
	AddAliases, RemoveAliases []string
	AddHandles, RemoveHandles []Handle
}

// Edit applies patch to person id.
func (s *Store) Edit(id string, patch Patch) error {
	if patch.TZ != nil {
		if err := checkTZ(*patch.TZ); err != nil {
			return err
		}
	}
	return s.inTx(func(tx *sql.Tx) error {
		if _, err := tx.Exec(`UPDATE people SET team = coalesce(?, team), role = coalesce(?, role),
				tz = coalesce(?, tz), channel = coalesce(?, channel) WHERE id = ?`,
			orNull(patch.Team), orNull(patch.Role), orNull(patch.TZ), orNull(patch.Channel), id); err != nil {
			return fmt.Errorf("edit: %w", err)
		}
		for _, a := range patch.RemoveAliases {
			res, err := tx.Exec(`DELETE FROM aliases WHERE person_id = ? AND alias = ?`, id, a)
			if err != nil {
				return fmt.Errorf("remove alias: %w", err)
			}
			if n, _ := res.RowsAffected(); n == 0 {
				return fmt.Errorf("%s has no alias %q", id, a)
			}
		}
		for _, h := range patch.RemoveHandles {
			res, err := tx.Exec(`DELETE FROM handles WHERE person_id = ? AND kind = ? AND value = ?`, id, h.Kind, h.Value)
			if err != nil {
				return fmt.Errorf("remove handle: %w", err)
			}
			if n, _ := res.RowsAffected(); n == 0 {
				return fmt.Errorf("%s has no handle %s", id, h)
			}
		}
		if err := addAliases(tx, id, patch.AddAliases); err != nil {
			return err
		}
		return addHandles(tx, id, patch.AddHandles)
	})
}

// orNull returns *p, or nil (SQL NULL) when p is nil.
func orNull(p *string) any {
	if p == nil {
		return nil
	}
	return *p
}

func checkTZ(tz string) error {
	if tz == "" {
		return nil
	}
	if _, err := time.LoadLocation(tz); err != nil {
		return fmt.Errorf("timezone %q: %w", tz, err)
	}
	return nil
}

func addAliases(q querier, id string, aliases []string) error {
	for _, a := range aliases {
		a = strings.TrimSpace(a)
		if a == "" {
			return errors.New("alias is empty")
		}
		if _, err := q.Exec(`INSERT OR IGNORE INTO aliases (person_id, alias) VALUES (?, ?)`, id, a); err != nil {
			return fmt.Errorf("add alias: %w", err)
		}
	}
	return nil
}

func addHandles(q querier, id string, handles []Handle) error {
	for _, h := range handles {
		var owner string
		err := q.QueryRow(`SELECT person_id FROM handles WHERE kind = ? AND value = ?`, h.Kind, h.Value).Scan(&owner)
		switch {
		case err == nil && owner == id:
			continue
		case err == nil:
			return &HandleTakenError{Handle: h, Owner: owner}
		case !errors.Is(err, sql.ErrNoRows):
			return fmt.Errorf("add handle: %w", err)
		}
		if _, err := q.Exec(`INSERT INTO handles (person_id, kind, value) VALUES (?, ?, ?)`, id, h.Kind, h.Value); err != nil {
			return fmt.Errorf("add handle: %w", err)
		}
	}
	return nil
}

// Get returns person id with aliases and handles.
func (s *Store) Get(id string) (Person, error) {
	return getPerson(s.db, id)
}

func getPerson(q querier, id string) (Person, error) {
	p := Person{Aliases: []string{}, Handles: []Handle{}}
	err := q.QueryRow(`SELECT id, name, team, role, tz, channel FROM people WHERE id = ?`, id).
		Scan(&p.ID, &p.Name, &p.Team, &p.Role, &p.TZ, &p.Channel)
	if errors.Is(err, sql.ErrNoRows) {
		return p, ErrNotFound
	}
	if err != nil {
		return p, fmt.Errorf("get %s: %w", id, err)
	}
	rows, err := q.Query(`SELECT alias FROM aliases WHERE person_id = ? ORDER BY alias`, id)
	if err != nil {
		return p, fmt.Errorf("get aliases: %w", err)
	}
	for rows.Next() {
		var a string
		if err := rows.Scan(&a); err != nil {
			rows.Close()
			return p, fmt.Errorf("get aliases: %w", err)
		}
		p.Aliases = append(p.Aliases, a)
	}
	rows.Close()
	rows, err = q.Query(`SELECT kind, value FROM handles WHERE person_id = ? ORDER BY kind, value`, id)
	if err != nil {
		return p, fmt.Errorf("get handles: %w", err)
	}
	defer rows.Close()
	for rows.Next() {
		var h Handle
		if err := rows.Scan(&h.Kind, &h.Value); err != nil {
			return p, fmt.Errorf("get handles: %w", err)
		}
		p.Handles = append(p.Handles, h)
	}
	if err := rows.Err(); err != nil {
		return p, fmt.Errorf("get handles: %w", err)
	}
	return p, nil
}

// Merge moves drop's handles, aliases, events and commitments to keep, fills
// keep's empty fields from drop, appends drop's notes to keep's, and deletes
// drop. It refuses when both have different handles of a single-account
// kind (chat, code, tracker).
func (s *Store) Merge(keepID, dropID string) error {
	if keepID == dropID {
		return errors.New("cannot merge a person into itself")
	}
	keep, err := s.Get(keepID)
	if err != nil {
		return err
	}
	drop, err := s.Get(dropID)
	if err != nil {
		return err
	}
	var conflicts []string
	for _, kh := range keep.Handles {
		if !slices.Contains(singleAccountKinds, kh.Kind) {
			continue
		}
		for _, dh := range drop.Handles {
			if dh.Kind == kh.Kind && !strings.EqualFold(dh.Value, kh.Value) {
				conflicts = append(conflicts, fmt.Sprintf("%s (%s) vs %s (%s)", kh, keep.ID, dh, drop.ID))
			}
		}
	}
	if len(conflicts) > 0 {
		return fmt.Errorf("handle conflict; remove one of each pair with contacts edit --remove-handle, then merge again:\n  %s", strings.Join(conflicts, "\n  "))
	}
	err = s.inTx(func(tx *sql.Tx) error {
		if _, err := tx.Exec(`UPDATE people SET
				team    = CASE WHEN team    = '' THEN ? ELSE team    END,
				role    = CASE WHEN role    = '' THEN ? ELSE role    END,
				tz      = CASE WHEN tz      = '' THEN ? ELSE tz      END,
				channel = CASE WHEN channel = '' THEN ? ELSE channel END
			WHERE id = ?`, drop.Team, drop.Role, drop.TZ, drop.Channel, keep.ID); err != nil {
			return fmt.Errorf("merge fields: %w", err)
		}
		for _, stmt := range []string{
			`INSERT OR IGNORE INTO aliases (person_id, alias) SELECT ?, alias FROM aliases WHERE person_id = ?`,
			`UPDATE OR IGNORE handles SET person_id = ? WHERE person_id = ?`,
			`UPDATE events SET person_id = ? WHERE person_id = ?`,
			`UPDATE commitments SET person_id = ? WHERE person_id = ?`,
		} {
			if _, err := tx.Exec(stmt, keep.ID, drop.ID); err != nil {
				return fmt.Errorf("merge: %w", err)
			}
		}
		if _, err := tx.Exec(`DELETE FROM people WHERE id = ?`, drop.ID); err != nil {
			return fmt.Errorf("merge: delete %s: %w", drop.ID, err)
		}
		return s.appendNotes(keep, drop)
	})
	if err != nil {
		return err
	}
	return removeIfExists(s.NotesPath(drop.ID))
}

// appendNotes appends drop's notes file, if any, to keep's under a
// separator line.
func (s *Store) appendNotes(keep, drop Person) error {
	notes, err := os.ReadFile(s.NotesPath(drop.ID))
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		return fmt.Errorf("read notes: %w", err)
	}
	f, err := os.OpenFile(s.NotesPath(keep.ID), os.O_WRONLY|os.O_APPEND|os.O_CREATE, 0o600)
	if err != nil {
		return fmt.Errorf("append notes: %w", err)
	}
	fi, err := f.Stat()
	if err != nil {
		f.Close()
		return fmt.Errorf("append notes: %w", err)
	}
	sep := fmt.Sprintf("\n--- merged from %s (%s) ---\n\n", drop.ID, drop.Name)
	if fi.Size() == 0 {
		sep = sep[1:]
	}
	if _, err := f.WriteString(sep + string(notes)); err != nil {
		f.Close()
		return fmt.Errorf("append notes: %w", err)
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("append notes: %w", err)
	}
	return nil
}

// Delete removes person id with their handles, aliases, events,
// commitments and notes file. Access-log rows keep the id.
func (s *Store) Delete(id string) error {
	err := s.inTx(func(tx *sql.Tx) error {
		res, err := tx.Exec(`DELETE FROM people WHERE id = ?`, id)
		if err != nil {
			return fmt.Errorf("delete %s: %w", id, err)
		}
		if n, _ := res.RowsAffected(); n == 0 {
			return ErrNotFound
		}
		return nil
	})
	if err != nil {
		return err
	}
	return removeIfExists(s.NotesPath(id))
}

func removeIfExists(path string) error {
	if err := os.Remove(path); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return fmt.Errorf("remove %s: %w", path, err)
	}
	return nil
}

// Find returns the one person query names: an exact handle (kind:value, or
// a value of any kind, with or without a leading @), else an id, else a
// name or alias, all case-insensitive.
func (s *Store) Find(query string) (Person, error) {
	return s.lookup(query, false)
}

// Resolve is Find, then a case-insensitive prefix match on ids, names,
// aliases and handle values when nothing matched exactly.
func (s *Store) Resolve(query string) (Person, error) {
	return s.lookup(query, true)
}

func (s *Store) lookup(query string, prefix bool) (Person, error) {
	q := strings.TrimSpace(query)
	if q == "" {
		return Person{}, ErrNotFound
	}
	steps := []func(string) ([]string, error){s.byHandle, s.byID, s.byName}
	if prefix {
		steps = append(steps, s.byPrefix)
	}
	for _, step := range steps {
		ids, err := step(q)
		if err != nil {
			return Person{}, err
		}
		switch len(ids) {
		case 0:
			continue
		case 1:
			return s.Get(ids[0])
		default:
			return Person{}, s.ambiguous(query, ids)
		}
	}
	return Person{}, ErrNotFound
}

func (s *Store) byHandle(q string) ([]string, error) {
	if h, err := ParseHandle(q); err == nil {
		return s.ids(`SELECT DISTINCT person_id FROM handles WHERE kind = ? AND value = ?`, h.Kind, h.Value)
	}
	return s.ids(`SELECT DISTINCT person_id FROM handles WHERE value = ? OR value = ?`, q, strings.TrimPrefix(q, "@"))
}

func (s *Store) byID(q string) ([]string, error) {
	return s.ids(`SELECT id FROM people WHERE id = ?`, strings.ToLower(q))
}

func (s *Store) byName(q string) ([]string, error) {
	return s.ids(`SELECT id FROM people WHERE lower(name) = lower(?)
		UNION SELECT person_id FROM aliases WHERE alias = ?`, q, q)
}

func (s *Store) byPrefix(q string) ([]string, error) {
	return s.ids(`SELECT id FROM people WHERE substr(lower(id), 1, length(?1)) = lower(?1)
			OR substr(lower(name), 1, length(?1)) = lower(?1)
		UNION SELECT person_id FROM aliases WHERE substr(lower(alias), 1, length(?1)) = lower(?1)
		UNION SELECT person_id FROM handles WHERE substr(lower(value), 1, length(?2)) = lower(?2)`,
		q, strings.TrimPrefix(q, "@"))
}

func (s *Store) ids(query string, args ...any) ([]string, error) {
	rows, err := s.db.Query(query, args...)
	if err != nil {
		return nil, fmt.Errorf("lookup: %w", err)
	}
	defer rows.Close()
	var ids []string
	for rows.Next() {
		var id string
		if err := rows.Scan(&id); err != nil {
			return nil, fmt.Errorf("lookup: %w", err)
		}
		ids = append(ids, id)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("lookup: %w", err)
	}
	slices.Sort(ids)
	return slices.Compact(ids), nil
}

func (s *Store) ambiguous(query string, ids []string) error {
	e := &AmbiguousError{Query: query}
	for _, id := range ids {
		p, err := s.Get(id)
		if err != nil {
			return err
		}
		e.Candidates = append(e.Candidates, Candidate{ID: p.ID, Name: p.Name, Team: p.Team})
	}
	return e
}
