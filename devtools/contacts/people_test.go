package contacts

import (
	"errors"
	"os"
	"slices"
	"strings"
	"testing"
)

func mustHandle(t *testing.T, s string) Handle {
	t.Helper()
	h, err := ParseHandle(s)
	if err != nil {
		t.Fatal(err)
	}
	return h
}

func mustAdd(t *testing.T, s *Store, p Person) string {
	t.Helper()
	id, err := s.Add(p)
	if err != nil {
		t.Fatalf("Add(%s): %v", p.Name, err)
	}
	return id
}

func count(t *testing.T, s *Store, query string, args ...any) int {
	t.Helper()
	var n int
	if err := s.db.QueryRow(query, args...).Scan(&n); err != nil {
		t.Fatal(err)
	}
	return n
}

func TestParseHandle(t *testing.T) {
	for in, want := range map[string]Handle{
		"chat:U123":           {"chat", "U123"},
		"email:a@example.com": {"email", "a@example.com"},
		"other:wiki:ada":      {"other:wiki", "ada"},
	} {
		if got, err := ParseHandle(in); err != nil || got != want {
			t.Errorf("ParseHandle(%q) = %v, %v; want %v", in, got, err, want)
		}
	}
	for _, in := range []string{"chat", "chat:", "phone:1", "other:x", "other::v"} {
		if _, err := ParseHandle(in); err == nil {
			t.Errorf("ParseHandle(%q) accepted", in)
		}
	}
}

func TestAddAssignsSlugIDs(t *testing.T) {
	s := newStore(t)
	if id := mustAdd(t, s, Person{Name: "Ada Lovelace"}); id != "ada-lovelace" {
		t.Errorf("id = %q", id)
	}
	if id := mustAdd(t, s, Person{Name: "Ada  Lovelace!"}); id != "ada-lovelace-2" {
		t.Errorf("second id = %q", id)
	}
	if _, err := s.Add(Person{Name: "Bad", TZ: "Mars/Olympus"}); err == nil {
		t.Error("bad timezone accepted")
	}
}

func TestDuplicateHandleRefused(t *testing.T) {
	s := newStore(t)
	mustAdd(t, s, Person{Name: "Ada", Handles: []Handle{mustHandle(t, "chat:ada")}})
	_, err := s.Add(Person{Name: "Other", Handles: []Handle{mustHandle(t, "chat:ADA")}})
	var taken *HandleTakenError
	if !errors.As(err, &taken) || taken.Owner != "ada" {
		t.Fatalf("Add duplicate = %v, want HandleTakenError owned by ada", err)
	}
	if count(t, s, `SELECT count(*) FROM people WHERE id = 'other'`) != 0 {
		t.Error("refused add left a person row")
	}
	bob := mustAdd(t, s, Person{Name: "Bob"})
	if err := s.Edit(bob, Patch{AddHandles: []Handle{mustHandle(t, "chat:ada")}}); !errors.As(err, &taken) {
		t.Errorf("Edit duplicate = %v, want HandleTakenError", err)
	}
}

func TestEdit(t *testing.T) {
	s := newStore(t)
	id := mustAdd(t, s, Person{Name: "Ada", Team: "core", Aliases: []string{"al"}, Handles: []Handle{mustHandle(t, "chat:ada")}})
	role := "lead"
	if err := s.Edit(id, Patch{
		Role:          &role,
		AddAliases:    []string{"countess"},
		RemoveAliases: []string{"al"},
		AddHandles:    []Handle{mustHandle(t, "code:ada-l")},
		RemoveHandles: []Handle{mustHandle(t, "chat:ada")},
	}); err != nil {
		t.Fatal(err)
	}
	p, err := s.Get(id)
	if err != nil {
		t.Fatal(err)
	}
	if p.Team != "core" || p.Role != "lead" || !slices.Equal(p.Aliases, []string{"countess"}) ||
		!slices.Equal(p.Handles, []Handle{{"code", "ada-l"}}) {
		t.Errorf("after edit: %+v", p)
	}
	if err := s.Edit(id, Patch{RemoveHandles: []Handle{mustHandle(t, "chat:nope")}}); err == nil {
		t.Error("removing a missing handle succeeded")
	}
}

func TestMergeMovesEverything(t *testing.T) {
	s := newStore(t)
	keep := mustAdd(t, s, Person{Name: "Ada", Team: "core", Handles: []Handle{mustHandle(t, "chat:ada")}})
	drop := mustAdd(t, s, Person{
		Name: "A. Lovelace", Team: "other", Role: "lead", Aliases: []string{"al"},
		Handles: []Handle{mustHandle(t, "code:ada-l"), mustHandle(t, "email:ada@example.com")},
	})
	for _, q := range []string{
		`INSERT INTO events (person_id, date, channel, link, summary, reporter, created_at) VALUES (?, '2026-01-01', 'chat', 'https://x', 's', 'r', 'now')`,
		`INSERT INTO commitments (person_id, direction, text, reporter, created_at) VALUES (?, 'mine', 't', 'r', 'now')`,
	} {
		if _, err := s.db.Exec(q, drop); err != nil {
			t.Fatal(err)
		}
	}
	if err := os.WriteFile(s.NotesPath(keep), []byte("keep notes\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(s.NotesPath(drop), []byte("drop notes\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := s.Merge(keep, drop); err != nil {
		t.Fatal(err)
	}
	p, err := s.Get(keep)
	if err != nil {
		t.Fatal(err)
	}
	if p.Team != "core" || p.Role != "lead" {
		t.Errorf("fields: team %q role %q; want core, lead", p.Team, p.Role)
	}
	if !slices.Equal(p.Aliases, []string{"al"}) || len(p.Handles) != 3 {
		t.Errorf("aliases %v handles %v", p.Aliases, p.Handles)
	}
	for _, table := range []string{"events", "commitments"} {
		if n := count(t, s, `SELECT count(*) FROM `+table+` WHERE person_id = ?`, keep); n != 1 {
			t.Errorf("%s moved = %d, want 1", table, n)
		}
	}
	if _, err := s.Get(drop); !errors.Is(err, ErrNotFound) {
		t.Errorf("drop still exists: %v", err)
	}
	notes, _ := os.ReadFile(s.NotesPath(keep))
	if want := "keep notes\n\n--- merged from a-lovelace (A. Lovelace) ---\n\ndrop notes\n"; string(notes) != want {
		t.Errorf("notes = %q, want %q", notes, want)
	}
	if _, err := os.Stat(s.NotesPath(drop)); !errors.Is(err, os.ErrNotExist) {
		t.Errorf("drop notes file still there: %v", err)
	}
}

func TestMergeRefusesHandleConflict(t *testing.T) {
	s := newStore(t)
	keep := mustAdd(t, s, Person{Name: "Ada", Handles: []Handle{mustHandle(t, "chat:ada")}})
	drop := mustAdd(t, s, Person{Name: "Ada2", Handles: []Handle{mustHandle(t, "chat:ada2")}})
	err := s.Merge(keep, drop)
	if err == nil || !strings.Contains(err.Error(), "conflict") {
		t.Fatalf("Merge = %v, want conflict", err)
	}
	if _, err := s.Get(drop); err != nil {
		t.Errorf("drop was deleted despite the conflict: %v", err)
	}
	if err := s.Edit(drop, Patch{RemoveHandles: []Handle{mustHandle(t, "chat:ada2")}}); err != nil {
		t.Fatal(err)
	}
	if err := s.Merge(keep, drop); err != nil {
		t.Errorf("Merge after resolving = %v", err)
	}
}

func TestDeleteRemovesRowsAndNotes(t *testing.T) {
	s := newStore(t)
	id := mustAdd(t, s, Person{Name: "Ada", Aliases: []string{"al"}, Handles: []Handle{mustHandle(t, "chat:ada")}})
	if err := os.WriteFile(s.NotesPath(id), []byte("n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := s.Delete(id); err != nil {
		t.Fatal(err)
	}
	for _, table := range []string{"people", "aliases", "handles"} {
		col := "person_id"
		if table == "people" {
			col = "id"
		}
		if n := count(t, s, `SELECT count(*) FROM `+table+` WHERE `+col+` = ?`, id); n != 0 {
			t.Errorf("%s rows left: %d", table, n)
		}
	}
	if _, err := os.Stat(s.NotesPath(id)); !errors.Is(err, os.ErrNotExist) {
		t.Errorf("notes file left: %v", err)
	}
	if err := s.Delete(id); !errors.Is(err, ErrNotFound) {
		t.Errorf("second Delete = %v, want ErrNotFound", err)
	}
}

func TestFind(t *testing.T) {
	s := newStore(t)
	ada := mustAdd(t, s, Person{Name: "Ada Lovelace", Aliases: []string{"Countess"}, Handles: []Handle{mustHandle(t, "chat:ada.l")}})
	mustAdd(t, s, Person{Name: "Sam Lee", Team: "core", Aliases: []string{"shared"}})
	mustAdd(t, s, Person{Name: "Sam Lee", Team: "infra"})
	for _, q := range []string{"ada-lovelace", "ada lovelace", "countess", "@ada.l", "chat:ada.l", "ADA.L"} {
		if p, err := s.Find(q); err != nil || p.ID != ada {
			t.Errorf("Find(%q) = %v, %v", q, p.ID, err)
		}
	}
	var amb *AmbiguousError
	if _, err := s.Find("sam lee"); !errors.As(err, &amb) || len(amb.Candidates) != 2 {
		t.Fatalf("Find(sam lee) = %v, want two candidates", err)
	}
	if !strings.Contains(amb.Error(), "sam-lee-2\tSam Lee\tinfra") {
		t.Errorf("ambiguity message lacks candidates: %s", amb.Error())
	}
	if _, err := s.Find("nobody"); !errors.Is(err, ErrNotFound) {
		t.Errorf("Find(nobody) = %v", err)
	}
}
