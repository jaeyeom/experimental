package slackwatch

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"
)

// NamedUser is one Slack user as users.list returns it.
// Display is the profile display name. It is empty when unset.
type NamedUser struct {
	ID      string
	Name    string
	Display string
}

// UserPage is one page of users.list.
type UserPage struct {
	Users []NamedUser
	Next  string
}

// UserLister lists users one page at a time. An empty cursor is the first
// page. A rate-limited page returns a *RateLimitError.
type UserLister interface {
	ListUsers(ctx context.Context, cursor string) (UserPage, error)
}

// UserLookup resolves a user name to one id. It reads and writes the user
// cache in Store. List is not called on a fresh cache hit. A name that
// matches more than one user is refused with those users listed.
type UserLookup struct {
	Store *Store
	List  UserLister
	Now   func() time.Time
	// TTL is how long a cached id is trusted. Zero trusts nothing.
	TTL time.Duration
	// MaxWait bounds the total RetryAfter slept during one lookup.
	// Zero fails on the first rate limit.
	MaxWait time.Duration
	// Sleep waits out a rate limit. Nil uses Wait.
	Sleep func(ctx context.Context, d time.Duration) error
	// OnWait is called with RetryAfter before each sleep. Nil is ignored.
	OnWait func(d time.Duration)
}

type userHit struct {
	id    string
	label string
}

// UserID returns the id for name. name is a handle or a display name,
// without the leading @. A cached unique id younger than TTL makes no
// List call.
func (l UserLookup) UserID(ctx context.Context, name string) (string, error) {
	key := strings.ToLower(strings.TrimSpace(name))
	if key == "" {
		return "", errors.New("user name is empty")
	}
	if id, ok, err := l.cached(key); err != nil || ok {
		return id, err
	}
	return l.resolve(ctx, key)
}

func (l UserLookup) cached(name string) (string, bool, error) {
	if l.Store == nil || l.TTL <= 0 {
		return "", false, nil
	}
	id, seen, ok, err := l.Store.UserByName(name)
	if err != nil || !ok {
		return "", false, err
	}
	now := l.now()
	if now.Before(seen) || now.Sub(seen) > l.TTL {
		return "", false, nil
	}
	return id, true, nil
}

func (l UserLookup) resolve(ctx context.Context, name string) (string, error) {
	found := map[string][]userHit{}
	var cursor string
	var waited time.Duration
	for {
		page, err := l.List.ListUsers(ctx, cursor)
		if err != nil {
			waited, err = l.limitWait(ctx, name, err, waited)
			if err != nil {
				return "", err
			}
			continue
		}
		addUserPage(found, page)
		if page.Next == "" {
			break
		}
		cursor = page.Next
	}
	now := l.now()
	for key, hits := range found {
		if len(hits) != 1 || l.Store == nil {
			continue
		}
		if err := l.Store.PutUser(key, hits[0].id, now); err != nil {
			return "", err
		}
	}
	hits := found[name]
	switch len(hits) {
	case 0:
		return "", fmt.Errorf("could not resolve @%s (not found)", name)
	case 1:
		return hits[0].id, nil
	default:
		return "", ambiguousUser(name, hits)
	}
}

func addUserPage(found map[string][]userHit, page UserPage) {
	for _, u := range page.Users {
		if u.ID == "" {
			continue
		}
		label := u.Display
		if label == "" {
			label = u.Name
		}
		addUserHit(found, u.Name, u.ID, label)
		if u.Display != "" && !strings.EqualFold(u.Display, u.Name) {
			addUserHit(found, u.Display, u.ID, label)
		}
	}
}

func addUserHit(found map[string][]userHit, key, id, label string) {
	key = strings.ToLower(strings.TrimSpace(key))
	if key == "" || id == "" {
		return
	}
	for _, hit := range found[key] {
		if hit.id == id {
			return
		}
	}
	found[key] = append(found[key], userHit{id: id, label: label})
}

func ambiguousUser(name string, hits []userHit) error {
	slices.SortFunc(hits, func(a, b userHit) int { return strings.Compare(a.id, b.id) })
	parts := make([]string, len(hits))
	for i, hit := range hits {
		parts[i] = hit.id + " (" + hit.label + ")"
	}
	return fmt.Errorf("ambiguous name %q: %s", name, strings.Join(parts, ", "))
}

func (l UserLookup) limitWait(ctx context.Context, name string, err error, waited time.Duration) (time.Duration, error) {
	var rl *RateLimitError
	if !errors.As(err, &rl) {
		return waited, fmt.Errorf("could not resolve @%s (%s)", name, err.Error())
	}
	if rl.RetryAfter <= 0 || waited+rl.RetryAfter > l.MaxWait {
		return waited, fmt.Errorf("could not resolve @%s (rate limit wait exceeded)", name)
	}
	if l.OnWait != nil {
		l.OnWait(rl.RetryAfter)
	}
	if err := l.sleep(ctx, rl.RetryAfter); err != nil {
		return waited, err
	}
	return waited + rl.RetryAfter, nil
}

func (l UserLookup) now() time.Time {
	if l.Now != nil {
		return l.Now()
	}
	return time.Now()
}

func (l UserLookup) sleep(ctx context.Context, d time.Duration) error {
	if l.Sleep != nil {
		return l.Sleep(ctx, d)
	}
	return Wait(ctx, d)
}

// PutUser stores the lowercase name → id as seen at seen.
func (s *Store) PutUser(name, id string, seen time.Time) error {
	name = strings.ToLower(strings.TrimSpace(name))
	if name == "" || id == "" {
		return errors.New("user cache needs a name and an id")
	}
	_, err := s.db.Exec(`
INSERT INTO users (name, id, seen_at) VALUES (?, ?, ?)
ON CONFLICT(name) DO UPDATE SET id = excluded.id, seen_at = excluded.seen_at`,
		name, id, seen.Unix())
	if err != nil {
		return fmt.Errorf("store user @%s: %w", name, err)
	}
	return nil
}

// UserByName returns the cached id and the time it was seen.
func (s *Store) UserByName(name string) (id string, seen time.Time, ok bool, err error) {
	name = strings.ToLower(strings.TrimSpace(name))
	var sec int64
	err = s.db.QueryRow(`SELECT id, seen_at FROM users WHERE name = ?`, name).Scan(&id, &sec)
	if errors.Is(err, sql.ErrNoRows) {
		return "", time.Time{}, false, nil
	}
	if err != nil {
		return "", time.Time{}, false, fmt.Errorf("read user @%s: %w", name, err)
	}
	return id, time.Unix(sec, 0), true, nil
}
