package store

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strconv"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

// PR is one observed pull request in the prs table.
// Zero times are unknown. A later observation never clears or moves back a
// time that is already set.
type PR struct {
	Repo     string // owner/repo.
	Number   int
	Source   string // Item source key. The first non-empty value is kept.
	ItemID   string // Item ID. The first non-empty value is kept.
	OpenedAt time.Time
	MergedAt time.Time
	ClosedAt time.Time
	Draft    bool      // Draft as of LastSeen.
	LastSeen time.Time // Latest observation.
}

const prColumns = `repo, number, source, item_id, opened_at, merged_at, closed_at, draft, last_seen`

// UpsertPRs writes observations by repo and number in one transaction.
// opened_at keeps the earliest value. merged_at and closed_at keep the first
// value seen. draft follows the newest observation, and last_seen only moves
// forward.
func (s *Store) UpsertPRs(ctx context.Context, prs []PR) error {
	for i := range prs {
		if err := validatePR(prs[i]); err != nil {
			return err
		}
	}
	return s.withTx(ctx, func(tx *sql.Tx) error {
		for _, pr := range prs {
			if err := upsertPR(ctx, tx, pr); err != nil {
				return err
			}
		}
		return nil
	})
}

func validatePR(pr PR) error {
	id := pr.Repo + "#" + strconv.Itoa(pr.Number)
	if _, _, err := source.ParseGitHubID(id); err != nil {
		return fmt.Errorf("upsert pr: expected owner/repo#N, found %q", id)
	}
	if pr.LastSeen.IsZero() {
		return fmt.Errorf("upsert pr %s: expected last_seen, found zero", id)
	}
	if pr.OpenedAt.IsZero() {
		return fmt.Errorf("upsert pr %s: expected opened_at, found zero", id)
	}
	if (pr.Source == "") != (pr.ItemID == "") {
		return fmt.Errorf("upsert pr %s: expected both item source and id or neither, found source %q id %q", id, pr.Source, pr.ItemID)
	}
	return nil
}

func upsertPR(ctx context.Context, tx *sql.Tx, pr PR) error {
	draft := 0
	if pr.Draft {
		draft = 1
	}
	_, err := tx.ExecContext(ctx, `
		INSERT INTO prs(`+prColumns+`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
		ON CONFLICT(repo, number) DO UPDATE SET
			source = CASE WHEN prs.source <> '' THEN prs.source ELSE excluded.source END,
			item_id = CASE WHEN prs.item_id <> '' THEN prs.item_id ELSE excluded.item_id END,
			opened_at = CASE
				WHEN prs.opened_at IS NULL THEN excluded.opened_at
				WHEN excluded.opened_at IS NULL THEN prs.opened_at
				ELSE MIN(prs.opened_at, excluded.opened_at) END,
			merged_at = COALESCE(prs.merged_at, excluded.merged_at),
			closed_at = COALESCE(prs.closed_at, excluded.closed_at),
			draft = CASE WHEN excluded.last_seen >= prs.last_seen THEN excluded.draft ELSE prs.draft END,
			last_seen = MAX(prs.last_seen, excluded.last_seen)`,
		pr.Repo, pr.Number, pr.Source, pr.ItemID, nullTime(pr.OpenedAt), nullTime(pr.MergedAt),
		nullTime(pr.ClosedAt), draft, formatTime(pr.LastSeen),
	)
	if err != nil {
		return fmt.Errorf("upsert pr %s#%d: %w", pr.Repo, pr.Number, err)
	}
	return nil
}

// PRs returns every prs row, ordered by repo and number.
func (s *Store) PRs(ctx context.Context) (out []PR, err error) {
	rows, err := s.db.QueryContext(ctx, `SELECT `+prColumns+` FROM prs ORDER BY repo, number`)
	if err != nil {
		return nil, fmt.Errorf("prs: %w", err)
	}
	defer func() { err = closeRows(rows, err) }()
	for rows.Next() {
		pr, err := scanPR(rows)
		if err != nil {
			return nil, err
		}
		out = append(out, pr)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("prs: %w", err)
	}
	return out, nil
}

// PR reads one prs row. A missing row returns source.ErrNotFound.
func (s *Store) PR(ctx context.Context, repo string, number int) (PR, error) {
	row := s.db.QueryRowContext(ctx, `SELECT `+prColumns+` FROM prs WHERE repo = ? AND number = ?`, repo, number)
	pr, err := scanPR(row)
	if errors.Is(err, sql.ErrNoRows) {
		return PR{}, fmt.Errorf("pr %s#%d: %w", repo, number, source.ErrNotFound)
	}
	return pr, err
}

// OpenPRItems returns the item IDs of sourceName that have a pull request
// with neither merged_at nor closed_at.
func (s *Store) OpenPRItems(ctx context.Context, sourceName string) (out map[string]bool, err error) {
	if sourceName == "" {
		return nil, errors.New("open pr items: expected a non-empty source, found empty")
	}
	rows, err := s.db.QueryContext(ctx, `
		SELECT DISTINCT item_id FROM prs
		WHERE source = ? AND item_id <> '' AND merged_at IS NULL AND closed_at IS NULL`, sourceName)
	if err != nil {
		return nil, fmt.Errorf("open pr items %s: %w", sourceName, err)
	}
	defer func() { err = closeRows(rows, err) }()
	out = map[string]bool{}
	for rows.Next() {
		var id string
		if err := rows.Scan(&id); err != nil {
			return nil, fmt.Errorf("open pr items %s: %w", sourceName, err)
		}
		out[id] = true
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("open pr items %s: %w", sourceName, err)
	}
	return out, nil
}

func scanPR(sc scanner) (PR, error) {
	var (
		pr                     PR
		opened, merged, closed sql.NullString
		draft                  int
		lastSeen               string
	)
	if err := sc.Scan(&pr.Repo, &pr.Number, &pr.Source, &pr.ItemID, &opened, &merged, &closed, &draft, &lastSeen); err != nil {
		return PR{}, err //nolint:wrapcheck // internal factory error already carries context
	}
	switch draft {
	case 0:
	case 1:
		pr.Draft = true
	default:
		return PR{}, fmt.Errorf("prs.draft for %s#%d: expected 0 or 1, found %d", pr.Repo, pr.Number, draft)
	}
	var err error
	for _, f := range []struct {
		column string
		raw    sql.NullString
		dst    *time.Time
	}{
		{"prs.opened_at", opened, &pr.OpenedAt},
		{"prs.merged_at", merged, &pr.MergedAt},
		{"prs.closed_at", closed, &pr.ClosedAt},
		{"prs.last_seen", sql.NullString{String: lastSeen, Valid: true}, &pr.LastSeen},
	} {
		*f.dst, err = parseTime(f.column, f.raw.String)
		if err != nil {
			return PR{}, fmt.Errorf("pr %s#%d: %w", pr.Repo, pr.Number, err)
		}
	}
	return pr, nil
}

// nullTime stores a zero time as NULL.
func nullTime(t time.Time) any {
	if t.IsZero() {
		return nil
	}
	return formatTime(t)
}
