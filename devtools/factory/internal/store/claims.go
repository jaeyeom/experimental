package store

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"strconv"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

// ClaimState is the lifecycle state of one local claim row.
type ClaimState string

const (
	// Claimed means the item was claimed and the agent has not started.
	Claimed ClaimState = "claimed"
	// Started means an agent was started for the claim.
	Started ClaimState = "started"
	// HandedOff means the started claim has an open pull request.
	HandedOff ClaimState = "handed_off"
	// Done means the claim's pull request merged.
	Done ClaimState = "done"
	// Released means a person undid the claim.
	Released ClaimState = "released"
	// Failed means starting the agent failed and the claim was left in place.
	Failed ClaimState = "failed"
)

// ClaimPR is one pull request recorded on a claim.
type ClaimPR struct {
	Repo   string `json:"repo"`
	Number int    `json:"number"`
	URL    string `json:"url"`
	Title  string `json:"title"`
	// Head is the pull request head branch. Empty means it was not recorded.
	Head string `json:"head,omitempty"`
}

// Claim is one local claim row. ClaimedAt identifies the attempt.
// Later state changes update the same row.
type Claim struct {
	Source      string
	ID          string
	ClaimedAt   time.Time
	Machine     string
	TabID       string
	WorkspaceID string
	State       ClaimState
	Detail      string
	UpdatedAt   time.Time
	PRs         []ClaimPR
	// StashCount is the number of git stash entries recorded at start.
	// Nil means start did not record it. Zero is a recorded empty stash.
	StashCount *int
	// Lane is the [[lane]] name the claim was started or adopted on.
	// Empty means no lane was configured: the local herdr server.
	Lane string
	// AgentRule is how start chose the agent args: the matched agent_rule's
	// name or first label, "default", or "flag". Empty means start did not
	// record it, as on every claim from before agent rules were recorded.
	AgentRule string
	// Model is the value of --model in the chosen agent args. Empty means the
	// args named no model, or AgentRule is empty and nothing was recorded.
	Model string
	// Spawner is how the worker was started (e.g. "agentbus", "subagent",
	// "herdr"). Empty for historical claims.
	Spawner string
	// WorkerHandle is an optional handle for the worker if distinct from TabID.
	WorkerHandle string
	// Worktree is the absolute path of the dedicated git worktree when started
	// with worktree isolation. Empty when using the shared checkout.
	Worktree string
	// OutputMode is the configured output mode ("pr" or "merge"). Empty for
	// historical claims, which callers treat as "pr".
	OutputMode string
}

// ErrActiveClaim means an active claim already exists for the same source and item.
var ErrActiveClaim = errors.New("active claim already exists")

const claimColumns = `source, id, claimed_at, machine, tab_id, workspace_id, state, detail, updated_at, prs_json, stash_count, lane, agent_rule, model, spawner, worker_handle, worktree, output_mode`

// InsertClaim inserts one claim row.
// An active claim (claimed, started, or handed_off) refuses when another
// active row already exists for the same source and id.
func (s *Store) InsertClaim(ctx context.Context, c Claim) error {
	if c.Source == "" {
		return errors.New("insert claim: expected a non-empty source, found empty")
	}
	if c.ID == "" {
		return errors.New("insert claim: expected a non-empty id, found empty")
	}
	if err := validateClaimState(c.State); err != nil {
		return err
	}
	if c.AgentRule == "" && c.Model != "" {
		return fmt.Errorf("insert claim %s/%s: expected an agent rule with model %q, found empty", c.Source, c.ID, c.Model)
	}
	prs, err := encodeClaimPRs(c.PRs)
	if err != nil {
		return err
	}
	stash, err := stashValue(c.StashCount)
	if err != nil {
		return err
	}
	if !c.State.Active() {
		return s.insertClaimRow(ctx, s.db, c, prs, stash)
	}
	s.claimMu.Lock()
	defer s.claimMu.Unlock()
	return s.withTx(ctx, func(tx *sql.Tx) error {
		existing, ok, err := activeClaimFor(ctx, tx, c.Source, c.ID)
		if err != nil {
			return fmt.Errorf("insert claim %s/%s: %w", c.Source, c.ID, err)
		}
		if ok {
			return fmt.Errorf("insert claim %s/%s: %w (%s on %s at %s, tab %s)",
				c.Source, c.ID, ErrActiveClaim, existing.State, emptyAsNone(existing.Machine),
				existing.ClaimedAt.UTC().Format(time.RFC3339), emptyAsNone(existing.TabID))
		}
		return s.insertClaimRow(ctx, tx, c, prs, stash)
	})
}

func (s *Store) insertClaimRow(ctx context.Context, q queryer, c Claim, prs string, stash any) error {
	_, err := q.ExecContext(ctx, `
		INSERT INTO claims(
			source, id, claimed_at, machine, tab_id, workspace_id, state, detail, updated_at, prs_json, stash_count, lane,
			agent_rule, model, spawner, worker_handle, worktree, output_mode
		) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
		c.Source, c.ID, formatTime(c.ClaimedAt), c.Machine, c.TabID, c.WorkspaceID,
		string(c.State), c.Detail, formatTime(c.UpdatedAt), prs, stash, c.Lane,
		c.AgentRule, c.Model, c.Spawner, c.WorkerHandle, c.Worktree, c.OutputMode,
	)
	if err != nil {
		return fmt.Errorf("insert claim %s/%s: %w", c.Source, c.ID, err)
	}
	return nil
}

func activeClaimFor(ctx context.Context, q queryer, sourceName, id string) (Claim, bool, error) {
	row := q.QueryRowContext(ctx, `
		SELECT `+claimColumns+` FROM claims
		WHERE source = ? AND id = ? AND state IN (?, ?, ?)
		ORDER BY claimed_at DESC
		LIMIT 1`, sourceName, id, string(Claimed), string(Started), string(HandedOff))
	c, err := scanClaim(row)
	if errors.Is(err, sql.ErrNoRows) {
		return Claim{}, false, nil
	}
	if err != nil {
		return Claim{}, false, err
	}
	return c, true, nil
}

// ActiveClaim returns the latest active claim (claimed, started, or handed_off)
// for sourceName and id, or false when none exists.
func (s *Store) ActiveClaim(ctx context.Context, sourceName, id string) (Claim, bool, error) {
	if sourceName == "" {
		return Claim{}, false, errors.New("active claim: expected a non-empty source, found empty")
	}
	if id == "" {
		return Claim{}, false, errors.New("active claim: expected a non-empty id, found empty")
	}
	c, ok, err := activeClaimFor(ctx, s.db, sourceName, id)
	if err != nil {
		return Claim{}, false, fmt.Errorf("active claim %s/%s: %w", sourceName, id, err)
	}
	return c, ok, nil
}

func emptyAsNone(v string) string {
	if v == "" {
		return "none"
	}
	return v
}

// UpdateClaim updates the claim identified by source, id, and claimedAt.
// It sets state, detail, the tab id, the workspace id, and updated_at.
// A missing row returns source.ErrNotFound. Moving a claim to done also
// records the item in the finished table, with this claim's claimed_at.
func (s *Store) UpdateClaim(ctx context.Context, sourceName, id string, claimedAt time.Time, state ClaimState, detail, tabID, wsID string) error {
	if sourceName == "" {
		return errors.New("update claim: expected a non-empty source, found empty")
	}
	if id == "" {
		return errors.New("update claim: expected a non-empty id, found empty")
	}
	if err := validateClaimState(state); err != nil {
		return err
	}
	now := time.Now()
	return s.withTx(ctx, func(tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `
			UPDATE claims
			SET state = ?, detail = ?, tab_id = ?, workspace_id = ?, updated_at = ?
			WHERE source = ? AND id = ? AND claimed_at = ?`,
			string(state), detail, tabID, wsID, formatTime(now),
			sourceName, id, formatTime(claimedAt),
		)
		if err != nil {
			return fmt.Errorf("update claim %s/%s: %w", sourceName, id, err)
		}
		n, err := res.RowsAffected()
		if err != nil {
			return fmt.Errorf("update claim %s/%s: %w", sourceName, id, err)
		}
		if n == 0 {
			return fmt.Errorf("update claim %s/%s at %s: %w", sourceName, id, formatTime(claimedAt), source.ErrNotFound)
		}
		if state != Done {
			return nil
		}
		return insertFinished(ctx, tx, sourceName, id, now, formatTime(claimedAt))
	})
}

// FillClaim sets the tab, workspace, and pull requests on an existing claim.
// It does not change the claim state. A missing row returns source.ErrNotFound.
// updatedAt is stored as given. The zero time is an error.
func (s *Store) FillClaim(ctx context.Context, sourceName, id string, claimedAt time.Time, tabID, wsID string, prs []ClaimPR, updatedAt time.Time) error {
	if sourceName == "" {
		return errors.New("fill claim: expected a non-empty source, found empty")
	}
	if id == "" {
		return errors.New("fill claim: expected a non-empty id, found empty")
	}
	if updatedAt.IsZero() {
		return errors.New("fill claim: expected updated_at, found zero")
	}
	encoded, err := encodeClaimPRs(prs)
	if err != nil {
		return err
	}
	res, err := s.db.ExecContext(ctx, `
		UPDATE claims
		SET tab_id = ?, workspace_id = ?, prs_json = ?, updated_at = ?
		WHERE source = ? AND id = ? AND claimed_at = ?`,
		tabID, wsID, encoded, formatTime(updatedAt),
		sourceName, id, formatTime(claimedAt),
	)
	if err != nil {
		return fmt.Errorf("fill claim %s/%s: %w", sourceName, id, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return fmt.Errorf("fill claim %s/%s: %w", sourceName, id, err)
	}
	if n == 0 {
		return fmt.Errorf("fill claim %s/%s at %s: %w", sourceName, id, formatTime(claimedAt), source.ErrNotFound)
	}
	return nil
}

// UpdateClaimWorktree sets worktree and updated_at on the claim identified by
// sourceName, id, and claimedAt. A missing row returns source.ErrNotFound.
func (s *Store) UpdateClaimWorktree(ctx context.Context, sourceName, id string, claimedAt time.Time, worktree string) error {
	const query = `UPDATE claims SET worktree = ?, updated_at = ? WHERE source = ? AND id = ? AND claimed_at = ?`
	return s.updateClaimField(ctx, "update claim worktree", query, sourceName, id, claimedAt, worktree)
}

// UpdateClaimWorkerHandle sets worker_handle and updated_at on the claim
// identified by sourceName, id, and claimedAt. A missing row returns source.ErrNotFound.
func (s *Store) UpdateClaimWorkerHandle(ctx context.Context, sourceName, id string, claimedAt time.Time, workerHandle string) error {
	const query = `UPDATE claims SET worker_handle = ?, updated_at = ? WHERE source = ? AND id = ? AND claimed_at = ?`
	return s.updateClaimField(ctx, "update claim worker_handle", query, sourceName, id, claimedAt, workerHandle)
}

func (s *Store) updateClaimField(ctx context.Context, op, query, sourceName, id string, claimedAt time.Time, value string) error {
	if sourceName == "" {
		return fmt.Errorf("%s: expected a non-empty source, found empty", op)
	}
	if id == "" {
		return fmt.Errorf("%s: expected a non-empty id, found empty", op)
	}
	res, err := s.db.ExecContext(ctx, query, value, formatTime(time.Now()), sourceName, id, formatTime(claimedAt))
	if err != nil {
		return fmt.Errorf("%s %s/%s: %w", op, sourceName, id, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return fmt.Errorf("%s %s/%s: %w", op, sourceName, id, err)
	}
	if n == 0 {
		return fmt.Errorf("%s %s/%s at %s: %w", op, sourceName, id, formatTime(claimedAt), source.ErrNotFound)
	}
	return nil
}

// ActiveClaims returns claims in the claimed, started, or handed_off state,
// ordered by claimed_at, source, and ID.
func (s *Store) ActiveClaims(ctx context.Context) ([]Claim, error) {
	claims, err := queryClaims(ctx, s.db, `
		SELECT `+claimColumns+` FROM claims
		WHERE state IN (?, ?, ?)
		ORDER BY claimed_at, source, id`,
		string(Claimed), string(Started), string(HandedOff),
	)
	if err != nil {
		return nil, fmt.Errorf("active claims: %w", err)
	}
	return claims, nil
}

// LatestClaim returns the claim with the greatest claimed_at for source and id.
// A missing claim returns source.ErrNotFound.
func (s *Store) LatestClaim(ctx context.Context, sourceName, id string) (Claim, error) {
	if sourceName == "" {
		return Claim{}, errors.New("latest claim: expected a non-empty source, found empty")
	}
	if id == "" {
		return Claim{}, errors.New("latest claim: expected a non-empty id, found empty")
	}
	row := s.db.QueryRowContext(ctx, `
		SELECT `+claimColumns+` FROM claims
		WHERE source = ? AND id = ?
		ORDER BY claimed_at DESC
		LIMIT 1`, sourceName, id)
	c, err := scanClaim(row)
	if errors.Is(err, sql.ErrNoRows) {
		return Claim{}, fmt.Errorf("claim %s/%s: %w", sourceName, id, source.ErrNotFound)
	}
	if err != nil {
		return Claim{}, fmt.Errorf("claim %s/%s: %w", sourceName, id, err)
	}
	return c, nil
}

// Claims returns every claim row, ordered by claimed_at, source, and ID.
func (s *Store) Claims(ctx context.Context) ([]Claim, error) {
	claims, err := queryClaims(ctx, s.db, `SELECT `+claimColumns+` FROM claims ORDER BY claimed_at, source, id`)
	if err != nil {
		return nil, fmt.Errorf("claims: %w", err)
	}
	return claims, nil
}

// LastStartedAt returns the latest claimed_at among claims that were not
// released. claimed_at is the start-attempt time and does not move when the
// row later changes state. No such claim returns the zero time and a nil error.
func (s *Store) LastStartedAt(ctx context.Context) (time.Time, error) {
	var raw sql.NullString
	err := s.db.QueryRowContext(ctx, `
		SELECT MAX(claimed_at) FROM claims WHERE state <> ?`, string(Released)).Scan(&raw)
	if err != nil {
		return time.Time{}, fmt.Errorf("last started at: %w", err)
	}
	if !raw.Valid || raw.String == "" {
		return time.Time{}, nil
	}
	started, err := parseTime("claims.claimed_at", raw.String)
	if err != nil {
		return time.Time{}, fmt.Errorf("last started at: %w", err)
	}
	return started, nil
}

// Active reports whether the claim still counts as in-flight work.
func (s ClaimState) Active() bool {
	switch s {
	case Claimed, Started, HandedOff:
		return true
	default:
		return false
	}
}

// MergeClaimPRs appends extra pull requests that are not already recorded.
// A pull request already stored by repo and number keeps a non-empty URL,
// title, or head from extra.
func MergeClaimPRs(have, extra []ClaimPR) []ClaimPR {
	out := append([]ClaimPR{}, have...)
	for _, pr := range extra {
		found := false
		for i := range out {
			if out[i].Repo != pr.Repo || out[i].Number != pr.Number {
				continue
			}
			out[i] = fillClaimPR(out[i], pr)
			found = true
			break
		}
		if !found {
			out = append(out, pr)
		}
	}
	return out
}

// SameClaimPRs reports whether the two lists are identical, including order.
func SameClaimPRs(a, b []ClaimPR) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func fillClaimPR(dst, src ClaimPR) ClaimPR {
	if src.URL != "" {
		dst.URL = src.URL
	}
	if src.Title != "" {
		dst.Title = src.Title
	}
	if src.Head != "" {
		dst.Head = src.Head
	}
	return dst
}

func validateClaimState(state ClaimState) error {
	switch state {
	case Claimed, Started, HandedOff, Done, Released, Failed:
		return nil
	default:
		return fmt.Errorf("claim state: expected claimed, started, handed_off, done, released, or failed, found %q", state)
	}
}

func queryClaims(ctx context.Context, q queryer, query string, args ...any) (out []Claim, err error) {
	rows, err := q.QueryContext(ctx, query, args...)
	if err != nil {
		return nil, err //nolint:wrapcheck // propagate context cancellation
	}
	defer func() { err = closeRows(rows, err) }()
	for rows.Next() {
		c, err := scanClaim(rows)
		if err != nil {
			return nil, err
		}
		out = append(out, c)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate claims: %w", err)
	}
	return out, nil
}

func scanClaim(sc scanner) (Claim, error) {
	var (
		c                    Claim
		state                string
		claimedAt, updatedAt string
		prs                  string
		stash                sql.NullInt64
	)
	if err := sc.Scan(
		&c.Source, &c.ID, &claimedAt, &c.Machine, &c.TabID, &c.WorkspaceID,
		&state, &c.Detail, &updatedAt, &prs, &stash, &c.Lane, &c.AgentRule, &c.Model,
		&c.Spawner, &c.WorkerHandle, &c.Worktree, &c.OutputMode,
	); err != nil {
		return Claim{}, err //nolint:wrapcheck // internal factory error already carries context
	}
	counted, err := stashCount(stash)
	if err != nil {
		return Claim{}, fmt.Errorf("claim %s/%s: %w", c.Source, c.ID, err)
	}
	c.StashCount = counted
	c.PRs, err = decodeClaimPRs(prs)
	if err != nil {
		return Claim{}, fmt.Errorf("claim %s/%s: %w", c.Source, c.ID, err)
	}
	c.ClaimedAt, err = parseTime("claims.claimed_at", claimedAt)
	if err != nil {
		return Claim{}, fmt.Errorf("claim %s/%s: %w", c.Source, c.ID, err)
	}
	c.UpdatedAt, err = parseTime("claims.updated_at", updatedAt)
	if err != nil {
		return Claim{}, fmt.Errorf("claim %s/%s: %w", c.Source, c.ID, err)
	}
	c.State = ClaimState(state)
	if err := validateClaimState(c.State); err != nil {
		return Claim{}, fmt.Errorf("claim %s/%s: %w", c.Source, c.ID, err)
	}
	return c, nil
}

func encodeClaimPRs(prs []ClaimPR) (string, error) {
	if len(prs) == 0 {
		return "", nil
	}
	for i, pr := range prs {
		if err := validateClaimPR(pr); err != nil {
			return "", fmt.Errorf("claim prs[%d]: %w", i, err)
		}
	}
	raw, err := json.Marshal(prs)
	if err != nil {
		return "", fmt.Errorf("claim prs: %w", err)
	}
	return string(raw), nil
}

func decodeClaimPRs(raw string) ([]ClaimPR, error) {
	if raw == "" || raw == "null" {
		return nil, nil
	}
	var prs []ClaimPR
	if err := json.Unmarshal([]byte(raw), &prs); err != nil {
		return nil, fmt.Errorf("prs_json: expected a JSON array, found %q", raw)
	}
	for i, pr := range prs {
		if err := validateClaimPR(pr); err != nil {
			return nil, fmt.Errorf("prs_json[%d]: %w", i, err)
		}
	}
	return prs, nil
}

func stashValue(n *int) (any, error) {
	if n == nil {
		return nil, nil
	}
	if *n < 0 {
		return nil, fmt.Errorf("insert claim: expected a non-negative stash count, found %d", *n)
	}
	return *n, nil
}

func stashCount(raw sql.NullInt64) (*int, error) {
	if !raw.Valid {
		return nil, nil
	}
	if raw.Int64 < 0 {
		return nil, fmt.Errorf("stash_count: expected a non-negative count, found %d", raw.Int64)
	}
	n := int(raw.Int64)
	return &n, nil
}

func validateClaimPR(pr ClaimPR) error {
	id := pr.Repo + "#" + strconv.Itoa(pr.Number)
	if _, _, err := source.ParseGitHubID(id); err != nil {
		return fmt.Errorf("expected owner/repo#N, found %q", id)
	}
	if pr.URL == "" {
		return fmt.Errorf("expected a pull request URL for %s, found empty", id)
	}
	return nil
}
