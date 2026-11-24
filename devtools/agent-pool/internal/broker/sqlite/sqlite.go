// Package sqlite implements broker.Store backed by a pure-Go SQLite WAL
// database using devtools/pkg/sqlitedb.
package sqlite

import (
	"context"
	"crypto/rand"
	"crypto/sha256"
	"crypto/subtle"
	"database/sql"
	_ "embed"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker"
	"github.com/jaeyeom/experimental/devtools/pkg/sqlitedb"
)

//go:embed schema.sql
var schemaSQL string

const (
	tokenByteLen  = 32
	saltByteLen   = 16
	idByteLen     = 16
	prefixProd    = "ap_prod_"
	prefixWorker  = "ap_work_"
	emptyJSONList = "[]"
	emptyJSONObj  = "{}"
)

var _ broker.Store = (*Store)(nil)

// Store is a SQLite-backed implementation of broker.Store.
type Store struct {
	db *sql.DB
}

// Open opens or initializes the SQLite broker database at path.
func Open(path string) (*Store, error) {
	db, err := sqlitedb.Open(path, "_pragma=foreign_keys(1)", "_txlock=immediate")
	if err != nil {
		return nil, fmt.Errorf("open broker sqlite store: %w", err)
	}
	db.SetMaxOpenConns(1)

	if _, err := db.Exec(schemaSQL); err != nil {
		_ = db.Close()
		return nil, fmt.Errorf("apply broker schema: %w", err)
	}
	return &Store{db: db}, nil
}

// Close closes the underlying SQLite database.
func (s *Store) Close() error {
	if err := s.db.Close(); err != nil {
		return fmt.Errorf("close broker sqlite store: %w", err)
	}
	return nil
}

// CreateToken generates a new random API token, persists its salted SHA-256
// hash, and returns the one-time plaintext token alongside its record.
func (s *Store) CreateToken(ctx context.Context, role broker.TokenRole, name string, allowedRepos []string) (string, broker.TokenRecord, error) {
	prefix, err := tokenPrefix(role)
	if err != nil {
		return "", broker.TokenRecord{}, err
	}

	rawToken, err := randomBytes(tokenByteLen)
	if err != nil {
		return "", broker.TokenRecord{}, fmt.Errorf("generate token bytes: %w", err)
	}
	salt, err := randomBytes(saltByteLen)
	if err != nil {
		return "", broker.TokenRecord{}, fmt.Errorf("generate token salt: %w", err)
	}
	id, err := randomHexID()
	if err != nil {
		return "", broker.TokenRecord{}, fmt.Errorf("generate token id: %w", err)
	}

	plaintext := prefix + base64.RawURLEncoding.EncodeToString(rawToken)
	hash := hashToken(salt, plaintext)

	reposJSON, err := marshalStringSlice(allowedRepos)
	if err != nil {
		return "", broker.TokenRecord{}, fmt.Errorf("marshal allowed repos: %w", err)
	}

	now := time.Now().UTC()
	const query = `
		INSERT INTO tokens (id, role, name, allowed_repos_json, salt, token_hash, created_at)
		VALUES (?, ?, ?, ?, ?, ?, ?)`
	if _, err := s.db.ExecContext(ctx, query, id, string(role), name, reposJSON, salt, hash[:], now.UnixNano()); err != nil {
		return "", broker.TokenRecord{}, fmt.Errorf("insert token: %w", err)
	}

	rec := broker.TokenRecord{
		ID:           id,
		Role:         role,
		Name:         name,
		AllowedRepos: cloneStrings(allowedRepos),
		CreatedAt:    now,
	}
	return plaintext, rec, nil
}

// AuthenticateToken verifies plaintext against stored salted SHA-256 hashes
// using constant-time comparison and rejects revoked or unknown tokens.
func (s *Store) AuthenticateToken(ctx context.Context, plaintext string) (broker.TokenRecord, error) {
	if plaintext == "" {
		return broker.TokenRecord{}, fmt.Errorf("empty token: %w", broker.ErrUnauthorized)
	}

	const query = `
		SELECT id, role, name, allowed_repos_json, salt, token_hash, created_at
		FROM tokens
		WHERE revoked_at IS NULL`
	rows, err := s.db.QueryContext(ctx, query)
	if err != nil {
		return broker.TokenRecord{}, fmt.Errorf("query active tokens: %w", err)
	}
	defer rows.Close()

	for rows.Next() {
		rec, salt, storedHash, err := scanAuthTokenRow(rows)
		if err != nil {
			return broker.TokenRecord{}, err
		}
		computed := hashToken(salt, plaintext)
		if subtle.ConstantTimeCompare(computed[:], storedHash) == 1 {
			return rec, nil
		}
	}
	if err := rows.Err(); err != nil {
		return broker.TokenRecord{}, fmt.Errorf("iterate active tokens: %w", err)
	}
	return broker.TokenRecord{}, fmt.Errorf("token not recognized: %w", broker.ErrUnauthorized)
}

// RevokeToken marks the token identified by id as revoked.
func (s *Store) RevokeToken(ctx context.Context, id string) error {
	now := time.Now().UTC().UnixNano()
	const query = `UPDATE tokens SET revoked_at = COALESCE(revoked_at, ?) WHERE id = ?`
	res, err := s.db.ExecContext(ctx, query, now, id)
	if err != nil {
		return fmt.Errorf("revoke token %s: %w", id, err)
	}
	affected, err := res.RowsAffected()
	if err != nil {
		return fmt.Errorf("revoke token %s rows affected: %w", id, err)
	}
	if affected == 0 {
		return fmt.Errorf("token %s: %w", id, broker.ErrNotFound)
	}
	return nil
}

// ListTokens returns all stored token metadata ordered by creation time.
func (s *Store) ListTokens(ctx context.Context) ([]broker.TokenRecord, error) {
	const query = `
		SELECT id, role, name, allowed_repos_json, created_at, revoked_at
		FROM tokens
		ORDER BY created_at ASC, id ASC`
	rows, err := s.db.QueryContext(ctx, query)
	if err != nil {
		return nil, fmt.Errorf("list tokens: %w", err)
	}
	defer rows.Close()

	var tokens []broker.TokenRecord
	for rows.Next() {
		rec, err := scanTokenListRow(rows)
		if err != nil {
			return nil, err
		}
		tokens = append(tokens, rec)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate tokens: %w", err)
	}
	return tokens, nil
}

// UpsertCatalog inserts or updates a repository's synced skill catalog.
func (s *Store) UpsertCatalog(ctx context.Context, rec broker.CatalogRecord) error {
	updatedAt := rec.UpdatedAt
	if updatedAt.IsZero() {
		updatedAt = time.Now().UTC()
	}
	const query = `
		INSERT INTO catalogs (repo, default_branch, catalog_sha, catalog_json, updated_at)
		VALUES (?, ?, ?, ?, ?)
		ON CONFLICT(repo) DO UPDATE SET
			default_branch = excluded.default_branch,
			catalog_sha = excluded.catalog_sha,
			catalog_json = excluded.catalog_json,
			updated_at = excluded.updated_at`
	if _, err := s.db.ExecContext(ctx, query, rec.Repo, rec.DefaultBranch, rec.CatalogSha, rec.CatalogJSON, updatedAt.UTC().UnixNano()); err != nil {
		return fmt.Errorf("upsert catalog %s: %w", rec.Repo, err)
	}
	return nil
}

// GetCatalog fetches the stored catalog for repo.
func (s *Store) GetCatalog(ctx context.Context, repo string) (broker.CatalogRecord, error) {
	const query = `
		SELECT repo, default_branch, catalog_sha, catalog_json, updated_at
		FROM catalogs
		WHERE repo = ?`
	var (
		rec       broker.CatalogRecord
		updatedNs int64
	)
	err := s.db.QueryRowContext(ctx, query, repo).Scan(
		&rec.Repo, &rec.DefaultBranch, &rec.CatalogSha, &rec.CatalogJSON, &updatedNs,
	)
	if errors.Is(err, sql.ErrNoRows) {
		return broker.CatalogRecord{}, fmt.Errorf("catalog %s: %w", repo, broker.ErrNotFound)
	}
	if err != nil {
		return broker.CatalogRecord{}, fmt.Errorf("get catalog %s: %w", repo, err)
	}
	rec.UpdatedAt = time.Unix(0, updatedNs).UTC()
	return rec, nil
}

// ListCatalogs returns all stored repository catalogs sorted by repo name.
func (s *Store) ListCatalogs(ctx context.Context) ([]broker.CatalogRecord, error) {
	const query = `
		SELECT repo, default_branch, catalog_sha, catalog_json, updated_at
		FROM catalogs
		ORDER BY repo ASC`
	rows, err := s.db.QueryContext(ctx, query)
	if err != nil {
		return nil, fmt.Errorf("list catalogs: %w", err)
	}
	defer rows.Close()

	var catalogs []broker.CatalogRecord
	for rows.Next() {
		var (
			rec       broker.CatalogRecord
			updatedNs int64
		)
		if err := rows.Scan(&rec.Repo, &rec.DefaultBranch, &rec.CatalogSha, &rec.CatalogJSON, &updatedNs); err != nil {
			return nil, fmt.Errorf("scan catalog row: %w", err)
		}
		rec.UpdatedAt = time.Unix(0, updatedNs).UTC()
		catalogs = append(catalogs, rec)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate catalogs: %w", err)
	}
	return catalogs, nil
}

// UpsertWorkerHeartbeat records or refreshes a worker's heartbeat state.
func (s *Store) UpsertWorkerHeartbeat(ctx context.Context, rec broker.WorkerRecord) error {
	reposJSON, err := marshalStringSlice(rec.SupportedRepos)
	if err != nil {
		return fmt.Errorf("marshal worker repos: %w", err)
	}
	skillsJSON, err := marshalSkillsMap(rec.SupportedSkills)
	if err != nil {
		return fmt.Errorf("marshal worker skills: %w", err)
	}
	quotaJSON, err := json.Marshal(rec.Quota)
	if err != nil {
		return fmt.Errorf("marshal worker quota: %w", err)
	}

	hb := rec.LastHeartbeat
	if hb.IsZero() {
		hb = time.Now().UTC()
	}

	const query = `
		INSERT INTO workers (
			worker_id, alias, supported_repos_json, supported_skills_json,
			accepting_jobs, concurrency_available, quota_json, last_heartbeat
		) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
		ON CONFLICT(worker_id) DO UPDATE SET
			alias = excluded.alias,
			supported_repos_json = excluded.supported_repos_json,
			supported_skills_json = excluded.supported_skills_json,
			accepting_jobs = excluded.accepting_jobs,
			concurrency_available = excluded.concurrency_available,
			quota_json = excluded.quota_json,
			last_heartbeat = excluded.last_heartbeat`
	if _, err := s.db.ExecContext(
		ctx, query,
		rec.WorkerID, rec.Alias, reposJSON, skillsJSON,
		boolToInt(rec.AcceptingJobs), rec.ConcurrencyAvailable, string(quotaJSON), hb.UTC().UnixNano(),
	); err != nil {
		return fmt.Errorf("upsert worker heartbeat %s: %w", rec.WorkerID, err)
	}
	return nil
}

// ListActiveWorkers returns workers whose last heartbeat is within staleAfter
// of now.
func (s *Store) ListActiveWorkers(ctx context.Context, staleAfter time.Duration, now time.Time) ([]broker.WorkerRecord, error) {
	cutoff := now.Add(-staleAfter).UTC().UnixNano()
	const query = `
		SELECT worker_id, alias, supported_repos_json, supported_skills_json,
		       accepting_jobs, concurrency_available, quota_json, last_heartbeat
		FROM workers
		WHERE last_heartbeat >= ?
		ORDER BY worker_id ASC`
	rows, err := s.db.QueryContext(ctx, query, cutoff)
	if err != nil {
		return nil, fmt.Errorf("list active workers: %w", err)
	}
	defer rows.Close()

	var workers []broker.WorkerRecord
	for rows.Next() {
		rec, err := scanWorkerRow(rows)
		if err != nil {
			return nil, err
		}
		workers = append(workers, rec)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate active workers: %w", err)
	}
	return workers, nil
}

func tokenPrefix(role broker.TokenRole) (string, error) {
	switch role {
	case broker.TokenRoleProducer:
		return prefixProd, nil
	case broker.TokenRoleWorker:
		return prefixWorker, nil
	default:
		return "", fmt.Errorf("unsupported token role %q", role)
	}
}

func hashToken(salt []byte, plaintext string) [sha256.Size]byte {
	h := sha256.New()
	_, _ = h.Write(salt)
	_, _ = h.Write([]byte(plaintext))
	var sum [sha256.Size]byte
	h.Sum(sum[:0])
	return sum
}

func randomBytes(n int) ([]byte, error) {
	b := make([]byte, n)
	if _, err := rand.Read(b); err != nil {
		return nil, fmt.Errorf("read random bytes: %w", err)
	}
	return b, nil
}

func randomHexID() (string, error) {
	b, err := randomBytes(idByteLen)
	if err != nil {
		return "", err
	}
	return hex.EncodeToString(b), nil
}

func scanAuthTokenRow(rows *sql.Rows) (broker.TokenRecord, []byte, []byte, error) {
	var (
		rec        broker.TokenRecord
		roleStr    string
		reposJSON  string
		salt       []byte
		storedHash []byte
		createdNs  int64
	)
	if err := rows.Scan(&rec.ID, &roleStr, &rec.Name, &reposJSON, &salt, &storedHash, &createdNs); err != nil {
		return broker.TokenRecord{}, nil, nil, fmt.Errorf("scan token auth row: %w", err)
	}
	repos, err := unmarshalStringSlice(reposJSON)
	if err != nil {
		return broker.TokenRecord{}, nil, nil, fmt.Errorf("unmarshal token repos: %w", err)
	}
	rec.Role = broker.TokenRole(roleStr)
	rec.AllowedRepos = repos
	rec.CreatedAt = time.Unix(0, createdNs).UTC()
	return rec, salt, storedHash, nil
}

func scanTokenListRow(rows *sql.Rows) (broker.TokenRecord, error) {
	var (
		rec       broker.TokenRecord
		roleStr   string
		reposJSON string
		createdNs int64
		revokedNs sql.NullInt64
	)
	if err := rows.Scan(&rec.ID, &roleStr, &rec.Name, &reposJSON, &createdNs, &revokedNs); err != nil {
		return broker.TokenRecord{}, fmt.Errorf("scan token row: %w", err)
	}
	repos, err := unmarshalStringSlice(reposJSON)
	if err != nil {
		return broker.TokenRecord{}, fmt.Errorf("unmarshal token repos: %w", err)
	}
	rec.Role = broker.TokenRole(roleStr)
	rec.AllowedRepos = repos
	rec.CreatedAt = time.Unix(0, createdNs).UTC()
	if revokedNs.Valid {
		t := time.Unix(0, revokedNs.Int64).UTC()
		rec.RevokedAt = &t
	}
	return rec, nil
}

func scanWorkerRow(rows *sql.Rows) (broker.WorkerRecord, error) {
	var (
		rec        broker.WorkerRecord
		reposJSON  string
		skillsJSON string
		accepting  int
		quotaJSON  string
		hbNs       int64
	)
	if err := rows.Scan(
		&rec.WorkerID, &rec.Alias, &reposJSON, &skillsJSON,
		&accepting, &rec.ConcurrencyAvailable, &quotaJSON, &hbNs,
	); err != nil {
		return broker.WorkerRecord{}, fmt.Errorf("scan worker row: %w", err)
	}
	repos, err := unmarshalStringSlice(reposJSON)
	if err != nil {
		return broker.WorkerRecord{}, fmt.Errorf("unmarshal worker repos: %w", err)
	}
	skills, err := unmarshalSkillsMap(skillsJSON)
	if err != nil {
		return broker.WorkerRecord{}, fmt.Errorf("unmarshal worker skills: %w", err)
	}
	if err := json.Unmarshal([]byte(quotaJSON), &rec.Quota); err != nil {
		return broker.WorkerRecord{}, fmt.Errorf("unmarshal worker quota: %w", err)
	}
	rec.SupportedRepos = repos
	rec.SupportedSkills = skills
	rec.AcceptingJobs = accepting != 0
	rec.LastHeartbeat = time.Unix(0, hbNs).UTC()
	return rec, nil
}

func marshalStringSlice(s []string) (string, error) {
	if len(s) == 0 {
		return emptyJSONList, nil
	}
	b, err := json.Marshal(s)
	if err != nil {
		return "", fmt.Errorf("marshal string slice: %w", err)
	}
	return string(b), nil
}

func unmarshalStringSlice(raw string) ([]string, error) {
	if raw == "" || raw == emptyJSONList {
		return nil, nil
	}
	var out []string
	if err := json.Unmarshal([]byte(raw), &out); err != nil {
		return nil, fmt.Errorf("unmarshal string slice: %w", err)
	}
	return out, nil
}

func marshalSkillsMap(m map[string][]string) (string, error) {
	if len(m) == 0 {
		return emptyJSONObj, nil
	}
	b, err := json.Marshal(m)
	if err != nil {
		return "", fmt.Errorf("marshal skills map: %w", err)
	}
	return string(b), nil
}

func unmarshalSkillsMap(raw string) (map[string][]string, error) {
	if raw == "" || raw == emptyJSONObj {
		return nil, nil
	}
	var out map[string][]string
	if err := json.Unmarshal([]byte(raw), &out); err != nil {
		return nil, fmt.Errorf("unmarshal skills map: %w", err)
	}
	return out, nil
}

func cloneStrings(s []string) []string {
	if len(s) == 0 {
		return nil
	}
	out := make([]string, len(s))
	copy(out, s)
	return out
}

func boolToInt(v bool) int {
	if v {
		return 1
	}
	return 0
}
