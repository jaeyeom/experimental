package memstore

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"slices"
	"strings"
	"time"
)

// Document kind constants stored in fts_docs.doc_kind.
const (
	DocKindEntity = "entity"
	DocKindFact   = "fact"
	DocKindNote   = "note"
	DocKindEvent  = "event"
)

// DefaultSearchLimit is the default maximum number of hits returned by Search.
const DefaultSearchLimit = 20

// SearchQuery configures a scoped full-text search across permitted vaults.
type SearchQuery struct {
	Query    string
	Vault    string
	Kinds    []string
	EntityID string
	Since    time.Time
	Until    time.Time
	Limit    int
	Actor    Actor
}

// SearchHit is one ranked result returned by Retriever.Search.
type SearchHit struct {
	Vault       string      `json:"vault"`
	DocID       string      `json:"docId"`
	DocKind     string      `json:"docKind"`
	EntityID    string      `json:"entityId,omitempty"`
	Compartment string      `json:"compartment"`
	Sensitivity Sensitivity `json:"sensitivity"`
	Title       string      `json:"title"`
	Snippet     string      `json:"snippet,omitempty"`
	Score       float64     `json:"score"`
}

// Retriever searches across permitted vaults for matching memory documents.
type Retriever interface {
	Search(ctx context.Context, q SearchQuery) ([]SearchHit, error)
}

// SQLiteRetriever implements Retriever using SQLite FTS5 BM25 ranking scoped
// to the Service's resolved Policy.
type SQLiteRetriever struct {
	service *Service
}

// NewSQLiteRetriever returns a *SQLiteRetriever backed by s.
func NewSQLiteRetriever(s *Service) *SQLiteRetriever {
	return &SQLiteRetriever{service: s}
}

// Retriever returns the Service's default Retriever.
func (s *Service) Retriever() Retriever {
	return NewSQLiteRetriever(s)
}

// Search delegates to SQLiteRetriever for convenience.
func (s *Service) Search(ctx context.Context, q SearchQuery) ([]SearchHit, error) {
	return NewSQLiteRetriever(s).Search(ctx, q)
}

// Search runs a profile-scoped FTS5 BM25 search across permitted vaults, merges
// and sorts hits by relevance, and logs one content-free access_log entry per
// queried vault.
func (r *SQLiteRetriever) Search(ctx context.Context, q SearchQuery) ([]SearchHit, error) {
	if !r.service.policy.HasCapability(CapabilityRead) {
		return nil, fmt.Errorf("%w: profile lacks read capability", ErrForbidden)
	}
	ftsQuery := formatFTS5Query(q.Query)
	if ftsQuery == "" {
		return nil, fmt.Errorf("search query is required")
	}

	vaultNames, err := r.service.targetVaults(q.Vault)
	if err != nil {
		return nil, err
	}

	limit := q.Limit
	if limit <= 0 {
		limit = DefaultSearchLimit
	}

	compsJSON, err := r.service.allowedCompartmentsJSON()
	if err != nil {
		return nil, err
	}
	kindsJSON, hasKinds, err := marshalKindsFilter(q.Kinds)
	if err != nil {
		return nil, err
	}

	var allHits []SearchHit
	for _, vaultName := range vaultNames {
		v := r.service.vaults[vaultName]
		hits, err := r.searchVault(ctx, v, vaultName, ftsQuery, compsJSON, hasKinds, kindsJSON, q, limit)
		if err != nil {
			return nil, err
		}
		allHits = append(allHits, hits...)
		if err := r.service.logAccess(ctx, v, q.Actor, "search", q.EntityID); err != nil {
			return nil, err
		}
	}

	slices.SortFunc(allHits, compareSearchHits)
	if len(allHits) > limit {
		allHits = allHits[:limit]
	}
	if allHits == nil {
		allHits = []SearchHit{}
	}
	return allHits, nil
}

func (r *SQLiteRetriever) searchVault(
	ctx context.Context,
	v *Vault,
	vaultName string,
	ftsQuery string,
	compsJSON string,
	hasKinds int,
	kindsJSON string,
	q SearchQuery,
	limit int,
) ([]SearchHit, error) {
	var hasSince, hasUntil int
	var sinceUnix, untilUnix int64
	if !q.Since.IsZero() {
		hasSince = 1
		sinceUnix = q.Since.UTC().Unix()
	}
	if !q.Until.IsZero() {
		hasUntil = 1
		untilUnix = q.Until.UTC().Unix()
	}

	rows, err := v.db.QueryContext(ctx,
		`SELECT fts_docs.doc_id, fts_docs.doc_kind, fts_docs.entity_id,
		        fts_docs.compartment, fts_docs.sensitivity_rank,
		        fts_docs.title, snippet(fts_docs, -1, '', '', '...', 24),
		        -bm25(fts_docs)
		FROM fts_docs
		LEFT JOIN episodic_events ee ON fts_docs.doc_kind = 'event' AND fts_docs.doc_id = ee.id
		LEFT JOIN entities en ON fts_docs.doc_kind = 'entity' AND fts_docs.doc_id = en.id
		LEFT JOIN facts fa ON fts_docs.doc_kind = 'fact' AND fts_docs.doc_id = fa.id
		LEFT JOIN note_blocks nb ON fts_docs.doc_kind = 'note' AND fts_docs.doc_id = nb.id
		WHERE fts_docs MATCH ?1
		  AND fts_docs.compartment IN (SELECT value FROM json_each(?2))
		  AND fts_docs.sensitivity_rank <= ?3
		  AND (?4 = 0 OR fts_docs.doc_kind IN (SELECT value FROM json_each(?5)))
		  AND (?6 = '' OR fts_docs.entity_id = ?6)
		  AND (?7 = 0 OR COALESCE(ee.occurred_at, en.updated_at, fa.updated_at, nb.updated_at, 0) >= ?8)
		  AND (?9 = 0 OR COALESCE(ee.occurred_at, en.updated_at, fa.updated_at, nb.updated_at, 0) <= ?10)
		ORDER BY bm25(fts_docs) ASC
		LIMIT ?11`,
		ftsQuery, compsJSON, r.service.policy.maxRank,
		hasKinds, kindsJSON, q.EntityID,
		hasSince, sinceUnix, hasUntil, untilUnix, limit,
	)
	if err != nil {
		return nil, fmt.Errorf("search vault %q: %w", vaultName, err)
	}
	defer rows.Close()

	var hits []SearchHit
	for rows.Next() {
		var h SearchHit
		var sensRank int
		if err := rows.Scan(
			&h.DocID, &h.DocKind, &h.EntityID,
			&h.Compartment, &sensRank,
			&h.Title, &h.Snippet, &h.Score,
		); err != nil {
			return nil, fmt.Errorf("scan search hit in %q: %w", vaultName, err)
		}
		h.Vault = vaultName
		h.Sensitivity = sensitivityFromRank(sensRank)
		hits = append(hits, h)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate search hits in %q: %w", vaultName, err)
	}
	return hits, nil
}

func compareSearchHits(a, b SearchHit) int {
	if a.Score > b.Score {
		return -1
	}
	if a.Score < b.Score {
		return 1
	}
	if a.Vault != b.Vault {
		return strings.Compare(a.Vault, b.Vault)
	}
	if a.DocKind != b.DocKind {
		return strings.Compare(a.DocKind, b.DocKind)
	}
	return strings.Compare(a.DocID, b.DocID)
}

func formatFTS5Query(raw string) string {
	fields := strings.Fields(raw)
	if len(fields) == 0 {
		return ""
	}
	parts := make([]string, 0, len(fields))
	for _, f := range fields {
		escaped := strings.ReplaceAll(f, `"`, `""`)
		parts = append(parts, `"`+escaped+`"`)
	}
	return strings.Join(parts, " ")
}

func marshalKindsFilter(kinds []string) (string, int, error) {
	if len(kinds) == 0 {
		return "[]", 0, nil
	}
	b, err := json.Marshal(kinds)
	if err != nil {
		return "", 0, fmt.Errorf("marshal kinds: %w", err)
	}
	return string(b), 1, nil
}

func sensitivityFromRank(rank int) Sensitivity {
	switch rank {
	case 0:
		return SensitivityPublic
	case 1:
		return SensitivityNormal
	case 2:
		return SensitivityPrivate
	case 3:
		return SensitivityRestricted
	default:
		return SensitivityNormal
	}
}

func upsertFTSDocTx(ctx context.Context, tx *sql.Tx, docID, docKind, entityID, compartment string, sens Sensitivity, title, body string) error {
	rank, ok := sens.rank()
	if !ok {
		return fmt.Errorf("unknown sensitivity %q", sens)
	}
	if err := deleteFTSDocTx(ctx, tx, docID, docKind); err != nil {
		return err
	}
	if _, err := tx.ExecContext(ctx,
		`INSERT INTO fts_docs (doc_id, doc_kind, entity_id, compartment, sensitivity_rank, title, body)
		VALUES (?, ?, ?, ?, ?, ?, ?)`,
		docID, docKind, entityID, compartment, rank, title, body,
	); err != nil {
		return fmt.Errorf("insert fts doc (%s %s): %w", docKind, docID, err)
	}
	return nil
}

func deleteFTSDocTx(ctx context.Context, tx *sql.Tx, docID, docKind string) error {
	if _, err := tx.ExecContext(ctx,
		`DELETE FROM fts_docs WHERE doc_id = ? AND doc_kind = ?`,
		docID, docKind,
	); err != nil {
		return fmt.Errorf("delete fts doc (%s %s): %w", docKind, docID, err)
	}
	return nil
}
