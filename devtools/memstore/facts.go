package memstore

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"
)

// Fact represents a structured attribute row in the facts table.
type Fact struct {
	ID           string      `json:"id"`
	Vault        string      `json:"vault,omitempty"`
	EntityID     string      `json:"entityId"`
	Key          string      `json:"key"`
	ValueJSON    string      `json:"valueJson"`
	ValueType    string      `json:"valueType"`
	Compartment  string      `json:"compartment"`
	Sensitivity  Sensitivity `json:"sensitivity"`
	Confidence   float64     `json:"confidence"`
	SourceType   string      `json:"sourceType"`
	SourceRef    string      `json:"sourceRef,omitempty"`
	ValidFrom    *time.Time  `json:"validFrom,omitempty"`
	ValidUntil   *time.Time  `json:"validUntil,omitempty"`
	SupersededBy string      `json:"supersededBy,omitempty"`
	CreatedAt    time.Time   `json:"createdAt"`
	UpdatedAt    time.Time   `json:"updatedAt"`
}

// FactInput is the input to SetFact.
type FactInput struct {
	EntityID    string
	Key         string
	ValueJSON   string
	ValueType   string
	Compartment string
	Sensitivity Sensitivity
	Confidence  float64
	SourceType  string
	SourceRef   string
	ValidFrom   *time.Time
	ValidUntil  *time.Time
}

// FactFilter selects facts in ListFacts.
type FactFilter struct {
	Vault             string
	EntityID          string
	Key               string
	IncludeExpired    bool
	IncludeSuperseded bool
}

// SetFact inserts a new fact, superseding any existing active fact for
// (entity_id, key, compartment) in the same transaction, and records revision
// and access_log entries.
func (s *Service) SetFact(ctx context.Context, vaultName string, in FactInput, actor Actor) (Fact, error) {
	if in.EntityID == "" || in.Key == "" || in.ValueJSON == "" || in.SourceType == "" {
		return Fact{}, fmt.Errorf("entity_id, key, value_json, and source_type are required")
	}
	compartment := defaultCompartment(in.Compartment)
	sens := defaultSensitivity(in.Sensitivity)
	v, err := s.authorize(vaultName, compartment, sens, CapabilityWrite)
	if err != nil {
		return Fact{}, err
	}

	valueType := in.ValueType
	if valueType == "" {
		valueType = "string"
	}
	confidence := in.Confidence
	if confidence == 0 {
		confidence = 1.0
	}

	id, err := newID("fact")
	if err != nil {
		return Fact{}, err
	}
	now := v.now().UTC()
	nowUnix := now.Unix()

	tx, err := v.db.BeginTx(ctx, nil)
	if err != nil {
		return Fact{}, fmt.Errorf("begin set fact tx: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit

	if err := insertAndSupersedeFactTx(ctx, tx, id, in, valueType, compartment, sens, confidence, nowUnix, actor); err != nil {
		return Fact{}, err
	}

	if err := tx.Commit(); err != nil {
		return Fact{}, fmt.Errorf("commit set fact: %w", err)
	}
	if err := s.logAccess(ctx, v, actor, "set-fact", in.EntityID); err != nil {
		return Fact{}, err
	}

	return Fact{
		ID:          id,
		Vault:       vaultName,
		EntityID:    in.EntityID,
		Key:         in.Key,
		ValueJSON:   in.ValueJSON,
		ValueType:   valueType,
		Compartment: compartment,
		Sensitivity: sens,
		Confidence:  confidence,
		SourceType:  in.SourceType,
		SourceRef:   in.SourceRef,
		ValidFrom:   in.ValidFrom,
		ValidUntil:  in.ValidUntil,
		CreatedAt:   now,
		UpdatedAt:   now,
	}, nil
}

func insertAndSupersedeFactTx(ctx context.Context, tx *sql.Tx, id string, in FactInput, valueType, compartment string, sens Sensitivity, confidence float64, nowUnix int64, actor Actor) error {
	var oldID string
	err := tx.QueryRowContext(ctx,
		`SELECT id FROM facts WHERE entity_id = ? AND key = ? AND compartment = ? AND superseded_by IS NULL`,
		in.EntityID, in.Key, compartment,
	).Scan(&oldID)
	if err != nil && !errors.Is(err, sql.ErrNoRows) {
		return fmt.Errorf("check active fact: %w", err)
	}

	if err := insertFactRow(ctx, tx, id, in, valueType, compartment, sens, confidence, nowUnix); err != nil {
		return err
	}

	diff := map[string]any{"entity_id": in.EntityID, "key": in.Key, "compartment": compartment, "sensitivity": string(sens)}
	if err := RecordRevision(ctx, tx, "facts", id, "insert", diff, actor.Caller, actor.Reason); err != nil {
		return err
	}

	if oldID != "" {
		if _, err := tx.ExecContext(ctx,
			`UPDATE facts SET superseded_by = ?, updated_at = ? WHERE id = ?`,
			id, nowUnix, oldID,
		); err != nil {
			return fmt.Errorf("supersede fact %q: %w", oldID, err)
		}
		if err := deleteFTSDocTx(ctx, tx, oldID, DocKindFact); err != nil {
			return err
		}
		supDiff := map[string]any{"superseded_by": id, "entity_id": in.EntityID, "key": in.Key, "compartment": compartment}
		if err := RecordRevision(ctx, tx, "facts", oldID, "supersede", supDiff, actor.Caller, actor.Reason); err != nil {
			return err
		}
	}
	if err := upsertFTSDocTx(ctx, tx, id, DocKindFact, in.EntityID, compartment, sens, in.Key, in.ValueJSON); err != nil {
		return err
	}
	return nil
}

func insertFactRow(ctx context.Context, tx *sql.Tx, id string, in FactInput, valueType, compartment string, sens Sensitivity, confidence float64, nowUnix int64) error {
	var validFrom, validUntil sql.NullInt64
	if in.ValidFrom != nil {
		validFrom = sql.NullInt64{Int64: in.ValidFrom.UTC().Unix(), Valid: true}
	}
	if in.ValidUntil != nil {
		validUntil = sql.NullInt64{Int64: in.ValidUntil.UTC().Unix(), Valid: true}
	}
	if _, err := tx.ExecContext(ctx,
		`INSERT INTO facts (id, entity_id, key, value_json, value_type, compartment, sensitivity, confidence, source_type, source_ref, valid_from, valid_until, created_at, updated_at)
		VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
		id, in.EntityID, in.Key, in.ValueJSON, valueType, compartment, string(sens), confidence, in.SourceType, in.SourceRef, validFrom, validUntil, nowUnix, nowUnix,
	); err != nil {
		return fmt.Errorf("insert fact: %w", err)
	}
	return nil
}

// ListFacts returns facts matching filter across permitted vaults, strictly
// enforcing profile compartment and sensitivity bounds in SQL.
func (s *Service) ListFacts(ctx context.Context, filter FactFilter, actor Actor) ([]Fact, error) {
	if !s.policy.HasCapability(CapabilityRead) {
		return nil, fmt.Errorf("%w: profile lacks read capability", ErrForbidden)
	}
	vaultNames, err := s.targetVaults(filter.Vault)
	if err != nil {
		return nil, err
	}

	out := []Fact{}
	for _, vaultName := range vaultNames {
		v := s.vaults[vaultName]
		facts, err := s.queryVaultFacts(ctx, v, filter)
		if err != nil {
			return nil, err
		}
		for i := range facts {
			facts[i].Vault = vaultName
		}
		out = append(out, facts...)
		if err := s.logAccess(ctx, v, actor, "list-facts", filter.EntityID); err != nil {
			return nil, err
		}
	}
	return out, nil
}

func (s *Service) queryVaultFacts(ctx context.Context, v *Vault, filter FactFilter) ([]Fact, error) {
	compsJSON, sensJSON, err := s.allowedScopeJSON()
	if err != nil {
		return nil, err
	}
	includeSup := 0
	if filter.IncludeSuperseded {
		includeSup = 1
	}
	includeExp := 0
	if filter.IncludeExpired {
		includeExp = 1
	}

	rows, err := v.db.QueryContext(ctx,
		`SELECT id, entity_id, key, value_json, value_type, compartment, sensitivity, confidence, source_type, source_ref, valid_from, valid_until, superseded_by, created_at, updated_at
		FROM facts
		WHERE compartment IN (SELECT value FROM json_each(?1))
		  AND sensitivity IN (SELECT value FROM json_each(?2))
		  AND (?3 = '' OR entity_id = ?3)
		  AND (?4 = '' OR key = ?4)
		  AND (?5 = 1 OR superseded_by IS NULL)
		  AND (?6 = 1 OR valid_until IS NULL OR valid_until > ?7)
		ORDER BY created_at, rowid`,
		compsJSON, sensJSON, filter.EntityID, filter.Key, includeSup, includeExp, v.now().UTC().Unix(),
	)
	if err != nil {
		return nil, fmt.Errorf("query facts: %w", err)
	}
	defer rows.Close()

	out := []Fact{}
	for rows.Next() {
		f, err := scanFactRow(rows)
		if err != nil {
			return nil, fmt.Errorf("scan fact: %w", err)
		}
		out = append(out, f)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate facts: %w", err)
	}
	return out, nil
}

func scanFactRow(s rowScanner) (Fact, error) {
	var f Fact
	var validFrom, validUntil sql.NullInt64
	var supersededBy sql.NullString
	var createdUnix, updatedUnix int64

	if err := s.Scan(
		&f.ID, &f.EntityID, &f.Key, &f.ValueJSON, &f.ValueType,
		&f.Compartment, &f.Sensitivity, &f.Confidence,
		&f.SourceType, &f.SourceRef, &validFrom, &validUntil, &supersededBy,
		&createdUnix, &updatedUnix,
	); err != nil {
		return Fact{}, fmt.Errorf("scan fact row: %w", err)
	}
	if validFrom.Valid {
		t := time.Unix(validFrom.Int64, 0).UTC()
		f.ValidFrom = &t
	}
	if validUntil.Valid {
		t := time.Unix(validUntil.Int64, 0).UTC()
		f.ValidUntil = &t
	}
	if supersededBy.Valid {
		f.SupersededBy = supersededBy.String
	}
	f.CreatedAt = time.Unix(createdUnix, 0).UTC()
	f.UpdatedAt = time.Unix(updatedUnix, 0).UTC()
	return f, nil
}
