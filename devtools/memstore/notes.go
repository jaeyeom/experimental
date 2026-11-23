package memstore

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"
)

// NoteBlock represents a row in the note_blocks table.
type NoteBlock struct {
	ID          string      `json:"id"`
	EntityID    string      `json:"entityId"`
	SectionKey  string      `json:"sectionKey"`
	Heading     string      `json:"heading"`
	SortOrder   int         `json:"sortOrder"`
	BodyMD      string      `json:"bodyMd"`
	Compartment string      `json:"compartment"`
	Sensitivity Sensitivity `json:"sensitivity"`
	UpdatedAt   time.Time   `json:"updatedAt"`
}

// NoteBlockInput is the input to SetNoteBlock.
type NoteBlockInput struct {
	EntityID    string
	SectionKey  string
	Heading     string
	SortOrder   int
	BodyMD      string
	Compartment string
	Sensitivity Sensitivity
}

// SetNoteBlock upserts a note block keyed by (entity_id, section_key) and
// records revision and access_log entries.
func (s *Service) SetNoteBlock(ctx context.Context, vaultName string, in NoteBlockInput, actor Actor) (NoteBlock, error) {
	if in.EntityID == "" || in.SectionKey == "" || in.Heading == "" {
		return NoteBlock{}, fmt.Errorf("entity_id, section_key, and heading are required")
	}
	compartment := defaultCompartment(in.Compartment)
	sens := defaultSensitivity(in.Sensitivity)
	v, err := s.authorize(vaultName, compartment, sens, CapabilityWrite)
	if err != nil {
		return NoteBlock{}, err
	}

	sortOrder := in.SortOrder
	if sortOrder == 0 {
		sortOrder = 100
	}
	now := v.now().UTC()
	nowUnix := now.Unix()

	tx, err := v.db.BeginTx(ctx, nil)
	if err != nil {
		return NoteBlock{}, fmt.Errorf("begin set note block tx: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit

	id, op, err := upsertNoteBlockRow(ctx, tx, in, sortOrder, compartment, sens, nowUnix)
	if err != nil {
		return NoteBlock{}, err
	}

	diff := map[string]any{"entity_id": in.EntityID, "section_key": in.SectionKey, "compartment": compartment, "sensitivity": string(sens)}
	if err := RecordRevision(ctx, tx, "note_blocks", id, op, diff, actor.Caller, actor.Reason); err != nil {
		return NoteBlock{}, err
	}
	if err := tx.Commit(); err != nil {
		return NoteBlock{}, fmt.Errorf("commit set note block: %w", err)
	}
	if err := s.logAccess(ctx, v, actor, "set-note-block", in.EntityID); err != nil {
		return NoteBlock{}, err
	}

	return NoteBlock{
		ID:          id,
		EntityID:    in.EntityID,
		SectionKey:  in.SectionKey,
		Heading:     in.Heading,
		SortOrder:   sortOrder,
		BodyMD:      in.BodyMD,
		Compartment: compartment,
		Sensitivity: sens,
		UpdatedAt:   now,
	}, nil
}

func upsertNoteBlockRow(ctx context.Context, tx *sql.Tx, in NoteBlockInput, sortOrder int, compartment string, sens Sensitivity, nowUnix int64) (string, string, error) {
	var id string
	err := tx.QueryRowContext(ctx,
		`SELECT id FROM note_blocks WHERE entity_id = ? AND section_key = ?`,
		in.EntityID, in.SectionKey,
	).Scan(&id)
	op := "update"
	if errors.Is(err, sql.ErrNoRows) {
		op = "insert"
		id, err = newID("note")
		if err != nil {
			return "", "", err
		}
	} else if err != nil {
		return "", "", fmt.Errorf("check existing note block: %w", err)
	}

	if op == "insert" {
		_, err = tx.ExecContext(ctx,
			`INSERT INTO note_blocks (id, entity_id, section_key, heading, sort_order, body_md, compartment, sensitivity, updated_at)
			VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)`,
			id, in.EntityID, in.SectionKey, in.Heading, sortOrder, in.BodyMD, compartment, string(sens), nowUnix,
		)
	} else {
		_, err = tx.ExecContext(ctx,
			`UPDATE note_blocks
			SET heading = ?, sort_order = ?, body_md = ?, compartment = ?, sensitivity = ?, updated_at = ?
			WHERE id = ?`,
			in.Heading, sortOrder, in.BodyMD, compartment, string(sens), nowUnix, id,
		)
	}
	if err != nil {
		return "", "", fmt.Errorf("%s note block: %w", op, err)
	}
	return id, op, nil
}

// ListNoteBlocks returns note blocks for entityID ordered by sort_order and
// section_key, strictly filtered by the service's profile.
func (s *Service) ListNoteBlocks(ctx context.Context, vaultName, entityID string, actor Actor) ([]NoteBlock, error) {
	if !s.policy.HasCapability(CapabilityRead) {
		return nil, fmt.Errorf("%w: profile lacks read capability", ErrForbidden)
	}
	v, err := s.openVault(vaultName)
	if err != nil {
		return nil, err
	}

	out, err := s.queryVaultNoteBlocks(ctx, v, entityID)
	if err != nil {
		return nil, err
	}
	if err := s.logAccess(ctx, v, actor, "list-note-blocks", entityID); err != nil {
		return nil, err
	}
	return out, nil
}

func (s *Service) queryVaultNoteBlocks(ctx context.Context, v *Vault, entityID string) ([]NoteBlock, error) {
	compsJSON, sensJSON, err := s.allowedScopeJSON()
	if err != nil {
		return nil, err
	}

	rows, err := v.db.QueryContext(ctx,
		`SELECT id, entity_id, section_key, heading, sort_order, body_md, compartment, sensitivity, updated_at
		FROM note_blocks
		WHERE entity_id = ?
		  AND compartment IN (SELECT value FROM json_each(?))
		  AND sensitivity IN (SELECT value FROM json_each(?))
		ORDER BY sort_order, section_key`,
		entityID, compsJSON, sensJSON,
	)
	if err != nil {
		return nil, fmt.Errorf("query note blocks: %w", err)
	}
	defer rows.Close()

	out := []NoteBlock{}
	for rows.Next() {
		var n NoteBlock
		var updatedUnix int64
		if err := rows.Scan(&n.ID, &n.EntityID, &n.SectionKey, &n.Heading, &n.SortOrder, &n.BodyMD, &n.Compartment, &n.Sensitivity, &updatedUnix); err != nil {
			return nil, fmt.Errorf("scan note block: %w", err)
		}
		n.UpdatedAt = time.Unix(updatedUnix, 0).UTC()
		out = append(out, n)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate note blocks: %w", err)
	}
	return out, nil
}
