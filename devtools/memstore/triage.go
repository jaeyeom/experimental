package memstore

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"time"
)

// Triage status values stored in triage_inbox.status.
const (
	TriageStatusPending  = "pending"
	TriageStatusApproved = "approved"
	TriageStatusRejected = "rejected"
)

// TriageItem represents a row in the triage_inbox table.
type TriageItem struct {
	ID                  string      `json:"id"`
	Vault               string      `json:"vault,omitempty"`
	Source              string      `json:"source"`
	ProposedCompartment string      `json:"proposedCompartment"`
	ProposedSensitivity Sensitivity `json:"proposedSensitivity"`
	TargetEntityID      string      `json:"targetEntityId,omitempty"`
	PayloadJSON         string      `json:"payloadJson"`
	Status              string      `json:"status"`
	DecideRef           string      `json:"decideRef,omitempty"`
	CreatedAt           time.Time   `json:"createdAt"`
	ResolvedAt          *time.Time  `json:"resolvedAt,omitempty"`
}

// TriageInput is the input to EnqueueTriage.
type TriageInput struct {
	Source              string
	ProposedCompartment string
	ProposedSensitivity Sensitivity
	TargetEntityID      string
	PayloadJSON         string
	DecideRef           string
}

// TriageFilter selects triage items in ListTriage.
type TriageFilter struct {
	Vault  string
	Status string
}

// TriagePayload is the structured JSON payload staged in triage_inbox and
// applied atomically on ApproveTriage.
type TriagePayload struct {
	Entity     *EntityInput     `json:"entity,omitempty"`
	Facts      []FactInput      `json:"facts,omitempty"`
	NoteBlocks []NoteBlockInput `json:"noteBlocks,omitempty"`
	Events     []EventInput     `json:"events,omitempty"`
}

// EnqueueTriage stages a payload in triage_inbox with status "pending" without
// mutating canonical entities, facts, note_blocks, or episodic_events.
func (s *Service) EnqueueTriage(ctx context.Context, vaultName string, in TriageInput) (TriageItem, error) {
	if in.Source == "" || in.PayloadJSON == "" {
		return TriageItem{}, fmt.Errorf("source and payload_json are required")
	}
	if !json.Valid([]byte(in.PayloadJSON)) {
		return TriageItem{}, fmt.Errorf("payload_json must be valid JSON")
	}
	compartment := defaultCompartment(in.ProposedCompartment)
	sens := defaultSensitivity(in.ProposedSensitivity)
	v, err := s.authorizeTriageWrite(vaultName, compartment, sens)
	if err != nil {
		return TriageItem{}, err
	}

	id, err := newID("tri")
	if err != nil {
		return TriageItem{}, err
	}
	now := v.now().UTC()
	nowUnix := now.Unix()

	tx, err := v.db.BeginTx(ctx, nil)
	if err != nil {
		return TriageItem{}, fmt.Errorf("begin enqueue triage tx: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit

	if err := insertTriageRowTx(ctx, tx, id, in, compartment, sens, nowUnix, Actor{Caller: in.Source}); err != nil {
		return TriageItem{}, err
	}
	if err := tx.Commit(); err != nil {
		return TriageItem{}, fmt.Errorf("commit enqueue triage: %w", err)
	}

	return TriageItem{
		ID:                  id,
		Vault:               vaultName,
		Source:              in.Source,
		ProposedCompartment: compartment,
		ProposedSensitivity: sens,
		TargetEntityID:      in.TargetEntityID,
		PayloadJSON:         in.PayloadJSON,
		Status:              TriageStatusPending,
		DecideRef:           in.DecideRef,
		CreatedAt:           now,
	}, nil
}

func insertTriageRowTx(ctx context.Context, tx *sql.Tx, id string, in TriageInput, compartment string, sens Sensitivity, nowUnix int64, actor Actor) error {
	var targetID, decideRef sql.NullString
	if in.TargetEntityID != "" {
		targetID = sql.NullString{String: in.TargetEntityID, Valid: true}
	}
	if in.DecideRef != "" {
		decideRef = sql.NullString{String: in.DecideRef, Valid: true}
	}
	if _, err := tx.ExecContext(ctx,
		`INSERT INTO triage_inbox (id, source, proposed_compartment, proposed_sensitivity, target_entity_id, payload_json, status, decide_ref, created_at)
		VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)`,
		id, in.Source, compartment, string(sens), targetID, in.PayloadJSON, TriageStatusPending, decideRef, nowUnix,
	); err != nil {
		return fmt.Errorf("insert triage item: %w", err)
	}
	diff := map[string]any{"source": in.Source, "compartment": compartment, "sensitivity": string(sens), "status": TriageStatusPending}
	return RecordRevision(ctx, tx, "triage_inbox", id, "insert", diff, actor.Caller, actor.Reason)
}

func (s *Service) authorizeTriageWrite(vaultName, compartment string, sens Sensitivity) (*Vault, error) {
	for _, cap := range []Capability{CapabilityWrite, CapabilityAdmin, CapabilityProposeFact, CapabilityWriteEpisodic} {
		if v, err := s.authorize(vaultName, compartment, sens, cap); err == nil {
			return v, nil
		}
	}
	return s.authorize(vaultName, compartment, sens, CapabilityWrite)
}

// ListTriage returns triage_inbox items across permitted vaults filtered by
// the service's profile scope and filter.Status.
func (s *Service) ListTriage(ctx context.Context, filter TriageFilter) ([]TriageItem, error) {
	if !s.policy.HasCapability(CapabilityRead) {
		return nil, fmt.Errorf("%w: profile lacks read capability", ErrForbidden)
	}
	if filter.Status != "" && !isValidTriageStatus(filter.Status) {
		return nil, fmt.Errorf("invalid triage status %q", filter.Status)
	}
	vaultNames, err := s.targetVaults(filter.Vault)
	if err != nil {
		return nil, err
	}

	out := []TriageItem{}
	for _, vaultName := range vaultNames {
		v := s.vaults[vaultName]
		items, err := s.queryVaultTriage(ctx, v, filter.Status)
		if err != nil {
			return nil, err
		}
		for i := range items {
			items[i].Vault = vaultName
		}
		out = append(out, items...)
	}
	return out, nil
}

func (s *Service) queryVaultTriage(ctx context.Context, v *Vault, status string) ([]TriageItem, error) {
	compsJSON, sensJSON, err := s.allowedScopeJSON()
	if err != nil {
		return nil, err
	}
	rows, err := v.db.QueryContext(ctx,
		`SELECT id, source, proposed_compartment, proposed_sensitivity, target_entity_id, payload_json, status, decide_ref, created_at, resolved_at
		FROM triage_inbox
		WHERE proposed_compartment IN (SELECT value FROM json_each(?1))
		  AND proposed_sensitivity IN (SELECT value FROM json_each(?2))
		  AND (?3 = '' OR status = ?3)
		ORDER BY created_at DESC, rowid DESC`,
		compsJSON, sensJSON, status,
	)
	if err != nil {
		return nil, fmt.Errorf("query triage inbox: %w", err)
	}
	defer rows.Close()

	out := []TriageItem{}
	for rows.Next() {
		item, err := scanTriageRow(rows)
		if err != nil {
			return nil, fmt.Errorf("scan triage item: %w", err)
		}
		out = append(out, item)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate triage inbox: %w", err)
	}
	return out, nil
}

// ApproveTriage atomically applies a pending triage item's staged payload to
// canonical tables (entities, facts, note_blocks, episodic_events), marks the
// item approved, and records revisions and an access_log entry.
func (s *Service) ApproveTriage(ctx context.Context, vaultName, id, overrideCompartment string, overrideSensitivity Sensitivity, actor Actor) error {
	v, item, err := s.loadTriageForResolution(ctx, vaultName, id)
	if err != nil {
		return err
	}
	if item.Status != TriageStatusPending {
		return fmt.Errorf("triage item %q is already %s", id, item.Status)
	}

	compartment := item.ProposedCompartment
	if overrideCompartment != "" {
		compartment = overrideCompartment
	}
	sens := item.ProposedSensitivity
	if overrideSensitivity != "" {
		sens = overrideSensitivity
	}
	if _, err := s.authorize(vaultName, compartment, sens, CapabilityWrite); err != nil {
		if !s.policy.HasCapability(CapabilityAdmin) {
			return err
		}
		if _, adminErr := s.authorize(vaultName, compartment, sens, CapabilityAdmin); adminErr != nil {
			return adminErr
		}
	}

	var payload TriagePayload
	if err := json.Unmarshal([]byte(item.PayloadJSON), &payload); err != nil {
		return fmt.Errorf("parse triage payload %q: %w", id, err)
	}

	now := v.now().UTC()
	nowUnix := now.Unix()

	tx, err := v.db.BeginTx(ctx, nil)
	if err != nil {
		return fmt.Errorf("begin approve triage tx: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit

	targetEntityID, err := applyPayloadTx(ctx, tx, payload, item.TargetEntityID, compartment, sens, now, actor)
	if err != nil {
		return err
	}

	res, err := tx.ExecContext(ctx,
		`UPDATE triage_inbox
		SET status = ?, proposed_compartment = ?, proposed_sensitivity = ?, resolved_at = ?
		WHERE id = ? AND status = ?`,
		TriageStatusApproved, compartment, string(sens), nowUnix, id, TriageStatusPending,
	)
	if err != nil {
		return fmt.Errorf("approve triage item %q: %w", id, err)
	}
	if n, _ := res.RowsAffected(); n != 1 {
		return fmt.Errorf("triage item %q is no longer pending", id)
	}

	diff := map[string]any{"status": TriageStatusApproved, "compartment": compartment, "sensitivity": string(sens)}
	if err := RecordRevision(ctx, tx, "triage_inbox", id, "update", diff, actor.Caller, actor.Reason); err != nil {
		return err
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("commit approve triage: %w", err)
	}
	return s.logAccess(ctx, v, actor, "approve-triage", targetEntityID)
}

// RejectTriage marks a pending triage item as rejected and records a revision
// and access_log entry.
func (s *Service) RejectTriage(ctx context.Context, vaultName, id string, actor Actor) error {
	v, item, err := s.loadTriageForResolution(ctx, vaultName, id)
	if err != nil {
		return err
	}
	if item.Status != TriageStatusPending {
		return fmt.Errorf("triage item %q is already %s", id, item.Status)
	}
	if _, err := s.authorize(vaultName, item.ProposedCompartment, item.ProposedSensitivity, CapabilityWrite); err != nil {
		if _, adminErr := s.authorize(vaultName, item.ProposedCompartment, item.ProposedSensitivity, CapabilityAdmin); adminErr != nil {
			return err
		}
	}

	nowUnix := v.now().UTC().Unix()
	tx, err := v.db.BeginTx(ctx, nil)
	if err != nil {
		return fmt.Errorf("begin reject triage tx: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit

	res, err := tx.ExecContext(ctx,
		`UPDATE triage_inbox SET status = ?, resolved_at = ? WHERE id = ? AND status = ?`,
		TriageStatusRejected, nowUnix, id, TriageStatusPending,
	)
	if err != nil {
		return fmt.Errorf("reject triage item %q: %w", id, err)
	}
	if n, _ := res.RowsAffected(); n != 1 {
		return fmt.Errorf("triage item %q is no longer pending", id)
	}
	diff := map[string]any{"status": TriageStatusRejected}
	if err := RecordRevision(ctx, tx, "triage_inbox", id, "update", diff, actor.Caller, actor.Reason); err != nil {
		return err
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("commit reject triage: %w", err)
	}
	return s.logAccess(ctx, v, actor, "reject-triage", item.TargetEntityID)
}

func (s *Service) loadTriageForResolution(ctx context.Context, vaultName, id string) (*Vault, TriageItem, error) {
	if id == "" {
		return nil, TriageItem{}, fmt.Errorf("triage id is required")
	}
	v, err := s.openVault(vaultName)
	if err != nil {
		return nil, TriageItem{}, err
	}
	row := v.db.QueryRowContext(ctx,
		`SELECT id, source, proposed_compartment, proposed_sensitivity, target_entity_id, payload_json, status, decide_ref, created_at, resolved_at
		FROM triage_inbox
		WHERE id = ?`,
		id,
	)
	item, err := scanTriageRow(row)
	if errors.Is(err, sql.ErrNoRows) {
		return nil, TriageItem{}, fmt.Errorf("%w: triage item %q", ErrNotFound, id)
	}
	if err != nil {
		return nil, TriageItem{}, fmt.Errorf("load triage item %q: %w", id, err)
	}
	item.Vault = vaultName
	return v, item, nil
}

func applyPayloadTx(ctx context.Context, tx *sql.Tx, payload TriagePayload, defaultEntityID, compartment string, sens Sensitivity, now time.Time, actor Actor) (string, error) {
	nowUnix := now.Unix()
	targetEntityID := defaultEntityID

	if payload.Entity != nil {
		eIn := *payload.Entity
		if eIn.Kind == "" || eIn.Slug == "" || eIn.Title == "" {
			return "", fmt.Errorf("staged entity requires kind, slug, and title")
		}
		entityID := eIn.Kind + ":" + eIn.Slug
		targetEntityID = entityID
		op, _, err := upsertEntityRow(ctx, tx, entityID, eIn, compartment, sens, nowUnix)
		if err != nil {
			return "", err
		}
		if eIn.Aliases != nil {
			if err := replaceAliases(ctx, tx, entityID, eIn.Aliases); err != nil {
				return "", err
			}
		}
		aliases, err := loadAliases(ctx, tx, entityID)
		if err != nil {
			return "", err
		}
		ftsBody := buildEntityFTSBody(aliases, eIn.Summary)
		if err := upsertFTSDocTx(ctx, tx, entityID, DocKindEntity, entityID, compartment, sens, eIn.Title, ftsBody); err != nil {
			return "", err
		}
		diff := map[string]any{"kind": eIn.Kind, "slug": eIn.Slug, "compartment": compartment, "sensitivity": string(sens)}
		if err := RecordRevision(ctx, tx, "entities", entityID, op, diff, actor.Caller, actor.Reason); err != nil {
			return "", err
		}
	}

	if err := applyPayloadFactsTx(ctx, tx, payload.Facts, targetEntityID, compartment, sens, nowUnix, actor); err != nil {
		return "", err
	}
	if err := applyPayloadNotesTx(ctx, tx, payload.NoteBlocks, targetEntityID, compartment, sens, nowUnix, actor); err != nil {
		return "", err
	}
	if err := applyPayloadEventsTx(ctx, tx, payload.Events, targetEntityID, compartment, sens, now, actor); err != nil {
		return "", err
	}
	return targetEntityID, nil
}

func applyPayloadFactsTx(ctx context.Context, tx *sql.Tx, facts []FactInput, defaultEntityID, compartment string, sens Sensitivity, nowUnix int64, actor Actor) error {
	for _, fIn := range facts {
		if fIn.EntityID == "" {
			fIn.EntityID = defaultEntityID
		}
		if fIn.SourceType == "" {
			fIn.SourceType = "triage"
		}
		if fIn.EntityID == "" || fIn.Key == "" || fIn.ValueJSON == "" {
			return fmt.Errorf("staged fact requires entity_id, key, and value_json")
		}
		valueType := fIn.ValueType
		if valueType == "" {
			valueType = "string"
		}
		confidence := fIn.Confidence
		if confidence == 0 {
			confidence = 1.0
		}
		factID, err := newID("fact")
		if err != nil {
			return err
		}
		if err := insertAndSupersedeFactTx(ctx, tx, factID, fIn, valueType, compartment, sens, confidence, nowUnix, actor); err != nil {
			return err
		}
	}
	return nil
}

func applyPayloadNotesTx(ctx context.Context, tx *sql.Tx, notes []NoteBlockInput, defaultEntityID, compartment string, sens Sensitivity, nowUnix int64, actor Actor) error {
	for _, nIn := range notes {
		if nIn.EntityID == "" {
			nIn.EntityID = defaultEntityID
		}
		if nIn.EntityID == "" || nIn.SectionKey == "" || nIn.Heading == "" {
			return fmt.Errorf("staged note block requires entity_id, section_key, and heading")
		}
		sortOrder := nIn.SortOrder
		if sortOrder == 0 {
			sortOrder = 100
		}
		noteID, op, err := upsertNoteBlockRow(ctx, tx, nIn, sortOrder, compartment, sens, nowUnix)
		if err != nil {
			return err
		}
		if err := upsertFTSDocTx(ctx, tx, noteID, DocKindNote, nIn.EntityID, compartment, sens, nIn.Heading, nIn.BodyMD); err != nil {
			return err
		}
		diff := map[string]any{"entity_id": nIn.EntityID, "section_key": nIn.SectionKey, "compartment": compartment, "sensitivity": string(sens)}
		if err := RecordRevision(ctx, tx, "note_blocks", noteID, op, diff, actor.Caller, actor.Reason); err != nil {
			return err
		}
	}
	return nil
}

func applyPayloadEventsTx(ctx context.Context, tx *sql.Tx, events []EventInput, defaultEntityID, compartment string, sens Sensitivity, now time.Time, actor Actor) error {
	nowUnix := now.Unix()
	for _, evIn := range events {
		if evIn.EntityID == "" && defaultEntityID != "" {
			evIn.EntityID = defaultEntityID
		}
		if evIn.Reporter == "" {
			evIn.Reporter = actor.Caller
			if evIn.Reporter == "" {
				evIn.Reporter = "triage"
			}
		}
		if evIn.Summary == "" {
			return fmt.Errorf("staged event requires summary")
		}
		occurredAt := evIn.OccurredAt.UTC()
		if occurredAt.IsZero() {
			occurredAt = now
		}
		eventDate := occurredAt.Format("2006-01-02")
		evID, err := newID("evt")
		if err != nil {
			return err
		}
		if err := insertEventRowTx(ctx, tx, evID, eventDate, occurredAt.Unix(), evIn, compartment, sens, nowUnix, actor); err != nil {
			return err
		}
	}
	return nil
}

func buildEntityFTSBody(aliases []string, summary string) string {
	parts := make([]string, 0, len(aliases)+1)
	parts = append(parts, aliases...)
	if summary != "" {
		parts = append(parts, summary)
	}
	return joinSpaceTrim(parts)
}

func joinSpaceTrim(parts []string) string {
	var out string
	for _, p := range parts {
		if p == "" {
			continue
		}
		if out != "" {
			out += " "
		}
		out += p
	}
	return out
}

func scanTriageRow(s rowScanner) (TriageItem, error) {
	var item TriageItem
	var targetID, decideRef sql.NullString
	var createdUnix int64
	var resolvedUnix sql.NullInt64

	if err := s.Scan(
		&item.ID, &item.Source, &item.ProposedCompartment, &item.ProposedSensitivity,
		&targetID, &item.PayloadJSON, &item.Status, &decideRef, &createdUnix, &resolvedUnix,
	); err != nil {
		return TriageItem{}, fmt.Errorf("scan triage row: %w", err)
	}
	if targetID.Valid {
		item.TargetEntityID = targetID.String
	}
	if decideRef.Valid {
		item.DecideRef = decideRef.String
	}
	item.CreatedAt = time.Unix(createdUnix, 0).UTC()
	if resolvedUnix.Valid {
		t := time.Unix(resolvedUnix.Int64, 0).UTC()
		item.ResolvedAt = &t
	}
	return item, nil
}

func isValidTriageStatus(status string) bool {
	switch status {
	case TriageStatusPending, TriageStatusApproved, TriageStatusRejected:
		return true
	default:
		return false
	}
}
