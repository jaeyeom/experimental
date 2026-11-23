package memstore

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"
)

// EpisodicEvent represents a row in the episodic_events table.
type EpisodicEvent struct {
	ID              string      `json:"id"`
	Vault           string      `json:"vault,omitempty"`
	EventDate       string      `json:"eventDate"`
	OccurredAt      time.Time   `json:"occurredAt"`
	EntityID        string      `json:"entityId,omitempty"`
	Compartment     string      `json:"compartment"`
	Sensitivity     Sensitivity `json:"sensitivity"`
	Channel         string      `json:"channel,omitempty"`
	SourceRef       string      `json:"sourceRef,omitempty"`
	Summary         string      `json:"summary"`
	DetailsMD       string      `json:"detailsMd,omitempty"`
	Reporter        string      `json:"reporter"`
	CompactedIntoID string      `json:"compactedIntoId,omitempty"`
	CreatedAt       time.Time   `json:"createdAt"`
}

// EventInput is the input to AddEvent.
type EventInput struct {
	OccurredAt  time.Time
	EntityID    string
	Compartment string
	Sensitivity Sensitivity
	Channel     string
	SourceRef   string
	Summary     string
	DetailsMD   string
	Reporter    string
}

// EventFilter selects episodic events in ListEvents.
type EventFilter struct {
	Vault            string
	Since            time.Time
	Until            time.Time
	EntityID         string
	Channel          string
	Limit            int
	IncludeCompacted bool
}

// AddEvent appends an episodic event, requiring CapabilityWrite or
// CapabilityWriteEpisodic. It is idempotent when both channel and source_ref
// are non-empty and already recorded in the vault.
func (s *Service) AddEvent(ctx context.Context, vaultName string, in EventInput, actor Actor) (EpisodicEvent, error) {
	if in.Summary == "" || in.Reporter == "" {
		return EpisodicEvent{}, fmt.Errorf("summary and reporter are required")
	}
	compartment := defaultCompartment(in.Compartment)
	sens := defaultSensitivity(in.Sensitivity)
	v, err := s.authorizeEpisodicWrite(vaultName, compartment, sens)
	if err != nil {
		return EpisodicEvent{}, err
	}

	if in.Channel != "" && in.SourceRef != "" {
		existing, found, err := findEventBySourceRef(ctx, v.db, in.Channel, in.SourceRef)
		if err != nil {
			return EpisodicEvent{}, err
		}
		if found {
			existing.Vault = vaultName
			if err := s.logAccess(ctx, v, actor, "add-event", existing.EntityID); err != nil {
				return EpisodicEvent{}, err
			}
			return existing, nil
		}
	}

	now := v.now().UTC()
	occurredAt := in.OccurredAt.UTC()
	if occurredAt.IsZero() {
		occurredAt = now
	}
	eventDate := occurredAt.Format("2006-01-02")

	id, err := newID("evt")
	if err != nil {
		return EpisodicEvent{}, err
	}
	if err := insertEventTx(ctx, v.db, id, eventDate, occurredAt.Unix(), in, compartment, sens, now.Unix(), actor); err != nil {
		return EpisodicEvent{}, err
	}
	if err := s.logAccess(ctx, v, actor, "add-event", in.EntityID); err != nil {
		return EpisodicEvent{}, err
	}

	return EpisodicEvent{
		ID:          id,
		Vault:       vaultName,
		EventDate:   eventDate,
		OccurredAt:  occurredAt,
		EntityID:    in.EntityID,
		Compartment: compartment,
		Sensitivity: sens,
		Channel:     in.Channel,
		SourceRef:   in.SourceRef,
		Summary:     in.Summary,
		DetailsMD:   in.DetailsMD,
		Reporter:    in.Reporter,
		CreatedAt:   now,
	}, nil
}

func insertEventTx(ctx context.Context, db *sql.DB, id, eventDate string, occurredUnix int64, in EventInput, compartment string, sens Sensitivity, nowUnix int64, actor Actor) error {
	tx, err := db.BeginTx(ctx, nil)
	if err != nil {
		return fmt.Errorf("begin add event tx: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit

	var entityID sql.NullString
	if in.EntityID != "" {
		entityID = sql.NullString{String: in.EntityID, Valid: true}
	}
	if _, err := tx.ExecContext(ctx,
		`INSERT INTO episodic_events (id, event_date, occurred_at, entity_id, compartment, sensitivity, channel, source_ref, summary, details_md, reporter, created_at)
		VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
		id, eventDate, occurredUnix, entityID, compartment, string(sens), in.Channel, in.SourceRef, in.Summary, in.DetailsMD, in.Reporter, nowUnix,
	); err != nil {
		return fmt.Errorf("insert episodic event: %w", err)
	}

	if err := upsertFTSDocTx(ctx, tx, id, DocKindEvent, in.EntityID, compartment, sens, in.Summary, in.DetailsMD); err != nil {
		return err
	}
	diff := map[string]any{"event_date": eventDate, "entity_id": in.EntityID, "compartment": compartment, "sensitivity": string(sens)}
	if err := RecordRevision(ctx, tx, "episodic_events", id, "insert", diff, actor.Caller, actor.Reason); err != nil {
		return err
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("commit add event: %w", err)
	}
	return nil
}

func (s *Service) authorizeEpisodicWrite(vaultName, compartment string, sens Sensitivity) (*Vault, error) {
	if v, err := s.authorize(vaultName, compartment, sens, CapabilityWrite); err == nil {
		return v, nil
	}
	return s.authorize(vaultName, compartment, sens, CapabilityWriteEpisodic)
}

func findEventBySourceRef(ctx context.Context, db *sql.DB, channel, sourceRef string) (EpisodicEvent, bool, error) {
	row := db.QueryRowContext(ctx,
		`SELECT id, event_date, occurred_at, entity_id, compartment, sensitivity, channel, source_ref, summary, details_md, reporter, compacted_into_id, created_at
		FROM episodic_events
		WHERE channel = ? AND source_ref = ?
		LIMIT 1`,
		channel, sourceRef,
	)
	e, err := scanEventRow(row)
	if errors.Is(err, sql.ErrNoRows) {
		return EpisodicEvent{}, false, nil
	}
	if err != nil {
		return EpisodicEvent{}, false, fmt.Errorf("lookup event by source_ref: %w", err)
	}
	return e, true, nil
}

// ListEvents returns episodic events matching filter across permitted vaults,
// strictly filtered by profile and excluding compacted events unless requested.
func (s *Service) ListEvents(ctx context.Context, filter EventFilter, actor Actor) ([]EpisodicEvent, error) {
	if !s.policy.HasCapability(CapabilityRead) {
		return nil, fmt.Errorf("%w: profile lacks read capability", ErrForbidden)
	}
	vaultNames, err := s.targetVaults(filter.Vault)
	if err != nil {
		return nil, err
	}

	out := []EpisodicEvent{}
	for _, vaultName := range vaultNames {
		v := s.vaults[vaultName]
		events, err := s.queryVaultEvents(ctx, v, filter)
		if err != nil {
			return nil, err
		}
		for i := range events {
			events[i].Vault = vaultName
		}
		out = append(out, events...)
		if err := s.logAccess(ctx, v, actor, "list-events", filter.EntityID); err != nil {
			return nil, err
		}
		if filter.Limit > 0 && len(out) >= filter.Limit {
			out = out[:filter.Limit]
			break
		}
	}
	return out, nil
}

func (s *Service) queryVaultEvents(ctx context.Context, v *Vault, filter EventFilter) ([]EpisodicEvent, error) {
	compsJSON, sensJSON, err := s.allowedScopeJSON()
	if err != nil {
		return nil, err
	}
	hasSince, sinceUnix := 0, int64(0)
	if !filter.Since.IsZero() {
		hasSince, sinceUnix = 1, filter.Since.UTC().Unix()
	}
	hasUntil, untilUnix := 0, int64(0)
	if !filter.Until.IsZero() {
		hasUntil, untilUnix = 1, filter.Until.UTC().Unix()
	}
	includeCompacted := 0
	if filter.IncludeCompacted {
		includeCompacted = 1
	}
	limit := filter.Limit
	if limit <= 0 {
		limit = -1
	}

	rows, err := v.db.QueryContext(ctx,
		`SELECT id, event_date, occurred_at, entity_id, compartment, sensitivity, channel, source_ref, summary, details_md, reporter, compacted_into_id, created_at
		FROM episodic_events
		WHERE compartment IN (SELECT value FROM json_each(?1))
		  AND sensitivity IN (SELECT value FROM json_each(?2))
		  AND (?3 = 0 OR occurred_at >= ?4)
		  AND (?5 = 0 OR occurred_at <= ?6)
		  AND (?7 = '' OR entity_id = ?7)
		  AND (?8 = '' OR channel = ?8)
		  AND (?9 = 1 OR compacted_into_id IS NULL)
		ORDER BY occurred_at DESC, id DESC
		LIMIT ?10`,
		compsJSON, sensJSON, hasSince, sinceUnix, hasUntil, untilUnix, filter.EntityID, filter.Channel, includeCompacted, limit,
	)
	if err != nil {
		return nil, fmt.Errorf("query episodic events: %w", err)
	}
	defer rows.Close()

	out := []EpisodicEvent{}
	for rows.Next() {
		e, err := scanEventRow(rows)
		if err != nil {
			return nil, fmt.Errorf("scan episodic event: %w", err)
		}
		out = append(out, e)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate episodic events: %w", err)
	}
	return out, nil
}

func scanEventRow(s rowScanner) (EpisodicEvent, error) {
	var e EpisodicEvent
	var occurredUnix, createdUnix int64
	var entityID, compactedInto sql.NullString

	if err := s.Scan(
		&e.ID, &e.EventDate, &occurredUnix, &entityID, &e.Compartment, &e.Sensitivity,
		&e.Channel, &e.SourceRef, &e.Summary, &e.DetailsMD, &e.Reporter, &compactedInto, &createdUnix,
	); err != nil {
		return EpisodicEvent{}, fmt.Errorf("scan episodic event row: %w", err)
	}
	if entityID.Valid {
		e.EntityID = entityID.String
	}
	if compactedInto.Valid {
		e.CompactedIntoID = compactedInto.String
	}
	e.OccurredAt = time.Unix(occurredUnix, 0).UTC()
	e.CreatedAt = time.Unix(createdUnix, 0).UTC()
	return e, nil
}
