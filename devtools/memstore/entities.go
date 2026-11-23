package memstore

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"
	"time"
)

// Entity represents a row in the entities table.
type Entity struct {
	ID                 string      `json:"id"`
	Kind               string      `json:"kind"`
	Slug               string      `json:"slug"`
	Title              string      `json:"title"`
	ContactsRef        string      `json:"contactsRef,omitempty"`
	DefaultCompartment string      `json:"defaultCompartment"`
	Sensitivity        Sensitivity `json:"sensitivity"`
	Summary            string      `json:"summary,omitempty"`
	PinnedL1           bool        `json:"pinnedL1,omitempty"`
	CreatedAt          time.Time   `json:"createdAt"`
	UpdatedAt          time.Time   `json:"updatedAt"`
	ArchivedAt         *time.Time  `json:"archivedAt,omitempty"`
}

// EntityInput is the input to PutEntity.
type EntityInput struct {
	Kind               string
	Slug               string
	Title              string
	ContactsRef        string
	DefaultCompartment string
	Sensitivity        Sensitivity
	Summary            string
	PinnedL1           bool
	ArchivedAt         *time.Time
	Aliases            []string
}

// EntityLink represents a directed relationship between two entities.
type EntityLink struct {
	FromEntityID string      `json:"fromEntityId"`
	Relation     string      `json:"relation"`
	ToEntityID   string      `json:"toEntityId"`
	Compartment  string      `json:"compartment"`
	Sensitivity  Sensitivity `json:"sensitivity"`
	Note         string      `json:"note,omitempty"`
	CreatedAt    time.Time   `json:"createdAt"`
}

// LinkInput is the input to LinkEntities.
type LinkInput struct {
	FromEntityID string
	Relation     string
	ToEntityID   string
	Compartment  string
	Sensitivity  Sensitivity
	Note         string
}

// EntityView is returned by GetEntity and contains only profile-permitted
// facts, note blocks, and links.
type EntityView struct {
	Vault      string       `json:"vault"`
	Entity     Entity       `json:"entity"`
	Aliases    []string     `json:"aliases,omitempty"`
	Facts      []Fact       `json:"facts,omitempty"`
	NoteBlocks []NoteBlock  `json:"noteBlocks,omitempty"`
	Links      []EntityLink `json:"links,omitempty"`
}

// EntityMatch is returned by ResolveEntity.
type EntityMatch struct {
	Vault  string `json:"vault"`
	Entity Entity `json:"entity"`
	Match  string `json:"match"`
}

// NormalizeAlias normalizes an alias string for case-insensitive,
// whitespace-collapsed lookup.
func NormalizeAlias(s string) string {
	return strings.Join(strings.Fields(strings.ToLower(strings.TrimSpace(s))), " ")
}

// PutEntity upserts an entity with canonical ID <kind>:<slug>, optionally
// replaces its normalized aliases when in.Aliases is non-nil, and records a
// revision and access_log entry.
func (s *Service) PutEntity(ctx context.Context, vaultName string, in EntityInput, actor Actor) (Entity, error) {
	if in.Kind == "" || in.Slug == "" || in.Title == "" {
		return Entity{}, fmt.Errorf("kind, slug, and title are required")
	}
	compartment := defaultCompartment(in.DefaultCompartment)
	sens := defaultSensitivity(in.Sensitivity)
	v, err := s.authorize(vaultName, compartment, sens, CapabilityWrite)
	if err != nil {
		return Entity{}, err
	}

	id := in.Kind + ":" + in.Slug
	now := v.now().UTC()
	nowUnix := now.Unix()

	tx, err := v.db.BeginTx(ctx, nil)
	if err != nil {
		return Entity{}, fmt.Errorf("begin put entity tx: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit

	op, createdUnix, err := upsertEntityRow(ctx, tx, id, in, compartment, sens, nowUnix)
	if err != nil {
		return Entity{}, err
	}
	if in.Aliases != nil {
		if err := replaceAliases(ctx, tx, id, in.Aliases); err != nil {
			return Entity{}, err
		}
	}
	diff := map[string]any{"kind": in.Kind, "slug": in.Slug, "compartment": compartment, "sensitivity": string(sens)}
	if err := RecordRevision(ctx, tx, "entities", id, op, diff, actor.Caller, actor.Reason); err != nil {
		return Entity{}, err
	}
	if err := tx.Commit(); err != nil {
		return Entity{}, fmt.Errorf("commit put entity: %w", err)
	}
	if err := s.logAccess(ctx, v, actor, "put-entity", id); err != nil {
		return Entity{}, err
	}

	return Entity{
		ID:                 id,
		Kind:               in.Kind,
		Slug:               in.Slug,
		Title:              in.Title,
		ContactsRef:        in.ContactsRef,
		DefaultCompartment: compartment,
		Sensitivity:        sens,
		Summary:            in.Summary,
		PinnedL1:           in.PinnedL1,
		CreatedAt:          time.Unix(createdUnix, 0).UTC(),
		UpdatedAt:          now,
		ArchivedAt:         in.ArchivedAt,
	}, nil
}

func upsertEntityRow(ctx context.Context, tx *sql.Tx, id string, in EntityInput, compartment string, sens Sensitivity, nowUnix int64) (string, int64, error) {
	var createdUnix int64
	err := tx.QueryRowContext(ctx, `SELECT created_at FROM entities WHERE id = ?`, id).Scan(&createdUnix)
	op := "update"
	if errors.Is(err, sql.ErrNoRows) {
		op = "insert"
		createdUnix = nowUnix
	} else if err != nil {
		return "", 0, fmt.Errorf("check existing entity %q: %w", id, err)
	}

	var contactsRef sql.NullString
	if in.ContactsRef != "" {
		contactsRef = sql.NullString{String: in.ContactsRef, Valid: true}
	}
	var archivedAt sql.NullInt64
	if in.ArchivedAt != nil {
		archivedAt = sql.NullInt64{Int64: in.ArchivedAt.UTC().Unix(), Valid: true}
	}
	pinned := 0
	if in.PinnedL1 {
		pinned = 1
	}

	if op == "insert" {
		_, err = tx.ExecContext(ctx,
			`INSERT INTO entities (id, kind, slug, title, contacts_ref, default_compartment, sensitivity, summary, pinned_l1, created_at, updated_at, archived_at)
			VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
			id, in.Kind, in.Slug, in.Title, contactsRef, compartment, string(sens), in.Summary, pinned, createdUnix, nowUnix, archivedAt,
		)
	} else {
		_, err = tx.ExecContext(ctx,
			`UPDATE entities
			SET title = ?, contacts_ref = ?, default_compartment = ?, sensitivity = ?, summary = ?, pinned_l1 = ?, updated_at = ?, archived_at = ?
			WHERE id = ?`,
			in.Title, contactsRef, compartment, string(sens), in.Summary, pinned, nowUnix, archivedAt, id,
		)
	}
	if err != nil {
		return "", 0, fmt.Errorf("%s entity %q: %w", op, id, err)
	}
	return op, createdUnix, nil
}

func replaceAliases(ctx context.Context, tx *sql.Tx, entityID string, aliases []string) error {
	if _, err := tx.ExecContext(ctx, `DELETE FROM entity_aliases WHERE entity_id = ?`, entityID); err != nil {
		return fmt.Errorf("clear aliases for %q: %w", entityID, err)
	}
	seen := make(map[string]struct{}, len(aliases))
	for _, a := range aliases {
		norm := NormalizeAlias(a)
		if norm == "" {
			continue
		}
		if _, dup := seen[norm]; dup {
			continue
		}
		seen[norm] = struct{}{}
		if _, err := tx.ExecContext(ctx,
			`INSERT INTO entity_aliases (entity_id, alias, normalized_alias) VALUES (?, ?, ?)`,
			entityID, strings.TrimSpace(a), norm,
		); err != nil {
			return fmt.Errorf("insert alias %q for %q: %w", a, entityID, err)
		}
	}
	return nil
}

// GetEntity returns the entity and only the facts, note blocks, and links
// permitted by the service's profile.
func (s *Service) GetEntity(ctx context.Context, vaultName, id string, actor Actor) (EntityView, error) {
	if !s.policy.HasCapability(CapabilityRead) {
		return EntityView{}, fmt.Errorf("%w: profile lacks read capability", ErrForbidden)
	}
	v, err := s.openVault(vaultName)
	if err != nil {
		return EntityView{}, err
	}

	e, err := scanEntityByID(ctx, v.db, id)
	if errors.Is(err, sql.ErrNoRows) {
		if logErr := s.logAccess(ctx, v, actor, "get-entity", id); logErr != nil {
			return EntityView{}, logErr
		}
		return EntityView{}, fmt.Errorf("%w: entity %q", ErrNotFound, id)
	} else if err != nil {
		return EntityView{}, err
	}

	if err := s.policy.CanAccess(vaultName, e.DefaultCompartment, e.Sensitivity, CapabilityRead); err != nil {
		return EntityView{}, fmt.Errorf("%w: %w", ErrForbidden, err)
	}

	aliases, err := loadAliases(ctx, v.db, id)
	if err != nil {
		return EntityView{}, err
	}
	facts, err := s.queryVaultFacts(ctx, v, FactFilter{Vault: vaultName, EntityID: id})
	if err != nil {
		return EntityView{}, err
	}
	notes, err := s.queryVaultNoteBlocks(ctx, v, id)
	if err != nil {
		return EntityView{}, err
	}
	links, err := s.queryEntityLinks(ctx, v.db, id)
	if err != nil {
		return EntityView{}, err
	}

	if err := s.logAccess(ctx, v, actor, "get-entity", id); err != nil {
		return EntityView{}, err
	}

	return EntityView{
		Vault:      vaultName,
		Entity:     e,
		Aliases:    aliases,
		Facts:      facts,
		NoteBlocks: notes,
		Links:      links,
	}, nil
}

func (s *Service) queryEntityLinks(ctx context.Context, db *sql.DB, entityID string) ([]EntityLink, error) {
	compsJSON, sensJSON, err := s.allowedScopeJSON()
	if err != nil {
		return nil, err
	}

	rows, err := db.QueryContext(ctx,
		`SELECT from_entity_id, relation, to_entity_id, compartment, sensitivity, note, created_at
		FROM entity_links
		WHERE (from_entity_id = ? OR to_entity_id = ?)
		  AND compartment IN (SELECT value FROM json_each(?))
		  AND sensitivity IN (SELECT value FROM json_each(?))
		ORDER BY created_at, from_entity_id, relation, to_entity_id`,
		entityID, entityID, compsJSON, sensJSON,
	)
	if err != nil {
		return nil, fmt.Errorf("query entity links: %w", err)
	}
	defer rows.Close()

	out := []EntityLink{}
	for rows.Next() {
		var l EntityLink
		var createdUnix int64
		if err := rows.Scan(&l.FromEntityID, &l.Relation, &l.ToEntityID, &l.Compartment, &l.Sensitivity, &l.Note, &createdUnix); err != nil {
			return nil, fmt.Errorf("scan entity link: %w", err)
		}
		l.CreatedAt = time.Unix(createdUnix, 0).UTC()
		out = append(out, l)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate entity links: %w", err)
	}
	return out, nil
}

// LinkEntities creates or updates a directed link between two entities in a
// vault, enforcing write policy and recording a revision and access log.
func (s *Service) LinkEntities(ctx context.Context, vaultName string, in LinkInput, actor Actor) error {
	if in.FromEntityID == "" || in.Relation == "" || in.ToEntityID == "" {
		return fmt.Errorf("from_entity_id, relation, and to_entity_id are required")
	}
	compartment := defaultCompartment(in.Compartment)
	sens := defaultSensitivity(in.Sensitivity)
	v, err := s.authorize(vaultName, compartment, sens, CapabilityWrite)
	if err != nil {
		return err
	}

	nowUnix := v.now().UTC().Unix()
	tx, err := v.db.BeginTx(ctx, nil)
	if err != nil {
		return fmt.Errorf("begin link entities tx: %w", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit

	_, err = tx.ExecContext(ctx,
		`INSERT INTO entity_links (from_entity_id, relation, to_entity_id, compartment, sensitivity, note, created_at)
		VALUES (?, ?, ?, ?, ?, ?, ?)
		ON CONFLICT(from_entity_id, relation, to_entity_id) DO UPDATE SET
			compartment = excluded.compartment,
			sensitivity = excluded.sensitivity,
			note = excluded.note`,
		in.FromEntityID, in.Relation, in.ToEntityID, compartment, string(sens), in.Note, nowUnix,
	)
	if err != nil {
		return fmt.Errorf("upsert entity link: %w", err)
	}

	rowID := in.FromEntityID + ":" + in.Relation + ":" + in.ToEntityID
	diff := map[string]any{"compartment": compartment, "sensitivity": string(sens)}
	if err := RecordRevision(ctx, tx, "entity_links", rowID, "insert", diff, actor.Caller, actor.Reason); err != nil {
		return err
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("commit link entities: %w", err)
	}
	return s.logAccess(ctx, v, actor, "link-entities", in.FromEntityID)
}

// ResolveEntity searches all profile-permitted vaults in priority order:
// exact ID -> exact normalized alias -> exact slug -> prefix match.
// It returns a single match, ErrNotFound, or *AmbiguousError (wrapping ErrAmbiguous).
func (s *Service) ResolveEntity(ctx context.Context, query string, actor Actor) ([]EntityMatch, error) {
	if !s.policy.HasCapability(CapabilityRead) {
		return nil, fmt.Errorf("%w: profile lacks read capability", ErrForbidden)
	}
	q := strings.TrimSpace(query)
	if q == "" {
		return nil, fmt.Errorf("%w: empty query", ErrNotFound)
	}

	tiers := []struct {
		name  string
		query func(context.Context, *Vault, string) ([]Entity, error)
	}{
		{"exact_id", s.matchExactID},
		{"exact_alias", s.matchExactAlias},
		{"exact_slug", s.matchExactSlug},
		{"prefix", s.matchPrefix},
	}

	var winning []EntityMatch
	for _, tier := range tiers {
		var matches []EntityMatch
		for _, vaultName := range s.order {
			entities, err := tier.query(ctx, s.vaults[vaultName], q)
			if err != nil {
				return nil, err
			}
			for _, e := range entities {
				matches = append(matches, EntityMatch{Vault: vaultName, Entity: e, Match: tier.name})
			}
		}
		if len(matches) > 0 {
			winning = matches
			break
		}
	}

	loggedID := ""
	if len(winning) == 1 {
		loggedID = winning[0].Entity.ID
	}
	for _, vaultName := range s.order {
		if err := s.logAccess(ctx, s.vaults[vaultName], actor, "resolve-entity", loggedID); err != nil {
			return nil, err
		}
	}

	if len(winning) == 0 {
		return nil, fmt.Errorf("%w: no entity matching %q", ErrNotFound, query)
	}
	if len(winning) > 1 {
		return winning, &AmbiguousError{Query: query, Candidates: winning}
	}
	return winning, nil
}

func (s *Service) matchExactID(ctx context.Context, v *Vault, q string) ([]Entity, error) {
	compsJSON, sensJSON, err := s.allowedScopeJSON()
	if err != nil {
		return nil, err
	}
	return scanEntityRows(ctx, v.db,
		`SELECT e.id, e.kind, e.slug, e.title, e.contacts_ref, e.default_compartment, e.sensitivity, e.summary, e.pinned_l1, e.created_at, e.updated_at, e.archived_at
		FROM entities e
		WHERE e.id = ?
		  AND e.default_compartment IN (SELECT value FROM json_each(?))
		  AND e.sensitivity IN (SELECT value FROM json_each(?))
		ORDER BY e.id`,
		q, compsJSON, sensJSON,
	)
}

func (s *Service) matchExactAlias(ctx context.Context, v *Vault, q string) ([]Entity, error) {
	norm := NormalizeAlias(q)
	if norm == "" {
		return nil, nil
	}
	compsJSON, sensJSON, err := s.allowedScopeJSON()
	if err != nil {
		return nil, err
	}
	return scanEntityRows(ctx, v.db,
		`SELECT DISTINCT e.id, e.kind, e.slug, e.title, e.contacts_ref, e.default_compartment, e.sensitivity, e.summary, e.pinned_l1, e.created_at, e.updated_at, e.archived_at
		FROM entities e
		JOIN entity_aliases a ON a.entity_id = e.id
		WHERE a.normalized_alias = ?
		  AND e.default_compartment IN (SELECT value FROM json_each(?))
		  AND e.sensitivity IN (SELECT value FROM json_each(?))
		ORDER BY e.id`,
		norm, compsJSON, sensJSON,
	)
}

func (s *Service) matchExactSlug(ctx context.Context, v *Vault, q string) ([]Entity, error) {
	compsJSON, sensJSON, err := s.allowedScopeJSON()
	if err != nil {
		return nil, err
	}
	return scanEntityRows(ctx, v.db,
		`SELECT e.id, e.kind, e.slug, e.title, e.contacts_ref, e.default_compartment, e.sensitivity, e.summary, e.pinned_l1, e.created_at, e.updated_at, e.archived_at
		FROM entities e
		WHERE e.slug = ?
		  AND e.default_compartment IN (SELECT value FROM json_each(?))
		  AND e.sensitivity IN (SELECT value FROM json_each(?))
		ORDER BY e.id`,
		strings.ToLower(q), compsJSON, sensJSON,
	)
}

func (s *Service) matchPrefix(ctx context.Context, v *Vault, q string) ([]Entity, error) {
	norm := NormalizeAlias(q)
	if norm == "" {
		return nil, nil
	}
	pattern := escapeLike(norm) + "%"
	compsJSON, sensJSON, err := s.allowedScopeJSON()
	if err != nil {
		return nil, err
	}
	return scanEntityRows(ctx, v.db,
		`SELECT DISTINCT e.id, e.kind, e.slug, e.title, e.contacts_ref, e.default_compartment, e.sensitivity, e.summary, e.pinned_l1, e.created_at, e.updated_at, e.archived_at
		FROM entities e
		LEFT JOIN entity_aliases a ON a.entity_id = e.id
		WHERE (LOWER(e.id) LIKE ?1 ESCAPE '\'
		    OR LOWER(e.slug) LIKE ?1 ESCAPE '\'
		    OR LOWER(e.title) LIKE ?1 ESCAPE '\'
		    OR a.normalized_alias LIKE ?1 ESCAPE '\')
		  AND e.default_compartment IN (SELECT value FROM json_each(?2))
		  AND e.sensitivity IN (SELECT value FROM json_each(?3))
		ORDER BY e.id`,
		pattern, compsJSON, sensJSON,
	)
}

func escapeLike(s string) string {
	r := strings.NewReplacer(`\`, `\\`, `%`, `\%`, `_`, `\_`)
	return r.Replace(s)
}

func scanEntityByID(ctx context.Context, db *sql.DB, id string) (Entity, error) {
	row := db.QueryRowContext(ctx,
		`SELECT id, kind, slug, title, contacts_ref, default_compartment, sensitivity, summary, pinned_l1, created_at, updated_at, archived_at
		FROM entities WHERE id = ?`, id)
	return scanEntityRow(row)
}

type rowScanner interface {
	Scan(dest ...any) error
}

func scanEntityRow(s rowScanner) (Entity, error) {
	var e Entity
	var contactsRef sql.NullString
	var pinned int
	var createdUnix, updatedUnix int64
	var archivedUnix sql.NullInt64

	if err := s.Scan(
		&e.ID, &e.Kind, &e.Slug, &e.Title, &contactsRef,
		&e.DefaultCompartment, &e.Sensitivity, &e.Summary, &pinned,
		&createdUnix, &updatedUnix, &archivedUnix,
	); err != nil {
		return Entity{}, fmt.Errorf("scan entity row: %w", err)
	}
	if contactsRef.Valid {
		e.ContactsRef = contactsRef.String
	}
	e.PinnedL1 = pinned != 0
	e.CreatedAt = time.Unix(createdUnix, 0).UTC()
	e.UpdatedAt = time.Unix(updatedUnix, 0).UTC()
	if archivedUnix.Valid {
		t := time.Unix(archivedUnix.Int64, 0).UTC()
		e.ArchivedAt = &t
	}
	return e, nil
}

func scanEntityRows(ctx context.Context, db *sql.DB, q string, args ...any) ([]Entity, error) {
	rows, err := db.QueryContext(ctx, q, args...)
	if err != nil {
		return nil, fmt.Errorf("query entities: %w", err)
	}
	defer rows.Close()

	var out []Entity
	for rows.Next() {
		e, err := scanEntityRow(rows)
		if err != nil {
			return nil, fmt.Errorf("scan entity: %w", err)
		}
		out = append(out, e)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate entities: %w", err)
	}
	return out, nil
}

func loadAliases(ctx context.Context, db *sql.DB, entityID string) ([]string, error) {
	rows, err := db.QueryContext(ctx,
		`SELECT alias FROM entity_aliases WHERE entity_id = ? ORDER BY normalized_alias`, entityID)
	if err != nil {
		return nil, fmt.Errorf("query aliases: %w", err)
	}
	defer rows.Close()

	var out []string
	for rows.Next() {
		var a string
		if err := rows.Scan(&a); err != nil {
			return nil, fmt.Errorf("scan alias: %w", err)
		}
		out = append(out, a)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate aliases: %w", err)
	}
	return out, nil
}
