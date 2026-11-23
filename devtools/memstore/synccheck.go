package memstore

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"
)

// File classification statuses returned by SyncCheck.
const (
	SyncStatusUnchanged = "unchanged"
	SyncStatusModified  = "modified"
	SyncStatusAdded     = "added"
	SyncStatusDeleted   = "deleted"
)

// SyncSourceExternalEdit is the triage_inbox.source value recorded by SyncCheck.
const SyncSourceExternalEdit = "md_external_edit"

// SyncCheckOptions configures SyncCheck.
type SyncCheckOptions struct {
	Stage bool
	Actor Actor
}

// SyncCheckChange describes one non-unchanged Markdown file detected by SyncCheck.
type SyncCheckChange struct {
	RelPath       string         `json:"relPath"`
	Status        string         `json:"status"`
	EntityID      string         `json:"entityId,omitempty"`
	Vault         string         `json:"vault,omitempty"`
	Proposal      *TriagePayload `json:"proposal,omitempty"`
	TriageID      string         `json:"triageId,omitempty"`
	AlreadyStaged bool           `json:"alreadyStaged,omitempty"`
}

// SyncCheckReport summarizes the drift detected (and optionally staged) by SyncCheck.
type SyncCheckReport struct {
	Profile   string            `json:"profile"`
	Dir       string            `json:"dir"`
	Unchanged int               `json:"unchanged"`
	Modified  []string          `json:"modified,omitempty"`
	Added     []string          `json:"added,omitempty"`
	Deleted   []string          `json:"deleted,omitempty"`
	Changes   []SyncCheckChange `json:"changes,omitempty"`
	Staged    int               `json:"staged"`
	Skipped   int               `json:"skipped"`
	TriageIDs []string          `json:"triageIds,omitempty"`
}

// SyncCheck inspects an exported Markdown directory against its
// .projection-manifest.json, builds structured diff proposals against current
// DB state for modified or added files, and optionally stages them into
// triage_inbox with source = "md_external_edit".
func SyncCheck(ctx context.Context, svc *Service, dir string, opts SyncCheckOptions) (SyncCheckReport, error) {
	if svc == nil {
		return SyncCheckReport{}, fmt.Errorf("service is required")
	}
	if !svc.policy.HasCapability(CapabilityRead) {
		return SyncCheckReport{}, fmt.Errorf("%w: profile %q lacks capability %q", ErrForbidden, svc.policy.name, CapabilityRead)
	}
	if dir == "" {
		return SyncCheckReport{}, fmt.Errorf("directory is required")
	}
	absDir, err := filepath.Abs(dir)
	if err != nil {
		return SyncCheckReport{}, fmt.Errorf("resolve directory %q: %w", dir, err)
	}

	manifest, err := loadRequiredManifest(filepath.Join(absDir, manifestFileName))
	if err != nil {
		return SyncCheckReport{}, err
	}

	diskPaths, err := collectMarkdownRelPaths(absDir, dir)
	if err != nil {
		return SyncCheckReport{}, err
	}

	report := SyncCheckReport{
		Profile: svc.policy.name,
		Dir:     absDir,
	}

	if err := classifyAndDiffFiles(ctx, svc, absDir, manifest, diskPaths, opts.Actor, &report); err != nil {
		return SyncCheckReport{}, err
	}

	if opts.Stage {
		if err := stageSyncChanges(ctx, svc, &report, opts.Actor); err != nil {
			return SyncCheckReport{}, err
		}
	}

	return report, nil
}

func loadRequiredManifest(manifestPath string) (ProjectionManifest, error) {
	data, err := os.ReadFile(manifestPath)
	if errors.Is(err, fs.ErrNotExist) {
		return ProjectionManifest{}, fmt.Errorf("projection manifest %s not found: run memstore export first", manifestPath)
	}
	if err != nil {
		return ProjectionManifest{}, fmt.Errorf("read projection manifest %s: %w", manifestPath, err)
	}
	var m ProjectionManifest
	if err := json.Unmarshal(data, &m); err != nil {
		return ProjectionManifest{}, fmt.Errorf("parse projection manifest %s: %w", manifestPath, err)
	}
	return m, nil
}

func classifyAndDiffFiles(ctx context.Context, svc *Service, absDir string, manifest ProjectionManifest, diskPaths []string, actor Actor, report *SyncCheckReport) error {
	manifestKeys := make([]string, 0, len(manifest.Files))
	for rel := range manifest.Files {
		manifestKeys = append(manifestKeys, rel)
	}
	slices.Sort(manifestKeys)

	for _, rel := range manifestKeys {
		entry := manifest.Files[rel]
		absPath := filepath.Join(absDir, filepath.FromSlash(rel))
		data, err := os.ReadFile(absPath)
		if errors.Is(err, fs.ErrNotExist) {
			report.Deleted = append(report.Deleted, rel)
			report.Changes = append(report.Changes, SyncCheckChange{
				RelPath:  rel,
				Status:   SyncStatusDeleted,
				EntityID: entry.EntityID,
			})
			continue
		}
		if err != nil {
			return fmt.Errorf("read %s: %w", absPath, err)
		}
		if sha256Hex(data) == entry.SHA256 {
			report.Unchanged++
			continue
		}
		report.Modified = append(report.Modified, rel)
		chg, err := buildSyncChange(ctx, svc, rel, SyncStatusModified, entry.EntityID, string(data), actor)
		if err != nil {
			return err
		}
		report.Changes = append(report.Changes, chg)
	}

	for _, rel := range diskPaths {
		if _, tracked := manifest.Files[rel]; tracked {
			continue
		}
		absPath := filepath.Join(absDir, filepath.FromSlash(rel))
		data, err := os.ReadFile(absPath)
		if err != nil {
			return fmt.Errorf("read %s: %w", absPath, err)
		}
		report.Added = append(report.Added, rel)
		chg, err := buildSyncChange(ctx, svc, rel, SyncStatusAdded, "", string(data), actor)
		if err != nil {
			return err
		}
		report.Changes = append(report.Changes, chg)
	}

	return nil
}

func buildSyncChange(ctx context.Context, svc *Service, rel, status, manifestEntityID, content string, actor Actor) (SyncCheckChange, error) {
	chg := SyncCheckChange{
		RelPath:  rel,
		Status:   status,
		EntityID: manifestEntityID,
	}

	if rel == "MEMORY.md" || rel == "TAXONOMY.md" {
		return chg, nil
	}

	if date, isEpisodic := parseEpisodicRelPath(rel); isEpisodic {
		return buildEpisodicSyncChange(ctx, svc, date, rel, status, content, actor)
	}

	kind, slug, ok := parseEntityRelPath(rel)
	if !ok {
		return chg, nil
	}
	return buildEntitySyncChange(ctx, svc, kind, slug, rel, status, manifestEntityID, content)
}

func buildEpisodicSyncChange(ctx context.Context, svc *Service, date, rel, status, content string, actor Actor) (SyncCheckChange, error) {
	parsed := parseEpisodicMarkdown(date, rel, content)
	dayStart, err := time.Parse("2006-01-02", date)
	if err != nil {
		return SyncCheckChange{}, fmt.Errorf("parse episodic date %q: %w", date, err)
	}
	dayEnd := dayStart.Add(24*time.Hour - time.Second)

	existing, err := svc.ListEvents(ctx, EventFilter{Since: dayStart, Until: dayEnd}, actor)
	if err != nil {
		return SyncCheckChange{}, err
	}

	var newEvents []EventInput
	for _, ev := range parsed {
		if !hasMatchingEvent(existing, ev) {
			newEvents = append(newEvents, ev)
		}
	}

	chg := SyncCheckChange{
		RelPath: rel,
		Status:  status,
	}
	if len(svc.order) > 0 {
		chg.Vault = svc.order[0]
	}
	if len(newEvents) > 0 {
		chg.Proposal = &TriagePayload{Events: newEvents}
	}
	return chg, nil
}

func hasMatchingEvent(existing []EpisodicEvent, cand EventInput) bool {
	for _, ex := range existing {
		if ex.OccurredAt.Unix() == cand.OccurredAt.Unix() &&
			ex.EntityID == cand.EntityID &&
			ex.Channel == cand.Channel &&
			ex.Summary == cand.Summary &&
			strings.TrimSpace(ex.DetailsMD) == strings.TrimSpace(cand.DetailsMD) {
			return true
		}
	}
	return false
}

func buildEntitySyncChange(ctx context.Context, svc *Service, kind, slug, rel, status, manifestEntityID, content string) (SyncCheckChange, error) {
	fm, body, err := splitFrontmatter(content)
	if err != nil {
		return SyncCheckChange{}, fmt.Errorf("parse frontmatter in %s: %w", rel, err)
	}
	preamble, sections := splitMarkdownSections(body)
	h1Title, summary := extractPreambleTitleAndSummary(preamble)
	slug, title, summary := resolveEntityFields(kind, slug, rel, fm, h1Title, summary)

	entityID := kind + ":" + slug
	if fm.EntityID != "" {
		entityID = fm.EntityID
	} else if manifestEntityID != "" {
		entityID = manifestEntityID
	}

	view, vaultName, contactsAttrs, found, err := svc.lookupEntityForSync(ctx, entityID)
	if err != nil {
		return SyncCheckChange{}, err
	}

	chg := SyncCheckChange{
		RelPath:  rel,
		Status:   status,
		EntityID: entityID,
		Vault:    vaultName,
	}

	if !found {
		if len(svc.order) > 0 {
			chg.Vault = svc.order[0]
		}
		comp, sens := defaultSyncScope(svc)
		aliases := fm.Aliases
		if aliases == nil {
			aliases = []string{}
		}
		facts, notes := diffEntitySections(sections, entityID, rel, comp, sens, nil, nil, nil)
		chg.Proposal = &TriagePayload{
			Entity: &EntityInput{
				Kind:               kind,
				Slug:               slug,
				Title:              title,
				DefaultCompartment: comp,
				Sensitivity:        sens,
				Summary:            summary,
				PinnedL1:           fm.PinnedL1,
				Aliases:            aliases,
			},
			Facts:      facts,
			NoteBlocks: notes,
		}
		return chg, nil
	}

	proposal := diffExistingEntity(view, title, summary, fm.Aliases, sections, rel, contactsAttrs)
	if proposal != nil {
		chg.Proposal = proposal
	}
	return chg, nil
}

func diffExistingEntity(view EntityView, title, summary string, fmAliases []string, sections []mdSection, rel string, contactsAttrs *ContactsAttributes) *TriagePayload {
	var entityIn *EntityInput
	if title != view.Entity.Title || summary != view.Entity.Summary || !normalizedAliasesEqual(fmAliases, view.Aliases) {
		aliases := fmAliases
		if aliases == nil {
			aliases = []string{}
		}
		entityIn = &EntityInput{
			Kind:               view.Entity.Kind,
			Slug:               view.Entity.Slug,
			Title:              title,
			ContactsRef:        view.Entity.ContactsRef,
			DefaultCompartment: view.Entity.DefaultCompartment,
			Sensitivity:        view.Entity.Sensitivity,
			Summary:            summary,
			PinnedL1:           view.Entity.PinnedL1,
			ArchivedAt:         view.Entity.ArchivedAt,
			Aliases:            aliases,
		}
	}

	dbFacts := make(map[string]string, len(view.Facts))
	for _, f := range view.Facts {
		dbFacts[f.Key] = f.ValueJSON
	}
	dbNotes := make(map[string]NoteBlock, len(view.NoteBlocks))
	for _, nb := range view.NoteBlocks {
		dbNotes[nb.SectionKey] = nb
	}

	facts, notes := diffEntitySections(sections, view.Entity.ID, rel, view.Entity.DefaultCompartment, view.Entity.Sensitivity, dbFacts, dbNotes, contactsAttrs)
	if entityIn == nil && len(facts) == 0 && len(notes) == 0 {
		return nil
	}
	return &TriagePayload{
		Entity:     entityIn,
		Facts:      facts,
		NoteBlocks: notes,
	}
}

func diffEntitySections(sections []mdSection, entityID, rel, comp string, sens Sensitivity, dbFacts map[string]string, dbNotes map[string]NoteBlock, contactsAttrs *ContactsAttributes) ([]FactInput, []NoteBlockInput) {
	var facts []FactInput
	var notes []NoteBlockInput
	usedKeys := make(map[string]int)

	for idx, sec := range sections {
		secFacts, remainingBody := extractSectionFacts(sec.body, entityID, rel)
		facts = append(facts, filterChangedFacts(secFacts, sec.heading, comp, sens, rel, dbFacts, contactsAttrs)...)

		if remainingBody == "" && (len(secFacts) > 0 || sec.heading == "Structured Attributes") {
			continue
		}
		secKey := uniqueSectionKey(sec.heading, usedKeys)
		existingNB, hasNB := dbNotes[secKey]
		if hasNB && existingNB.Heading == sec.heading && strings.TrimSpace(existingNB.BodyMD) == strings.TrimSpace(remainingBody) {
			continue
		}
		sortOrder := (idx + 1) * 10
		if hasNB && existingNB.SortOrder != 0 {
			sortOrder = existingNB.SortOrder
		}
		notes = append(notes, NoteBlockInput{
			EntityID:    entityID,
			SectionKey:  secKey,
			Heading:     sec.heading,
			SortOrder:   sortOrder,
			BodyMD:      remainingBody,
			Compartment: comp,
			Sensitivity: sens,
		})
	}
	return facts, notes
}

func filterChangedFacts(secFacts []FactInput, heading, comp string, sens Sensitivity, rel string, dbFacts map[string]string, contactsAttrs *ContactsAttributes) []FactInput {
	var changed []FactInput
	for _, fIn := range secFacts {
		if strings.HasPrefix(fIn.Key, "link (") && strings.HasSuffix(fIn.Key, ")") {
			continue
		}
		if heading == "Structured Attributes" && isUnchangedContactsFact(fIn, contactsAttrs) {
			continue
		}
		if dbVal, ok := dbFacts[fIn.Key]; ok && dbVal == fIn.ValueJSON {
			continue
		}
		fIn.Compartment = comp
		fIn.Sensitivity = sens
		fIn.SourceType = SyncSourceExternalEdit
		fIn.SourceRef = rel
		changed = append(changed, fIn)
	}
	return changed
}

func isUnchangedContactsFact(f FactInput, c *ContactsAttributes) bool {
	if c == nil {
		return false
	}
	var expected string
	switch f.Key {
	case "team":
		expected = c.Team
	case "role":
		expected = c.Role
	case "timezone":
		expected = c.Timezone
	case "handles":
		expected = strings.Join(c.Handles, ", ")
	case "open_commitments":
		expected = strings.Join(c.OpenCommitments, "; ")
	default:
		return false
	}
	if expected == "" {
		return false
	}
	valJSON, _ := encodeFactValue(expected)
	return f.ValueJSON == valJSON
}

func normalizedAliasesEqual(a, b []string) bool {
	na := normalizeSortedAliases(a)
	nb := normalizeSortedAliases(b)
	return slices.Equal(na, nb)
}

func normalizeSortedAliases(aliases []string) []string {
	out := make([]string, 0, len(aliases))
	for _, a := range aliases {
		if norm := NormalizeAlias(a); norm != "" {
			out = append(out, norm)
		}
	}
	slices.Sort(out)
	return slices.Compact(out)
}

func (s *Service) lookupEntityForSync(ctx context.Context, entityID string) (EntityView, string, *ContactsAttributes, bool, error) {
	compsJSON, sensJSON, err := s.allowedScopeJSON()
	if err != nil {
		return EntityView{}, "", nil, false, err
	}

	for _, vaultName := range s.order {
		v := s.vaults[vaultName]
		entities, err := scanEntityRows(ctx, v.db,
			`SELECT id, kind, slug, title, contacts_ref, default_compartment, sensitivity, summary, pinned_l1, created_at, updated_at, archived_at
			FROM entities
			WHERE id = ?
			  AND default_compartment IN (SELECT value FROM json_each(?))
			  AND sensitivity IN (SELECT value FROM json_each(?))`,
			entityID, compsJSON, sensJSON,
		)
		if err != nil {
			return EntityView{}, "", nil, false, err
		}
		if len(entities) == 0 {
			continue
		}
		e := entities[0]
		aliases, err := loadAliases(ctx, v.db, e.ID)
		if err != nil {
			return EntityView{}, "", nil, false, err
		}
		facts, err := s.queryVaultFacts(ctx, v, FactFilter{Vault: vaultName, EntityID: e.ID})
		if err != nil {
			return EntityView{}, "", nil, false, err
		}
		notes, err := s.queryVaultNoteBlocks(ctx, v, e.ID)
		if err != nil {
			return EntityView{}, "", nil, false, err
		}

		var contactsAttrs *ContactsAttributes
		if e.Kind == "person" && e.ContactsRef != "" && v.config.ContactsConfig != "" {
			attrs, ok, err := loadContactsAttributes(v.config.ContactsConfig, e.ContactsRef, os.Getenv, v.now)
			if err != nil {
				return EntityView{}, "", nil, false, err
			}
			if ok {
				contactsAttrs = &attrs
			}
		}

		return EntityView{
			Vault:      vaultName,
			Entity:     e,
			Aliases:    aliases,
			Facts:      facts,
			NoteBlocks: notes,
		}, vaultName, contactsAttrs, true, nil
	}
	return EntityView{}, "", nil, false, nil
}

func defaultSyncScope(svc *Service) (string, Sensitivity) {
	comp := "general"
	comps := svc.policy.AllowedCompartments()
	if len(comps) > 0 && !slices.Contains(comps, comp) {
		comp = comps[0]
	}
	sens := SensitivityNormal
	if r, ok := sens.rank(); !ok || r > svc.policy.maxRank {
		sens = svc.policy.MaxSensitivity()
	}
	return comp, sens
}

func stageSyncChanges(ctx context.Context, svc *Service, report *SyncCheckReport, actor Actor) error {
	pendingByVault := make(map[string][]TriageItem)

	for i := range report.Changes {
		chg := &report.Changes[i]
		if chg.Proposal == nil {
			continue
		}
		vaultName := chg.Vault
		if vaultName == "" && len(svc.order) > 0 {
			vaultName = svc.order[0]
			chg.Vault = vaultName
		}
		if vaultName == "" {
			return fmt.Errorf("no permitted vault available to stage %s", chg.RelPath)
		}

		comp, sens := proposalScope(svc, chg.Proposal)
		rawPayload, err := json.Marshal(chg.Proposal)
		if err != nil {
			return fmt.Errorf("marshal proposal for %s: %w", chg.RelPath, err)
		}
		payloadJSON := string(rawPayload)

		pending, ok := pendingByVault[vaultName]
		if !ok {
			pending, err = svc.ListTriage(ctx, TriageFilter{Vault: vaultName, Status: TriageStatusPending})
			if err != nil {
				return err
			}
			pendingByVault[vaultName] = pending
		}

		if existingID, dup := findDuplicatePendingTriage(pending, SyncSourceExternalEdit, chg.EntityID, comp, sens, payloadJSON); dup {
			chg.TriageID = existingID
			chg.AlreadyStaged = true
			report.Skipped++
			continue
		}

		item, err := svc.EnqueueTriage(ctx, vaultName, TriageInput{
			Source:              SyncSourceExternalEdit,
			ProposedCompartment: comp,
			ProposedSensitivity: sens,
			TargetEntityID:      chg.EntityID,
			PayloadJSON:         payloadJSON,
			DecideRef:           actor.Reason,
		})
		if err != nil {
			return err
		}
		pendingByVault[vaultName] = append(pendingByVault[vaultName], item)
		chg.TriageID = item.ID
		report.Staged++
		report.TriageIDs = append(report.TriageIDs, item.ID)
	}
	return nil
}

func proposalScope(svc *Service, p *TriagePayload) (string, Sensitivity) {
	if p.Entity != nil && p.Entity.DefaultCompartment != "" && p.Entity.Sensitivity != "" {
		return p.Entity.DefaultCompartment, p.Entity.Sensitivity
	}
	for _, f := range p.Facts {
		if f.Compartment != "" && f.Sensitivity != "" {
			return f.Compartment, f.Sensitivity
		}
	}
	for _, n := range p.NoteBlocks {
		if n.Compartment != "" && n.Sensitivity != "" {
			return n.Compartment, n.Sensitivity
		}
	}
	return defaultSyncScope(svc)
}

func findDuplicatePendingTriage(pending []TriageItem, source, entityID, comp string, sens Sensitivity, payloadJSON string) (string, bool) {
	for _, it := range pending {
		if it.Source == source &&
			it.TargetEntityID == entityID &&
			it.ProposedCompartment == comp &&
			it.ProposedSensitivity == sens &&
			it.PayloadJSON == payloadJSON {
			return it.ID, true
		}
	}
	return "", false
}
