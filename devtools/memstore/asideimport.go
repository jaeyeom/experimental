package memstore

import (
	"context"
	"encoding/json"
	"fmt"
	"io/fs"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"time"

	"github.com/pelletier/go-toml/v2"
	"gopkg.in/yaml.v3"
)

// Import modes for classification rules.
const (
	ImportModeDirect = "direct"
	ImportModeTriage = "triage"
)

// ImportRule defines one TOML [[rule]] entry mapping Aside relative paths to
// a target vault, compartment, sensitivity, and import mode.
type ImportRule struct {
	Glob        string `toml:"glob"`
	Vault       string `toml:"vault"`
	Compartment string `toml:"compartment"`
	Sensitivity string `toml:"sensitivity"`
	Mode        string `toml:"mode"`
}

type importRulesFile struct {
	Rules []ImportRule `toml:"rule"`
}

// ImportOptions configures ImportAside.
type ImportOptions struct {
	Vault     string
	RulesPath string
	DryRun    bool
	Profile   string
	Actor     Actor
	Now       func() time.Time
}

// ImportReport summarizes the items parsed, written directly, or staged in
// triage_inbox by ImportAside.
type ImportReport struct {
	SrcDir         string   `json:"srcDir"`
	DefaultVault   string   `json:"defaultVault"`
	DryRun         bool     `json:"dryRun"`
	FilesScanned   int      `json:"filesScanned"`
	Entities       int      `json:"entities"`
	Facts          int      `json:"facts"`
	NoteBlocks     int      `json:"noteBlocks"`
	EpisodicEvents int      `json:"episodicEvents"`
	DirectWrites   int      `json:"directWrites"`
	TriageStaged   int      `json:"triageStaged"`
	TriageIDs      []string `json:"triageIds,omitempty"`
}

type asideFrontmatter struct {
	Title    string   `yaml:"title"`
	Aliases  []string `yaml:"aliases"`
	Summary  string   `yaml:"summary"`
	EntityID string   `yaml:"entity_id"`
	PinnedL1 bool     `yaml:"pinned_l1"`
}

type classifiedImportItem struct {
	relPath     string
	vault       string
	compartment string
	sensitivity Sensitivity
	mode        string
	entityID    string
	payload     TriagePayload
}

type resolvedRule struct {
	vault       string
	compartment string
	sensitivity Sensitivity
	mode        string
}

var (
	attrBulletRe    = regexp.MustCompile(`^-\s+\*\*([^*]+)\*\*:\s*(.*)$`)
	episodicEntryRe = regexp.MustCompile(`^-\s+\*\*(\d{2}:\d{2}(?::\d{2})?Z?)\*\*(?:\s+\[([^\]]+)\])?(?:\s+\(([^)]+)\))?:\s*(.*)$`)
	memoryLinkRe    = regexp.MustCompile(`^-\s+\[[^\]]+\]\(([^)]+)\)`)
	dateSlugRe      = regexp.MustCompile(`^\d{4}-\d{2}-\d{2}$`)
	nonSlugRe       = regexp.MustCompile(`[^a-z0-9]+`)
)

// ImportAside scans an Aside-format Markdown directory tree at srcDir,
// parses entities, structured attributes, note blocks, and episodic logs,
// and either writes matched direct items or stages items into triage_inbox.
func ImportAside(ctx context.Context, cfg Config, srcDir string, opts ImportOptions) (ImportReport, error) {
	if srcDir == "" {
		return ImportReport{}, fmt.Errorf("source directory is required")
	}
	if opts.Vault == "" {
		return ImportReport{}, fmt.Errorf("default vault is required")
	}
	if _, ok := cfg.Vaults[opts.Vault]; !ok {
		return ImportReport{}, fmt.Errorf("unknown default vault %q", opts.Vault)
	}
	rules, err := loadImportRules(opts.RulesPath)
	if err != nil {
		return ImportReport{}, err
	}

	items, filesScanned, err := scanAsideDirectory(srcDir, cfg, opts.Vault, rules)
	if err != nil {
		return ImportReport{}, err
	}

	report := buildImportReport(srcDir, opts, filesScanned, items)
	if opts.DryRun {
		return report, nil
	}

	triageIDs, err := executeAsideImport(ctx, cfg, items, opts)
	if err != nil {
		return ImportReport{}, err
	}
	report.TriageIDs = triageIDs
	return report, nil
}

func buildImportReport(srcDir string, opts ImportOptions, filesScanned int, items []classifiedImportItem) ImportReport {
	report := ImportReport{
		SrcDir:       srcDir,
		DefaultVault: opts.Vault,
		DryRun:       opts.DryRun,
		FilesScanned: filesScanned,
	}
	for _, it := range items {
		if it.payload.Entity != nil {
			report.Entities++
		}
		report.Facts += len(it.payload.Facts)
		report.NoteBlocks += len(it.payload.NoteBlocks)
		report.EpisodicEvents += len(it.payload.Events)
		if it.mode == ImportModeDirect {
			report.DirectWrites++
		} else {
			report.TriageStaged++
		}
	}
	return report
}

func loadImportRules(rulesPath string) ([]ImportRule, error) {
	if rulesPath == "" {
		return nil, nil
	}
	data, err := os.ReadFile(rulesPath)
	if err != nil {
		return nil, fmt.Errorf("read rules %s: %w", rulesPath, err)
	}
	var parsed importRulesFile
	if err := toml.Unmarshal(data, &parsed); err != nil {
		return nil, fmt.Errorf("parse rules %s: %w", rulesPath, err)
	}
	for i := range parsed.Rules {
		r := &parsed.Rules[i]
		if r.Glob == "" {
			return nil, fmt.Errorf("rules %s: rule %d has empty glob", rulesPath, i)
		}
		if r.Mode == "" {
			r.Mode = ImportModeTriage
		}
		if r.Mode != ImportModeDirect && r.Mode != ImportModeTriage {
			return nil, fmt.Errorf("rules %s: rule %d has invalid mode %q", rulesPath, i, r.Mode)
		}
		if r.Sensitivity != "" {
			if _, ok := Sensitivity(r.Sensitivity).rank(); !ok {
				return nil, fmt.Errorf("rules %s: rule %d has invalid sensitivity %q", rulesPath, i, r.Sensitivity)
			}
		}
	}
	return parsed.Rules, nil
}

func classifyRelPath(relPath, defaultVault string, rules []ImportRule, cfg Config) (resolvedRule, error) {
	res := resolvedRule{
		vault:       defaultVault,
		compartment: "general",
		sensitivity: SensitivityNormal,
		mode:        ImportModeTriage,
	}
	for _, r := range rules {
		matched, err := path.Match(r.Glob, relPath)
		if err != nil {
			return resolvedRule{}, fmt.Errorf("match glob %q: %w", r.Glob, err)
		}
		if !matched {
			continue
		}
		if r.Vault != "" {
			res.vault = r.Vault
		}
		if r.Compartment != "" {
			res.compartment = r.Compartment
		}
		if r.Sensitivity != "" {
			res.sensitivity = Sensitivity(r.Sensitivity)
		}
		res.mode = r.Mode
		break
	}
	if _, ok := cfg.Vaults[res.vault]; !ok {
		return resolvedRule{}, fmt.Errorf("rule for %q references unknown vault %q", relPath, res.vault)
	}
	return res, nil
}

func scanAsideDirectory(srcDir string, cfg Config, defaultVault string, rules []ImportRule) ([]classifiedImportItem, int, error) {
	absSrc, err := filepath.Abs(srcDir)
	if err != nil {
		return nil, 0, fmt.Errorf("resolve source directory %q: %w", srcDir, err)
	}
	relPaths, err := collectMarkdownRelPaths(absSrc, srcDir)
	if err != nil {
		return nil, 0, err
	}

	pinnedPaths, hasUserFile, filesScanned, err := collectAsideMetadata(absSrc, relPaths)
	if err != nil {
		return nil, 0, err
	}

	seenEntities := make(map[string]bool)
	var items []classifiedImportItem
	for _, rel := range relPaths {
		if rel == "MEMORY.md" || rel == "TAXONOMY.md" {
			continue
		}
		if rel == "USER.md" && hasUserFile {
			filesScanned++
			continue
		}
		item, ok, err := parseAsideFile(absSrc, rel, pinnedPaths[rel], cfg, defaultVault, rules)
		if err != nil {
			return nil, 0, err
		}
		if !ok {
			continue
		}
		filesScanned++
		if item.entityID != "" {
			if seenEntities[item.entityID] {
				continue
			}
			seenEntities[item.entityID] = true
		}
		items = append(items, item)
	}
	return items, filesScanned, nil
}

func collectMarkdownRelPaths(absSrc, srcDir string) ([]string, error) {
	var relPaths []string
	err := filepath.WalkDir(absSrc, func(p string, d fs.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if d.IsDir() {
			if strings.HasPrefix(d.Name(), ".") && p != absSrc {
				return filepath.SkipDir
			}
			return nil
		}
		if strings.HasPrefix(d.Name(), ".") || !strings.HasSuffix(d.Name(), ".md") {
			return nil
		}
		rel, err := filepath.Rel(absSrc, p)
		if err != nil {
			return fmt.Errorf("relative path for %q: %w", p, err)
		}
		relPaths = append(relPaths, filepath.ToSlash(rel))
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("scan aside directory %s: %w", srcDir, err)
	}
	slices.Sort(relPaths)
	return relPaths, nil
}

func collectAsideMetadata(absSrc string, relPaths []string) (map[string]bool, bool, int, error) {
	pinnedPaths := make(map[string]bool)
	hasUserFile := false
	filesScanned := 0

	for _, rel := range relPaths {
		if rel == "MEMORY.md" {
			data, err := os.ReadFile(filepath.Join(absSrc, filepath.FromSlash(rel)))
			if err != nil {
				return nil, false, 0, fmt.Errorf("read %s: %w", rel, err)
			}
			for _, p := range parseMemoryPinnedPaths(string(data)) {
				pinnedPaths[p] = true
			}
			filesScanned++
			continue
		}
		if strings.HasPrefix(rel, "users/") && strings.HasSuffix(rel, ".md") {
			hasUserFile = true
		}
	}
	return pinnedPaths, hasUserFile, filesScanned, nil
}

func parseAsideFile(absSrc, rel string, pinned bool, cfg Config, defaultVault string, rules []ImportRule) (classifiedImportItem, bool, error) {
	data, err := os.ReadFile(filepath.Join(absSrc, filepath.FromSlash(rel)))
	if err != nil {
		return classifiedImportItem{}, false, fmt.Errorf("read %s: %w", rel, err)
	}
	content := string(data)

	cls, err := classifyRelPath(rel, defaultVault, rules, cfg)
	if err != nil {
		return classifiedImportItem{}, false, err
	}

	if date, isEpisodic := parseEpisodicRelPath(rel); isEpisodic {
		events := parseEpisodicMarkdown(date, rel, content)
		return classifiedImportItem{
			relPath:     rel,
			vault:       cls.vault,
			compartment: cls.compartment,
			sensitivity: cls.sensitivity,
			mode:        cls.mode,
			payload:     TriagePayload{Events: events},
		}, true, nil
	}

	kind, slug, ok := parseEntityRelPath(rel)
	if !ok {
		return classifiedImportItem{}, false, nil
	}
	payload, entityID, err := parseEntityMarkdown(kind, slug, rel, content, pinned)
	if err != nil {
		return classifiedImportItem{}, false, err
	}
	return classifiedImportItem{
		relPath:     rel,
		vault:       cls.vault,
		compartment: cls.compartment,
		sensitivity: cls.sensitivity,
		mode:        cls.mode,
		entityID:    entityID,
		payload:     payload,
	}, true, nil
}

func parseEntityRelPath(rel string) (string, string, bool) {
	if rel == "USER.md" {
		return "user", "user", true
	}
	parts := strings.Split(rel, "/")
	if len(parts) == 2 && strings.HasSuffix(parts[1], ".md") {
		kind := folderToEntityKind(parts[0])
		if kind == "" {
			return "", "", false
		}
		slug := strings.TrimSuffix(parts[1], ".md")
		if slug == "" {
			return "", "", false
		}
		return kind, slug, true
	}
	if len(parts) == 3 && parts[0] == "routines" && parts[2] == "MEMORY.md" && parts[1] != "" {
		return "routine", parts[1], true
	}
	return "", "", false
}

func folderToEntityKind(dir string) string {
	switch dir {
	case "people":
		return "person"
	case "companies":
		return "company"
	case "sites":
		return "site"
	case "projects":
		return "project"
	case "concepts":
		return "concept"
	case "users":
		return "user"
	case "agent":
		return "agent"
	default:
		return ""
	}
}

func parseEpisodicRelPath(rel string) (string, bool) {
	rest, ok := strings.CutPrefix(rel, "episodic/")
	if !ok || !strings.HasSuffix(rest, ".md") {
		return "", false
	}
	date := strings.TrimSuffix(rest, ".md")
	if !dateSlugRe.MatchString(date) {
		return "", false
	}
	return date, true
}

func parseMemoryPinnedPaths(content string) []string {
	var paths []string
	for _, line := range strings.Split(content, "\n") {
		m := memoryLinkRe.FindStringSubmatch(strings.TrimSpace(line))
		if len(m) == 2 {
			paths = append(paths, strings.TrimSpace(m[1]))
		}
	}
	return paths
}

func parseEntityMarkdown(kind, slug, relPath, content string, pinned bool) (TriagePayload, string, error) {
	fm, body, err := splitFrontmatter(content)
	if err != nil {
		return TriagePayload{}, "", fmt.Errorf("parse frontmatter in %s: %w", relPath, err)
	}
	preamble, sections := splitMarkdownSections(body)
	h1Title, summary := extractPreambleTitleAndSummary(preamble)

	slug, title, summary := resolveEntityFields(kind, slug, relPath, fm, h1Title, summary)
	entityID := kind + ":" + slug
	aliases := fm.Aliases
	if aliases == nil {
		aliases = []string{}
	}

	facts, notes := parseEntitySections(sections, entityID, relPath)
	return TriagePayload{
		Entity: &EntityInput{
			Kind:     kind,
			Slug:     slug,
			Title:    title,
			Summary:  summary,
			PinnedL1: pinned || fm.PinnedL1,
			Aliases:  aliases,
		},
		Facts:      facts,
		NoteBlocks: notes,
	}, entityID, nil
}

func resolveEntityFields(kind, slug, relPath string, fm asideFrontmatter, h1Title, summary string) (string, string, string) {
	if kind == "user" && slug == "user" && strings.HasPrefix(fm.EntityID, "user:") {
		if s := strings.TrimPrefix(fm.EntityID, "user:"); s != "" {
			slug = s
		}
	}
	title := strings.TrimSpace(fm.Title)
	if title == "" {
		title = h1Title
	}
	if title == "" {
		title = slug
	}
	if kind == "user" && relPath == "USER.md" && fm.EntityID == "" && h1Title != "" {
		if s := slugifyHeading(h1Title); s != "" {
			slug = s
		}
	}
	if fm.Summary != "" {
		summary = fm.Summary
	}
	return slug, title, summary
}

func parseEntitySections(sections []mdSection, entityID, relPath string) ([]FactInput, []NoteBlockInput) {
	var facts []FactInput
	var notes []NoteBlockInput
	usedKeys := make(map[string]int)

	for idx, sec := range sections {
		secFacts, remainingBody := extractSectionFacts(sec.body, entityID, relPath)
		facts = append(facts, secFacts...)
		if remainingBody == "" && len(secFacts) > 0 {
			continue
		}
		secKey := uniqueSectionKey(sec.heading, usedKeys)
		notes = append(notes, NoteBlockInput{
			EntityID:   entityID,
			SectionKey: secKey,
			Heading:    sec.heading,
			SortOrder:  (idx + 1) * 10,
			BodyMD:     remainingBody,
		})
	}
	return facts, notes
}

func splitFrontmatter(content string) (asideFrontmatter, string, error) {
	normalized := strings.ReplaceAll(content, "\r\n", "\n")
	if !strings.HasPrefix(normalized, "---\n") {
		return asideFrontmatter{}, normalized, nil
	}
	rest := normalized[len("---\n"):]
	idx := strings.Index(rest, "\n---\n")
	endOffset := len("\n---\n")
	if idx < 0 {
		if strings.HasSuffix(rest, "\n---") {
			idx = len(rest) - len("\n---")
			endOffset = len("\n---")
		} else {
			return asideFrontmatter{}, normalized, nil
		}
	}
	yamlRaw := rest[:idx]
	body := rest[idx+endOffset:]
	var fm asideFrontmatter
	if err := yaml.Unmarshal([]byte(yamlRaw), &fm); err != nil {
		return asideFrontmatter{}, "", fmt.Errorf("unmarshal yaml frontmatter: %w", err)
	}
	return fm, body, nil
}

type mdSection struct {
	heading string
	body    string
}

func splitMarkdownSections(body string) (string, []mdSection) {
	lines := strings.Split(strings.ReplaceAll(body, "\r\n", "\n"), "\n")
	var preamble []string
	var sections []mdSection
	inSection := false
	var curHeading string
	var curLines []string
	inFence := false

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "```") || strings.HasPrefix(trimmed, "~~~") {
			inFence = !inFence
		}
		if !inFence && strings.HasPrefix(line, "## ") {
			if inSection {
				sections = append(sections, mdSection{
					heading: curHeading,
					body:    strings.TrimSpace(strings.Join(curLines, "\n")),
				})
			}
			inSection = true
			curHeading = strings.TrimSpace(strings.TrimPrefix(line, "## "))
			curLines = nil
			continue
		}
		if inSection {
			curLines = append(curLines, line)
		} else {
			preamble = append(preamble, line)
		}
	}
	if inSection {
		sections = append(sections, mdSection{
			heading: curHeading,
			body:    strings.TrimSpace(strings.Join(curLines, "\n")),
		})
	}
	return strings.TrimSpace(strings.Join(preamble, "\n")), sections
}

func extractPreambleTitleAndSummary(preamble string) (string, string) {
	if preamble == "" {
		return "", ""
	}
	lines := strings.Split(preamble, "\n")
	var title string
	var rest []string
	foundTitle := false
	for _, line := range lines {
		if !foundTitle && strings.HasPrefix(line, "# ") {
			title = strings.TrimSpace(strings.TrimPrefix(line, "# "))
			foundTitle = true
			continue
		}
		rest = append(rest, line)
	}
	return title, strings.TrimSpace(strings.Join(rest, "\n"))
}

func extractSectionFacts(secBody, entityID, relPath string) ([]FactInput, string) {
	if secBody == "" {
		return nil, ""
	}
	var facts []FactInput
	var remaining []string
	inFence := false
	for _, line := range strings.Split(secBody, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "```") || strings.HasPrefix(trimmed, "~~~") {
			inFence = !inFence
			remaining = append(remaining, line)
			continue
		}
		if !inFence {
			if m := attrBulletRe.FindStringSubmatch(trimmed); len(m) == 3 {
				key := strings.TrimSpace(m[1])
				val := strings.TrimSpace(m[2])
				valJSON, valType := encodeFactValue(val)
				facts = append(facts, FactInput{
					EntityID:   entityID,
					Key:        key,
					ValueJSON:  valJSON,
					ValueType:  valType,
					SourceType: "aside_import",
					SourceRef:  relPath,
				})
				continue
			}
		}
		remaining = append(remaining, line)
	}
	return facts, strings.TrimSpace(strings.Join(remaining, "\n"))
}

func encodeFactValue(raw string) (string, string) {
	if raw != "" && json.Valid([]byte(raw)) {
		var v any
		if err := json.Unmarshal([]byte(raw), &v); err == nil {
			switch v.(type) {
			case float64:
				return raw, "number"
			case bool:
				return raw, "bool"
			case []any, map[string]any:
				return raw, "json"
			}
		}
	}
	b, _ := json.Marshal(raw)
	return string(b), "string"
}

func uniqueSectionKey(heading string, used map[string]int) string {
	base := slugifyHeading(heading)
	if base == "" {
		base = "section"
	}
	count := used[base]
	used[base] = count + 1
	if count == 0 {
		return base
	}
	return fmt.Sprintf("%s-%d", base, count+1)
}

func slugifyHeading(s string) string {
	lower := strings.ToLower(strings.TrimSpace(s))
	slug := nonSlugRe.ReplaceAllString(lower, "-")
	return strings.Trim(slug, "-")
}

func parseEpisodicMarkdown(date, relPath, content string) []EventInput {
	lines := strings.Split(strings.ReplaceAll(content, "\r\n", "\n"), "\n")
	var events []EventInput
	var cur *EventInput
	var details []string

	flush := func() {
		if cur == nil {
			return
		}
		cur.DetailsMD = strings.TrimSpace(strings.Join(details, "\n"))
		events = append(events, *cur)
		cur = nil
		details = nil
	}

	for idx, line := range lines {
		trimmed := strings.TrimSpace(line)
		if trimmed == "" {
			if cur != nil && len(details) > 0 {
				details = append(details, "")
			}
			continue
		}
		if strings.HasPrefix(line, "# ") && cur == nil {
			continue
		}
		if strings.HasPrefix(line, "  ") && cur != nil {
			details = append(details, strings.TrimPrefix(line, "  "))
			continue
		}
		if strings.HasPrefix(trimmed, "- ") {
			flush()
			ev := parseEpisodicLine(date, relPath, idx+1, trimmed)
			cur = &ev
			continue
		}
		if cur != nil {
			details = append(details, trimmed)
		}
	}
	flush()
	return events
}

func parseEpisodicLine(date, relPath string, lineNum int, line string) EventInput {
	sourceRef := fmt.Sprintf("%s#L%d", relPath, lineNum)
	if m := episodicEntryRe.FindStringSubmatch(line); len(m) == 5 {
		occurredAt := parseEpisodicTimestamp(date, m[1])
		return EventInput{
			OccurredAt: occurredAt,
			EntityID:   strings.TrimSpace(m[2]),
			Channel:    strings.TrimSpace(m[3]),
			Summary:    strings.TrimSpace(m[4]),
			SourceRef:  sourceRef,
			Reporter:   "aside_import",
		}
	}
	summary := strings.TrimSpace(strings.TrimPrefix(line, "- "))
	return EventInput{
		OccurredAt: parseEpisodicTimestamp(date, "12:00:00Z"),
		Summary:    summary,
		SourceRef:  sourceRef,
		Reporter:   "aside_import",
	}
}

func parseEpisodicTimestamp(date, timePart string) time.Time {
	clean := strings.TrimSuffix(timePart, "Z")
	if len(clean) == 5 {
		clean += ":00"
	}
	if t, err := time.Parse("2006-01-02 15:04:05", date+" "+clean); err == nil {
		return t.UTC()
	}
	t, _ := time.Parse("2006-01-02", date)
	return t.UTC()
}

func executeAsideImport(ctx context.Context, cfg Config, items []classifiedImportItem, opts ImportOptions) ([]string, error) {
	vaults := make(map[string]*Vault)
	defer closeVaults(vaults)

	nowFn := opts.Now
	if nowFn == nil {
		nowFn = time.Now
	}
	actor := opts.Actor
	if actor.Caller == "" {
		actor.Caller = "aside_import"
	}
	if actor.Reason == "" {
		actor.Reason = "import-aside"
	}

	var triageIDs []string
	for _, it := range items {
		v, ok := vaults[it.vault]
		if !ok {
			var err error
			v, err = OpenVault(cfg.Vaults[it.vault])
			if err != nil {
				return nil, fmt.Errorf("open vault %q: %w", it.vault, err)
			}
			vaults[it.vault] = v
		}

		now := nowFn().UTC()
		if it.mode == ImportModeDirect {
			if err := writeDirectImportItem(ctx, v, it, now, actor, opts.Profile); err != nil {
				return nil, err
			}
			continue
		}
		triID, err := stageTriageImportItem(ctx, v, it, now, actor, opts.Profile)
		if err != nil {
			return nil, err
		}
		triageIDs = append(triageIDs, triID)
	}
	return triageIDs, nil
}

func writeDirectImportItem(ctx context.Context, v *Vault, it classifiedImportItem, now time.Time, actor Actor, profile string) error {
	tx, err := v.db.BeginTx(ctx, nil)
	if err != nil {
		return fmt.Errorf("begin direct import tx for %s: %w", it.relPath, err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit

	targetID, err := applyPayloadTx(ctx, tx, it.payload, it.entityID, it.compartment, it.sensitivity, now, actor)
	if err != nil {
		return fmt.Errorf("apply direct import %s: %w", it.relPath, err)
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("commit direct import %s: %w", it.relPath, err)
	}
	return v.LogAccess(ctx, actor.Caller, profile, "import-aside-direct", targetID, actor.Reason)
}

func stageTriageImportItem(ctx context.Context, v *Vault, it classifiedImportItem, now time.Time, actor Actor, profile string) (string, error) {
	rawPayload, err := json.Marshal(it.payload)
	if err != nil {
		return "", fmt.Errorf("marshal triage payload for %s: %w", it.relPath, err)
	}
	id, err := newID("tri")
	if err != nil {
		return "", err
	}
	in := TriageInput{
		Source:              "aside_import",
		ProposedCompartment: it.compartment,
		ProposedSensitivity: it.sensitivity,
		TargetEntityID:      it.entityID,
		PayloadJSON:         string(rawPayload),
	}

	tx, err := v.db.BeginTx(ctx, nil)
	if err != nil {
		return "", fmt.Errorf("begin triage import tx for %s: %w", it.relPath, err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after commit

	if err := insertTriageRowTx(ctx, tx, id, in, it.compartment, it.sensitivity, now.Unix(), actor); err != nil {
		return "", err
	}
	if err := tx.Commit(); err != nil {
		return "", fmt.Errorf("commit triage import %s: %w", it.relPath, err)
	}
	if err := v.LogAccess(ctx, actor.Caller, profile, "import-aside-triage", it.entityID, actor.Reason); err != nil {
		return "", err
	}
	return id, nil
}
