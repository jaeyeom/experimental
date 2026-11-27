package toolcli

import (
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path"
	"path/filepath"
	"strings"
)

// Agent identifies a supported coding agent.
type Agent string

const (
	// AgentClaude is Claude Code.
	AgentClaude Agent = "claude"
	// AgentCodex is OpenAI Codex.
	AgentCodex Agent = "codex"
	// AgentAntigravity is Antigravity CLI.
	AgentAntigravity Agent = "antigravity"
	// AgentGrok is Grok Build CLI.
	AgentGrok Agent = "grok"
)

// Scope identifies where an agent skill is installed.
type Scope string

const (
	// ScopeUser installs into the user's home directory.
	ScopeUser Scope = "user"
	// ScopeProject installs into the current project directory.
	ScopeProject Scope = "project"
)

// AgentLocation defines the relative skill path under user home or project root.
type AgentLocation struct {
	Agent       Agent
	UserRoot    string
	UserSkill   func(home, tool string) string
	ProjectRoot string
	ProjSkill   func(work, tool string) string
}

// AgentLocations is the canonical table of skill paths for supported agents.
var AgentLocations = []AgentLocation{
	{
		Agent:       AgentClaude,
		UserRoot:    ".claude",
		UserSkill:   func(home, tool string) string { return filepath.Join(home, ".claude", "skills", tool, "SKILL.md") },
		ProjectRoot: ".claude",
		ProjSkill:   func(work, tool string) string { return filepath.Join(work, ".claude", "skills", tool, "SKILL.md") },
	},
	{
		Agent:       AgentCodex,
		UserRoot:    ".agents",
		UserSkill:   func(home, tool string) string { return filepath.Join(home, ".agents", "skills", tool, "SKILL.md") },
		ProjectRoot: ".agents",
		ProjSkill:   func(work, tool string) string { return filepath.Join(work, ".agents", "skills", tool, "SKILL.md") },
	},
	{
		Agent:    AgentAntigravity,
		UserRoot: ".gemini",
		UserSkill: func(home, tool string) string {
			return filepath.Join(home, ".gemini", "config", "skills", tool, "SKILL.md")
		},
		ProjectRoot: ".agents",
		ProjSkill:   func(work, tool string) string { return filepath.Join(work, ".agents", "skills", tool, "SKILL.md") },
	},
	{
		Agent:       AgentGrok,
		UserRoot:    ".grok",
		UserSkill:   func(home, tool string) string { return filepath.Join(home, ".grok", "skills", tool, "SKILL.md") },
		ProjectRoot: ".grok",
		ProjSkill:   func(work, tool string) string { return filepath.Join(work, ".grok", "skills", tool, "SKILL.md") },
	},
}

// RunSkill writes the raw embedded skill markdown to stdout.
func (t *Tool) RunSkill(stdout io.Writer) error {
	if _, err := io.WriteString(stdout, t.SkillText); err != nil {
		return fmt.Errorf("write skill: %w", err)
	}
	return nil
}

func (t *Tool) cmdSkill(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		if err := t.RunSkill(stdout); err != nil {
			return failErr(stderr, err, 1)
		}
		return 0
	}
	switch args[0] {
	case "install":
		return t.cmdSkillInstall(args[1:], stdout, stderr)
	case "uninstall":
		return t.cmdSkillUninstall(args[1:], stdout, stderr)
	case "status":
		return t.cmdSkillStatus(args[1:], stdout, stderr)
	case "ref":
		return t.cmdSkillRef(args[1:], stdout, stderr)
	default:
		return failMsg(stderr, fmt.Sprintf("unknown skill subcommand %q (expected install, uninstall, status, or ref)", args[0]), 2)
	}
}

type agentListFlag []string

func (a *agentListFlag) String() string { return strings.Join(*a, ",") }
func (a *agentListFlag) Set(v string) error {
	*a = append(*a, v)
	return nil
}

func (t *Tool) skills() []Skill {
	var out []Skill
	seen := map[string]bool{}
	if t.SkillText != "" || len(t.ExtraSkills) == 0 {
		out = append(out, Skill{Name: t.Name, Text: t.SkillText, Files: t.SkillFiles})
		seen[t.Name] = true
	}
	for _, s := range t.ExtraSkills {
		if s.Name == "" || s.Text == "" || seen[s.Name] {
			continue
		}
		seen[s.Name] = true
		out = append(out, s)
	}
	return out
}

func validateSkillFilePath(raw string) (string, error) {
	if strings.ContainsRune(raw, '\x00') {
		return "", fmt.Errorf("skill companion file path %q must not contain NUL bytes", raw)
	}
	if strings.TrimSpace(raw) == "" {
		return "", errors.New("skill companion file path must not be empty")
	}
	if strings.TrimSpace(raw) != raw {
		return "", fmt.Errorf("skill companion file path %q must not have leading or trailing whitespace", raw)
	}
	if strings.Contains(raw, "\\") {
		return "", fmt.Errorf("skill companion file path %q must use forward slashes", raw)
	}
	if strings.HasPrefix(raw, "/") || filepath.IsAbs(raw) || filepath.VolumeName(raw) != "" {
		return "", fmt.Errorf("skill companion file path %q must be relative", raw)
	}
	return validateSkillFileSegments(raw)
}

func validateSkillFileSegments(raw string) (string, error) {
	for _, seg := range strings.Split(raw, "/") {
		if seg == "" || seg == "." || seg == ".." {
			return "", fmt.Errorf("invalid skill companion file path %q", raw)
		}
	}
	cleaned := path.Clean(raw)
	if strings.EqualFold(cleaned, "SKILL.md") {
		return "", fmt.Errorf("skill companion file path %q must not be SKILL.md", raw)
	}
	return cleaned, nil
}

func validateSkillFiles(files []SkillFile) error {
	seen := make(map[string]bool, len(files))
	for _, f := range files {
		cleaned, err := validateSkillFilePath(f.Path)
		if err != nil {
			return err
		}
		key := strings.ToLower(cleaned)
		if seen[key] {
			return fmt.Errorf("duplicate skill companion file path %q", f.Path)
		}
		seen[key] = true
	}
	return nil
}

func validateAllSkillFiles(skills []Skill) error {
	for _, sk := range skills {
		if err := validateSkillFiles(sk.Files); err != nil {
			return err
		}
	}
	return nil
}

func companionFilePath(skillFile, rel string) string {
	return filepath.Join(filepath.Dir(skillFile), filepath.FromSlash(path.Clean(rel)))
}

type skillInstallPlan struct {
	skill   Skill
	content string
	targets []skillTarget
}

// SkillInstallOptions configures RunSkillInstall.
type SkillInstallOptions struct {
	Agents []string
	Scope  Scope
	Dir    string
	DryRun bool
	Force  bool
}

// RunSkillInstall installs the embedded skill(s) into the requested agent locations.
func (t *Tool) RunSkillInstall(opts SkillInstallOptions, stdout io.Writer) error {
	skills := t.skills()
	if err := validateAllSkillFiles(skills); err != nil {
		return err
	}
	if opts.Dir != "" && len(skills) > 1 {
		return errors.New("--dir is not supported when multiple skills are configured")
	}

	plans, err := t.planSkillInstall(skills, opts)
	if err != nil {
		return err
	}
	for _, p := range plans {
		if err := applySkillInstallPlan(p, opts, stdout); err != nil {
			return err
		}
	}
	return nil
}

func (t *Tool) planSkillInstall(skills []Skill, opts SkillInstallOptions) ([]skillInstallPlan, error) {
	rev := currentRevLabel(t.buildInfo())
	plans := make([]skillInstallPlan, 0, len(skills))
	for _, sk := range skills {
		targets, err := t.resolveSkillTargetsFor(sk.Name, opts.Agents, opts.Scope, opts.Dir)
		if err != nil {
			return nil, err
		}
		for _, tgt := range targets {
			owned, err := checkSkillFileInstall(tgt.path, sk.Name, opts.Force)
			if err != nil {
				return nil, err
			}
			if err := checkCompanionFilesInstall(tgt.path, sk.Files, owned, opts.Force); err != nil {
				return nil, err
			}
		}
		plans = append(plans, skillInstallPlan{
			skill:   sk,
			content: stampSkill(sk.Text, sk.Name, rev),
			targets: targets,
		})
	}
	return plans, nil
}

func applySkillInstallPlan(p skillInstallPlan, opts SkillInstallOptions, stdout io.Writer) error {
	action := "installed"
	if opts.DryRun {
		action = "would install"
	}
	for _, tgt := range p.targets {
		if !opts.DryRun {
			if err := installSkillFile(tgt.path, p.content, p.skill.Name, opts.Force, false); err != nil {
				return err
			}
			if err := installCompanionFiles(tgt.path, p.skill.Files); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(stdout, "%s %s (%s): %s\n", action, tgt.label, tgt.scope, tgt.path); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
	}
	return nil
}

func (t *Tool) cmdSkillInstall(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("skill install", flag.ContinueOnError)
	fs.SetOutput(stderr)
	var agents agentListFlag
	fs.Var(&agents, "agent", "target agent (claude|codex|antigravity|grok|all)")
	scope := fs.String("scope", string(ScopeUser), "installation scope (user|project)")
	dir := fs.String("dir", "", "override target skill directory")
	dryRun := fs.Bool("dry-run", false, "preview changes without writing")
	force := fs.Bool("force", false, "overwrite existing foreign files")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		return failMsg(stderr, "skill install does not take positional arguments", 2)
	}

	if err := t.RunSkillInstall(SkillInstallOptions{
		Agents: agents,
		Scope:  Scope(*scope),
		Dir:    *dir,
		DryRun: *dryRun,
		Force:  *force,
	}, stdout); err != nil {
		return failErr(stderr, err, 2)
	}
	return 0
}

// SkillUninstallOptions configures RunSkillUninstall.
type SkillUninstallOptions struct {
	Agents []string
	Scope  Scope
	Dir    string
	DryRun bool
}

type skillUninstallPlan struct {
	skill   Skill
	targets []skillTarget
}

// RunSkillUninstall removes skill files stamped by this tool.
func (t *Tool) RunSkillUninstall(opts SkillUninstallOptions, stdout io.Writer) error {
	skills := t.skills()
	if err := validateAllSkillFiles(skills); err != nil {
		return err
	}
	if opts.Dir != "" && len(skills) > 1 {
		return errors.New("--dir is not supported when multiple skills are configured")
	}

	plans, err := t.planSkillUninstall(skills, opts)
	if err != nil {
		return err
	}
	for _, p := range plans {
		if err := applySkillUninstallPlan(p, opts, stdout); err != nil {
			return err
		}
	}
	return nil
}

func (t *Tool) planSkillUninstall(skills []Skill, opts SkillUninstallOptions) ([]skillUninstallPlan, error) {
	plans := make([]skillUninstallPlan, 0, len(skills))
	for _, sk := range skills {
		targets, err := t.resolveSkillTargetsFor(sk.Name, opts.Agents, opts.Scope, opts.Dir)
		if err != nil {
			return nil, err
		}
		for _, tgt := range targets {
			removed, err := uninstallSkillFile(tgt.path, sk.Name, true)
			if err != nil {
				return nil, err
			}
			if removed {
				if err := checkCompanionFilesUninstall(tgt.path, sk.Files); err != nil {
					return nil, err
				}
			}
		}
		plans = append(plans, skillUninstallPlan{skill: sk, targets: targets})
	}
	return plans, nil
}

func applySkillUninstallPlan(p skillUninstallPlan, opts SkillUninstallOptions, stdout io.Writer) error {
	action := "removed"
	if opts.DryRun {
		action = "would remove"
	}
	for _, tgt := range p.targets {
		removed, err := uninstallSkillFile(tgt.path, p.skill.Name, opts.DryRun)
		if err != nil {
			return err
		}
		if !removed {
			if _, err := fmt.Fprintf(stdout, "not installed %s (%s): %s\n", tgt.label, tgt.scope, tgt.path); err != nil {
				return fmt.Errorf("write output: %w", err)
			}
			continue
		}
		if !opts.DryRun {
			if err := uninstallCompanionFiles(tgt.path, p.skill.Files); err != nil {
				return err
			}
			removeEmptySkillDir(filepath.Dir(tgt.path))
		}
		if _, err := fmt.Fprintf(stdout, "%s %s (%s): %s\n", action, tgt.label, tgt.scope, tgt.path); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
	}
	return nil
}

func (t *Tool) cmdSkillUninstall(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("skill uninstall", flag.ContinueOnError)
	fs.SetOutput(stderr)
	var agents agentListFlag
	fs.Var(&agents, "agent", "target agent (claude|codex|antigravity|grok|all)")
	scope := fs.String("scope", string(ScopeUser), "installation scope (user|project)")
	dir := fs.String("dir", "", "override target skill directory")
	dryRun := fs.Bool("dry-run", false, "preview changes without deleting")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		return failMsg(stderr, "skill uninstall does not take positional arguments", 2)
	}

	if err := t.RunSkillUninstall(SkillUninstallOptions{
		Agents: agents,
		Scope:  Scope(*scope),
		Dir:    *dir,
		DryRun: *dryRun,
	}, stdout); err != nil {
		return failErr(stderr, err, 2)
	}
	return 0
}

// SkillStatusEntry describes one agent+scope skill installation state.
type SkillStatusEntry struct {
	Skill            string `json:"skill,omitempty"`
	Agent            Agent  `json:"agent"`
	Scope            Scope  `json:"scope"`
	Path             string `json:"path"`
	Status           string `json:"status"`
	InstalledVersion string `json:"installedVersion,omitempty"`
	InstalledHash    string `json:"installedHash,omitempty"`
}

// SkillStatusOutput is the JSON payload for `<tool> skill status --json`.
type SkillStatusOutput struct {
	Tool        string             `json:"tool"`
	CurrentHash string             `json:"currentHash"`
	Entries     []SkillStatusEntry `json:"entries"`
}

// RunSkillStatus reports the installation status across all known agents and scopes.
func (t *Tool) RunSkillStatus(asJSON bool, stdout io.Writer) error {
	if err := validateAllSkillFiles(t.skills()); err != nil {
		return err
	}
	entries := t.collectSkillStatus()
	out := SkillStatusOutput{
		Tool:        t.Name,
		CurrentHash: skillHash(t.SkillText),
		Entries:     entries,
	}

	if asJSON {
		return writeJSON(stdout, out)
	}

	for _, e := range entries {
		ver := e.InstalledVersion
		if ver == "" {
			ver = "-"
		}
		if _, err := fmt.Fprintf(stdout, "%-12s  %-7s  %-8s  %s  %s\n", e.Agent, e.Scope, e.Status, ver, e.Path); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
	}
	return nil
}

func (t *Tool) cmdSkillStatus(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("skill status", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "output JSON")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		return failMsg(stderr, "skill status does not take positional arguments", 2)
	}

	if err := t.RunSkillStatus(*jsonOut, stdout); err != nil {
		return failErr(stderr, err, 2)
	}
	return 0
}

// SkillRefOptions configures RunSkillRef.
type SkillRefOptions struct {
	Skill string
	Path  string
}

// RunSkillRef prints available companion reference paths or a single embedded reference file.
func (t *Tool) RunSkillRef(opts SkillRefOptions, stdout io.Writer) error {
	skills := t.skills()
	if err := validateAllSkillFiles(skills); err != nil {
		return err
	}
	sk, ok := resolveSkillForRef(skills, t.Name, opts.Skill)
	if !ok {
		return fmt.Errorf("unknown skill %q", opts.Skill)
	}
	if opts.Path == "" {
		for _, f := range sk.Files {
			if _, err := fmt.Fprintln(stdout, f.Path); err != nil {
				return fmt.Errorf("write output: %w", err)
			}
		}
		return nil
	}
	cleaned, err := validateSkillFilePath(opts.Path)
	if err != nil {
		return err
	}
	for _, f := range sk.Files {
		if path.Clean(f.Path) == cleaned {
			if _, err := io.WriteString(stdout, f.Content); err != nil {
				return fmt.Errorf("write skill reference: %w", err)
			}
			return nil
		}
	}
	return fmt.Errorf("unknown skill reference %q", opts.Path)
}

func resolveSkillForRef(skills []Skill, primaryName, requested string) (Skill, bool) {
	if requested == "" {
		if len(skills) == 1 {
			return skills[0], true
		}
		requested = primaryName
	}
	for _, sk := range skills {
		if sk.Name == requested {
			return sk, true
		}
	}
	return Skill{}, false
}

func (t *Tool) cmdSkillRef(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("skill ref", flag.ContinueOnError)
	fs.SetOutput(stderr)
	skillName := fs.String("skill", "", "target skill name (defaults to primary skill)")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() > 1 {
		return failMsg(stderr, "skill ref accepts at most one path argument", 2)
	}
	var refPath string
	if fs.NArg() == 1 {
		refPath = fs.Arg(0)
	}
	if err := t.RunSkillRef(SkillRefOptions{
		Skill: *skillName,
		Path:  refPath,
	}, stdout); err != nil {
		return failErr(stderr, err, 2)
	}
	return 0
}

type skillTarget struct {
	label string
	scope Scope
	path  string
}

func (t *Tool) resolveSkillTargetsFor(skillName string, agents []string, scope Scope, dir string) ([]skillTarget, error) {
	if scope == "" {
		scope = ScopeUser
	}
	if dir != "" {
		return []skillTarget{{
			label: "custom",
			scope: scope,
			path:  filepath.Join(dir, "SKILL.md"),
		}}, nil
	}
	if scope != ScopeUser && scope != ScopeProject {
		return nil, fmt.Errorf("invalid scope %q (expected user or project)", scope)
	}
	if len(agents) == 0 {
		return nil, errors.New("at least one --agent (or --dir) is required")
	}
	baseDir, err := t.scopeBaseDir(scope)
	if err != nil {
		return nil, err
	}

	var targets []skillTarget
	seen := map[string]bool{}
	for _, a := range agents {
		if a == "all" {
			detected := detectAgents(baseDir, scope)
			if len(detected) == 0 {
				return nil, fmt.Errorf("no supported agents found for %s scope", scope)
			}
			for _, loc := range detected {
				p := skillPathForLoc(loc, baseDir, scope, skillName)
				if !seen[p] {
					seen[p] = true
					targets = append(targets, skillTarget{label: string(loc.Agent), scope: scope, path: p})
				}
			}
			continue
		}
		loc, ok := lookupAgentLocation(Agent(a))
		if !ok {
			return nil, fmt.Errorf("unknown agent %q (expected claude, codex, antigravity, grok, or all)", a)
		}
		p := skillPathForLoc(loc, baseDir, scope, skillName)
		if !seen[p] {
			seen[p] = true
			targets = append(targets, skillTarget{label: string(loc.Agent), scope: scope, path: p})
		}
	}
	return targets, nil
}

func (t *Tool) scopeBaseDir(scope Scope) (string, error) {
	if scope == ScopeProject {
		return t.workDir()
	}
	return t.homeDir()
}

func lookupAgentLocation(agent Agent) (AgentLocation, bool) {
	for _, loc := range AgentLocations {
		if loc.Agent == agent {
			return loc, true
		}
	}
	return AgentLocation{}, false
}

func skillPathForLoc(loc AgentLocation, baseDir string, scope Scope, tool string) string {
	if scope == ScopeProject {
		return loc.ProjSkill(baseDir, tool)
	}
	return loc.UserSkill(baseDir, tool)
}

func detectAgents(baseDir string, scope Scope) []AgentLocation {
	var found []AgentLocation
	for _, loc := range AgentLocations {
		root := loc.UserRoot
		if scope == ScopeProject {
			root = loc.ProjectRoot
		}
		info, err := os.Stat(filepath.Join(baseDir, root))
		if err == nil && info.IsDir() {
			found = append(found, loc)
		}
	}
	return found
}

type skillStatusRecord struct {
	entry    SkillStatusEntry
	wantHash string
}

func (t *Tool) collectSkillStatusRecords() []skillStatusRecord {
	home, homeErr := t.homeDir()
	work, workErr := t.workDir()
	skills := t.skills()
	multiSkill := len(skills) > 1

	var records []skillStatusRecord
	for _, sk := range skills {
		wantHash := skillHash(sk.Text)
		skillLabel := ""
		if multiSkill {
			skillLabel = sk.Name
		}
		for _, loc := range AgentLocations {
			if homeErr == nil {
				entry := inspectSkillEntry(loc.Agent, ScopeUser, loc.UserSkill(home, sk.Name), sk.Name, wantHash, sk.Files)
				entry.Skill = skillLabel
				records = append(records, skillStatusRecord{entry: entry, wantHash: wantHash})
			}
			if workErr == nil {
				entry := inspectSkillEntry(loc.Agent, ScopeProject, loc.ProjSkill(work, sk.Name), sk.Name, wantHash, sk.Files)
				entry.Skill = skillLabel
				records = append(records, skillStatusRecord{entry: entry, wantHash: wantHash})
			}
		}
	}
	return records
}

func (t *Tool) collectSkillStatus() []SkillStatusEntry {
	records := t.collectSkillStatusRecords()
	entries := make([]SkillStatusEntry, len(records))
	for i, r := range records {
		entries[i] = r.entry
	}
	return entries
}

func inspectSkillEntry(agent Agent, scope Scope, path, tool, wantHash string, files []SkillFile) SkillStatusEntry {
	entry := SkillStatusEntry{
		Agent:  agent,
		Scope:  scope,
		Path:   path,
		Status: "missing",
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return entry
	}
	meta := parseFrontMatterStamp(string(data))
	if meta.tool != tool {
		entry.Status = "foreign"
		return entry
	}
	entry.InstalledVersion = meta.version
	entry.InstalledHash = meta.hash
	if meta.hash == wantHash && companionFilesCurrent(path, files) {
		entry.Status = "current"
	} else {
		entry.Status = "stale"
	}
	return entry
}

func companionFilesCurrent(skillFile string, files []SkillFile) bool {
	for _, f := range files {
		p := companionFilePath(skillFile, f.Path)
		data, err := os.ReadFile(p)
		if err != nil || string(data) != f.Content {
			return false
		}
	}
	return true
}

func checkSkillFileInstall(path, tool string, force bool) (owned bool, err error) {
	existing, err := os.ReadFile(path)
	if err == nil {
		meta := parseFrontMatterStamp(string(existing))
		if meta.tool == tool {
			return true, nil
		}
		if !force {
			return false, fmt.Errorf("refusing to overwrite foreign file %s without --force", path)
		}
		return false, nil
	}
	if !errors.Is(err, fs.ErrNotExist) {
		return false, fmt.Errorf("read %s: %w", path, err)
	}
	return false, nil
}

func checkCompanionFilesInstall(skillFile string, files []SkillFile, skillOwned, force bool) error {
	for _, f := range files {
		p := companionFilePath(skillFile, f.Path)
		info, err := os.Stat(p)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				continue
			}
			return fmt.Errorf("stat %s: %w", p, err)
		}
		if info.IsDir() {
			return fmt.Errorf("skill companion path %s is a directory", p)
		}
		if !skillOwned && !force {
			return fmt.Errorf("refusing to overwrite foreign file %s without --force", p)
		}
	}
	return nil
}

func installCompanionFiles(skillFile string, files []SkillFile) error {
	for _, f := range files {
		p := companionFilePath(skillFile, f.Path)
		if err := os.MkdirAll(filepath.Dir(p), 0o750); err != nil {
			return fmt.Errorf("create skill companion directory: %w", err)
		}
		if err := os.WriteFile(p, []byte(f.Content), 0o600); err != nil {
			return fmt.Errorf("write skill companion file: %w", err)
		}
	}
	return nil
}

func checkCompanionFilesUninstall(skillFile string, files []SkillFile) error {
	for _, f := range files {
		p := companionFilePath(skillFile, f.Path)
		info, err := os.Stat(p)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				continue
			}
			return fmt.Errorf("stat %s: %w", p, err)
		}
		if info.IsDir() {
			return fmt.Errorf("skill companion path %s is a directory", p)
		}
	}
	return nil
}

func uninstallCompanionFiles(skillFile string, files []SkillFile) error {
	skillDir := filepath.Dir(skillFile)
	for _, f := range files {
		p := companionFilePath(skillFile, f.Path)
		if err := os.Remove(p); err != nil && !errors.Is(err, fs.ErrNotExist) {
			return fmt.Errorf("remove %s: %w", p, err)
		}
		for dir := filepath.Dir(p); dir != skillDir && strings.HasPrefix(dir, skillDir+string(filepath.Separator)); dir = filepath.Dir(dir) {
			if err := os.Remove(dir); err != nil {
				break
			}
		}
	}
	return nil
}

func removeEmptySkillDir(skillDir string) {
	if skillDir == "" || skillDir == "." || skillDir == string(filepath.Separator) {
		return
	}
	_ = os.Remove(skillDir)
}

func installSkillFile(path, content, tool string, force, dryRun bool) error {
	if _, err := checkSkillFileInstall(path, tool, force); err != nil {
		return err
	}
	if dryRun {
		return nil
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o750); err != nil {
		return fmt.Errorf("create skill directory: %w", err)
	}
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		return fmt.Errorf("write skill file: %w", err)
	}
	return nil
}

func uninstallSkillFile(path, tool string, dryRun bool) (bool, error) {
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("read %s: %w", path, err)
	}
	meta := parseFrontMatterStamp(string(data))
	if meta.tool != tool {
		return false, fmt.Errorf("refusing to remove foreign file %s", path)
	}
	if dryRun {
		return true, nil
	}
	if err := os.Remove(path); err != nil {
		return false, fmt.Errorf("remove %s: %w", path, err)
	}
	return true, nil
}

func skillHash(raw string) string {
	sum := sha256.Sum256([]byte(raw))
	return "sha256:" + hex.EncodeToString(sum[:])
}

type frontMatterStamp struct {
	tool    string
	version string
	hash    string
}

func stampSkill(raw, tool, version string) string {
	h := skillHash(raw)
	stampLines := []string{
		"toolcli-tool: " + tool,
		"toolcli-version: " + version,
		"toolcli-hash: " + h,
	}

	if strings.HasPrefix(raw, "---\n") {
		rest := raw[4:]
		if idx := strings.Index(rest, "\n---\n"); idx >= 0 {
			fm := rest[:idx]
			body := rest[idx+5:]
			filtered := filterStampLines(strings.Split(fm, "\n"))
			filtered = append(filtered, stampLines...)
			return "---\n" + strings.Join(filtered, "\n") + "\n---\n" + body
		}
	}
	return "---\n" + strings.Join(stampLines, "\n") + "\n---\n" + raw
}

func filterStampLines(lines []string) []string {
	out := make([]string, 0, len(lines))
	for _, l := range lines {
		if strings.HasPrefix(l, "toolcli-tool:") ||
			strings.HasPrefix(l, "toolcli-version:") ||
			strings.HasPrefix(l, "toolcli-hash:") {
			continue
		}
		out = append(out, l)
	}
	return out
}

func parseFrontMatterStamp(content string) frontMatterStamp {
	if !strings.HasPrefix(content, "---\n") {
		return frontMatterStamp{}
	}
	rest := content[4:]
	idx := strings.Index(rest, "\n---\n")
	if idx < 0 {
		return frontMatterStamp{}
	}
	var stamp frontMatterStamp
	for _, line := range strings.Split(rest[:idx], "\n") {
		switch {
		case strings.HasPrefix(line, "toolcli-tool:"):
			stamp.tool = strings.TrimSpace(strings.TrimPrefix(line, "toolcli-tool:"))
		case strings.HasPrefix(line, "toolcli-version:"):
			stamp.version = strings.TrimSpace(strings.TrimPrefix(line, "toolcli-version:"))
		case strings.HasPrefix(line, "toolcli-hash:"):
			stamp.hash = strings.TrimSpace(strings.TrimPrefix(line, "toolcli-hash:"))
		}
	}
	return stamp
}
