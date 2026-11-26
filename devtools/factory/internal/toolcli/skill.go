package toolcli

import (
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
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

// SkillInstallOptions configures skill installation.
type SkillInstallOptions struct {
	Agents []string
	Scope  Scope
	Dir    string
	DryRun bool
	Force  bool
}

// RunSkillInstall installs the embedded skill into the requested agent locations.
func (t *Tool) RunSkillInstall(opts SkillInstallOptions, stdout io.Writer) error {
	targets, err := t.resolveSkillTargets(opts.Agents, opts.Scope, opts.Dir)
	if err != nil {
		return err
	}
	stamped := StampSkill(t.SkillText, t.Name, currentRevLabel(t.buildInfo()))
	for _, tgt := range targets {
		if err := installSkillFile(tgt.path, stamped, t.Name, opts.Force, opts.DryRun); err != nil {
			return err
		}
		action := "installed"
		if opts.DryRun {
			action = "would install"
		}
		if _, err := fmt.Fprintf(stdout, "%s %s (%s): %s\n", action, tgt.label, tgt.scope, tgt.path); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
	}
	return nil
}

// SkillUninstallOptions configures skill uninstallation.
type SkillUninstallOptions struct {
	Agents []string
	Scope  Scope
	Dir    string
	DryRun bool
}

// RunSkillUninstall removes skill files stamped by this tool.
func (t *Tool) RunSkillUninstall(opts SkillUninstallOptions, stdout io.Writer) error {
	targets, err := t.resolveSkillTargets(opts.Agents, opts.Scope, opts.Dir)
	if err != nil {
		return err
	}
	for _, tgt := range targets {
		removed, err := uninstallSkillFile(tgt.path, t.Name, opts.DryRun)
		if err != nil {
			return err
		}
		if !removed {
			if _, err := fmt.Fprintf(stdout, "not installed %s (%s): %s\n", tgt.label, tgt.scope, tgt.path); err != nil {
				return fmt.Errorf("write output: %w", err)
			}
			continue
		}
		action := "removed"
		if opts.DryRun {
			action = "would remove"
		}
		if _, err := fmt.Fprintf(stdout, "%s %s (%s): %s\n", action, tgt.label, tgt.scope, tgt.path); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
	}
	return nil
}

// SkillStatusEntry describes one agent+scope skill installation state.
type SkillStatusEntry struct {
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
	entries := t.collectSkillStatus()
	out := SkillStatusOutput{
		Tool:        t.Name,
		CurrentHash: SkillHash(t.SkillText),
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

type skillTarget struct {
	label string
	scope Scope
	path  string
}

func (t *Tool) resolveSkillTargets(agents []string, scope Scope, dir string) ([]skillTarget, error) {
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
	return t.expandAgentTargets(agents, baseDir, scope)
}

func (t *Tool) expandAgentTargets(agents []string, baseDir string, scope Scope) ([]skillTarget, error) {
	var targets []skillTarget
	seen := map[string]bool{}
	for _, a := range agents {
		if a == "all" {
			detected := detectAgents(baseDir, scope)
			if len(detected) == 0 {
				return nil, fmt.Errorf("no supported agents found for %s scope", scope)
			}
			for _, loc := range detected {
				p := skillPathForLoc(loc, baseDir, scope, t.Name)
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
		p := skillPathForLoc(loc, baseDir, scope, t.Name)
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

func (t *Tool) collectSkillStatus() []SkillStatusEntry {
	home, homeErr := t.homeDir()
	work, workErr := t.workDir()
	wantHash := SkillHash(t.SkillText)

	var entries []SkillStatusEntry
	for _, loc := range AgentLocations {
		if homeErr == nil {
			entries = append(entries, inspectSkillEntry(loc.Agent, ScopeUser, loc.UserSkill(home, t.Name), t.Name, wantHash))
		}
		if workErr == nil {
			entries = append(entries, inspectSkillEntry(loc.Agent, ScopeProject, loc.ProjSkill(work, t.Name), t.Name, wantHash))
		}
	}
	return entries
}

func inspectSkillEntry(agent Agent, scope Scope, path, tool, wantHash string) SkillStatusEntry {
	entry := SkillStatusEntry{
		Agent:  agent,
		Scope:  scope,
		Path:   path,
		Status: "missing",
	}
	data, err := os.ReadFile(path) //nolint:gosec // G304: path is resolved from canonical agent skill locations or --dir
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
	if meta.hash == wantHash {
		entry.Status = "current"
	} else {
		entry.Status = "stale"
	}
	return entry
}

func installSkillFile(path, content, tool string, force, dryRun bool) error {
	existing, err := os.ReadFile(path) //nolint:gosec // G304: path is resolved from canonical agent skill locations or --dir
	if err == nil {
		meta := parseFrontMatterStamp(string(existing))
		if meta.tool != tool && !force {
			return fmt.Errorf("refusing to overwrite foreign file %s without --force", path)
		}
	} else if !errors.Is(err, fs.ErrNotExist) {
		return fmt.Errorf("read %s: %w", path, err)
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
	data, err := os.ReadFile(path) //nolint:gosec // G304: path is resolved from canonical agent skill locations or --dir
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

// SkillHash returns the sha256 digest prefix-tagged string for raw skill text.
func SkillHash(raw string) string {
	sum := sha256.Sum256([]byte(raw))
	return "sha256:" + hex.EncodeToString(sum[:])
}

type frontMatterStamp struct {
	tool    string
	version string
	hash    string
}

// StampSkill adds toolcli-tool, toolcli-version, and toolcli-hash to the skill's YAML front matter.
func StampSkill(raw, tool, version string) string {
	h := SkillHash(raw)
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
