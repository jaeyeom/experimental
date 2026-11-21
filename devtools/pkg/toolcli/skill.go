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

func (t *Tool) cmdSkill(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		if _, err := io.WriteString(stdout, t.SkillText); err != nil {
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
	default:
		return failMsg(stderr, fmt.Sprintf("unknown skill subcommand %q (expected install, uninstall, or status)", args[0]), 2)
	}
}

type agentListFlag []string

func (a *agentListFlag) String() string { return strings.Join(*a, ",") }
func (a *agentListFlag) Set(v string) error {
	*a = append(*a, v)
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

	targets, err := t.resolveSkillTargets(agents, Scope(*scope), *dir)
	if err != nil {
		return failErr(stderr, err, 2)
	}

	stamped := stampSkill(t.SkillText, t.Name, currentRevLabel(t.buildInfo()))
	for _, tgt := range targets {
		if err := installSkillFile(tgt.path, stamped, t.Name, *force, *dryRun); err != nil {
			return failErr(stderr, err, 2)
		}
		action := "installed"
		if *dryRun {
			action = "would install"
		}
		_, _ = fmt.Fprintf(stdout, "%s %s (%s): %s\n", action, tgt.label, tgt.scope, tgt.path)
	}
	return 0
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

	targets, err := t.resolveSkillTargets(agents, Scope(*scope), *dir)
	if err != nil {
		return failErr(stderr, err, 2)
	}

	for _, tgt := range targets {
		removed, err := uninstallSkillFile(tgt.path, t.Name, *dryRun)
		if err != nil {
			return failErr(stderr, err, 2)
		}
		if !removed {
			_, _ = fmt.Fprintf(stdout, "not installed %s (%s): %s\n", tgt.label, tgt.scope, tgt.path)
			continue
		}
		action := "removed"
		if *dryRun {
			action = "would remove"
		}
		_, _ = fmt.Fprintf(stdout, "%s %s (%s): %s\n", action, tgt.label, tgt.scope, tgt.path)
	}
	return 0
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

	entries := t.collectSkillStatus()
	out := SkillStatusOutput{
		Tool:        t.Name,
		CurrentHash: skillHash(t.SkillText),
		Entries:     entries,
	}

	if *jsonOut {
		if err := writeJSON(stdout, out); err != nil {
			return failErr(stderr, err, 2)
		}
		return 0
	}

	for _, e := range entries {
		ver := e.InstalledVersion
		if ver == "" {
			ver = "-"
		}
		_, _ = fmt.Fprintf(stdout, "%-12s  %-7s  %-8s  %s  %s\n", e.Agent, e.Scope, e.Status, ver, e.Path)
	}
	return 0
}

type skillTarget struct {
	label string
	scope Scope
	path  string
}

func (t *Tool) resolveSkillTargets(agents []string, scope Scope, dir string) ([]skillTarget, error) {
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
	wantHash := skillHash(t.SkillText)

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
	if meta.hash == wantHash {
		entry.Status = "current"
	} else {
		entry.Status = "stale"
	}
	return entry
}

func installSkillFile(path, content, tool string, force, dryRun bool) error {
	existing, err := os.ReadFile(path)
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
