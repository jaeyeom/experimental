// Package catalog parses and validates .agent-pool/catalog.json files,
// validates skill arguments and outputs against strict JSON Schema subsets,
// and builds deterministic prompts for skill execution.
package catalog

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"strings"
)

// SupportedVersion is the only catalog schema version accepted by this package.
const SupportedVersion = 1

// Default limit values applied when omitted (zero) in catalog.json.
const (
	DefaultMaxTurns            = 25
	DefaultMaxEffectiveTokens  = 150000
	DefaultTimeoutSeconds      = 300
	DefaultMaxDeliverableBytes = 131072
)

// Hard upper ceilings enforced on skill limits.
const (
	MaxAllowedTurns            = 100
	MaxAllowedTimeoutSeconds   = 1800
	MaxAllowedDeliverableBytes = 1048576
)

const (
	claudeSkillsPrefix    = ".claude/skills/"
	agentPoolSkillsPrefix = ".agent-pool/skills/"
	skillFilenameSuffix   = "/SKILL.md"
)

var skillNameRe = regexp.MustCompile(`^[a-z0-9][a-z0-9-]{0,63}$`)

// Mode controls the execution capability of a skill.
type Mode string

const (
	// ModeReadOnly restricts skill execution to read-only repository inspection.
	ModeReadOnly Mode = "readonly"
)

// Limits defines per-skill execution and deliverable bounds.
type Limits struct {
	MaxTurns            int   `json:"maxTurns"`
	MaxEffectiveTokens  int64 `json:"maxEffectiveTokens"`
	TimeoutSeconds      int   `json:"timeoutSeconds"`
	MaxDeliverableBytes int   `json:"maxDeliverableBytes"`
}

// Skill describes a single invocable skill entry in the catalog.
type Skill struct {
	Description  string          `json:"description"`
	Mode         Mode            `json:"mode"`
	SkillPath    string          `json:"skillPath"`
	ArgsSchema   json.RawMessage `json:"argsSchema"`
	OutputSchema json.RawMessage `json:"outputSchema"`
	Limits       Limits          `json:"limits"`
}

// Catalog is the parsed representation of .agent-pool/catalog.json.
type Catalog struct {
	Version int              `json:"version"`
	Skills  map[string]Skill `json:"skills"`
}

// Parse decodes and validates a catalog.json payload, applying default limits
// for omitted fields and rejecting unknown fields, unsupported versions,
// non-readonly modes, unsafe skill paths, and invalid schemas.
func Parse(data []byte) (*Catalog, error) {
	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()

	var cat Catalog
	if err := dec.Decode(&cat); err != nil {
		return nil, fmt.Errorf("decode catalog JSON: %w", err)
	}
	if err := ensureEOF(dec); err != nil {
		return nil, fmt.Errorf("catalog JSON trailing data: %w", err)
	}
	if cat.Version != SupportedVersion {
		return nil, fmt.Errorf("unsupported catalog version %d: expected %d", cat.Version, SupportedVersion)
	}
	if cat.Skills == nil {
		return nil, errors.New("catalog missing required skills map")
	}

	for name, skill := range cat.Skills {
		normalized, err := validateSkill(name, skill)
		if err != nil {
			return nil, fmt.Errorf("skill %q: %w", name, err)
		}
		cat.Skills[name] = normalized
	}
	return &cat, nil
}

// LoadFromDir reads <repoRoot>/.agent-pool/catalog.json, parses it, and
// verifies that every referenced skill file exists inside repoRoot without
// escaping via symlinks.
func LoadFromDir(repoRoot string) (*Catalog, error) {
	resolvedRoot, err := resolveRootDir(repoRoot)
	if err != nil {
		return nil, err
	}

	catalogPath := filepath.Join(resolvedRoot, ".agent-pool", "catalog.json")
	if err := verifyContainedFile(resolvedRoot, catalogPath); err != nil {
		return nil, fmt.Errorf("catalog file: %w", err)
	}

	data, err := os.ReadFile(catalogPath)
	if err != nil {
		return nil, fmt.Errorf("read catalog file: %w", err)
	}

	cat, err := Parse(data)
	if err != nil {
		return nil, err
	}

	for name, skill := range cat.Skills {
		skillAbs := filepath.Join(resolvedRoot, filepath.FromSlash(skill.SkillPath))
		if err := verifyContainedFile(resolvedRoot, skillAbs); err != nil {
			return nil, fmt.Errorf("skill %q path %q: %w", name, skill.SkillPath, err)
		}
	}
	return cat, nil
}

// ValidateSkillPath checks that skillPath is a relative, slash-normalized path
// containing no ".." segments, starting with ".claude/skills/" or
// ".agent-pool/skills/", and ending with "/SKILL.md".
func ValidateSkillPath(skillPath string) error {
	if skillPath == "" {
		return errors.New("skillPath must not be empty")
	}
	if strings.Contains(skillPath, `\`) {
		return fmt.Errorf("skillPath %q must use forward slashes", skillPath)
	}
	if strings.HasPrefix(skillPath, "/") || filepath.IsAbs(skillPath) {
		return fmt.Errorf("skillPath %q must be relative", skillPath)
	}
	if path.Clean(skillPath) != skillPath {
		return fmt.Errorf("skillPath %q must be slash-normalized without '..' or '.' segments", skillPath)
	}
	for _, seg := range strings.Split(skillPath, "/") {
		if seg == ".." {
			return fmt.Errorf("skillPath %q must not contain '..' segments", skillPath)
		}
	}
	prefix, ok := matchSkillPrefix(skillPath)
	if !ok {
		return fmt.Errorf("skillPath %q must start with %q or %q", skillPath, claudeSkillsPrefix, agentPoolSkillsPrefix)
	}
	if !strings.HasSuffix(skillPath, skillFilenameSuffix) {
		return fmt.Errorf("skillPath %q must end with %q", skillPath, skillFilenameSuffix)
	}
	rest := strings.TrimPrefix(skillPath, prefix)
	if !strings.Contains(rest, "/") {
		return fmt.Errorf("skillPath %q must include a skill directory before SKILL.md", skillPath)
	}
	return nil
}

func matchSkillPrefix(skillPath string) (string, bool) {
	if strings.HasPrefix(skillPath, claudeSkillsPrefix) {
		return claudeSkillsPrefix, true
	}
	if strings.HasPrefix(skillPath, agentPoolSkillsPrefix) {
		return agentPoolSkillsPrefix, true
	}
	return "", false
}

func validateSkill(name string, skill Skill) (Skill, error) {
	if !skillNameRe.MatchString(name) {
		return Skill{}, fmt.Errorf("invalid skill name %q: must match %s", name, skillNameRe.String())
	}
	if skill.Mode != ModeReadOnly {
		return Skill{}, fmt.Errorf("unsupported mode %q: only %q is allowed in v1", skill.Mode, ModeReadOnly)
	}
	if err := ValidateSkillPath(skill.SkillPath); err != nil {
		return Skill{}, err
	}
	limits, err := normalizeLimits(skill.Limits)
	if err != nil {
		return Skill{}, err
	}
	if _, err := validateArgsSchema(skill.ArgsSchema); err != nil {
		return Skill{}, fmt.Errorf("invalid argsSchema: %w", err)
	}
	if _, err := validateOutputSchema(skill.OutputSchema); err != nil {
		return Skill{}, fmt.Errorf("invalid outputSchema: %w", err)
	}
	skill.Limits = limits
	return skill, nil
}

func normalizeLimits(l Limits) (Limits, error) {
	maxTurns, err := applyIntLimit("maxTurns", l.MaxTurns, DefaultMaxTurns, MaxAllowedTurns)
	if err != nil {
		return Limits{}, err
	}
	timeout, err := applyIntLimit("timeoutSeconds", l.TimeoutSeconds, DefaultTimeoutSeconds, MaxAllowedTimeoutSeconds)
	if err != nil {
		return Limits{}, err
	}
	maxBytes, err := applyIntLimit("maxDeliverableBytes", l.MaxDeliverableBytes, DefaultMaxDeliverableBytes, MaxAllowedDeliverableBytes)
	if err != nil {
		return Limits{}, err
	}
	tokens := l.MaxEffectiveTokens
	if tokens < 0 {
		return Limits{}, fmt.Errorf("maxEffectiveTokens must be non-negative, got %d", tokens)
	}
	if tokens == 0 {
		tokens = DefaultMaxEffectiveTokens
	}
	return Limits{
		MaxTurns:            maxTurns,
		MaxEffectiveTokens:  tokens,
		TimeoutSeconds:      timeout,
		MaxDeliverableBytes: maxBytes,
	}, nil
}

func applyIntLimit(field string, value, defVal, maxVal int) (int, error) {
	if value < 0 {
		return 0, fmt.Errorf("%s must be non-negative, got %d", field, value)
	}
	if value == 0 {
		return defVal, nil
	}
	if value > maxVal {
		return 0, fmt.Errorf("%s exceeds hard ceiling %d, got %d", field, maxVal, value)
	}
	return value, nil
}

func resolveRootDir(repoRoot string) (string, error) {
	absRoot, err := filepath.Abs(repoRoot)
	if err != nil {
		return "", fmt.Errorf("resolve repoRoot %q: %w", repoRoot, err)
	}
	resolved, err := filepath.EvalSymlinks(absRoot)
	if err != nil {
		return "", fmt.Errorf("eval symlinks for repoRoot %q: %w", repoRoot, err)
	}
	return resolved, nil
}

func verifyContainedFile(resolvedRoot, targetPath string) error {
	resolvedTarget, err := filepath.EvalSymlinks(targetPath)
	if err != nil {
		return fmt.Errorf("resolve file path: %w", err)
	}
	rel, err := filepath.Rel(resolvedRoot, resolvedTarget)
	if err != nil {
		return fmt.Errorf("check containment: %w", err)
	}
	if rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) || filepath.IsAbs(rel) {
		return fmt.Errorf("path escapes repository root")
	}
	info, err := os.Stat(resolvedTarget)
	if err != nil {
		return fmt.Errorf("stat file: %w", err)
	}
	if info.IsDir() {
		return errors.New("expected regular file, got directory")
	}
	return nil
}

func ensureEOF(dec *json.Decoder) error {
	var extra any
	err := dec.Decode(&extra)
	if errors.Is(err, io.EOF) {
		return nil
	}
	if err != nil {
		return fmt.Errorf("unexpected trailing token: %w", err)
	}
	return errors.New("unexpected trailing JSON value")
}
