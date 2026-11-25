// Package secretwatch scans systemd journal entries for leaked secrets and
// notifies without ever exposing the secret values.
package secretwatch

import (
	"fmt"
	"math"
	"path"
	"regexp"
	"strings"
)

const (
	// MaxPrefixRunes is the maximum number of leading secret characters
	// revealed in masked prefixes.
	MaxPrefixRunes = 4

	genericAssignmentMinEntropy = 3.2
)

// Rule defines a named secret-detection pattern.
type Rule struct {
	Name       string
	Pattern    *regexp.Regexp
	MinEntropy float64
	Validate   func(fullMatch, secret string, submatches []string) bool
}

// CompiledRuleSet holds ordered detection rules and allowlist matchers.
type CompiledRuleSet struct {
	rules         []Rule
	allowPatterns []*regexp.Regexp
	allowUnits    []string
}

// Match describes one detected secret without storing the secret itself.
type Match struct {
	Rule         string `json:"rule"`
	MaskedPrefix string `json:"maskedPrefix"`
}

// DefaultRules returns the built-in secret detection rules ordered from most
// specific to most generic.
func DefaultRules() []Rule {
	return []Rule{
		{
			Name:    "github-token",
			Pattern: regexp.MustCompile(`\b(gh[pousr]_[A-Za-z0-9]{24,255}|github_pat_[A-Za-z0-9_]{24,255})\b`),
		},
		{
			Name:    "gcp-service-account-key",
			Pattern: regexp.MustCompile(`(?s)"private_key"\s*:\s*"(-----BEGIN [A-Z0-9 ]*PRIVATE KEY-----[^"]{16,})"`),
		},
		{
			Name:    "pem-private-key",
			Pattern: regexp.MustCompile(`(-----BEGIN (?:[A-Z0-9]+ )*PRIVATE KEY(?: BLOCK)?-----)`),
		},
		{
			Name:    "aws-access-key",
			Pattern: regexp.MustCompile(`\b((?:A3T[A-Z0-9]|AKIA|AGPA|AIDA|AROA|AIPA|ANPA|ANVA|ASIA)[A-Z0-9]{16})\b`),
		},
		{
			Name:    "gcp-api-key",
			Pattern: regexp.MustCompile(`\b(AIza[0-9A-Za-z_-]{35})\b`),
		},
		{
			Name:     "slack-token",
			Pattern:  regexp.MustCompile(`\b(xox[a-zA-Z]-[0-9A-Za-z-]{15,255})\b`),
			Validate: validateSlackToken,
		},
		{
			Name:     "auth-header",
			Pattern:  regexp.MustCompile(`(?i)(?:\bauthorization["']?\s*:\s*["']?(?:(bearer|basic|token|apikey|api-key)\s+)?([A-Za-z0-9._~+/-]{12,}=*)|\bbearer\s+([A-Za-z0-9._~+/-]{16,}=*))`),
			Validate: validateAuthHeader,
		},
		{
			Name:       "generic-secret-assignment",
			Pattern:    regexp.MustCompile(`(?i)\b([A-Za-z0-9_]*_(?:TOKEN|SECRET|KEY)|TOKEN|SECRET)\s*=\s*["']?([^\s"'` + "`" + `<>]{16,})["']?`),
			MinEntropy: genericAssignmentMinEntropy,
			Validate:   validateGenericAssignment,
		},
	}
}

// CompileRuleSet compiles the active built-in rules, custom rules, and
// allowlist entries from cfg.
func CompileRuleSet(cfg Config) (*CompiledRuleSet, error) {
	disabled := make(map[string]bool, len(cfg.DisableRules))
	for _, name := range cfg.DisableRules {
		disabled[name] = true
	}

	builtins := DefaultRules()
	var specific []Rule
	var fallback []Rule
	for _, r := range builtins {
		if disabled[r.Name] {
			continue
		}
		if r.Name == "generic-secret-assignment" {
			fallback = append(fallback, r)
		} else {
			specific = append(specific, r)
		}
	}

	custom := make([]Rule, 0, len(cfg.Rules))
	for _, rc := range cfg.Rules {
		re, err := regexp.Compile(rc.Pattern)
		if err != nil {
			return nil, fmt.Errorf("compile rule %q: %w", rc.Name, err)
		}
		custom = append(custom, Rule{
			Name:       rc.Name,
			Pattern:    re,
			MinEntropy: rc.MinEntropy,
		})
	}

	allowPatterns := make([]*regexp.Regexp, 0, len(cfg.Allowlist.Patterns))
	for _, pat := range cfg.Allowlist.Patterns {
		re, err := regexp.Compile(pat)
		if err != nil {
			return nil, fmt.Errorf("compile allowlist pattern %q: %w", pat, err)
		}
		allowPatterns = append(allowPatterns, re)
	}

	rules := make([]Rule, 0, len(specific)+len(custom)+len(fallback))
	rules = append(rules, specific...)
	rules = append(rules, custom...)
	rules = append(rules, fallback...)

	return &CompiledRuleSet{
		rules:         rules,
		allowPatterns: allowPatterns,
		allowUnits:    append([]string(nil), cfg.Allowlist.Units...),
	}, nil
}

// UnitAllowed reports whether unit matches any configured allowlist unit or glob.
func (rs *CompiledRuleSet) UnitAllowed(unit string) bool {
	if unit == "" || len(rs.allowUnits) == 0 {
		return false
	}
	for _, pattern := range rs.allowUnits {
		if pattern == unit {
			return true
		}
		if ok, err := path.Match(pattern, unit); err == nil && ok {
			return true
		}
	}
	return false
}

// ScanMessage inspects a journal message for secret-shaped strings under unit.
// It returns at most one Match, masked to at most 4 characters, or false when
// nothing matches or the finding is allowlisted.
func (rs *CompiledRuleSet) ScanMessage(unit, message string) (Match, bool) {
	if message == "" || rs.UnitAllowed(unit) {
		return Match{}, false
	}
	contextMsg := rs.stripAllowlistedMatches(message)
	if rs.matchesAllowPattern(contextMsg) {
		return Match{}, false
	}
	for _, rule := range rs.rules {
		if m, ok := rs.matchRule(rule, message); ok {
			return m, true
		}
	}
	return Match{}, false
}

func (rs *CompiledRuleSet) stripAllowlistedMatches(message string) string {
	if len(rs.allowPatterns) == 0 {
		return message
	}
	buf := []byte(message)
	changed := false
	for _, rule := range rs.rules {
		for _, idx := range rule.Pattern.FindAllStringSubmatchIndex(message, -1) {
			fullMatch, secret, submatches := extractSubmatches(message, idx)
			if secret == "" {
				continue
			}
			if rule.Validate != nil && !rule.Validate(fullMatch, secret, submatches) {
				continue
			}
			if rule.MinEntropy > 0 && shannonEntropy(secret) < rule.MinEntropy {
				continue
			}
			if rs.matchesAllowPattern(fullMatch) || rs.matchesAllowPattern(secret) {
				for i := idx[0]; i < idx[1]; i++ {
					buf[i] = ' '
				}
				changed = true
			}
		}
	}
	if !changed {
		return message
	}
	return string(buf)
}

func (rs *CompiledRuleSet) matchRule(rule Rule, message string) (Match, bool) {
	indices := rule.Pattern.FindAllStringSubmatchIndex(message, -1)
	for _, idx := range indices {
		fullMatch, secret, submatches := extractSubmatches(message, idx)
		if secret == "" {
			continue
		}
		if rule.Validate != nil && !rule.Validate(fullMatch, secret, submatches) {
			continue
		}
		if rule.MinEntropy > 0 && shannonEntropy(secret) < rule.MinEntropy {
			continue
		}
		if rs.matchesAllowPattern(fullMatch) || rs.matchesAllowPattern(secret) {
			continue
		}
		return Match{
			Rule:         rule.Name,
			MaskedPrefix: MaskSecret(secret),
		}, true
	}
	return Match{}, false
}

func (rs *CompiledRuleSet) matchesAllowPattern(text string) bool {
	for _, re := range rs.allowPatterns {
		if re.MatchString(text) {
			return true
		}
	}
	return false
}

func extractSubmatches(message string, idx []int) (fullMatch, secret string, submatches []string) {
	if len(idx) < 2 || idx[0] < 0 || idx[1] < 0 {
		return "", "", nil
	}
	fullMatch = message[idx[0]:idx[1]]
	submatches = make([]string, 0, len(idx)/2)
	submatches = append(submatches, fullMatch)
	for i := 2; i+1 < len(idx); i += 2 {
		if idx[i] >= 0 && idx[i+1] >= 0 {
			submatches = append(submatches, message[idx[i]:idx[i+1]])
		} else {
			submatches = append(submatches, "")
		}
	}
	for i := len(submatches) - 1; i >= 1; i-- {
		if submatches[i] != "" {
			secret = submatches[i]
			break
		}
	}
	if secret == "" {
		secret = fullMatch
	}
	return fullMatch, secret, submatches
}

// MaskSecret masks secret to at most 4 leading characters followed by "****".
// Values of 4 runes or fewer return "****" so the full value is never exposed.
func MaskSecret(secret string) string {
	runes := []rune(secret)
	if len(runes) <= MaxPrefixRunes {
		return "****"
	}
	prefix := make([]rune, 0, MaxPrefixRunes)
	for _, r := range runes[:MaxPrefixRunes] {
		if r < ' ' || r == 0x7f {
			prefix = append(prefix, '?')
			continue
		}
		prefix = append(prefix, r)
	}
	return string(prefix) + "****"
}

func validateSlackToken(_, secret string, _ []string) bool {
	dash := strings.IndexByte(secret, '-')
	if dash < 0 || dash+1 >= len(secret) {
		return false
	}
	return containsDigit(secret[dash+1:])
}

func validateAuthHeader(_, secret string, submatches []string) bool {
	if isPlaceholder(secret) {
		return false
	}
	// Require token diversity (digit, symbol, or mixed case) unless an
	// explicit non-empty scheme was present and it has non-dictionary chars.
	scheme := ""
	if len(submatches) > 1 {
		scheme = submatches[1]
	}
	if scheme == "" {
		return hasTokenComplexity(secret)
	}
	return hasTokenComplexity(secret)
}

func validateGenericAssignment(_, secret string, submatches []string) bool {
	key := ""
	if len(submatches) > 1 {
		key = submatches[1]
	}
	upperKey := strings.ToUpper(key)
	if strings.Contains(upperKey, "PUBLIC") || strings.HasSuffix(upperKey, "_PUB_KEY") {
		return false
	}
	if strings.HasPrefix(secret, "/") ||
		strings.HasPrefix(secret, "./") ||
		strings.HasPrefix(secret, "../") ||
		strings.HasPrefix(secret, "$") ||
		strings.HasPrefix(secret, "{") ||
		strings.HasPrefix(secret, "[") ||
		strings.HasPrefix(secret, "<") ||
		strings.Contains(secret, "://") {
		return false
	}
	return !isPlaceholder(secret)
}

func isPlaceholder(s string) bool {
	lower := strings.ToLower(s)
	for _, marker := range []string{
		"redacted",
		"placeholder",
		"example",
		"changeme",
		"your_",
		"dummy",
		"sample",
		"unauthorized",
		"authentication",
		"authorization",
		"credentials",
	} {
		if strings.Contains(lower, marker) {
			return true
		}
	}
	return allSameRune(s)
}

func allSameRune(s string) bool {
	var first rune
	for i, r := range s {
		if i == 0 {
			first = r
			continue
		}
		if r != first {
			return false
		}
	}
	return true
}

func containsDigit(s string) bool {
	for i := 0; i < len(s); i++ {
		if s[i] >= '0' && s[i] <= '9' {
			return true
		}
	}
	return false
}

func hasTokenComplexity(s string) bool {
	var hasUpper, hasLower, hasDigit, hasSymbol bool
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case c >= 'A' && c <= 'Z':
			hasUpper = true
		case c >= 'a' && c <= 'z':
			hasLower = true
		case c >= '0' && c <= '9':
			hasDigit = true
		default:
			hasSymbol = true
		}
	}
	return hasDigit || hasSymbol || (hasUpper && hasLower)
}

func shannonEntropy(s string) float64 {
	if len(s) == 0 {
		return 0
	}
	var counts [256]int
	for i := 0; i < len(s); i++ {
		counts[s[i]]++
	}
	n := float64(len(s))
	var entropy float64
	for _, c := range counts {
		if c == 0 {
			continue
		}
		p := float64(c) / n
		entropy -= p * math.Log2(p)
	}
	return entropy
}
