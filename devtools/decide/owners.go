package decide

import (
	"cmp"
	"fmt"
	"path"
	"slices"
	"strings"
)

// Owners mode values. Empty means OwnersOff.
const (
	OwnersOff     = "off"
	OwnersWarn    = "warn"
	OwnersEnforce = "enforce"
)

// TagOffLine is added to items opened by a non-allowed owner in warn mode.
const TagOffLine = "off-line"

// TagUndelivered marks an item whose agent notification needs attention.
const TagUndelivered = "undelivered"

// OwnersConfig configures the allow-list of owners who may open items.
//
// Mode is "off" (default), "warn", or "enforce". Allowed lists owner names
// or glob patterns (* within a path segment). Managers optionally maps an
// owner pattern to a manager name used in warning/error messages.
type OwnersConfig struct {
	Mode     string            `json:"mode,omitempty"`
	Allowed  []string          `json:"allowed,omitempty"`
	Managers map[string]string `json:"managers,omitempty"`
}

// EffectiveMode returns the configured mode, defaulting to [OwnersOff].
func (o OwnersConfig) EffectiveMode() string {
	if o.Mode == "" {
		return OwnersOff
	}
	return o.Mode
}

// MatchOwner reports whether owner matches any pattern in allowed.
// Patterns use [path.Match] (* within a path segment, not across /).
func MatchOwner(allowed []string, owner string) bool {
	for _, pat := range allowed {
		if ok, err := path.Match(pat, owner); err == nil && ok {
			return true
		}
	}
	return false
}

// LookupManager returns the manager for owner from managers.
//
// Matching patterns are tried in specificity order, most specific first:
//  1. More path segments (more '/' characters).
//  2. Fewer glob wildcards ('*', '?', '[').
//  3. Longer pattern string.
//  4. Lexicographical order as a stable tie-breaker.
func LookupManager(managers map[string]string, owner string) (string, bool) {
	if len(managers) == 0 {
		return "", false
	}
	patterns := make([]string, 0, len(managers))
	for pat := range managers {
		patterns = append(patterns, pat)
	}
	slices.SortFunc(patterns, compareOwnerPatterns)
	for _, pat := range patterns {
		if ok, err := path.Match(pat, owner); err == nil && ok {
			return managers[pat], true
		}
	}
	return "", false
}

// CheckOwner checks whether owner may open a new item under o.
// If o is off or owner matches o.Allowed, it returns ("", false).
// Otherwise it returns the message to print on stderr and whether the
// caller must refuse (true in enforce mode, false in warn mode).
func CheckOwner(o OwnersConfig, owner string) (msg string, enforce bool) {
	mode := o.EffectiveMode()
	if mode == OwnersOff || MatchOwner(o.Allowed, owner) {
		return "", false
	}
	target := "your manager"
	if mgr, ok := LookupManager(o.Managers, owner); ok {
		target = mgr
	}
	msg = fmt.Sprintf("owner %s is not a direct report; ask %s instead", owner, target)
	return msg, mode == OwnersEnforce
}

func compareOwnerPatterns(a, b string) int {
	if c := cmp.Compare(strings.Count(b, "/"), strings.Count(a, "/")); c != 0 {
		return c
	}
	if c := cmp.Compare(wildcardCount(a), wildcardCount(b)); c != 0 {
		return c
	}
	if c := cmp.Compare(len(b), len(a)); c != 0 {
		return c
	}
	return cmp.Compare(a, b)
}

func wildcardCount(pat string) int {
	return strings.Count(pat, "*") + strings.Count(pat, "?") + strings.Count(pat, "[")
}

func checkOwnersConfig(o OwnersConfig) error {
	switch o.Mode {
	case "", OwnersOff, OwnersWarn, OwnersEnforce:
	default:
		return fmt.Errorf("decide config owners.mode: unknown mode %q (want off, warn, or enforce)", o.Mode)
	}
	for i, pat := range o.Allowed {
		if pat == "" {
			return fmt.Errorf("decide config owners.allowed[%d]: empty pattern", i)
		}
		if _, err := path.Match(pat, ""); err != nil {
			return fmt.Errorf("decide config owners.allowed[%d] %q: %w", i, pat, err)
		}
	}
	if o.Mode != "" && o.Mode != OwnersOff && len(o.Allowed) == 0 {
		return fmt.Errorf("decide config owners.allowed: must be non-empty when owners.mode is %q", o.Mode)
	}
	if len(o.Managers) > 0 {
		patterns := make([]string, 0, len(o.Managers))
		for pat := range o.Managers {
			patterns = append(patterns, pat)
		}
		slices.Sort(patterns)
		for _, pat := range patterns {
			if pat == "" {
				return fmt.Errorf("decide config owners.managers: empty pattern")
			}
			if _, err := path.Match(pat, ""); err != nil {
				return fmt.Errorf("decide config owners.managers[%q]: %w", pat, err)
			}
			if o.Managers[pat] == "" {
				return fmt.Errorf("decide config owners.managers[%q]: empty manager name", pat)
			}
		}
	}
	return nil
}
