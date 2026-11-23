package baseline

import (
	"errors"
	"fmt"
	"path"
	"strings"
)

// NormalizeTarget cleans a repo-relative forward-slash target path and rejects
// empty paths, absolute paths, backslashes, and parent directory traversals.
func NormalizeTarget(p string) (string, error) {
	if p == "" {
		return "", errors.New("empty target path")
	}
	if strings.Contains(p, "\\") {
		return "", fmt.Errorf("backslash not allowed in target path %q", p)
	}
	if strings.HasPrefix(p, "/") {
		return "", fmt.Errorf("absolute path not allowed: %q", p)
	}
	for _, seg := range strings.Split(p, "/") {
		if seg == ".." {
			return "", fmt.Errorf("parent directory traversal not allowed in %q", p)
		}
	}
	cleaned := path.Clean(p)
	if cleaned == "." || cleaned == ".." || strings.HasPrefix(cleaned, "../") {
		return "", fmt.Errorf("invalid target path %q", p)
	}
	return cleaned, nil
}

// IsGlob reports whether pattern contains glob wildcard characters (*, ?, or [).
func IsGlob(pattern string) bool {
	return strings.ContainsAny(pattern, "*?[")
}

// MatchGlob reports whether target matches pattern, supporting *, ?, [...],
// and ** (zero or more path segments).
func MatchGlob(pattern, target string) (bool, error) {
	patSegs := strings.Split(pattern, "/")
	if err := validateGlobSegments(patSegs, pattern); err != nil {
		return false, err
	}
	tgtSegs := strings.Split(target, "/")
	return matchSegments(patSegs, tgtSegs)
}

func validateGlobSegments(patSegs []string, pattern string) error {
	for _, seg := range patSegs {
		if seg == "**" {
			continue
		}
		if _, err := path.Match(seg, ""); err != nil {
			return fmt.Errorf("invalid glob pattern %q: %w", pattern, err)
		}
	}
	return nil
}

func matchSegments(patSegs, tgtSegs []string) (bool, error) {
	pi, ti := 0, 0
	starPi, starTi := -1, -1

	for ti < len(tgtSegs) {
		if pi < len(patSegs) && patSegs[pi] == "**" {
			starPi = pi
			starTi = ti
			pi++
			continue
		}
		if pi < len(patSegs) {
			matched, err := path.Match(patSegs[pi], tgtSegs[ti])
			if err != nil {
				return false, fmt.Errorf("match segment %q: %w", patSegs[pi], err)
			}
			if matched {
				pi++
				ti++
				continue
			}
		}
		if starPi != -1 {
			pi = starPi + 1
			starTi++
			ti = starTi
			continue
		}
		return false, nil
	}

	for pi < len(patSegs) && patSegs[pi] == "**" {
		pi++
	}
	return pi == len(patSegs), nil
}

// Resolve finds the highest-precedence matching baseline entry for target.
// Exact path matches take precedence over globs. Among matching globs, the
// pattern with the longest non-wildcard prefix wins, breaking ties
// lexicographically.
func (f *File) Resolve(target string) (matchedKey string, entry Entry, ok bool) {
	if f == nil || len(f.Entries) == 0 {
		return "", Entry{}, false
	}
	if norm, err := NormalizeTarget(target); err == nil {
		target = norm
	}

	if e, exists := f.Entries[target]; exists {
		return target, e, true
	}

	return f.resolveGlob(target)
}

func (f *File) resolveGlob(target string) (string, Entry, bool) {
	var bestKey string
	var bestEntry Entry
	bestPrefixLen := -1

	for k, e := range f.Entries {
		if !IsGlob(k) {
			continue
		}
		matched, err := MatchGlob(k, target)
		if err != nil || !matched {
			continue
		}
		prefixLen := nonWildcardPrefixLen(k)
		if bestPrefixLen == -1 || prefixLen > bestPrefixLen || (prefixLen == bestPrefixLen && k < bestKey) {
			bestKey = k
			bestEntry = e
			bestPrefixLen = prefixLen
		}
	}
	if bestPrefixLen == -1 {
		return "", Entry{}, false
	}
	return bestKey, bestEntry, true
}

func nonWildcardPrefixLen(pattern string) int {
	if idx := strings.IndexAny(pattern, "*?["); idx != -1 {
		return idx
	}
	return len(pattern)
}
