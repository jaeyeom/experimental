package scan

import "strings"

// codeOwner is a user login or an org/team slug from one CODEOWNERS rule.
type codeOwner struct {
	login string
	team  string
}

// codeRule is one CODEOWNERS pattern and the owners that win if it matches last.
type codeRule struct {
	pattern string
	owners  []codeOwner
}

// parseCODEOWNERS reads GitHub CODEOWNERS text. The last matching pattern wins.
// Blank lines, comments, and section headers are ignored.
func parseCODEOWNERS(text string) []codeRule {
	var rules []codeRule
	for _, line := range strings.Split(text, "\n") {
		line = stripCODEOWNERSComment(line)
		pattern, tokens := splitCODEOWNERS(line)
		if pattern == "" || strings.HasPrefix(pattern, "[") || strings.HasPrefix(pattern, "^[") {
			continue
		}
		owners := codeOwners(tokens)
		rules = append(rules, codeRule{pattern: pattern, owners: owners})
	}
	return rules
}

func stripCODEOWNERSComment(line string) string {
	escaped := false
	for i, r := range line {
		if escaped {
			escaped = false
			continue
		}
		if r == '\\' {
			escaped = true
			continue
		}
		if r == '#' {
			return line[:i]
		}
	}
	return line
}

func splitCODEOWNERS(line string) (string, []string) {
	var tokens []string
	var b strings.Builder
	escaped := false
	flush := func() {
		if b.Len() == 0 {
			return
		}
		tokens = append(tokens, b.String())
		b.Reset()
	}
	for _, r := range line {
		switch {
		case escaped:
			b.WriteRune(r)
			escaped = false
		case r == '\\':
			escaped = true
		case r == ' ' || r == '\t':
			flush()
		default:
			b.WriteRune(r)
		}
	}
	flush()
	if len(tokens) == 0 {
		return "", nil
	}
	return tokens[0], tokens[1:]
}

func codeOwners(tokens []string) []codeOwner {
	var owners []codeOwner
	for _, token := range tokens {
		owner, ok := parseCodeOwner(token)
		if ok {
			owners = append(owners, owner)
		}
	}
	return owners
}

func parseCodeOwner(token string) (codeOwner, bool) {
	if !strings.HasPrefix(token, "@") || len(token) < 2 {
		return codeOwner{}, false
	}
	body := token[1:]
	org, slug, ok := strings.Cut(body, "/")
	if !ok {
		return codeOwner{login: body}, true
	}
	if org == "" || slug == "" || strings.Contains(slug, "/") {
		return codeOwner{}, false
	}
	return codeOwner{team: org + "/" + slug}, true
}

// lastCodeOwner returns the last rule whose pattern matches file.
func lastCodeOwner(rules []codeRule, file string) (codeRule, bool) {
	file = strings.TrimPrefix(file, "/")
	var matched codeRule
	found := false
	for _, rule := range rules {
		if codeownersMatch(rule.pattern, file) {
			matched = rule
			found = true
		}
	}
	return matched, found
}

// codeownersMatch reports whether a GitHub CODEOWNERS pattern matches file.
// file is a repository path with no leading slash.
func codeownersMatch(pattern, file string) bool {
	file = strings.TrimPrefix(strings.TrimSpace(file), "/")
	pattern = strings.TrimSpace(pattern)
	if pattern == "" || file == "" {
		return false
	}
	anchored := strings.HasPrefix(pattern, "/")
	directory := strings.HasSuffix(pattern, "/")
	pattern = strings.Trim(pattern, "/")
	if pattern == "" {
		return true
	}
	if !anchored && !strings.Contains(pattern, "/") {
		if directory {
			return matchUnanchoredDir(pattern, file)
		}
		return globSegment(pattern, lastPathSegment(file))
	}
	if directory {
		if strings.ContainsAny(pattern, "*?") {
			return globPath(pattern, file) || globPath(pattern+"/**", file)
		}
		return file == pattern || strings.HasPrefix(file, pattern+"/")
	}
	return globPath(pattern, file)
}

func matchUnanchoredDir(pattern, file string) bool {
	segs := strings.Split(file, "/")
	if len(segs) < 2 {
		return false
	}
	for _, seg := range segs[:len(segs)-1] {
		if globSegment(pattern, seg) {
			return true
		}
	}
	return false
}

func lastPathSegment(file string) string {
	if i := strings.LastIndex(file, "/"); i >= 0 {
		return file[i+1:]
	}
	return file
}

func globPath(pattern, file string) bool {
	return matchPathParts(strings.Split(pattern, "/"), strings.Split(file, "/"))
}

func matchPathParts(pat, segs []string) bool {
	for len(pat) > 0 {
		if pat[0] == "**" {
			if len(pat) == 1 {
				return true
			}
			for i := 0; i <= len(segs); i++ {
				if matchPathParts(pat[1:], segs[i:]) {
					return true
				}
			}
			return false
		}
		if len(segs) == 0 || !globSegment(pat[0], segs[0]) {
			return false
		}
		pat = pat[1:]
		segs = segs[1:]
	}
	return len(segs) == 0
}

func globSegment(pattern, segment string) bool {
	if pattern == "**" {
		return true
	}
	return matchSegment(pattern, segment)
}

func matchSegment(pattern, segment string) bool {
	for len(pattern) > 0 {
		switch pattern[0] {
		case '*':
			for i := 0; i <= len(segment); i++ {
				if matchSegment(pattern[1:], segment[i:]) {
					return true
				}
			}
			return false
		case '?':
			if segment == "" {
				return false
			}
			pattern = pattern[1:]
			segment = segment[1:]
		default:
			if segment == "" || segment[0] != pattern[0] {
				return false
			}
			pattern = pattern[1:]
			segment = segment[1:]
		}
	}
	return segment == ""
}
