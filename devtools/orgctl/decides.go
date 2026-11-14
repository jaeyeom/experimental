package orgctl

import (
	"fmt"
	"slices"
	"strings"
	"unicode"
)

// defaultApprovalWords are the verbs a role must not claim.
func defaultApprovalWords() []string {
	return []string{
		"merge", "post", "approve", "waive", "install",
		"settings", "deploy", "release", "delete", "grant",
	}
}

// CheckDecides rejects a decides entry that contains an approval word.
// Words match case-insensitively, as a whole word, including the
// inflections s, ed, and ing. extra and ApprovalWords extend the default
// list. AllowApprovalWords drops default words, and each dropped word is
// returned as a warning.
func (s Spec) CheckDecides(extra []string) ([]string, error) {
	defaults := map[string]bool{}
	for _, w := range defaultApprovalWords() {
		defaults[w] = true
	}
	dropped := map[string]bool{}
	var warnings []string
	for _, w := range s.AllowApprovalWords {
		w = strings.ToLower(strings.TrimSpace(w))
		if w == "" {
			continue
		}
		if defaults[w] {
			dropped[w] = true
		}
		warnings = append(warnings, fmt.Sprintf("warning: allowApprovalWords %q", w))
	}
	seen := map[string]bool{}
	var words []string
	add := func(list []string) {
		for _, w := range list {
			w = strings.ToLower(strings.TrimSpace(w))
			if w == "" || seen[w] || dropped[w] {
				continue
			}
			seen[w] = true
			words = append(words, w)
		}
	}
	add(defaultApprovalWords())
	add(s.ApprovalWords)
	add(extra)

	names := make([]string, 0, len(s.Roles))
	for name := range s.Roles {
		names = append(names, name)
	}
	slices.Sort(names)
	for _, name := range names {
		for _, entry := range s.Roles[name].Decides {
			word, ok := approvalWord(entry, words)
			if !ok {
				continue
			}
			return warnings, fmt.Errorf("role %s: decides %q contains approval word %q; approvals belong to the user, not a role", name, entry, word)
		}
	}
	return warnings, nil
}

// approvalWord reports the first listed approval word that appears in entry.
func approvalWord(entry string, words []string) (string, bool) {
	var tokens []string
	var b strings.Builder
	flush := func() {
		if b.Len() == 0 {
			return
		}
		tokens = append(tokens, b.String())
		b.Reset()
	}
	for _, r := range entry {
		if unicode.IsLetter(r) {
			b.WriteRune(unicode.ToLower(r))
			continue
		}
		flush()
	}
	flush()
	for _, word := range words {
		for _, token := range tokens {
			if approvalToken(token, word) {
				return word, true
			}
		}
	}
	return "", false
}

// approvalToken reports whether token is word or an s, ed, or ing form.
func approvalToken(token, word string) bool {
	if token == word || token == word+"s" || token == word+"ed" || token == word+"ing" {
		return true
	}
	if strings.HasSuffix(word, "e") {
		stem := strings.TrimSuffix(word, "e")
		if token == word+"d" || token == stem+"ing" {
			return true
		}
	}
	return false
}
