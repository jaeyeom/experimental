package tui

import (
	"cmp"
	"slices"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/charmbracelet/lipgloss"
)

// matchedDescLabel is the group header shown above recipients or
// contacts that matched only in their description ("when" text).
const matchedDescLabel = "matched description"

type matchRank int

const (
	rankNamePrefix matchRank = iota
	rankSegmentPrefix
	rankNameSubstring
	rankNameSubsequence
	rankAlias
	rankDescription
)

// matchSpan is a half-open byte range [start, end) in a matched string.
type matchSpan struct {
	start int
	end   int
}

// matchCandidate is the searchable view of a compose target or contact.
type matchCandidate struct {
	Name    string
	Aliases []string
	When    string
	Starred bool
}

// recipientMatch describes how a candidate matched a search query.
type recipientMatch struct {
	matched    bool
	rank       matchRank
	descOnly   bool
	nameSpans  []matchSpan
	alias      string
	aliasSpans []matchSpan
	whenSpans  []matchSpan
}

// rankedMatch pairs an original candidate index with its match result.
type rankedMatch struct {
	index int
	match recipientMatch
}

type indexedRune struct {
	start int
	end   int
	r     rune
}

func indexLowerRunes(s string) []indexedRune {
	out := make([]indexedRune, 0, len(s))
	for i, r := range s {
		out = append(out, indexedRune{
			start: i,
			end:   i + utf8.RuneLen(r),
			r:     unicode.ToLower(r),
		})
	}
	return out
}

func lowerRunes(s string) []rune {
	out := make([]rune, 0, len(s))
	for _, r := range s {
		out = append(out, unicode.ToLower(r))
	}
	return out
}

func runesHasPrefix(ir []indexedRune, needle []rune) bool {
	if len(needle) > len(ir) {
		return false
	}
	for i, r := range needle {
		if ir[i].r != r {
			return false
		}
	}
	return true
}

// matchNameWord matches one lower-case word against a name or alias in
// priority order: prefix, segment prefix after '/', substring, then
// fuzzy subsequence.
func matchNameWord(name string, word []rune) (matchRank, []matchSpan, bool) {
	if len(word) == 0 {
		return rankNamePrefix, nil, true
	}
	ir := indexLowerRunes(name)
	if len(ir) < len(word) {
		return 0, nil, false
	}
	if runesHasPrefix(ir, word) {
		return rankNamePrefix, []matchSpan{{start: ir[0].start, end: ir[len(word)-1].end}}, true
	}
	for i := 1; i <= len(ir)-len(word); i++ {
		if ir[i-1].r == '/' && runesHasPrefix(ir[i:], word) {
			return rankSegmentPrefix, []matchSpan{{start: ir[i].start, end: ir[i+len(word)-1].end}}, true
		}
	}
	for i := 1; i <= len(ir)-len(word); i++ {
		if runesHasPrefix(ir[i:], word) {
			return rankNameSubstring, []matchSpan{{start: ir[i].start, end: ir[i+len(word)-1].end}}, true
		}
	}
	spans := make([]matchSpan, 0, len(word))
	wi := 0
	for _, ch := range ir {
		if ch.r == word[wi] {
			spans = append(spans, matchSpan{start: ch.start, end: ch.end})
			wi++
			if wi == len(word) {
				return rankNameSubsequence, mergeSpans(spans), true
			}
		}
	}
	return 0, nil, false
}

// matchSubstring finds the first case-insensitive substring match of
// word in text.
func matchSubstring(text string, word []rune) ([]matchSpan, bool) {
	if len(word) == 0 {
		return nil, true
	}
	ir := indexLowerRunes(text)
	for i := 0; i <= len(ir)-len(word); i++ {
		if runesHasPrefix(ir[i:], word) {
			return []matchSpan{{start: ir[i].start, end: ir[i+len(word)-1].end}}, true
		}
	}
	return nil, false
}

func mergeSpans(spans []matchSpan) []matchSpan {
	if len(spans) <= 1 {
		return spans
	}
	sorted := slices.Clone(spans)
	slices.SortFunc(sorted, func(a, b matchSpan) int {
		if c := cmp.Compare(a.start, b.start); c != 0 {
			return c
		}
		return cmp.Compare(a.end, b.end)
	})
	out := sorted[:1]
	for _, sp := range sorted[1:] {
		last := &out[len(out)-1]
		if sp.start <= last.end {
			if sp.end > last.end {
				last.end = sp.end
			}
		} else {
			out = append(out, sp)
		}
	}
	return out
}

// matchCandidateQuery matches query (possibly multi-word) against c.
// Every word must match in c.Name, c.Aliases, or c.When.
func matchCandidateQuery(c matchCandidate, query string) recipientMatch {
	words := strings.Fields(query)
	if len(words) == 0 {
		return recipientMatch{matched: true}
	}
	when := strings.TrimSpace(c.When)
	var (
		anyNameOrAlias bool
		worstRank      matchRank
		nameSpans      []matchSpan
		matchedAlias   string
		aliasSpans     []matchSpan
		whenSpans      []matchSpan
	)
	for _, w := range words {
		wr := lowerRunes(w)
		if r, spans, ok := matchNameWord(c.Name, wr); ok {
			anyNameOrAlias = true
			nameSpans = append(nameSpans, spans...)
			if r > worstRank {
				worstRank = r
			}
			continue
		}
		var (
			bestAlias      string
			bestAliasRank  matchRank
			bestAliasSpans []matchSpan
			aliasOK        bool
		)
		for _, a := range c.Aliases {
			if r, spans, ok := matchNameWord(a, wr); ok && (!aliasOK || r < bestAliasRank) {
				bestAlias = a
				bestAliasRank = r
				bestAliasSpans = spans
				aliasOK = true
			}
		}
		if aliasOK {
			anyNameOrAlias = true
			if rankAlias > worstRank {
				worstRank = rankAlias
			}
			if matchedAlias == "" || matchedAlias == bestAlias {
				matchedAlias = bestAlias
				aliasSpans = append(aliasSpans, bestAliasSpans...)
			}
			continue
		}
		if spans, ok := matchSubstring(when, wr); ok {
			whenSpans = append(whenSpans, spans...)
			if rankDescription > worstRank {
				worstRank = rankDescription
			}
			continue
		}
		return recipientMatch{}
	}
	return recipientMatch{
		matched:    true,
		rank:       worstRank,
		descOnly:   !anyNameOrAlias,
		nameSpans:  mergeSpans(nameSpans),
		alias:      matchedAlias,
		aliasSpans: mergeSpans(aliasSpans),
		whenSpans:  mergeSpans(whenSpans),
	}
}

// rankCandidates filters and ranks cands for query. Empty/whitespace
// queries return all candidates in their original order. Ties keep
// starred candidates first, then original order.
func rankCandidates(cands []matchCandidate, query string) []rankedMatch {
	if strings.TrimSpace(query) == "" {
		out := make([]rankedMatch, len(cands))
		for i := range cands {
			out[i] = rankedMatch{index: i, match: recipientMatch{matched: true}}
		}
		return out
	}
	out := make([]rankedMatch, 0, len(cands))
	for i, c := range cands {
		if m := matchCandidateQuery(c, query); m.matched {
			out = append(out, rankedMatch{index: i, match: m})
		}
	}
	slices.SortStableFunc(out, func(a, b rankedMatch) int {
		if a.match.descOnly != b.match.descOnly {
			if a.match.descOnly {
				return 1
			}
			return -1
		}
		if c := cmp.Compare(a.match.rank, b.match.rank); c != 0 {
			return c
		}
		if sa, sb := cands[a.index].Starred, cands[b.index].Starred; sa != sb {
			if sa {
				return -1
			}
			return 1
		}
		return 0
	})
	return out
}

// whenSnippet returns a short snippet of when (at most limit runes of
// content) centered around the first match span, adding "…" where
// trimmed, and adjusts spans to byte offsets within the returned snippet.
func whenSnippet(when string, spans []matchSpan, limit int) (string, []matchSpan) {
	when = strings.TrimSpace(when)
	if when == "" || limit <= 0 {
		return when, nil
	}
	ir := indexLowerRunes(when)
	if len(ir) <= limit {
		return when, spans
	}
	if len(spans) == 0 {
		return when[:ir[limit-1].end] + "…", nil
	}
	first := spans[0]
	rStart, rEnd := 0, len(ir)
	for i, ch := range ir {
		if ch.start == first.start {
			rStart = i
		}
		if ch.end == first.end {
			rEnd = i + 1
			break
		}
	}
	matchLen := rEnd - rStart
	padLeft := max((limit-matchLen)/2, 0)
	winStart := max(rStart-padLeft, 0)
	winEnd := winStart + limit
	if winEnd > len(ir) {
		winEnd = len(ir)
		winStart = max(winEnd-limit, 0)
	}
	byteStart := ir[winStart].start
	byteEnd := ir[winEnd-1].end
	var b strings.Builder
	prefixBytes := 0
	if winStart > 0 {
		b.WriteString("…")
		prefixBytes = len("…")
	}
	b.WriteString(when[byteStart:byteEnd])
	if winEnd < len(ir) {
		b.WriteString("…")
	}
	var adjusted []matchSpan
	for _, sp := range spans {
		s := max(sp.start, byteStart)
		e := min(sp.end, byteEnd)
		if s >= e {
			continue
		}
		adjusted = append(adjusted, matchSpan{
			start: s - byteStart + prefixBytes,
			end:   e - byteStart + prefixBytes,
		})
	}
	return b.String(), mergeSpans(adjusted)
}

// paintSpans renders s in base style with each span rendered in searchMatch.
func paintSpans(s string, base lipgloss.Style, spans []matchSpan) string {
	if len(spans) == 0 {
		return base.Render(s)
	}
	useStyles()
	var b strings.Builder
	pos := 0
	for _, sp := range spans {
		start, end := max(sp.start, pos), min(sp.end, len(s))
		if start >= end {
			continue
		}
		if pos < start {
			b.WriteString(base.Render(s[pos:start]))
		}
		b.WriteString(searchMatch.Render(s[start:end]))
		pos = end
	}
	if pos < len(s) {
		b.WriteString(base.Render(s[pos:]))
	}
	return b.String()
}

// aliasesFor returns all known aliases (from m.aliases and extra, such
// as contact.Aka) that map to canonical name, sorted and deduplicated.
func (m *Model) aliasesFor(name string, extra []string) []string {
	var out []string
	for alias, target := range m.aliases {
		if target == name && alias != "" && alias != name {
			out = append(out, alias)
		}
	}
	for _, a := range extra {
		if a != "" && a != name && !slices.Contains(out, a) {
			out = append(out, a)
		}
	}
	slices.Sort(out)
	return out
}

// targetAliases returns aliases for a compose target name, combining
// configured contactAliases with any ledger Aka names on the contact.
func (m *Model) targetAliases(name string) []string {
	var extra []string
	for _, c := range m.contacts {
		if c.Name == name {
			extra = c.Aka
			break
		}
	}
	return m.aliasesFor(name, extra)
}
