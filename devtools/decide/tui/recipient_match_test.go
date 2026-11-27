package tui

import (
	"strings"
	"testing"
)

func spanTexts(s string, spans []matchSpan) []string {
	out := make([]string, 0, len(spans))
	for _, sp := range spans {
		out = append(out, s[sp.start:sp.end])
	}
	return out
}

func TestRankCandidatesOrder(t *testing.T) {
	t.Parallel()
	cands := []matchCandidate{
		{Name: "doctor", When: "checks decide health"},
		{Name: "tools/config", When: "edits decide config"},
		{Name: "ops/legacy", Aliases: []string{"decide-old"}, When: "legacy agent"},
		{Name: "d/e/c/i/d/e", When: "subsequence match"},
		{Name: "mydecide", When: "substring match"},
		{Name: "pm/decide", When: "product manager"},
		{Name: "decide", When: "top-level"},
	}
	got := rankCandidates(cands, "decide")
	wantNames := []string{
		"decide",       // 1. name prefix
		"pm/decide",    // 2. segment after / prefix
		"mydecide",     // 3. substring in name
		"d/e/c/i/d/e",  // 4. fuzzy subsequence
		"ops/legacy",   // 5. contact alias
		"doctor",       // 6. description only
		"tools/config", // 6. description only
	}
	if len(got) != len(wantNames) {
		t.Fatalf("got %d matches, want %d", len(got), len(wantNames))
	}
	for i, w := range wantNames {
		if name := cands[got[i].index].Name; name != w {
			t.Errorf("rank[%d] = %q, want %q", i, name, w)
		}
	}
	for i := range 5 {
		if got[i].match.descOnly {
			t.Errorf("rank[%d] (%s) unexpectedly descOnly", i, cands[got[i].index].Name)
		}
	}
	for i := 5; i < 7; i++ {
		if !got[i].match.descOnly {
			t.Errorf("rank[%d] (%s) want descOnly=true", i, cands[got[i].index].Name)
		}
	}
}

func TestRankCandidatesTiesKeepStarredFirstThenOrder(t *testing.T) {
	t.Parallel()
	cands := []matchCandidate{
		{Name: "a/decide"},
		{Name: "b/decide", Starred: true},
		{Name: "c/decide"},
		{Name: "d/decide", Starred: true},
	}
	got := rankCandidates(cands, "decide")
	want := []string{"b/decide", "d/decide", "a/decide", "c/decide"}
	var names []string
	for _, r := range got {
		names = append(names, cands[r.index].Name)
	}
	if strings.Join(names, ",") != strings.Join(want, ",") {
		t.Fatalf("got %v, want %v", names, want)
	}
}

func TestMatchSubsequenceAndHighlightSpans(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name      string
		cand      matchCandidate
		query     string
		wantMatch bool
		wantRank  matchRank
		wantDesc  bool
		wantName  []string
		wantAlias []string
		wantWhen  []string
	}{
		{
			name:      "prefix",
			cand:      matchCandidate{Name: "decide"},
			query:     "dec",
			wantMatch: true,
			wantRank:  rankNamePrefix,
			wantName:  []string{"dec"},
		},
		{
			name:      "segment prefix",
			cand:      matchCandidate{Name: "pm/decide"},
			query:     "dec",
			wantMatch: true,
			wantRank:  rankSegmentPrefix,
			wantName:  []string{"dec"},
		},
		{
			name:      "substring",
			cand:      matchCandidate{Name: "pm/decide"},
			query:     "cid",
			wantMatch: true,
			wantRank:  rankNameSubstring,
			wantName:  []string{"cid"},
		},
		{
			name:      "subsequence pmd matches pm/decide",
			cand:      matchCandidate{Name: "pm/decide"},
			query:     "pmd",
			wantMatch: true,
			wantRank:  rankNameSubsequence,
			wantName:  []string{"pm", "d"},
		},
		{
			name:      "multi-word pm dec",
			cand:      matchCandidate{Name: "pm/decide"},
			query:     "pm dec",
			wantMatch: true,
			wantRank:  rankSegmentPrefix,
			wantName:  []string{"pm", "dec"},
		},
		{
			name:      "multi-word split across name and when",
			cand:      matchCandidate{Name: "pm/decide", When: "handles product roadmap"},
			query:     "pm roadmap",
			wantMatch: true,
			wantRank:  rankDescription,
			wantDesc:  false,
			wantName:  []string{"pm"},
			wantWhen:  []string{"roadmap"},
		},
		{
			name:      "multi-word missing one word does not match",
			cand:      matchCandidate{Name: "pm/decide", When: "handles product roadmap"},
			query:     "pm missing",
			wantMatch: false,
		},
		{
			name:      "alias match",
			cand:      matchCandidate{Name: "pm/decide", Aliases: []string{"chief"}},
			query:     "chi",
			wantMatch: true,
			wantRank:  rankAlias,
			wantAlias: []string{"chi"},
		},
		{
			name:      "description only match",
			cand:      matchCandidate{Name: "doctor", When: "checks Decide health"},
			query:     "decide",
			wantMatch: true,
			wantRank:  rankDescription,
			wantDesc:  true,
			wantWhen:  []string{"Decide"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := matchCandidateQuery(tc.cand, tc.query)
			if got.matched != tc.wantMatch {
				t.Fatalf("matched = %v, want %v", got.matched, tc.wantMatch)
			}
			if !tc.wantMatch {
				return
			}
			if got.rank != tc.wantRank {
				t.Errorf("rank = %d, want %d", got.rank, tc.wantRank)
			}
			if got.descOnly != tc.wantDesc {
				t.Errorf("descOnly = %v, want %v", got.descOnly, tc.wantDesc)
			}
			if gotName := spanTexts(tc.cand.Name, got.nameSpans); strings.Join(gotName, "|") != strings.Join(tc.wantName, "|") {
				t.Errorf("nameSpans = %q, want %q", gotName, tc.wantName)
			}
			if gotAlias := spanTexts(got.alias, got.aliasSpans); strings.Join(gotAlias, "|") != strings.Join(tc.wantAlias, "|") {
				t.Errorf("aliasSpans = %q, want %q", gotAlias, tc.wantAlias)
			}
			if gotWhen := spanTexts(strings.TrimSpace(tc.cand.When), got.whenSpans); strings.Join(gotWhen, "|") != strings.Join(tc.wantWhen, "|") {
				t.Errorf("whenSpans = %q, want %q", gotWhen, tc.wantWhen)
			}
		})
	}
}

func TestWhenSnippetCentersAroundMatch(t *testing.T) {
	t.Parallel()
	when := "Monitors the entire fleet of background workers and helps decide when to restart stuck panes"
	m := matchCandidateQuery(matchCandidate{Name: "doctor", When: when}, "decide")
	if !m.matched || !m.descOnly {
		t.Fatalf("match = %+v", m)
	}
	snip, spans := whenSnippet(when, m.whenSpans, 30)
	if !strings.HasPrefix(snip, "…") || !strings.HasSuffix(snip, "…") {
		t.Fatalf("snippet = %q, want leading and trailing ellipsis", snip)
	}
	if !strings.Contains(snip, "decide") {
		t.Fatalf("snippet = %q, missing matched word", snip)
	}
	if got := spanTexts(snip, spans); strings.Join(got, "|") != "decide" {
		t.Fatalf("snippet spans = %q, want [decide]", got)
	}
}

func TestPickerRankedSearchWithMatchedDescriptionAndHighlights(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	const agentsJSON = `[
		{"name":"doctor","when":"Monitors the entire fleet of background workers and helps decide when to restart stuck panes","liveness":"alive","handle":"herdr:local/default/w1:p1"},
		{"name":"tools/config","when":"Updates decide settings across hosts","liveness":"alive","handle":"herdr:local/default/w1:p2"},
		{"name":"ops/legacy","when":"Legacy operations","liveness":"alive","handle":"herdr:local/default/w1:p3"},
		{"name":"pm/decide","when":"Product manager for inbox","liveness":"alive","handle":"herdr:local/default/w1:p4"}
	]`
	bus := &fakeBus{list: agentsJSON}
	m := busModel(t, bus)
	m.Targets = nil
	m.aliases = map[string]string{"decide-alias": "ops/legacy"}
	m.width, m.height = 120, 40
	loadBus(t, m)

	// 1. "decide" ranks pm/decide first, alias match after name matches,
	//    and description-only matches under "matched description" with snippets.
	press(t, m, "cdecide")
	wantOrder := "pm/decide,ops/legacy,doctor,tools/config"
	if got := strings.Join(visibleNames(m), ","); got != wantOrder {
		t.Fatalf("visibleNames(decide) = %q, want %q", got, wantOrder)
	}
	if tg, ok := m.selectedTarget(); !ok || tg.Name != "pm/decide" {
		t.Fatalf("selectedTarget = %+v (%v), want pm/decide", tg, ok)
	}
	rawView := m.View()
	plainView := visibleLine(rawView)
	if !strings.Contains(plainView, matchedDescLabel) {
		t.Fatalf("picker view missing %q header:\n%s", matchedDescLabel, plainView)
	}
	if strings.Index(plainView, "pm/decide") > strings.Index(plainView, matchedDescLabel) {
		t.Fatalf("pm/decide should appear before %q:\n%s", matchedDescLabel, plainView)
	}
	if strings.Index(plainView, matchedDescLabel) > strings.Index(plainView, "doctor") {
		t.Fatalf("doctor should appear after %q:\n%s", matchedDescLabel, plainView)
	}
	// Long description of doctor must be snippeted around "decide".
	if !strings.Contains(plainView, "helps decide when") {
		t.Fatalf("doctor description snippet missing around match:\n%s", plainView)
	}
	// Highlight spans rendered with searchMatch.
	useStyles()
	if !strings.Contains(rawView, searchMatch.Render("decide")) {
		t.Fatalf("picker view missing highlighted %q", "decide")
	}

	// 2. "pmd" finds pm/decide by subsequence and highlights "pm" and "d".
	press(t, m, "\x1b") // clear filter
	press(t, m, "pmd")
	if got := strings.Join(visibleNames(m), ","); got != "pm/decide" {
		t.Fatalf("visibleNames(pmd) = %q, want pm/decide", got)
	}
	rawView = m.View()
	if !strings.Contains(rawView, searchMatch.Render("pm")) || !strings.Contains(rawView, searchMatch.Render("d")) {
		t.Fatalf("subsequence highlights missing in view:\n%s", rawView)
	}

	// 3. "pm dec" (two words) matches pm/decide.
	press(t, m, "\x1b")
	press(t, m, "pm dec")
	if got := strings.Join(visibleNames(m), ","); got != "pm/decide" {
		t.Fatalf("visibleNames(pm dec) = %q, want pm/decide", got)
	}
}

func TestContactsSearchMatchesPickerOrderAndHighlights(t *testing.T) {
	t.Setenv("NO_COLOR", "")
	const agentsJSON = `[
		{"name":"doctor","when":"Monitors the entire fleet of background workers and helps decide when to restart stuck panes","liveness":"alive","handle":"herdr:local/default/w1:p1"},
		{"name":"tools/config","when":"Updates decide settings across hosts","liveness":"alive","handle":"herdr:local/default/w1:p2"},
		{"name":"ops/legacy","when":"Legacy operations","liveness":"alive","handle":"herdr:local/default/w1:p3"},
		{"name":"pm/decide","when":"Product manager for inbox","liveness":"alive","handle":"herdr:local/default/w1:p4"},
		{"name":"alpha/decide","when":"Alpha decide","liveness":"alive","handle":"herdr:local/default/w1:p5"}
	]`
	bus := &fakeBus{list: agentsJSON}
	m := busModel(t, bus)
	m.Targets = nil
	m.aliases = map[string]string{"decide-alias": "ops/legacy"}
	m.favorites = []string{"pm/decide"}
	m.width, m.height = 140, 40
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	loadBus(t, m)

	// Picker order for "decide": starred pm/decide beats unstarred alpha/decide (both segment prefix).
	press(t, m, "cdecide")
	pickerOrder := visibleNames(m)
	press(t, m, "\x1b\x1b") // clear filter, leave compose

	// Contacts search for "decide".
	if err := m.showContacts(); err != nil {
		t.Fatal(err)
	}
	finishContactBus(t, m)
	press(t, m, "/decide\r")
	contactsOrder := contactNames(m.visibleContacts())

	if strings.Join(contactsOrder, ",") != strings.Join(pickerOrder, ",") {
		t.Fatalf("contacts order %q != picker order %q", contactsOrder, pickerOrder)
	}
	wantOrder := "pm/decide,alpha/decide,ops/legacy,doctor,tools/config"
	if strings.Join(contactsOrder, ",") != wantOrder {
		t.Fatalf("order = %q, want %q", contactsOrder, wantOrder)
	}

	rawView := m.View()
	plainView := visibleLine(rawView)
	if !strings.Contains(plainView, matchedDescLabel) {
		t.Fatalf("contacts view missing %q header:\n%s", matchedDescLabel, plainView)
	}
	if !strings.Contains(plainView, "helps decide when") {
		t.Fatalf("contacts view missing description snippet:\n%s", plainView)
	}
	useStyles()
	if !strings.Contains(rawView, searchMatch.Render("decide")) {
		t.Fatalf("contacts view missing highlighted %q", "decide")
	}
}
