package rcamarker

import "testing"

func TestRenderDefaultMarker(t *testing.T) {
	got := Render("RCA {event_id} class={class}", "EVT", "flake")
	if got != "RCA EVT class=flake" {
		t.Fatalf("Render() = %q, want RCA EVT class=flake", got)
	}
}

func TestParseAcceptsClasses(t *testing.T) {
	const marker = "RCA {event_id} class={class}"
	for _, class := range []string{"our-code", "flake", "infra", "manual"} {
		t.Run(class, func(t *testing.T) {
			comment := "note\n" + Render(marker, "EVT", class)
			got, matched, err := Parse(marker, "EVT", []string{comment})
			if err != nil {
				t.Fatalf("Parse() error = %v", err)
			}
			if !matched || got != class {
				t.Fatalf("Parse() = %q, %v, want %q, true", got, matched, class)
			}
		})
	}
}

func TestParseOurCodeWinsAndLastOtherClass(t *testing.T) {
	const marker = "RCA {event_id} class={class}"
	got, matched, err := Parse(marker, "EVT", []string{
		"RCA EVT class=flake",
		"RCA EVT class=our-code",
		"RCA EVT class=infra",
	})
	if err != nil {
		t.Fatalf("Parse() error = %v", err)
	}
	if !matched || got != "our-code" {
		t.Fatalf("Parse() = %q, %v, want our-code, true", got, matched)
	}

	got, matched, err = Parse(marker, "EVT", []string{
		"RCA EVT class=flake",
		"RCA EVT class=infra",
	})
	if err != nil {
		t.Fatalf("Parse() error = %v", err)
	}
	if !matched || got != "infra" {
		t.Fatalf("Parse() = %q, %v, want infra, true", got, matched)
	}
}

func TestParseMisses(t *testing.T) {
	const marker = "RCA {event_id} class={class}"
	tests := []struct {
		name     string
		marker   string
		eventID  string
		comments []string
	}{
		{name: "empty marker", marker: "", eventID: "EVT", comments: []string{"RCA EVT class=flake"}},
		{name: "missing class", marker: "RCA {event_id}", eventID: "EVT", comments: []string{"RCA EVT"}},
		{name: "missing event", marker: "class={class}", eventID: "EVT", comments: []string{"class=flake"}},
		{name: "other class", marker: marker, eventID: "EVT", comments: []string{"RCA EVT class=other"}},
		{name: "other event", marker: marker, eventID: "EVT", comments: []string{"RCA OTHER class=flake"}},
		{name: "no comments", marker: marker, eventID: "EVT"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, matched, err := Parse(tc.marker, tc.eventID, tc.comments)
			if err != nil {
				t.Fatalf("Parse() error = %v", err)
			}
			if matched || got != "" {
				t.Fatalf("Parse() = %q, %v, want unmatched", got, matched)
			}
		})
	}
}

func TestMatchExtractsEventAndClass(t *testing.T) {
	const marker = "RCA {event_id} class={class}"
	got, ok, err := Match(marker, "Investigated.\nRCA EVT.1 class=our-code\n")
	if err != nil {
		t.Fatalf("Match() error = %v", err)
	}
	if !ok || got.EventID != "EVT.1" || got.Class != "our-code" || len(got.Keys) != 0 {
		t.Fatalf("Match() = %+v, %v", got, ok)
	}
	for _, class := range []string{"flake", "infra"} {
		got, ok, err := Match(marker, Render(marker, "EVT", class))
		if err != nil || !ok || got.Class != class {
			t.Fatalf("Match(%s) = %+v, %v, %v", class, got, ok, err)
		}
	}
	_, ok, err = Match(marker, "RCA EVT class=other")
	if err != nil || ok {
		t.Fatalf("Match(other) = ok %v, err %v", ok, err)
	}
}

func TestRenderCustomMarker(t *testing.T) {
	const marker = "ENTRY {event_id} class={class}"
	got := Render(marker, "NEW", "infra")
	if got != "ENTRY NEW class=infra" {
		t.Fatalf("Render() = %q", got)
	}
	class, matched, err := Parse(marker, "NEW", []string{got})
	if err != nil {
		t.Fatalf("Parse() error = %v", err)
	}
	if !matched || class != "infra" {
		t.Fatalf("Parse() = %q, %v", class, matched)
	}
}

const commentMarker = "<!-- gh-merge-rca {event_id} class={class} -->"

func TestRenderAddsKeysBeforeSuffix(t *testing.T) {
	got := Render(commentMarker, "EVT", "flake",
		Key{Name: KeyCommit, Value: "abc123"},
		Key{Name: KeyJob, Value: "unit tests"},
		Key{Name: KeyTest, Value: ""},
		Key{Name: KeyTest, Value: "a-->b"},
	)
	want := "<!-- gh-merge-rca EVT class=flake commit=abc123 job=unit_tests test=a--b -->"
	if got != want {
		t.Fatalf("Render() = %q, want %q", got, want)
	}
	if got := Render("RCA {event_id} class={class}", "EVT", "infra", Key{Name: KeyJob, Value: "build"}); got != "RCA EVT class=infra job=build" {
		t.Fatalf("Render() without suffix = %q", got)
	}
}

func TestMatchTrailingKeys(t *testing.T) {
	tests := []struct {
		name    string
		marker  string
		comment string
		ok      bool
		keys    map[string]string
	}{
		{name: "old marker", marker: commentMarker, comment: "body\n<!-- gh-merge-rca EVT class=flake -->", ok: true, keys: map[string]string{}},
		{
			name:    "extra keys",
			marker:  commentMarker,
			comment: "<!-- gh-merge-rca EVT class=flake tool=other commit=abc job=build test=TestX supersedes=42 -->",
			ok:      true,
			keys:    map[string]string{"tool": "other", "commit": "abc", "job": "build", "test": "TestX", "supersedes": "42"},
		},
		{name: "unknown key ignored", marker: commentMarker, comment: "<!-- gh-merge-rca EVT class=flake color=red commit=abc -->", ok: true, keys: map[string]string{"commit": "abc"}},
		{name: "token without value", marker: commentMarker, comment: "<!-- gh-merge-rca EVT class=flake commit= -->"},
		{name: "token without key", marker: commentMarker, comment: "<!-- gh-merge-rca EVT class=flake =abc -->"},
		{name: "bare word", marker: commentMarker, comment: "<!-- gh-merge-rca EVT class=flake junk -->"},
		{name: "value with arrow", marker: commentMarker, comment: "<!-- gh-merge-rca EVT class=flake job=a-->b -->"},
		{name: "no suffix keys", marker: "RCA {event_id} class={class}", comment: "RCA EVT class=flake job=build\n", ok: true, keys: map[string]string{"job": "build"}},
		{name: "no suffix bare word", marker: "RCA {event_id} class={class}", comment: "RCA EVT class=flake junk"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, ok, err := Match(tc.marker, tc.comment)
			if err != nil {
				t.Fatalf("Match() error = %v", err)
			}
			if ok != tc.ok {
				t.Fatalf("Match() ok = %v, want %v (%+v)", ok, tc.ok, got)
			}
			if !ok {
				return
			}
			if got.EventID != "EVT" || got.Class != "flake" {
				t.Fatalf("Match() = %+v", got)
			}
			if len(got.Keys) != len(tc.keys) {
				t.Fatalf("Keys = %v, want %v", got.Keys, tc.keys)
			}
			for k, v := range tc.keys {
				if got.Keys[k] != v {
					t.Fatalf("Keys = %v, want %v", got.Keys, tc.keys)
				}
			}
		})
	}
}

func TestParseOldAndKeyedMarkers(t *testing.T) {
	got, matched, err := Parse(commentMarker, "EVT", []string{"<!-- gh-merge-rca EVT class=infra commit=abc job=build -->"})
	if err != nil || !matched || got != "infra" {
		t.Fatalf("Parse() = %q, %v, %v", got, matched, err)
	}
	_, matched, err = Parse(commentMarker, "EVT", []string{"<!-- gh-merge-rca EVT class=infra junk -->"})
	if err != nil || matched {
		t.Fatalf("Parse(malformed) matched = %v, err = %v", matched, err)
	}
}

func TestParseSupersedes(t *testing.T) {
	const marker = "RCA {event_id} class={class}"
	tests := []struct {
		name     string
		comments []string
		want     string
	}{
		{name: "our-code wins without supersedes", comments: []string{"RCA EVT class=our-code", "RCA EVT class=flake"}, want: "our-code"},
		{name: "supersedes replaces our-code", comments: []string{"RCA EVT class=our-code", "RCA EVT class=flake supersedes=101"}, want: "flake"},
		{name: "later our-code still wins", comments: []string{"RCA EVT class=our-code", "RCA EVT class=flake supersedes=101", "RCA EVT class=our-code"}, want: "our-code"},
		{name: "supersedes to our-code", comments: []string{"RCA EVT class=flake", "RCA EVT class=our-code supersedes=101"}, want: "our-code"},
		{name: "other tool supersedes ignored", comments: []string{"RCA EVT class=our-code", "RCA EVT class=flake tool=other supersedes=101"}, want: "our-code"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, matched, err := Parse(marker, "EVT", tc.comments)
			if err != nil || !matched || got != tc.want {
				t.Fatalf("Parse() = %q, %v, %v, want %q", got, matched, err, tc.want)
			}
		})
	}
}

func TestParseIgnoresOtherTools(t *testing.T) {
	const marker = "RCA {event_id} class={class}"
	comments := []string{"RCA EVT class=flake tool=other", "RCA EVT class=our-code tool=bot"}
	got, matched, err := Parse(marker, "EVT", comments)
	if err != nil || matched || got != "" {
		t.Fatalf("Parse() = %q, %v, %v, want unmatched", got, matched, err)
	}
	got, matched, err = Parse(marker, "EVT", []string{"RCA EVT class=infra tool=gh-merge"})
	if err != nil || !matched || got != "infra" {
		t.Fatalf("Parse(tool=gh-merge) = %q, %v, %v", got, matched, err)
	}
	others, err := Others(marker, "EVT", append(comments, "RCA EVT class=infra", "RCA OTHER class=flake tool=x"))
	if err != nil {
		t.Fatalf("Others() error = %v", err)
	}
	if len(others) != 2 || others[0].Tool() != "other" || others[0].Class != "flake" || others[1].Tool() != "bot" {
		t.Fatalf("Others() = %+v", others)
	}
}
