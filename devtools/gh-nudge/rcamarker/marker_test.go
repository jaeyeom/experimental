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
	for _, class := range []string{"our-code", "flake", "infra"} {
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
