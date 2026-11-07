package scan

import "testing"

func TestSharedRCAMarker(t *testing.T) {
	const marker = "RCA {event_id} class={class}"
	for _, class := range []string{"our-code", "flake", "infra"} {
		t.Run(class, func(t *testing.T) {
			line := renderRCAMarker(marker, "EVT", class)
			want := "RCA EVT class=" + class
			if line != want {
				t.Fatalf("renderRCAMarker() = %q, want %q", line, want)
			}
			got, matched, err := parseRCAMarker(marker, "EVT", []string{line})
			if err != nil {
				t.Fatalf("parseRCAMarker() error = %v", err)
			}
			if !matched || got != class {
				t.Fatalf("parseRCAMarker() = %q, %v, want %q, true", got, matched, class)
			}
		})
	}
}
