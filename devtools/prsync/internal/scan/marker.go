package scan

import (
	"fmt"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/rcamarker"
)

// renderRCAMarker renders the gh-merge rca_marker template.
func renderRCAMarker(marker, eventID, class string) string {
	return rcamarker.Render(marker, eventID, class)
}

// parseRCAMarker parses comments with the gh-merge rca_marker template.
func parseRCAMarker(marker, eventID string, comments []string) (string, bool, error) {
	class, matched, err := rcamarker.Parse(marker, eventID, comments)
	if err != nil {
		return "", false, fmt.Errorf("rca marker: %w", err)
	}
	return class, matched, nil
}
