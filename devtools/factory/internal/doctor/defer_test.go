package doctor

import (
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func TestBadDeferLabels(t *testing.T) {
	t.Parallel()

	items := map[string]source.Item{
		"#1": {ID: "#1", Labels: []string{"not-before:2026-12-01"}},
		"#2": {ID: "#2", Labels: []string{"not-before:next week"}},
		"#3": {ID: "#3", Labels: []string{"not-before:bad"}, Done: true},
	}
	in := baseInput(items)
	in.Kind = "github"
	in.Config.DeferLabelPrefix = "not-before:"
	got := badDeferLabels(in)
	want := []Finding{finding(CheckBadDeferLabel, SeverityWarn,
		`#2 label "not-before:next week" is not not-before:YYYY-MM-DD, so it defers nothing`,
		"fix the date or remove the label", "#2")}
	equalFindings(t, got, want)

	in.Config.DeferLabelPrefix = ""
	if got := badDeferLabels(in); len(got) != 0 {
		t.Fatalf("no prefix: %+v", got)
	}
}
