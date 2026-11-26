package source

import (
	"slices"
	"testing"
	"time"
)

func TestLabelNotBefore(t *testing.T) {
	got, bad := LabelNotBefore([]string{"bug", "not-before:2026-12-01", "not-before:2027-01-05", "not-before:soon"}, "not-before:")
	if got == nil || !got.Equal(time.Date(2027, 1, 5, 0, 0, 0, 0, time.UTC)) {
		t.Fatalf("date = %v, want the latest 2027-01-05", got)
	}
	if !slices.Equal(bad, []string{"not-before:soon"}) {
		t.Fatalf("bad = %v", bad)
	}
	if got, bad := LabelNotBefore([]string{"not-before:2026-12-01"}, ""); got != nil || bad != nil {
		t.Fatalf("empty prefix: %v %v", got, bad)
	}
}
