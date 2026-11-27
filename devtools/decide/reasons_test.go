package decide

import (
	"slices"
	"strings"
	"testing"
)

func TestReasonsLoadFromConfigAndStartingList(t *testing.T) {
	writeConfig(t, `{}`)
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatalf("LoadConfig empty: %v", err)
	}
	reasons := cfg.EffectiveReasons()
	if len(reasons) != 14 {
		t.Fatalf("default reason count = %d, want 14", len(reasons))
	}
	wantCategories := map[string]bool{}
	var foundEscalation *Reason
	for i := range reasons {
		r := reasons[i]
		wantCategories[r.Category] = true
		if r.ID == ReasonAlreadyDecided {
			foundEscalation = &r
		}
	}
	for _, cat := range []string{"format", "main-text", "choices", "escalation", "other"} {
		if !wantCategories[cat] {
			t.Errorf("default reasons missing category %q", cat)
		}
	}
	if foundEscalation == nil || foundEscalation.Category != "escalation" ||
		foundEscalation.Label != "should not have been asked: an earlier human decision covers it" ||
		!foundEscalation.AllowEarlierItem {
		t.Fatalf("escalation reason = %+v", foundEscalation)
	}

	writeConfig(t, `{
		"reasons": [
			{"id": "missing-benchmark", "category": "custom", "label": "No benchmark numbers provided"},
			{"id": "other", "category": "other", "label": "Something else"}
		]
	}`)
	cfg, err = LoadConfig()
	if err != nil {
		t.Fatalf("LoadConfig custom reasons: %v", err)
	}
	reasons = cfg.EffectiveReasons()
	if len(reasons) != 2 || reasons[0].ID != "missing-benchmark" || !reasons[1].RequireNote {
		t.Fatalf("custom reasons = %+v", reasons)
	}
}

func TestValidateConfigRejectsInvalidReasons(t *testing.T) {
	cases := []struct {
		name string
		json string
		want string
	}{
		{"empty id", `{"reasons":[{"id":"","label":"Bad"}]}`, "id is empty"},
		{"whitespace id", `{"reasons":[{"id":"has space","label":"Bad"}]}`, "whitespace"},
		{"duplicate id", `{"reasons":[{"id":"dup","label":"One"},{"id":"DUP","label":"Two"}]}`, "duplicate"},
		{"empty label", `{"reasons":[{"id":"x","label":"   "}]}`, "label is empty"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			writeConfig(t, tc.json)
			_, err := LoadConfig()
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("err = %v, want substring %q", err, tc.want)
			}
		})
	}
}

func TestValidateReasonSelectionRequiresNoteForOther(t *testing.T) {
	catalog := DefaultReasons()
	if _, _, err := ValidateReasonSelection(catalog, nil, "note"); err == nil {
		t.Fatal("expected error for empty reasons")
	}
	if _, _, err := ValidateReasonSelection(catalog, []string{"bogus"}, ""); err == nil {
		t.Fatal("expected error for unknown reason")
	}
	if _, _, err := ValidateReasonSelection(catalog, []string{"other"}, "   "); err == nil || !strings.Contains(err.Error(), "requires a note") {
		t.Fatalf("other without note err = %v", err)
	}
	if _, _, err := ValidateReasonSelection(catalog, []string{"vague-pronoun", "other"}, ""); err == nil {
		t.Fatal("expected error when other is among multiple reasons without note")
	}
	selected, note, err := ValidateReasonSelection(catalog, []string{"VAGUE-PRONOUN", "actor-unclear", "vague-pronoun"}, "  clarify who acts  ")
	if err != nil {
		t.Fatalf("ValidateReasonSelection: %v", err)
	}
	if !slices.Equal(selected, []string{"vague-pronoun", "actor-unclear"}) {
		t.Fatalf("selected = %v", selected)
	}
	if note != "clarify who acts" {
		t.Fatalf("note = %q", note)
	}

	if got, err := ValidateEarlierItemID(catalog, []string{ReasonAlreadyDecided}, "  D-001  "); err != nil || got != "D-001" {
		t.Fatalf("ValidateEarlierItemID valid = (%q, %v)", got, err)
	}
	if got, err := ValidateEarlierItemID(catalog, []string{ReasonAlreadyDecided}, ""); err != nil || got != "" {
		t.Fatalf("ValidateEarlierItemID optional empty = (%q, %v)", got, err)
	}
	if _, err := ValidateEarlierItemID(catalog, []string{"vague-pronoun"}, "D-001"); err == nil {
		t.Fatal("expected error passing earlierItemId without escalation reason")
	}
}

func TestDisplaySettingsExportReasonsAndAsksWhenConfigured(t *testing.T) {
	hub := hubFile(t, `{
		"asks": {"enabled": true},
		"reasons": [{"id": "custom", "category": "custom", "label": "Custom reason"}]
	}`).Display()
	eff, err := MergeDisplay(hubFile(t, `{}`), hub)
	if err != nil {
		t.Fatalf("MergeDisplay: %v", err)
	}
	if !eff.Config.Asks.Enabled {
		t.Fatal("expected hub asks.enabled=true to merge")
	}
	if len(eff.Config.Reasons) != 1 || eff.Config.Reasons[0].ID != "custom" {
		t.Fatalf("merged reasons = %+v", eff.Config.Reasons)
	}
}
