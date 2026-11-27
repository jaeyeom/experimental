package decide

import (
	"fmt"
	"strings"
)

// ReasonOther is the reserved reason ID that always requires a free-text note.
const ReasonOther = "other"

// Reason describes one selectable reason for flagging a message/item or
// explaining why offered choices did not fit.
type Reason struct {
	ID          string `json:"id"`
	Category    string `json:"category"`
	Label       string `json:"label"`
	RequireNote bool   `json:"requireNote,omitempty"`
}

// AsksConfig configures the optional asks view and open ask tracking.
type AsksConfig struct {
	Enabled bool `json:"enabled,omitempty"`
}

// DefaultReasons returns the starting catalog of flag and misfit cause reasons.
func DefaultReasons() []Reason {
	return []Reason{
		{ID: "long-single-line", Category: "format", Label: "long single line with no structure"},
		{ID: "verdict-not-first", Category: "format", Label: "verdict not in the first line"},
		{ID: "no-blank-after-first", Category: "format", Label: "no blank line after the first line"},
		{ID: "too-long", Category: "format", Label: "too long, or too many paragraphs"},

		{ID: "no-exact-action", Category: "main-text", Label: "no shell command or exact action for the user"},
		{ID: "vague-pronoun", Category: "main-text", Label: `vague pronoun ("it", "that")`},
		{ID: "unexplained-jargon", Category: "main-text", Label: "jargon or ids without a plain explanation"},

		{ID: "actor-unclear", Category: "choices", Label: `actor unclear: does "I" or "you" mean the agent or the human?`},
		{ID: "timing-unclear", Category: "choices", Label: "timing unclear: for a human action, is the choice picked before doing it, or after it is done?"},
		{ID: "no-best-choice", Category: "choices", Label: "no best choice"},
		{ID: "consequence-unclear", Category: "choices", Label: "consequence of a choice unclear"},
		{ID: "overlapping-choices", Category: "choices", Label: "two choices overlap, or one choice has two actions"},

		{ID: ReasonOther, Category: "other", Label: "other (note required)", RequireNote: true},
	}
}

// EffectiveReasons returns c.Reasons normalized when configured, else DefaultReasons().
func (c Config) EffectiveReasons() []Reason {
	if len(c.Reasons) == 0 {
		return DefaultReasons()
	}
	return normalizeReasons(c.Reasons)
}

func normalizeReasons(src []Reason) []Reason {
	out := make([]Reason, len(src))
	for i, r := range src {
		r.ID = strings.TrimSpace(r.ID)
		r.Category = strings.TrimSpace(r.Category)
		r.Label = strings.TrimSpace(r.Label)
		if r.Category == "" {
			if strings.EqualFold(r.ID, ReasonOther) {
				r.Category = "other"
			} else {
				r.Category = "general"
			}
		}
		if strings.EqualFold(r.ID, ReasonOther) {
			r.RequireNote = true
		}
		out[i] = r
	}
	return out
}

func checkReasonsConfig(reasons []Reason) error {
	if len(reasons) == 0 {
		return nil
	}
	seen := make(map[string]bool, len(reasons))
	for i, r := range reasons {
		id := strings.TrimSpace(r.ID)
		if id == "" {
			return fmt.Errorf("decide config reasons[%d]: id is empty", i)
		}
		if strings.ContainsAny(id, " \t\n\r") {
			return fmt.Errorf("decide config reasons[%d] (%q): id must not contain whitespace", i, id)
		}
		lower := strings.ToLower(id)
		if seen[lower] {
			return fmt.Errorf("decide config reasons[%d]: duplicate id %q", i, id)
		}
		seen[lower] = true
		if strings.TrimSpace(r.Label) == "" {
			return fmt.Errorf("decide config reasons[%d] (%q): label is empty", i, id)
		}
	}
	return nil
}

// ValidateReasonSelection validates selected reason IDs against catalog, deduplicates
// them while preserving selection order, and enforces a non-empty note when any
// selected reason requires one (such as "other").
func ValidateReasonSelection(catalog []Reason, reasonIDs []string, note string) ([]string, string, error) {
	if len(catalog) == 0 {
		catalog = DefaultReasons()
	}
	byID := make(map[string]Reason, len(catalog))
	for _, r := range catalog {
		byID[strings.ToLower(strings.TrimSpace(r.ID))] = r
	}
	trimmedNote := strings.TrimSpace(note)
	seen := make(map[string]bool, len(reasonIDs))
	selected := make([]string, 0, len(reasonIDs))
	for _, raw := range reasonIDs {
		key := strings.ToLower(strings.TrimSpace(raw))
		if key == "" {
			continue
		}
		r, ok := byID[key]
		if !ok {
			return nil, "", fmt.Errorf("decide reason %q is not in configured reasons", raw)
		}
		canon := strings.TrimSpace(r.ID)
		if seen[canon] {
			continue
		}
		seen[canon] = true
		selected = append(selected, canon)
		if (r.RequireNote || strings.EqualFold(canon, ReasonOther)) && trimmedNote == "" {
			return nil, "", fmt.Errorf("decide reason %q requires a note", canon)
		}
	}
	if len(selected) == 0 {
		return nil, "", fmt.Errorf("decide requires at least one reason")
	}
	return selected, trimmedNote, nil
}
