package baseline

import (
	"errors"
	"fmt"
	"regexp"
	"sort"
)

// TransitionDeleted is the Transition.After value for an allowed deletion.
const TransitionDeleted = "deleted"

// blobHashRe matches a lowercase SHA-1 (40) or SHA-256 (64) git object hash.
var blobHashRe = regexp.MustCompile(`^(?:[0-9a-f]{40}|[0-9a-f]{64})$`)

// Transition allows one exact content change of a target: from the Before
// blob to the After blob (or TransitionDeleted). Checkers that guard files
// against change use it to pin a reviewed exception to exactly what was
// reviewed; any other change to the same target still fails.
//
// Entries with transitions are never stale: once the change lands it no
// longer matches anything and stays as the record of what was allowed.
type Transition struct {
	Before string `json:"before"`
	After  string `json:"after"`
	Reason string `json:"reason"`
	Issue  string `json:"issue,omitempty"`
}

type canonicalTransition struct {
	After  string `json:"after"`
	Before string `json:"before"`
	Issue  string `json:"issue,omitempty"`
	Reason string `json:"reason"`
}

func validateTransition(t Transition) error {
	if !blobHashRe.MatchString(t.Before) {
		return fmt.Errorf("transition before %q is not a lowercase 40- or 64-hex blob hash", t.Before)
	}
	if t.After != TransitionDeleted && !blobHashRe.MatchString(t.After) {
		return fmt.Errorf("transition after %q is neither a lowercase 40- or 64-hex blob hash nor %q", t.After, TransitionDeleted)
	}
	if t.Reason == "" {
		return errors.New("transition reason is empty")
	}
	return nil
}

func validateEntryTransitions(key string, e Entry) error {
	if len(e.Transitions) == 0 {
		return nil
	}
	if IsGlob(key) {
		return fmt.Errorf("entry %q: transitions are not allowed on glob keys", key)
	}
	if len(e.Limits) > 0 {
		return fmt.Errorf("entry %q: has both limits and transitions", key)
	}
	seen := map[[2]string]bool{}
	for _, t := range e.Transitions {
		if err := validateTransition(t); err != nil {
			return fmt.Errorf("entry %q: %w", key, err)
		}
		k := [2]string{t.Before, t.After}
		if seen[k] {
			return fmt.Errorf("entry %q: duplicate transition %s -> %s", key, t.Before, t.After)
		}
		seen[k] = true
	}
	return nil
}

func canonicalTransitions(ts []Transition) []canonicalTransition {
	if len(ts) == 0 {
		return nil
	}
	out := make([]canonicalTransition, len(ts))
	for i, t := range ts {
		out[i] = canonicalTransition{After: t.After, Before: t.Before, Issue: t.Issue, Reason: t.Reason}
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].Before != out[j].Before {
			return out[i].Before < out[j].Before
		}
		return out[i].After < out[j].After
	})
	return out
}

// AllowsTransition reports whether the exact-path entry for path allows the
// change from blob before to after (a blob hash or TransitionDeleted). Glob
// entries never allow transitions.
func (f *File) AllowsTransition(path, before, after string) (Transition, bool) {
	if f == nil {
		return Transition{}, false
	}
	if norm, err := NormalizeTarget(path); err == nil {
		path = norm
	}
	for _, t := range f.Entries[path].Transitions {
		if t.Before == before && t.After == after {
			return t, true
		}
	}
	return Transition{}, false
}

// AddTransition records t on the exact-path entry for path, creating the
// entry if needed. Adding a (before, after) pair that already exists is a
// no-op that keeps the original reason.
func (f *File) AddTransition(path string, t Transition) error {
	norm, err := NormalizeTarget(path)
	if err != nil {
		return err
	}
	if IsGlob(norm) {
		return fmt.Errorf("cannot add transition to glob key %q", norm)
	}
	if err := validateTransition(t); err != nil {
		return err
	}
	if f.Entries == nil {
		f.Entries = map[string]Entry{}
	}
	entry := f.Entries[norm]
	if len(entry.Limits) > 0 {
		return fmt.Errorf("entry %q has limits; cannot add transitions", norm)
	}
	if _, ok := f.AllowsTransition(norm, t.Before, t.After); ok {
		return nil
	}
	entry.Transitions = append(append([]Transition(nil), entry.Transitions...), t)
	f.Entries[norm] = entry
	return nil
}
