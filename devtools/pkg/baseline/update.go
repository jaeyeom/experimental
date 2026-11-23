package baseline

// UpdateMode controls how UpdatedFile modifies baseline entries.
type UpdateMode int

const (
	// ModeUpdate records new or worsened violations, removes stale entries,
	// and tightens improved limits while preserving metadata and manual globs.
	ModeUpdate UpdateMode = iota
	// ModeTighten only removes stale entries and tightens looser-than-needed
	// limits toward defaults; it never adds new entries or loosens limits.
	ModeTighten
)

// UpdatedFile returns a new baseline File updated according to mode using the
// observations collected during evaluation and existingTargets.
func (e *Evaluator) UpdatedFile(existingTargets map[string]bool, mode UpdateMode) *File {
	version := e.file.Version
	if version == 0 {
		version = CurrentVersion
	}
	out := &File{
		Version: version,
		Checker: e.file.Checker,
		Entries: map[string]Entry{},
	}

	normExisting := normalizeTargetSet(existingTargets)

	for key, entry := range e.file.Entries {
		if IsGlob(key) {
			out.Entries[key] = cloneEntry(entry)
			continue
		}
		if !normExisting[key] {
			continue
		}
		if len(entry.Limits) == 0 {
			out.Entries[key] = cloneEntry(entry)
			continue
		}
		if updated, keep := e.updateExactEntry(key, entry, mode); keep {
			out.Entries[key] = updated
		}
	}

	if mode == ModeUpdate {
		e.addNewViolations(out)
	}
	return out
}

func (e *Evaluator) updateExactEntry(key string, entry Entry, mode UpdateMode) (Entry, bool) {
	newLimits := make(map[string]int64, len(entry.Limits))
	for metric, limit := range entry.Limits {
		obs, ok := e.observations[key][metric]
		if !ok {
			newLimits[metric] = limit
			continue
		}
		if obs.stale {
			continue
		}
		if obs.canRatchet {
			newLimits[metric] = obs.ratchetTo
			continue
		}
		if obs.failed && mode == ModeUpdate {
			newLimits[metric] = obs.actual
			continue
		}
		newLimits[metric] = limit
	}

	if mode == ModeUpdate {
		for metric, obs := range e.observations[key] {
			if _, exists := entry.Limits[metric]; !exists && obs.failed {
				newLimits[metric] = obs.actual
			}
		}
	}

	if len(newLimits) == 0 {
		return Entry{}, false
	}
	return Entry{
		Limits: newLimits,
		Reason: entry.Reason,
		Issue:  entry.Issue,
	}, true
}

func (e *Evaluator) addNewViolations(out *File) {
	for target, metrics := range e.observations {
		if _, exists := e.file.Entries[target]; exists {
			continue
		}
		limits := map[string]int64{}
		for metric, obs := range metrics {
			if obs.failed {
				limits[metric] = obs.actual
			}
		}
		if len(limits) > 0 {
			out.Entries[target] = Entry{Limits: limits}
		}
	}
}

func cloneEntry(e Entry) Entry {
	var limits map[string]int64
	if len(e.Limits) > 0 {
		limits = make(map[string]int64, len(e.Limits))
		for k, v := range e.Limits {
			limits[k] = v
		}
	}
	return Entry{
		Limits: limits,
		Reason: e.Reason,
		Issue:  e.Issue,
	}
}
