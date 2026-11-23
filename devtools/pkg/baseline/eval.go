package baseline

import "sort"

// Status represents the outcome of evaluating a metric against a baseline.
type Status string

const (
	// StatusPass indicates the target satisfies its effective limit or is exempt.
	StatusPass Status = "pass"
	// StatusFail indicates the target violates its effective limit.
	StatusFail Status = "fail"
)

// Result describes the evaluation outcome for a single target and metric.
type Result struct {
	Status     Status
	Target     string
	Metric     string
	Actual     int64
	Default    int64
	Effective  int64
	MatchedKey string
	Exempt     bool
	Stale      bool
	CanRatchet bool
	RatchetTo  int64
}

// Passed reports whether the evaluation status is StatusPass.
func (r Result) Passed() bool {
	return r.Status == StatusPass
}

// StaleKind categorizes a baseline staleness finding.
type StaleKind string

const (
	// StaleDeletedTarget indicates an exact-path entry whose target no longer exists.
	StaleDeletedTarget StaleKind = "deleted_target"
	// StaleSatisfiesDefault indicates an exact-path metric override that now passes the default limit.
	StaleSatisfiesDefault StaleKind = "satisfies_default"
	// StaleRatchet indicates an exact-path metric override that can be tightened toward the default.
	StaleRatchet StaleKind = "ratchet"
	// StaleUnmatchedGlob indicates a glob entry that matched zero existing targets.
	StaleUnmatchedGlob StaleKind = "unmatched_glob"
)

// StaleFinding describes a stale or ratchetable baseline entry.
type StaleFinding struct {
	Kind      StaleKind
	Key       string
	Metric    string
	Current   int64
	Suggested int64
}

type metricDirection int

const (
	dirMax metricDirection = iota
	dirMin
)

type observation struct {
	dir          metricDirection
	actual       int64
	defaultLimit int64
	effective    int64
	failed       bool
	exact        bool
	hasOverride  bool
	stale        bool
	canRatchet   bool
	ratchetTo    int64
}

// Evaluator evaluates targets against a baseline File and tracks matched
// entries and observed metrics for staleness detection and ratcheting.
type Evaluator struct {
	file         *File
	matchedKeys  map[string]bool
	observations map[string]map[string]observation
}

// NewEvaluator constructs an Evaluator for file. If file is nil, an empty
// baseline is used.
func NewEvaluator(file *File) *Evaluator {
	if file == nil {
		file = &File{
			Version: CurrentVersion,
			Entries: map[string]Entry{},
		}
	}
	return &Evaluator{
		file:         file,
		matchedKeys:  map[string]bool{},
		observations: map[string]map[string]observation{},
	}
}

// CheckExempt reports whether target resolves to a pure exemption entry
// (an entry with no limits).
func (e *Evaluator) CheckExempt(target string) bool {
	norm := normalizeOrKeep(target)
	key, entry, ok := e.file.Resolve(norm)
	if !ok {
		return false
	}
	e.matchedKeys[key] = true
	return len(entry.Limits) == 0
}

// CheckMax evaluates an upper-bound metric (actual <= effectiveMax).
func (e *Evaluator) CheckMax(target, metric string, actual, defaultMax int64) Result {
	return e.checkMetric(target, metric, actual, defaultMax, dirMax)
}

// CheckMin evaluates a lower-bound metric (actual >= effectiveMin).
func (e *Evaluator) CheckMin(target, metric string, actual, defaultMin int64) Result {
	return e.checkMetric(target, metric, actual, defaultMin, dirMin)
}

func (e *Evaluator) checkMetric(target, metric string, actual, defaultLimit int64, dir metricDirection) Result {
	norm := normalizeOrKeep(target)
	key, entry, ok := e.file.Resolve(norm)
	if ok {
		e.matchedKeys[key] = true
		if len(entry.Limits) == 0 {
			return Result{
				Status:     StatusPass,
				Target:     norm,
				Metric:     metric,
				Actual:     actual,
				Default:    defaultLimit,
				Effective:  defaultLimit,
				MatchedKey: key,
				Exempt:     true,
			}
		}
	}

	effective := defaultLimit
	hasOverride := false
	if ok {
		if v, exists := entry.Limits[metric]; exists {
			effective = v
			hasOverride = true
		}
	}

	exact := ok && !IsGlob(key)
	obs := evaluateObservation(dir, actual, defaultLimit, effective, exact, hasOverride)
	e.recordObservation(norm, metric, obs)

	status := StatusPass
	if obs.failed {
		status = StatusFail
	}
	return Result{
		Status:     status,
		Target:     norm,
		Metric:     metric,
		Actual:     actual,
		Default:    defaultLimit,
		Effective:  effective,
		MatchedKey: key,
		Stale:      obs.stale,
		CanRatchet: obs.canRatchet,
		RatchetTo:  obs.ratchetTo,
	}
}

func evaluateObservation(dir metricDirection, actual, defaultLimit, effective int64, exact, hasOverride bool) observation {
	obs := observation{
		dir:          dir,
		actual:       actual,
		defaultLimit: defaultLimit,
		effective:    effective,
		exact:        exact,
		hasOverride:  hasOverride,
	}
	if dir == dirMax {
		obs.failed = actual > effective
		if exact && hasOverride {
			obs.stale = actual <= defaultLimit
			obs.canRatchet = defaultLimit < actual && actual < effective
		}
	} else {
		obs.failed = actual < effective
		if exact && hasOverride {
			obs.stale = actual >= defaultLimit
			obs.canRatchet = effective < actual && actual < defaultLimit
		}
	}
	if obs.canRatchet {
		obs.ratchetTo = actual
	}
	return obs
}

func (e *Evaluator) recordObservation(target, metric string, obs observation) {
	if e.observations[target] == nil {
		e.observations[target] = map[string]observation{}
	}
	e.observations[target][metric] = obs
}

// StaleFindings returns all staleness and ratchet findings for the baseline
// against existingTargets in deterministic sorted order.
func (e *Evaluator) StaleFindings(existingTargets map[string]bool) []StaleFinding {
	if len(e.file.Entries) == 0 {
		return nil
	}

	normExisting := normalizeTargetSet(existingTargets)
	keys := sortedEntryKeys(e.file.Entries)

	var findings []StaleFinding
	for _, key := range keys {
		entry := e.file.Entries[key]
		if IsGlob(key) {
			if !globMatchesAny(key, normExisting) {
				findings = append(findings, StaleFinding{
					Kind: StaleUnmatchedGlob,
					Key:  key,
				})
			}
			continue
		}
		findings = append(findings, e.exactEntryFindings(key, entry, normExisting)...)
	}
	return findings
}

func (e *Evaluator) exactEntryFindings(key string, entry Entry, normExisting map[string]bool) []StaleFinding {
	if !normExisting[key] {
		return []StaleFinding{{
			Kind: StaleDeletedTarget,
			Key:  key,
		}}
	}
	if len(entry.Limits) == 0 {
		return nil
	}

	metrics := sortedLimitKeys(entry.Limits)
	var findings []StaleFinding
	for _, metric := range metrics {
		limit := entry.Limits[metric]
		obs, ok := e.observations[key][metric]
		if !ok {
			continue
		}
		if obs.stale {
			findings = append(findings, StaleFinding{
				Kind:      StaleSatisfiesDefault,
				Key:       key,
				Metric:    metric,
				Current:   limit,
				Suggested: obs.defaultLimit,
			})
		} else if obs.canRatchet {
			findings = append(findings, StaleFinding{
				Kind:      StaleRatchet,
				Key:       key,
				Metric:    metric,
				Current:   limit,
				Suggested: obs.ratchetTo,
			})
		}
	}
	return findings
}

func normalizeOrKeep(target string) string {
	if norm, err := NormalizeTarget(target); err == nil {
		return norm
	}
	return target
}

func normalizeTargetSet(targets map[string]bool) map[string]bool {
	out := make(map[string]bool, len(targets))
	for t, exists := range targets {
		if exists {
			out[normalizeOrKeep(t)] = true
		}
	}
	return out
}

func globMatchesAny(pattern string, targets map[string]bool) bool {
	for t := range targets {
		if matched, err := MatchGlob(pattern, t); err == nil && matched {
			return true
		}
	}
	return false
}

func sortedEntryKeys(entries map[string]Entry) []string {
	keys := make([]string, 0, len(entries))
	for k := range entries {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}

func sortedLimitKeys(limits map[string]int64) []string {
	keys := make([]string, 0, len(limits))
	for k := range limits {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}
