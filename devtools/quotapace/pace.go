package quotapace

import "time"

// States, from best to worst. Missing means no recent reading.
const (
	Green   = "green"
	Amber   = "amber"
	Red     = "red"
	Missing = "missing"
)

// ExitCode is the status and check exit code for a state.
func ExitCode(state string) int {
	switch state {
	case Green:
		return 0
	case Amber:
		return 1
	case Red:
		return 2
	default:
		return 3
	}
}

// Projection is where the latest reading stands against the plan.
// Paces are in points per hour. The current pace is PaceRecent when
// known, otherwise PaceSoFar. A nil pointer means not computable.
type Projection struct {
	At         time.Time `json:"at"`
	Percent    float64   `json:"percent"`
	OnTrack    float64   `json:"onTrack"`
	AmberLine  float64   `json:"amberLine"`
	Limit      float64   `json:"limit"`
	PaceSoFar  float64   `json:"paceSoFar"`
	PaceRecent *float64  `json:"paceRecent"`
	Projected  float64   `json:"projectedAtReset"`
	HoursTo100 *float64  `json:"hoursTo100"`
	NeededPace *float64  `json:"neededPace"`
	State      string    `json:"state"`
}

// Project computes the projection from the readings in the plan's
// window. recent is the span for PaceRecent. ok is false when the
// window has no reading.
func Project(p Plan, readings []Reading, offset float64, recent time.Duration) (Projection, bool) {
	in := windowReadings(p, readings)
	if len(in) == 0 {
		return Projection{}, false
	}
	latest := in[len(in)-1]
	pct := latest.Percent + offset
	pr := Projection{
		At:        latest.At,
		Percent:   pct,
		OnTrack:   p.OnTrack(latest.At),
		AmberLine: p.OnTrack(latest.At) + p.Amber,
		Limit:     Limit,
		PaceSoFar: pace(p.Baseline, p.BaselineAt, pct, latest.At),
	}
	pr.PaceRecent = recentPace(in, offset, recent)
	current := pr.PaceSoFar
	if pr.PaceRecent != nil {
		current = *pr.PaceRecent
	}
	remaining := max(hours(p.ResetsAt.Sub(latest.At)), 0)
	pr.Projected = pct + current*remaining
	switch {
	case pct >= Limit:
		pr.HoursTo100 = ptr(0)
	case current > 0:
		pr.HoursTo100 = ptr((Limit - pct) / current)
	}
	if remaining > 0 {
		pr.NeededPace = ptr((p.Target - pct) / remaining)
	}
	pr.State = classify(pct, pr.OnTrack, pr.Projected)
	return pr, true
}

// classify is red when the projection passes the limit, green at or
// under the on-track line, and amber otherwise.
func classify(pct, onTrack, projected float64) string {
	const eps = 1e-9
	switch {
	case projected > Limit+eps:
		return Red
	case pct <= onTrack+eps:
		return Green
	default:
		return Amber
	}
}

// recentPace is the pace from the earliest reading within recent of the
// latest one. It is nil when there is no such earlier reading.
func recentPace(in []Reading, offset float64, recent time.Duration) *float64 {
	if recent <= 0 || len(in) < 2 {
		return nil
	}
	latest := in[len(in)-1]
	for _, r := range in[:len(in)-1] {
		if latest.At.Sub(r.At) <= recent && r.At.Before(latest.At) {
			return ptr(pace(r.Percent+offset, r.At, latest.Percent+offset, latest.At))
		}
	}
	return nil
}

func pace(fromPct float64, from time.Time, toPct float64, to time.Time) float64 {
	h := hours(to.Sub(from))
	if h <= 0 {
		return 0
	}
	return (toPct - fromPct) / h
}

func hours(d time.Duration) float64 { return d.Hours() }

func ptr(v float64) *float64 { return &v }
