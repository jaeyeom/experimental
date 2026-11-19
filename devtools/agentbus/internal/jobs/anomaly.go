package jobs

import (
	"cmp"
	"errors"
	"fmt"
	"maps"
	"slices"
	"sort"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

// Anomaly rules. Each names the event the daemon sends to the
// anomaly role.
const (
	// RuleDead: the latest job of a kind and subject is dead.
	RuleDead = "dead"
	// RuleQueueAge: a ready job of a kind waited longer than its queue age.
	RuleQueueAge = "queue-age"
	// RuleRunnerSilent: a runner sent no heartbeat for runnerSilent.
	RuleRunnerSilent = "runner-silent"
	// RuleSubjectFailures: the last subjectFailures jobs that ended on
	// a subject failed or died.
	RuleSubjectFailures = "subject-failures"
	// RuleLeaseOverrun: a job held its checkout lease past its estimate.
	RuleLeaseOverrun = "lease-overrun"
	// RuleGateUnanswered: a job waited in gated past the deadline.
	RuleGateUnanswered = "gate-unanswered"
	// RuleUnknownUnsettled: a lost outward job stayed unsettled.
	RuleUnknownUnsettled = "unknown-unsettled"
)

// Default anomaly thresholds.
const (
	DefaultQueueAge         = time.Hour
	DefaultRunnerSilent     = 5 * time.Minute
	DefaultSubjectFailures  = 3
	DefaultLeaseOverrun     = time.Hour
	DefaultGateUnanswered   = 24 * time.Hour
	DefaultUnknownUnsettled = time.Hour
)

// KindDurations is a threshold with per-kind overrides.
type KindDurations struct {
	After time.Duration
	Kinds map[string]time.Duration
}

// For is the threshold of kind.
func (k KindDurations) For(kind string) time.Duration {
	if d, ok := k.Kinds[kind]; ok {
		return d
	}
	return k.After
}

// AnomalyConfig is where anomaly events go and the threshold of each
// rule. With no role, no rule runs.
type AnomalyConfig struct {
	// Role is the agentbus name that gets anomaly events.
	Role             string
	QueueAge         KindDurations
	RunnerSilent     time.Duration
	SubjectFailures  int
	LeaseOverrun     KindDurations
	GateUnanswered   time.Duration
	UnknownUnsettled time.Duration
}

func (a AnomalyConfig) withDefaults() AnomalyConfig {
	if a.QueueAge.After <= 0 {
		a.QueueAge.After = DefaultQueueAge
	}
	if a.RunnerSilent <= 0 {
		a.RunnerSilent = DefaultRunnerSilent
	}
	if a.SubjectFailures <= 0 {
		a.SubjectFailures = DefaultSubjectFailures
	}
	if a.LeaseOverrun.After <= 0 {
		a.LeaseOverrun.After = DefaultLeaseOverrun
	}
	if a.GateUnanswered <= 0 {
		a.GateUnanswered = DefaultGateUnanswered
	}
	if a.UnknownUnsettled <= 0 {
		a.UnknownUnsettled = DefaultUnknownUnsettled
	}
	return a
}

type kindDurationsFile struct {
	After string            `json:"after"`
	Kinds map[string]string `json:"kinds"`
}

type anomalyFile struct {
	Role             string             `json:"role"`
	QueueAge         *kindDurationsFile `json:"queueAge"`
	RunnerSilent     string             `json:"runnerSilent"`
	SubjectFailures  *int               `json:"subjectFailures"`
	LeaseOverrun     *kindDurationsFile `json:"leaseOverrun"`
	GateUnanswered   string             `json:"gateUnanswered"`
	UnknownUnsettled string             `json:"unknownUnsettled"`
}

func anomalyFromFile(f anomalyFile) (AnomalyConfig, error) {
	a := AnomalyConfig{Role: strings.TrimSpace(f.Role)}
	if a.Role == "" {
		return AnomalyConfig{}, errors.New("anomalies: role is required")
	}
	var err error
	if a.QueueAge, err = kindDurationsFromFile("queueAge", f.QueueAge); err != nil {
		return AnomalyConfig{}, err
	}
	if a.LeaseOverrun, err = kindDurationsFromFile("leaseOverrun", f.LeaseOverrun); err != nil {
		return AnomalyConfig{}, err
	}
	for _, d := range []struct {
		name string
		text string
		to   *time.Duration
	}{
		{"runnerSilent", f.RunnerSilent, &a.RunnerSilent},
		{"gateUnanswered", f.GateUnanswered, &a.GateUnanswered},
		{"unknownUnsettled", f.UnknownUnsettled, &a.UnknownUnsettled},
	} {
		if *d.to, err = anomalyDuration(d.name, d.text); err != nil {
			return AnomalyConfig{}, err
		}
	}
	if f.SubjectFailures != nil {
		if *f.SubjectFailures < 1 {
			return AnomalyConfig{}, fmt.Errorf("anomalies: subjectFailures %d: want at least 1", *f.SubjectFailures)
		}
		a.SubjectFailures = *f.SubjectFailures
	}
	return a.withDefaults(), nil
}

func kindDurationsFromFile(name string, f *kindDurationsFile) (KindDurations, error) {
	var k KindDurations
	if f == nil {
		return k, nil
	}
	var err error
	if k.After, err = anomalyDuration(name+".after", f.After); err != nil {
		return KindDurations{}, err
	}
	for kind, text := range f.Kinds {
		if err := checkKind(kind); err != nil {
			return KindDurations{}, fmt.Errorf("anomalies: %s: %w", name, err)
		}
		d, err := anomalyDuration(name+".kinds."+kind, text)
		if err != nil {
			return KindDurations{}, err
		}
		if d == 0 {
			continue
		}
		if k.Kinds == nil {
			k.Kinds = map[string]time.Duration{}
		}
		k.Kinds[kind] = d
	}
	return k, nil
}

// anomalyDuration parses a threshold. Empty is the default, zero.
func anomalyDuration(name, text string) (time.Duration, error) {
	if text == "" {
		return 0, nil
	}
	d, err := time.ParseDuration(text)
	if err != nil || d <= 0 {
		return 0, fmt.Errorf("anomalies: %s %q: want a positive duration such as \"30m\"", name, text)
	}
	return d, nil
}

// Anomaly is one active anomaly. Key names it while it is active: it is
// reported once, and again only after it clears and recurs.
type Anomaly struct {
	Rule string
	Key  string
	Text string
}

// runnerSeen is a runner's last heartbeat.
type runnerSeen struct {
	machine string
	at      time.Time
}

// Beat records a runner's heartbeat: it is alive, whether or not it
// holds a job. The daemon keeps beats in memory only.
func (s *Store) Beat(a Args) (Reply, error) {
	runner := strings.TrimSpace(a.Runner)
	if runner == "" {
		return Reply{}, errors.New("beat needs --runner")
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.runners == nil {
		s.runners = map[string]runnerSeen{}
	}
	s.runners[runner] = runnerSeen{machine: a.Machine, at: s.now().UTC()}
	return Reply{}, nil
}

// CheckAnomalies finds the active anomalies and returns those not
// reported before. Reported keys are saved with the queue, so a restart
// does not report them again; a key that is no longer active is
// forgotten, so it is reported again when it recurs. Each new and
// cleared anomaly is logged as a job event. Without an anomaly role it
// does nothing.
func (s *Store) CheckAnomalies() ([]Anomaly, error) {
	if s.cfg.Anomalies.Role == "" {
		return nil, nil
	}
	var fired []Anomaly
	err := s.mutate(func(now time.Time) ([]eventlog.Event, error) {
		active := s.anomalies(now)
		reported := map[string]bool{}
		for _, k := range s.doc.Reported {
			reported[k] = true
		}
		var events []eventlog.Event
		keys := make([]string, 0, len(active))
		seen := map[string]bool{}
		for _, a := range active {
			keys = append(keys, a.Key)
			seen[a.Key] = true
			if !reported[a.Key] {
				fired = append(fired, a)
				events = append(events, eventlog.Event{Event: eventlog.EventJob, Actor: "daemon", Detail: "anomaly " + a.Key, Reason: a.Text})
			}
		}
		for _, k := range s.doc.Reported {
			if !seen[k] {
				events = append(events, eventlog.Event{Event: eventlog.EventJob, Actor: "daemon", Detail: "anomaly cleared " + k, Reason: "-"})
			}
		}
		sort.Strings(keys)
		s.doc.Reported = keys
		return events, nil
	})
	if err != nil {
		return nil, err
	}
	return fired, nil
}

// anomalies are the anomalies active at now, in a stable order. The
// caller holds s.mu.
func (s *Store) anomalies(now time.Time) []Anomaly {
	a := &anomalySet{cfg: s.cfg.Anomalies.withDefaults(), now: now}
	for _, j := range s.doc.Jobs {
		a.job(j)
	}
	a.dead(s.doc.Jobs)
	a.queueAge(s.doc.Jobs)
	for _, subject := range s.failingSubjects(a.cfg.SubjectFailures) {
		a.add(RuleSubjectFailures, subject, "the last %d jobs that ended on %s failed or died", a.cfg.SubjectFailures, subject)
	}
	for _, name := range sortedKeys(s.runners) {
		r := s.runners[name]
		if now.Sub(r.at) > a.cfg.RunnerSilent {
			a.add(RuleRunnerSilent, name, "runner %s on %s sent no heartbeat since %s", name, cmp.Or(r.machine, "local"), stamp(r.at))
		}
	}
	return a.out
}

// anomalySet collects the anomalies active at now.
type anomalySet struct {
	cfg AnomalyConfig
	now time.Time
	out []Anomaly
}

func (a *anomalySet) add(rule, key, format string, args ...any) {
	a.out = append(a.out, Anomaly{Rule: rule, Key: rule + " " + key, Text: "agentbus jobs anomaly " + rule + ": " + fmt.Sprintf(format, args...)})
}

// job adds the anomalies of one job: an unanswered gate, a lease held
// past its estimate, and an unsettled lost job.
func (a *anomalySet) job(j *Job) {
	if j.State == StateGated {
		if since := j.entered(StateGated); a.now.Sub(since) > a.cfg.GateUnanswered {
			a.add(RuleGateUnanswered, j.ID, "job %s (%s %s) has waited in gated since %s%s", j.ID, j.Kind, j.Subject, stamp(since), decideNote(j))
		}
	}
	if j.Lease != "" {
		estimate := a.cfg.LeaseOverrun.For(j.Kind)
		if since := j.entered(StateRunning); a.now.Sub(since) > estimate {
			a.add(RuleLeaseOverrun, j.ID, "job %s (%s %s, %s) has held lease %s since %s, past its estimate of %s", j.ID, j.Kind, j.Subject, j.State, j.Lease, stamp(since), estimate)
		}
	}
	if j.unsettled() {
		if since := j.entered(StateUnknown); a.now.Sub(since) > a.cfg.UnknownUnsettled {
			a.add(RuleUnknownUnsettled, j.ID, "job %s (%s %s) has been unknown since %s and is not reconciled; settle it with agentbus jobs done or fail", j.ID, j.Kind, j.Subject, stamp(since))
		}
	}
}

// dead adds each kind and subject whose latest job is dead.
func (a *anomalySet) dead(all []*Job) {
	latest := map[string]*Job{}
	var order []string
	for _, j := range all {
		ks := j.Kind + " " + j.Subject
		if _, ok := latest[ks]; !ok {
			order = append(order, ks)
		}
		latest[ks] = j
	}
	for _, ks := range order {
		if j := latest[ks]; j.State == StateDead {
			a.add(RuleDead, ks, "job %s (%s) is dead: %s", j.ID, ks, j.Note)
		}
	}
}

// queueAge adds each kind with a ready job that waited past its queue
// age, since it was last made ready or its retry backoff ended.
func (a *anomalySet) queueAge(all []*Job) {
	oldest := map[string]*Job{}
	waiting := map[string]int{}
	for _, j := range all {
		if j.State != StateReady {
			continue
		}
		since := j.entered(StateReady)
		if j.NotBefore.After(since) {
			since = j.NotBefore
		}
		if a.now.Sub(since) > a.cfg.QueueAge.For(j.Kind) {
			waiting[j.Kind]++
			if oldest[j.Kind] == nil {
				oldest[j.Kind] = j
			}
		}
	}
	for _, kind := range sortedKeys(oldest) {
		j := oldest[kind]
		a.add(RuleQueueAge, kind, "%d ready %s job(s) waited longer than %s; the oldest is %s (%s)", waiting[kind], kind, a.cfg.QueueAge.For(kind), j.ID, j.Subject)
	}
}

// failingSubjects are the subjects whose last n jobs to end, of any
// kind, all failed or died.
func (s *Store) failingSubjects(n int) []string {
	ended := map[string][]*Job{}
	for _, j := range s.doc.Jobs {
		if j.State.Final() {
			ended[j.Subject] = append(ended[j.Subject], j)
		}
	}
	var out []string
	for _, subject := range sortedKeys(ended) {
		js := ended[subject]
		if len(js) < n {
			continue
		}
		slices.SortStableFunc(js, func(a, b *Job) int { return a.Updated.Compare(b.Updated) })
		if !slices.ContainsFunc(js[len(js)-n:], func(j *Job) bool { return j.State == StateDone }) {
			out = append(out, subject)
		}
	}
	return out
}

// entered is when j last entered state, or when it was last updated.
func (j *Job) entered(state State) time.Time {
	for i := len(j.History) - 1; i >= 0; i-- {
		if j.History[i].To == state {
			return j.History[i].At
		}
	}
	return j.Updated
}

func decideNote(j *Job) string {
	switch {
	case j.Decide != "":
		return "; decide item " + j.Decide + " is unanswered"
	case j.waitsOnBus():
		return "; message " + j.AskMsg + " to " + j.AskTo + " is unanswered"
	case j.ParkedOn != "":
		return "; decide item " + j.ParkedOn + " is unanswered"
	default:
		return ""
	}
}

func stamp(t time.Time) string { return t.UTC().Format(time.RFC3339) }

func sortedKeys[V any](m map[string]V) []string {
	return slices.Sorted(maps.Keys(m))
}
