package jobs

import (
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
)

// anomalyConfig turns every rule on with short thresholds.
func anomalyConfig() Config {
	return Config{
		Kinds: map[string]Kind{"rebase": {Idempotent: true, RetryExits: []int{75}}},
		Anomalies: AnomalyConfig{
			Role:             "coord/lead",
			QueueAge:         KindDurations{After: 10 * time.Minute, Kinds: map[string]time.Duration{"merge": time.Hour}},
			RunnerSilent:     5 * time.Minute,
			SubjectFailures:  3,
			LeaseOverrun:     KindDurations{After: 30 * time.Minute},
			GateUnanswered:   time.Hour,
			UnknownUnsettled: time.Hour,
		},
	}
}

// checkAnomalies runs one check and returns the rules that fired.
func checkAnomalies(t *testing.T, s *Store) []string {
	t.Helper()
	fired, err := s.CheckAnomalies()
	if err != nil {
		t.Fatalf("CheckAnomalies: %v", err)
	}
	var rules []string
	for _, a := range fired {
		if !strings.Contains(a.Text, a.Rule) {
			t.Errorf("anomaly text %q does not name its rule %s", a.Text, a.Rule)
		}
		rules = append(rules, a.Rule)
	}
	return rules
}

func runFail(t *testing.T, s *Store, a Args, exit *int) Job {
	t.Helper()
	mustEnqueue(t, s, a)
	j := mustClaim(t, s, "r1")
	if _, err := s.Fail(Args{ID: j.ID, Runner: "r1", Exit: exit}); err != nil {
		t.Fatal(err)
	}
	return j
}

func runDone(t *testing.T, s *Store, a Args) {
	t.Helper()
	mustEnqueue(t, s, a)
	j := mustClaim(t, s, "r1")
	if _, err := s.Done(Args{ID: j.ID, Runner: "r1"}); err != nil {
		t.Fatal(err)
	}
}

// loseRunner claims an outward job and lets its heartbeat lapse, so it
// is unknown.
func loseRunner(t *testing.T, s *Store, c *clock, subject string) Job {
	t.Helper()
	mustEnqueue(t, s, Args{Kind: "merge", Subject: subject})
	j := mustClaim(t, s, "r1")
	c.t = c.t.Add(DefaultHeartbeatTimeout + time.Minute)
	if _, err := s.Expire(); err != nil {
		t.Fatal(err)
	}
	return j
}

// TestAnomalyRules checks, for each rule, that an anomaly fires once,
// does not repeat while it is active, and fires again after it clears
// and recurs.
func TestAnomalyRules(t *testing.T) {
	type step func(t *testing.T, s *Store, c *clock)
	tests := []struct {
		rule                  string
		trigger, clear, recur step
	}{
		{
			rule: RuleDead,
			trigger: func(t *testing.T, s *Store, _ *clock) {
				runFail(t, s, Args{Kind: "rebase", Subject: "o/r#1", Head: "a"}, intp(75))
			},
			// A newer job for the kind and subject that is not dead.
			clear: func(t *testing.T, s *Store, _ *clock) {
				mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1", Head: "b"})
			},
			recur: func(t *testing.T, s *Store, _ *clock) {
				j := mustClaim(t, s, "r1")
				if _, err := s.Fail(Args{ID: j.ID, Runner: "r1", Exit: intp(75)}); err != nil {
					t.Fatal(err)
				}
			},
		},
		{
			rule: RuleQueueAge,
			trigger: func(t *testing.T, s *Store, c *clock) {
				mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"})
				c.t = c.t.Add(11 * time.Minute)
			},
			clear: func(t *testing.T, s *Store, _ *clock) { mustClaim(t, s, "r1") },
			recur: func(t *testing.T, s *Store, c *clock) {
				mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#2"})
				c.t = c.t.Add(11 * time.Minute)
			},
		},
		{
			rule: RuleRunnerSilent,
			trigger: func(t *testing.T, s *Store, c *clock) {
				mustBeat(t, s, "r1")
				c.t = c.t.Add(6 * time.Minute)
			},
			clear: func(t *testing.T, s *Store, _ *clock) { mustBeat(t, s, "r1") },
			recur: func(_ *testing.T, _ *Store, c *clock) { c.t = c.t.Add(6 * time.Minute) },
		},
		{
			rule: RuleSubjectFailures,
			trigger: func(t *testing.T, s *Store, _ *clock) {
				for _, head := range []string{"a", "b", "c"} {
					runFail(t, s, Args{Kind: "push", Subject: "o/r#1", Head: head}, nil)
				}
			},
			clear: func(t *testing.T, s *Store, _ *clock) {
				runDone(t, s, Args{Kind: "push", Subject: "o/r#1", Head: "d"})
			},
			recur: func(t *testing.T, s *Store, _ *clock) {
				for _, head := range []string{"e", "f", "g"} {
					runFail(t, s, Args{Kind: "push", Subject: "o/r#1", Head: head}, nil)
				}
			},
		},
		{
			rule: RuleLeaseOverrun,
			trigger: func(t *testing.T, s *Store, c *clock) {
				mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1", Head: "a", Needs: Needs{Checkout: "/w"}})
				j := mustClaim(t, s, "r1")
				c.t = c.t.Add(31 * time.Minute)
				if _, err := s.Heartbeat(Args{ID: j.ID, Runner: "r1"}); err != nil {
					t.Fatal(err)
				}
			},
			clear: func(t *testing.T, s *Store, _ *clock) {
				j := s.doc.Jobs[len(s.doc.Jobs)-1]
				if _, err := s.Done(Args{ID: j.ID, Runner: "r1"}); err != nil {
					t.Fatal(err)
				}
			},
			recur: func(t *testing.T, s *Store, c *clock) {
				mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1", Head: "b", Needs: Needs{Checkout: "/w"}})
				j := mustClaim(t, s, "r1")
				c.t = c.t.Add(31 * time.Minute)
				if _, err := s.Heartbeat(Args{ID: j.ID, Runner: "r1"}); err != nil {
					t.Fatal(err)
				}
			},
		},
		{
			rule: RuleGateUnanswered,
			trigger: func(t *testing.T, s *Store, c *clock) {
				s.SetPolicy(Policy{Default: GateAsk})
				mustEnqueue(t, s, Args{Kind: "merge", Subject: "o/r#1"})
				c.t = c.t.Add(61 * time.Minute)
			},
			clear: func(t *testing.T, s *Store, _ *clock) {
				for _, j := range s.Gated() {
					if err := s.SetDecide(j.ID, "d1"); err != nil {
						t.Fatal(err)
					}
					if err := s.Ungate(j.ID, "d1", false, "rejected"); err != nil {
						t.Fatal(err)
					}
				}
			},
			recur: func(t *testing.T, s *Store, c *clock) {
				mustEnqueue(t, s, Args{Kind: "merge", Subject: "o/r#2"})
				c.t = c.t.Add(61 * time.Minute)
			},
		},
		{
			rule: RuleUnknownUnsettled,
			trigger: func(t *testing.T, s *Store, c *clock) {
				loseRunner(t, s, c, "o/r#1")
				c.t = c.t.Add(61 * time.Minute)
			},
			clear: func(t *testing.T, s *Store, _ *clock) {
				for _, j := range s.Unsettled() {
					if _, err := s.Done(Args{ID: j.ID, Note: "checked"}); err != nil {
						t.Fatal(err)
					}
				}
			},
			recur: func(t *testing.T, s *Store, c *clock) {
				loseRunner(t, s, c, "o/r#2")
				c.t = c.t.Add(61 * time.Minute)
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.rule, func(t *testing.T) {
			s, c, _ := leaseStore(t, anomalyConfig())
			if got := checkAnomalies(t, s); len(got) != 0 {
				t.Fatalf("empty queue fired %v", got)
			}
			tt.trigger(t, s, c)
			if got := checkAnomalies(t, s); !slices.Equal(got, []string{tt.rule}) {
				t.Fatalf("after trigger fired %v, want [%s]", got, tt.rule)
			}
			if got := checkAnomalies(t, s); len(got) != 0 {
				t.Fatalf("second check fired %v while still active, want none", got)
			}
			tt.clear(t, s, c)
			if got := checkAnomalies(t, s); len(got) != 0 {
				t.Fatalf("after clear fired %v, want none", got)
			}
			tt.recur(t, s, c)
			if got := checkAnomalies(t, s); !slices.Equal(got, []string{tt.rule}) {
				t.Fatalf("after recur fired %v, want [%s]", got, tt.rule)
			}
		})
	}
}

func mustBeat(t *testing.T, s *Store, runner string) {
	t.Helper()
	if _, err := s.Beat(Args{Runner: runner, Machine: "tiger"}); err != nil {
		t.Fatalf("Beat: %v", err)
	}
}

func TestAnomalyThresholdPerKind(t *testing.T) {
	s, c, _ := newStore(t, anomalyConfig())
	mustEnqueue(t, s, Args{Kind: "merge", Subject: "o/r#1"})
	c.t = c.t.Add(30 * time.Minute)
	if got := checkAnomalies(t, s); len(got) != 0 {
		t.Fatalf("merge waited 30m under its 1h queue age, fired %v", got)
	}
	c.t = c.t.Add(31 * time.Minute)
	if got := checkAnomalies(t, s); !slices.Equal(got, []string{RuleQueueAge}) {
		t.Fatalf("fired %v, want queue-age", got)
	}
}

func TestAnomaliesOffWithoutRole(t *testing.T) {
	cfg := anomalyConfig()
	cfg.Anomalies.Role = ""
	s, c, _ := newStore(t, cfg)
	mustEnqueue(t, s, Args{Kind: "rebase", Subject: "o/r#1"})
	c.t = c.t.Add(time.Hour)
	if got := checkAnomalies(t, s); len(got) != 0 {
		t.Fatalf("no role fired %v", got)
	}
}

func TestAnomalyReportedSurvivesRestart(t *testing.T) {
	s, c, lg := newStore(t, anomalyConfig())
	runFail(t, s, Args{Kind: "rebase", Subject: "o/r#1"}, intp(75))
	if got := checkAnomalies(t, s); len(got) != 1 {
		t.Fatalf("fired %v, want dead", got)
	}
	if !slices.ContainsFunc(lg.events, func(e eventlog.Event) bool { return strings.HasPrefix(e.Detail, "anomaly dead") }) {
		t.Errorf("no anomaly event in %+v", lg.events)
	}
	again, err := Open(s.dir, lg, c.now, anomalyConfig())
	if err != nil {
		t.Fatal(err)
	}
	if got := checkAnomalies(t, again); len(got) != 0 {
		t.Fatalf("reopened store fired %v again", got)
	}
}

func TestParseConfigAnomalies(t *testing.T) {
	cfg, err := ParseConfig([]byte(`{
  "anomalies": {
    "role": "coord/lead",
    "queueAge": {"after": "20m", "kinds": {"merge": "2h"}},
    "runnerSilent": "3m",
    "subjectFailures": 4,
    "leaseOverrun": {"after": "45m"},
    "gateUnanswered": "12h",
    "unknownUnsettled": "30m"
  }
}`))
	if err != nil {
		t.Fatal(err)
	}
	a := cfg.Anomalies
	if a.Role != "coord/lead" || a.QueueAge.For("merge") != 2*time.Hour || a.QueueAge.For("rebase") != 20*time.Minute ||
		a.RunnerSilent != 3*time.Minute || a.SubjectFailures != 4 || a.LeaseOverrun.For("x") != 45*time.Minute ||
		a.GateUnanswered != 12*time.Hour || a.UnknownUnsettled != 30*time.Minute {
		t.Fatalf("anomalies = %+v", a)
	}
	// Unset thresholds take the defaults.
	cfg, err = ParseConfig([]byte(`{"anomalies": {"role": "coord/lead"}}`))
	if err != nil {
		t.Fatal(err)
	}
	if d := cfg.Anomalies; d.QueueAge.For("x") != DefaultQueueAge || d.RunnerSilent != DefaultRunnerSilent ||
		d.SubjectFailures != DefaultSubjectFailures || d.LeaseOverrun.For("x") != DefaultLeaseOverrun ||
		d.GateUnanswered != DefaultGateUnanswered || d.UnknownUnsettled != DefaultUnknownUnsettled {
		t.Fatalf("defaults = %+v", d)
	}
	for _, text := range []string{
		`{"anomalies": {}}`,
		`{"anomalies": {"role": "r", "runnerSilent": "soon"}}`,
		`{"anomalies": {"role": "r", "subjectFailures": 0}}`,
		`{"anomalies": {"role": "r", "queueAge": {"after": "-1m"}}}`,
		`{"anomalies": {"role": "r", "queueAge": {"kinds": {"bad kind": "1m"}}}}`,
		`{"anomalies": {"role": "r", "dead": true}}`,
	} {
		if _, err := ParseConfig([]byte(text)); err == nil {
			t.Errorf("ParseConfig(%s) succeeded, want an error", text)
		}
	}
}
