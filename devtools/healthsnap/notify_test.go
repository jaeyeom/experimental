package healthsnap

import (
	"slices"
	"testing"
)

func TestResolveNotifyRouteAndCommand(t *testing.T) {
	alert := AlertRecord{
		RuleID:   "r1",
		CheckID:  "c1",
		Event:    EventFire,
		Previous: SeverityOK,
		State:    SeverityWarn,
		Value:    float64(90),
		Message:  "[healthsnap] FIRE r1 (warn)",
	}

	t.Run("default agentbus routing splits judgment", func(t *testing.T) {
		cfg := NotifyConfig{
			Recipient:         "ops/inbox",
			JudgmentRecipient: "ops/triage",
		}
		tmpl, rec, ok := resolveNotifyRoute(cfg, false)
		if !ok || rec != "ops/inbox" || !slices.Equal(tmpl, []string{"agentbus", "send", "{recipient}", "{message}"}) {
			t.Fatalf("non-judgment route = (%v, %q, %v)", tmpl, rec, ok)
		}

		alertJ := alert
		alertJ.Judgment = true
		tmplJ, recJ, okJ := resolveNotifyRoute(cfg, true)
		if !okJ || recJ != "ops/triage" || !slices.Equal(tmplJ, []string{"agentbus", "send", "{recipient}", "{message}"}) {
			t.Fatalf("judgment route = (%v, %q, %v)", tmplJ, recJ, okJ)
		}
		argv, env := BuildNotifyCommand(tmplJ, []string{"PATH=/usr/bin"}, recJ, alertJ)
		if !slices.Equal(argv, []string{"agentbus", "send", "ops/triage", "[healthsnap] FIRE r1 (warn)"}) {
			t.Fatalf("argv = %v", argv)
		}
		if !slices.Contains(env, "HEALTHSNAP_JUDGMENT=true") || !slices.Contains(env, "HEALTHSNAP_RECIPIENT=ops/triage") {
			t.Fatalf("missing expected env vars: %v", env)
		}
	})

	t.Run("custom command without recipient", func(t *testing.T) {
		cfg := NotifyConfig{
			Command: []string{"notify-send", "{rule}", "{message}"},
		}
		tmpl, rec, ok := resolveNotifyRoute(cfg, false)
		if !ok || rec != "" {
			t.Fatalf("route = (%v, %q, %v)", tmpl, rec, ok)
		}
		argv, _ := BuildNotifyCommand(tmpl, nil, rec, alert)
		if !slices.Equal(argv, []string{"notify-send", "r1", "[healthsnap] FIRE r1 (warn)"}) {
			t.Fatalf("argv = %v", argv)
		}
	})

	t.Run("only judgmentRecipient configured skips non-judgment", func(t *testing.T) {
		cfg := NotifyConfig{
			JudgmentRecipient: "ops/triage",
		}
		if _, _, ok := resolveNotifyRoute(cfg, false); ok {
			t.Fatal("expected non-judgment route to be skipped when recipient is unset")
		}
		if _, rec, ok := resolveNotifyRoute(cfg, true); !ok || rec != "ops/triage" {
			t.Fatalf("expected judgment route to ops/triage, got %q %v", rec, ok)
		}
	})

	t.Run("custom command referencing recipient skips when recipient empty", func(t *testing.T) {
		cfg := NotifyConfig{
			Command:           []string{"send", "{recipient}", "{message}"},
			JudgmentRecipient: "ops/triage",
		}
		if _, _, ok := resolveNotifyRoute(cfg, false); ok {
			t.Fatal("expected command requiring {recipient} to skip when recipient is empty")
		}
	})
}
