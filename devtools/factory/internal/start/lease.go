package start

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

// leaseMode is the source's lease mode. Empty is off: a config.Source built
// by hand has no lease rule, as before the rule existed. Load always sets it.
func (s *Starter) leaseMode() (string, error) {
	mode := s.Cfg.Lease.Mode
	if mode == "" {
		return config.LeaseModeOff, nil
	}
	if err := config.ValidLeaseMode(mode); err != nil {
		return "", fmt.Errorf("start: lease.mode: %w", err)
	}
	return mode, nil
}

// leaseRule is the rule text for it, or empty when the mode is off.
func (s *Starter) leaseRule(it source.Item) (string, error) {
	mode, err := s.leaseMode()
	if err != nil {
		return "", err
	}
	if mode == config.LeaseModeOff {
		return "", nil
	}
	claimFor := s.Cfg.Lease.ClaimFor
	if claimFor == "" {
		return "", errors.New("start: lease.claim_for: expected a duration, found empty")
	}
	return leaseRuleText(s.Cfg.Cwd, claimFor, it.ID+" "+it.Title, s.Cfg.Lease.Restore), nil
}

// leaseRuleText is the rule a worker reads before it can act.
// checkout is the lane checkout or the source cwd. why is shell-quoted.
func leaseRuleText(checkout, claimFor, why string, restore bool) string {
	restoreLine := ""
	if restore {
		restoreLine = "  agentbus lease restore\n"
	}
	return "Before changing the shared checkout (" + checkout + "), run:\n" +
		"  agentbus lease claim --for " + claimFor + " --why " + shellQuote(why) + "\n" +
		restoreLine +
		"If it says queued, end your turn and wait for the \"[lease] granted\" message.\n" +
		"Run `agentbus lease release` when the tree is clean and pushed. Reading needs no claim."
}

// shellQuote quotes s for a POSIX shell with single quotes.
func shellQuote(s string) string {
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}

// placePrompt renders tmpl and puts leaseRule where {{lease_rule}} is (or at
// the top), trustRule where {{trust_rule}} is (or after the lease rule), and
// appendText where {{start_prompt_append}} is (or after any prepended rules).
// Each literal part around placeholders is rendered on its own, so an item title
// cannot inject a placeholder. An empty rule removes its placeholder and leaves
// the rest as rendered.
func placePrompt(tmpl, leaseRule, trustRule, appendText string, render func(string) string) string {
	var body strings.Builder
	rest := tmpl
	var hasLease, hasTrust, hasAppend bool
	for rest != "" {
		idx, ph, val := nextPlaceholder(rest, leaseRule, trustRule, appendText)
		if idx < 0 {
			body.WriteString(render(rest))
			break
		}
		body.WriteString(render(rest[:idx]))
		body.WriteString(val)
		switch ph {
		case config.LeaseRulePlaceholder:
			hasLease = true
		case config.TrustRulePlaceholder:
			hasTrust = true
		case config.StartPromptAppendPlaceholder:
			hasAppend = true
		}
		rest = rest[idx+len(ph):]
	}
	var parts []string
	if !hasLease && leaseRule != "" {
		parts = append(parts, leaseRule)
	}
	if !hasTrust && trustRule != "" {
		parts = append(parts, trustRule)
	}
	if !hasAppend && appendText != "" {
		parts = append(parts, appendText)
	}
	parts = append(parts, body.String())
	return strings.Join(parts, "\n\n")
}

func nextPlaceholder(s, leaseRule, trustRule, appendText string) (idx int, ph, val string) {
	candidates := []struct {
		ph  string
		val string
	}{
		{config.LeaseRulePlaceholder, leaseRule},
		{config.TrustRulePlaceholder, trustRule},
		{config.StartPromptAppendPlaceholder, appendText},
	}
	bestIdx := -1
	var bestPh, bestVal string
	for _, c := range candidates {
		i := strings.Index(s, c.ph)
		if i >= 0 && (bestIdx < 0 || i < bestIdx) {
			bestIdx = i
			bestPh = c.ph
			bestVal = c.val
		}
	}
	return bestIdx, bestPh, bestVal
}

// writeLeasePlan prints the rule under a lease line when it is in the prompt.
func (s *Starter) writeLeasePlan(w io.Writer, it source.Item) error {
	rule, err := s.leaseRule(it)
	if err != nil || rule == "" {
		return err
	}
	where := "at the top of the prompt"
	if strings.Contains(s.Cfg.StartPrompt, config.LeaseRulePlaceholder) {
		where = "at " + config.LeaseRulePlaceholder
	}
	text := fmt.Sprintf("%-8s %s, rule %s:\n", "lease", s.Cfg.Lease.Mode, where)
	for _, line := range strings.Split(rule, "\n") {
		text += fmt.Sprintf("%-8s %s\n", "", line)
	}
	if _, err := io.WriteString(w, text); err != nil {
		return fmt.Errorf("write start: %w", err)
	}
	return nil
}

// leaseRecord is the lease read just before launch, for the run log.
type leaseRecord struct {
	State    string `json:"state"`
	Holder   string `json:"holder,omitempty"`
	Deadline string `json:"deadline,omitempty"`
	Queue    int    `json:"queue"`
	Error    string `json:"error,omitempty"`
}

// checkLease reads the checkout lease under prompt+check, prints one
// `lease: ...` line, and returns what the run log records. It never
// refuses the start: a lease that cannot be read is `unknown (<error>)`.
// Nil means the mode is not prompt+check.
func (s *Starter) checkLease(ctx context.Context) (*leaseRecord, error) {
	mode, err := s.leaseMode()
	if err != nil || mode != config.LeaseModePromptCheck {
		return nil, err
	}
	rec := s.readLease(ctx)
	line := "lease: " + rec.State
	switch {
	case rec.Error != "":
		line = "lease: unknown (" + rec.Error + ")"
	case rec.summary != "":
		line = "lease: " + rec.summary
	}
	if _, err := fmt.Fprintln(s.Out, line); err != nil {
		return nil, fmt.Errorf("write start: %w", err)
	}
	return &rec.leaseRecord, nil
}

type leaseRead struct {
	leaseRecord
	summary string
}

func (s *Starter) readLease(ctx context.Context) leaseRead {
	unknown := func(why string) leaseRead {
		return leaseRead{leaseRecord: leaseRecord{State: "unknown", Error: why}}
	}
	if s.Machine != "" {
		return unknown("checkout is on remote machine " + s.Machine + "; agentbus runs locally")
	}
	if s.LeaseStatus == nil {
		return unknown("expected an agentbus reader, found nil")
	}
	reading, err := s.LeaseStatus(ctx, s.Cfg.Cwd)
	if err != nil {
		return unknown(err.Error())
	}
	if !reading.Guarded {
		return unknown("not a guarded checkout in agentbus lease_checkouts")
	}
	st := reading.Status
	rec := leaseRecord{State: st.State, Holder: st.Who(), Queue: len(st.Queue)}
	if st.Holder != nil {
		rec.Deadline = st.Holder.Deadline
	}
	return leaseRead{leaseRecord: rec, summary: st.Summary(nil)}
}
