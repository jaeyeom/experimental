package start

import (
	"context"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/ready"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func (s *Starter) prepared(it source.Item) (ws, tab, prompt string, err error) {
	ws, tab = ready.TabRef(it, s.Cfg, s.Kind)
	if ws == "" || tab == "" {
		return "", "", "", fmt.Errorf("start %s: expected a workspace and a tab, found workspace %q tab %q", it.ID, ws, tab)
	}
	rule, err := s.leaseRule(it)
	if err != nil {
		return "", "", "", err
	}
	appendText := s.renderAppend(it)
	prompt = placePrompt(s.Cfg.StartPrompt, rule, trustRule(s.Cfg), appendText, func(part string) string {
		return renderPrompt(part, it, s.Cfg.Name, s.Cfg.Base, s.Caller, s.CallerBus)
	})
	if out := s.effectiveOutput(); out.Mode == config.OutputModeMerge {
		prompt += "\n\n" + mergeOutputInstruction(s.Cfg.Base, it.ID, s.effectiveSpawner(), s.Machine != "", out.PushCmd)
	}
	if strings.TrimSpace(prompt) == "" {
		return "", "", "", fmt.Errorf("start %s: expected a rendered prompt, found blank", it.ID)
	}
	return ws, tab, prompt, nil
}

func (s *Starter) renderAppend(it source.Item) string {
	if s.Cfg.StartPromptAppend == "" {
		return ""
	}
	return renderPrompt(s.Cfg.StartPromptAppend, it, s.Cfg.Name, s.Cfg.Base, s.Caller, s.CallerBus)
}

func (s *Starter) writeWarning(it source.Item, snap ready.Snapshot) error {
	_, err := io.WriteString(s.Out, forceWarningText(it, snap, s.Cfg, s.Kind))
	if err != nil {
		return fmt.Errorf("write start: %w", err)
	}
	return nil
}

func (s *Starter) writeTakeoverWarning(id string, it source.Item, me string) error {
	_, err := io.WriteString(s.Out, takeoverWarningText(id, it, me, s.effectiveClaim(), s.Kind))
	if err != nil {
		return fmt.Errorf("write start: %w", err)
	}
	return nil
}

// forceWarningText names each ready check --force skipped.
// With none failing, the warning stays the short form.
func forceWarningText(it source.Item, snap ready.Snapshot, cfg config.Source, kind string) string {
	skipped := skippedReady(ready.Evaluate(it, snap, cfg, kind))
	if len(skipped) == 0 {
		return "warning: --force skips the ready checks and the start gate\n"
	}
	return "warning: --force skips the ready checks (" + strings.Join(skipped, "; ") + ") and the start gate\n"
}

func skippedReady(result ready.Result) []string {
	parts := make([]string, 0, len(result.Checks))
	for _, check := range result.Checks {
		if check.Pass {
			continue
		}
		if check.Detail == "" {
			parts = append(parts, string(check.Rule))
			continue
		}
		parts = append(parts, check.Detail)
	}
	return parts
}

// WriteBlocked prints the dry-run plan for it and marks the plan blocked.
// It does not claim the item or start an agent.
func (s *Starter) WriteBlocked(it source.Item, opts Options) error {
	if s == nil {
		return errors.New("start: expected a starter, found nil")
	}
	if s.Out == nil {
		return errors.New("start: expected a writer, found nil")
	}
	if it.ID == "" {
		return errors.New("start: expected a non-empty id, found empty")
	}
	if err := s.validateKind(); err != nil {
		return err
	}
	if err := s.validateConfig(); err != nil {
		return err
	}
	ws, tab, prompt, err := s.prepared(it)
	if err != nil {
		return err
	}
	choice := config.ChooseAgentArgs(it.Labels, s.Defaults.AgentArgs, s.Rules, opts.AgentArgs, opts.AgentArgsSet)
	return s.writePlan(context.Background(), it, ws, tab, prompt, true, choice, false)
}

func (s *Starter) writePlan(ctx context.Context, it source.Item, ws, tab, prompt string, blocked bool, choice config.AgentArgChoice, ownClaim bool) error {
	claimLine, err := claimText(it.ID, s.Cfg, s.Kind)
	if err != nil {
		return err
	}
	shown, err := promptPreview(prompt)
	if err != nil {
		return err
	}
	argsLine, err := config.FormatAgentArgs(choice)
	if err != nil {
		return err //nolint:wrapcheck // internal factory error already carries context
	}
	targetLabel := "tab"
	targetValue := fmt.Sprintf("workspace %q / tab %q (cwd %s)", ws, tab, s.Cfg.Cwd)
	if s.effectiveSpawner() == config.SpawnerSubagent {
		targetLabel = "spawner"
		targetValue = fmt.Sprintf("subagent (cwd %s)", s.Cfg.Cwd)
	}
	commentLine := s.planCommentLine(it.ID, ownClaim)
	text := fmt.Sprintf("%-8s %s\n%s%-8s %s\n%-8s %s\n%s%-8s %s\n",
		"claim", claimLine,
		commentLine,
		targetLabel, targetValue,
		"agent", s.effectiveAgent(),
		argsLine,
		"prompt", shown,
	)
	worktreeLine, err := s.planWorktreeLine(ctx, it.ID)
	if err != nil {
		return err
	}
	text += worktreeLine
	if out := s.effectiveOutput(); out.Mode == config.OutputModeMerge {
		text += fmt.Sprintf("%-8s %s (exit worker on completion)\n", "output", config.OutputModeMerge)
		if out.PushCmd != "" {
			text += fmt.Sprintf("%-8s %s\n", "push_cmd", renderPushCmd(out.PushCmd, s.Cfg.Base, it.ID))
		}
	}
	if s.Lane != "" {
		text += fmt.Sprintf("%-8s %s\n", "lane", laneText(s.Lane, s.Machine))
	}
	if s.Machine != "" && callerOnly(s.Cfg.StartPrompt+"\n"+s.Cfg.StartPromptAppend) {
		text += fmt.Sprintf("warning: start_prompt reports back with {caller}, a herdr pane on this machine; "+
			"a worker on lane %s (machine %s) cannot reach it. Use {report_back} or {caller_bus}\n", s.Lane, s.Machine)
	}
	if blocked {
		text = fmt.Sprintf("%-8s %s\n", "blocked", "yes") + text
	}
	if _, err := io.WriteString(s.Out, text); err != nil {
		return fmt.Errorf("write start: %w", err)
	}
	if err := s.writeLeasePlan(s.Out, it); err != nil {
		return err
	}
	if err := s.writeTrustPlan(s.Out); err != nil {
		return err
	}
	return s.writeAppendPlan(s.Out, it)
}

func (s *Starter) planWorktreeLine(ctx context.Context, id string) (string, error) {
	if !s.effectiveWorktree() || s.Machine != "" {
		return "", nil
	}
	wtDir, wtBranch, err := s.allocateWorktree(ctx, id)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%-8s %s (branch %s)\n", "worktree", wtDir, wtBranch), nil
}

func (s *Starter) planCommentLine(id string, ownClaim bool) string {
	claim := s.effectiveClaim()
	if !claim.Comment || ownClaim {
		return ""
	}
	preview := renderClaimComment(claim, s.effectiveAgent(), previewTimestamp, id, s.sourceKey())
	return fmt.Sprintf("%-8s %s\n", "comment", preview)
}

func (s *Starter) writeAppendPlan(w io.Writer, it source.Item) error {
	appendText := s.renderAppend(it)
	if appendText == "" {
		return nil
	}
	where := "at the top of the prompt"
	if strings.Contains(s.Cfg.StartPrompt, config.StartPromptAppendPlaceholder) {
		where = "at " + config.StartPromptAppendPlaceholder
	} else {
		rule, err := s.leaseRule(it)
		if err != nil {
			return err
		}
		switch {
		case trustRule(s.Cfg) != "" && !strings.Contains(s.Cfg.StartPrompt, config.TrustRulePlaceholder):
			where = "after the trust rule"
		case rule != "" && !strings.Contains(s.Cfg.StartPrompt, config.LeaseRulePlaceholder):
			where = "after the lease rule"
		}
	}
	text := fmt.Sprintf("%-8s %s:\n", "append", where)
	for _, line := range strings.Split(appendText, "\n") {
		text += fmt.Sprintf("%-8s %s\n", "", line)
	}
	if _, err := io.WriteString(w, text); err != nil {
		return fmt.Errorf("write start: %w", err)
	}
	return nil
}

// writeNotes prints each gate note, such as a lease that was not read.
func (s *Starter) writeNotes(notes []string) error {
	for _, note := range notes {
		if _, err := fmt.Fprintf(s.Out, "%-8s %s\n", "note", note); err != nil {
			return fmt.Errorf("write start: %w", err)
		}
	}
	return nil
}

func laneText(lane, machine string) string {
	if machine == "" {
		return lane + " (local herdr)"
	}
	return lane + " (herdr --machine " + machine + ")"
}

func claimText(id string, cfg config.Source, kind string) (string, error) {
	claim := effectiveClaimFor(cfg, kind)
	var marks []string
	switch kind {
	case "jira", "github":
		if claim.MarkInProgress {
			if claim.Status != "" {
				marks = append(marks, claim.Status)
			}
			if claim.Label != "" {
				if claim.Status != "" {
					marks = append(marks, "label "+claim.Label)
				} else {
					marks = append(marks, claim.Label)
				}
			}
		}
	default:
		return "", fmt.Errorf("start: expected kind jira or github, found %q", kind)
	}
	text := id
	if len(marks) > 0 {
		text += " → " + strings.Join(marks, ", ")
	}
	switch {
	case claim.Assign:
		text += " (assign to me)"
	case len(marks) == 0:
		text += " (no tracker changes)"
	}
	return text, nil
}

// callerOnly reports whether tmpl reports back with {caller} alone, which
// a worker on another machine cannot reach.
func callerOnly(tmpl string) bool {
	return strings.Contains(tmpl, "{caller}") &&
		!strings.Contains(tmpl, "{caller_bus}") && !strings.Contains(tmpl, "{report_back}")
}

// ReportBack is the {report_back} instruction. With a caller bus name or
// handle it is an agentbus send, which reaches another machine and wakes a
// hibernated caller. Otherwise it is a herdr prompt to the caller's pane.
// Both empty is empty.
func ReportBack(caller, callerBus string) string {
	switch {
	case callerBus != "":
		return "agentbus send " + callerBus + ` "<message>"`
	case caller != "":
		return "herdr agent prompt " + caller + ` "<message>"`
	default:
		return ""
	}
}

func renderPrompt(tmpl string, it source.Item, sourceName, base, caller, callerBus string) string {
	report := ReportBack(caller, callerBus)
	for _, ph := range []struct{ name, value string }{
		{"{caller}", caller}, {"{caller_bus}", callerBus}, {"{report_back}", report},
	} {
		if ph.value == "" && strings.Contains(tmpl, ph.name) {
			slog.Warn("start prompt placeholder expanded to empty", "placeholder", ph.name)
		}
	}
	return strings.NewReplacer(
		"{id}", it.ID,
		"{title}", it.Title,
		"{url}", it.URL,
		"{source}", sourceName,
		"{base}", base,
		"{parent}", it.Parent,
		"{caller_bus}", callerBus,
		"{report_back}", report,
		"{caller}", caller,
	).Replace(tmpl)
}

func renderPushCmd(tmpl, base, id string) string {
	return strings.NewReplacer("{base}", base, "{id}", id).Replace(tmpl)
}

func mergeOutputInstruction(base, id, spawner string, remote bool, pushCmd string) string {
	ref := id
	if _, num, ok := strings.Cut(id, "#"); ok && num != "" {
		ref = fmt.Sprintf("%s (e.g., Fixes #%s)", id, num)
	}
	pushClause := ""
	if pushCmd != "" {
		pushClause = fmt.Sprintf(" using `%s` (if it blocks or exits non-zero, resolve the blocker or report back rather than bypassing with raw git push)", renderPushCmd(pushCmd, base, id))
	}
	var head string
	if base != "" {
		head = fmt.Sprintf(
			"Output mode is merge: do not open a pull request. Rebase onto origin/%s (after fetching origin/%s) with no merge commits (linear history), fast-forward %s, and push to origin/%s with commit(s) referencing %s%s.",
			base, base, base, base, ref, pushClause,
		)
	} else {
		head = fmt.Sprintf(
			"Output mode is merge: do not open a pull request. Rebase onto the base branch (after fetching origin) with no merge commits (linear history), fast-forward the base branch, and push with commit(s) referencing %s%s.",
			ref, pushClause,
		)
	}
	return head + " " + mergeExitInstruction(id, spawner, remote)
}

func mergeExitInstruction(id, spawner string, remote bool) string {
	switch {
	case spawner == config.SpawnerSubagent:
		return fmt.Sprintf(
			"After running `factory verify %s`, releasing any checkout lease (`agentbus lease release`), and reporting back, finish and return immediately; do not wait for review comments (the coordinator will run `factory settle %s` and discard this subagent).",
			id, id,
		)
	case remote:
		return "After releasing any checkout lease (`agentbus lease release`) and reporting back, close this tab immediately (`herdr tab close \"$HERDR_TAB_ID\"`) and exit rather than waiting for review comments."
	default:
		return fmt.Sprintf(
			"After running `factory verify %s`, releasing any checkout lease (`agentbus lease release`), and reporting back, run `factory settle %s` as your final command so the claim settles and closes this tab immediately rather than waiting for review comments.",
			id, id,
		)
	}
}

func promptPreview(prompt string) (string, error) {
	if prompt == "" {
		return "", fmt.Errorf("start prompt: expected a non-empty prompt, found empty")
	}
	line, _, _ := strings.Cut(prompt, "\n")
	line = strings.TrimRight(line, "\r")
	return line + "…", nil
}
