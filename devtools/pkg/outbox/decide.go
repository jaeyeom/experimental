package outbox

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os/exec"
	"slices"
	"strings"
	"time"

	"github.com/chorong-sh/chorong/decide"
)

// commandTimeout bounds a single external subprocess invocation.
const commandTimeout = 2 * time.Minute

// Runner invokes an external tool with optional stdin and returns its stdout.
type Runner func(ctx context.Context, stdin []byte, args ...string) ([]byte, error)

// EnvFunc builds the process environment for a subprocess call.
type EnvFunc func(ctx context.Context, args []string) ([]string, error)

// Exec returns a Runner that runs argv directly (without a shell) followed by args.
func Exec(argv []string) Runner {
	return ExecWithEnv(argv, nil)
}

// ExecWithEnv returns a Runner running argv with an optional custom environment builder.
func ExecWithEnv(argv []string, envFn EnvFunc) Runner {
	return func(ctx context.Context, stdin []byte, args ...string) ([]byte, error) {
		if len(argv) == 0 || argv[0] == "" {
			return nil, errors.New("exec: empty command")
		}
		ctx, cancel := context.WithTimeout(ctx, commandTimeout)
		defer cancel()
		fullArgs := append(slices.Clip(argv[1:]), args...)
		// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
		cmd := exec.CommandContext(ctx, argv[0], fullArgs...) //nolint:gosec // G204: operator config.
		if envFn != nil {
			env, err := envFn(ctx, args)
			if err != nil {
				return nil, fmt.Errorf("%s %s env: %w", argv[0], firstArg(args), err)
			}
			cmd.Env = env
		}
		if stdin != nil {
			cmd.Stdin = bytes.NewReader(stdin)
		}
		var stdout, stderr bytes.Buffer
		cmd.Stdout, cmd.Stderr = &stdout, &stderr
		cmd.WaitDelay = time.Second
		if err := cmd.Run(); err != nil {
			return nil, fmt.Errorf("%s %s: %w: %s", argv[0], firstArg(args), err, strings.TrimSpace(stderr.String()))
		}
		return stdout.Bytes(), nil
	}
}

func firstArg(args []string) string {
	if len(args) == 0 {
		return ""
	}
	return args[0]
}

// RunJSON invokes run and decodes its stdout JSON into dst.
func RunJSON(ctx context.Context, run Runner, stdin []byte, dst any, args ...string) error {
	out, err := run(ctx, stdin, args...)
	if err != nil {
		return err
	}
	if err := json.Unmarshal(out, dst); err != nil {
		return fmt.Errorf("%s: parse output: %w", firstArg(args), err)
	}
	return nil
}

// AskRequest configures a decide ask invocation.
type AskRequest struct {
	Owner            string
	NotifyTarget     string
	Class            string
	Kind             string
	Parent           string
	OwnerRepliesOnly bool
	LongLines        bool
	Options          []decide.Option
	Recommended      string
	FreeText         bool
	Verbatim         string
	Question         string
}

// ReplyOptionsRequest configures a decide reply that re-attaches options.
type ReplyOptionsRequest struct {
	Options     []decide.Option
	Recommended string
	FreeText    bool
	LongLines   bool
	Verbatim    string
	Text        string
}

// Approver files, inspects, and closes decide approval items.
type Approver interface {
	Ask(ctx context.Context, req AskRequest) (string, error)
	Show(ctx context.Context, id string) (decide.Item, error)
	Done(ctx context.Context, id, outcome string) error
}

// DecideCLI implements Approver by invoking the decide CLI via Run.
type DecideCLI struct {
	Run Runner
}

// Ask files an approval item in decide and returns its new ID.
func (d DecideCLI) Ask(ctx context.Context, req AskRequest) (string, error) {
	args := buildAskArgs(req)
	var it decide.Item
	err := runWithLegacyFlagFallback(ctx, d.Run, &it, args)
	if err != nil {
		return "", fmt.Errorf("decide ask: %w", err)
	}
	if it.ID == "" {
		return "", errors.New("decide ask printed no id")
	}
	return it.ID, nil
}

func runWithLegacyFlagFallback(ctx context.Context, run Runner, dst any, args []string) error {
	err := RunJSON(ctx, run, nil, dst, args...)
	fallbacks := []struct {
		flag   string
		needle string
	}{
		{flag: "--long-lines", needle: "flag provided but not defined: -long-lines"},
		{flag: "--owner-replies-only", needle: "flag provided but not defined: -owner-replies-only"},
	}
	for range fallbacks {
		if err == nil {
			return nil
		}
		stripped := false
		for _, fb := range fallbacks {
			if strings.Contains(err.Error(), fb.needle) && slices.Contains(args, fb.flag) {
				args = slices.DeleteFunc(slices.Clone(args), func(a string) bool { return a == fb.flag })
				stripped = true
				break
			}
		}
		if !stripped {
			return err
		}
		err = RunJSON(ctx, run, nil, dst, args...)
	}
	return err
}

func buildAskArgs(req AskRequest) []string {
	args := []string{"ask"}
	if req.Class != "" {
		args = append(args, "--class", req.Class)
	}
	if req.LongLines {
		args = append(args, "--long-lines")
	}
	args = append(args, "--owner", req.Owner)
	if req.OwnerRepliesOnly {
		args = append(args, "--owner-replies-only")
	}
	if req.NotifyTarget != "" {
		args = append(args, "--notify-pane", req.NotifyTarget)
	}
	if req.Parent != "" {
		args = append(args, "--parent", req.Parent)
	}
	if req.Verbatim != "" {
		args = append(args, "--verbatim", req.Verbatim)
	}
	kind := req.Kind
	if kind == "" {
		kind = decide.KindRelay
	}
	args = append(args, "--kind", kind)
	args = appendChoiceArgs(args, req.Options, req.Recommended, req.FreeText)
	return append(args, "--json", req.Question)
}

func appendChoiceArgs(args []string, opts []decide.Option, recommended string, freeText bool) []string {
	rec := recommended
	for _, opt := range opts {
		args = append(args, "--option", opt.Key+":"+opt.Label)
		if rec == "" && opt.Recommended {
			rec = opt.Key
		}
	}
	if rec != "" {
		args = append(args, "--rec", rec)
	}
	if freeText {
		args = append(args, "--free-text")
	}
	return args
}

// Show reads a decide item by ID.
func (d DecideCLI) Show(ctx context.Context, id string) (decide.Item, error) {
	var it decide.Item
	if err := RunJSON(ctx, d.Run, nil, &it, "show", "--json", id); err != nil {
		return decide.Item{}, fmt.Errorf("decide show: %w", err)
	}
	return it, nil
}

// Done closes a decide item with outcome.
func (d DecideCLI) Done(ctx context.Context, id, outcome string) error {
	var it decide.Item
	if err := RunJSON(ctx, d.Run, nil, &it, "done", "--json", id, outcome); err != nil {
		return fmt.Errorf("decide done: %w", err)
	}
	return nil
}

// DoneQuiet closes a decide item quietly without leaving it unread.
// Falls back to Done when an older decide binary rejects --quiet.
func (d DecideCLI) DoneQuiet(ctx context.Context, id, outcome string) error {
	var it decide.Item
	err := RunJSON(ctx, d.Run, nil, &it, "done", "--quiet", "--json", id, outcome)
	if err != nil && strings.Contains(err.Error(), "flag provided but not defined: -quiet") {
		return d.Done(ctx, id, outcome)
	}
	if err != nil {
		return fmt.Errorf("decide done: %w", err)
	}
	return nil
}

// Reply appends text onto an existing decide item thread.
func (d DecideCLI) Reply(ctx context.Context, id, text string) error {
	var it decide.Item
	if err := RunJSON(ctx, d.Run, nil, &it, "reply", "--json", id, text); err != nil {
		return fmt.Errorf("decide reply: %w", err)
	}
	return nil
}

// ReplyWithOptions appends text and re-attaches options on an open decide item.
func (d DecideCLI) ReplyWithOptions(ctx context.Context, id string, req ReplyOptionsRequest) error {
	args := []string{"reply"}
	if req.LongLines {
		args = append(args, "--long-lines")
	}
	args = appendChoiceArgs(args, req.Options, req.Recommended, req.FreeText)
	if req.Verbatim != "" {
		args = append(args, "--verbatim", req.Verbatim)
	}
	args = append(args, "--json", id, req.Text)
	var it decide.Item
	if err := runWithLegacyFlagFallback(ctx, d.Run, &it, args); err != nil {
		return fmt.Errorf("decide reply: %w", err)
	}
	return nil
}

// Action is the confirmation verdict derived from a decide item.
type Action string

// Verdict actions evaluated from a decide item's thread.
const (
	ActionPending               Action = "pending"
	ActionApprove               Action = "approve"
	ActionEdit                  Action = "edit"
	ActionCancel                Action = "cancel"
	ActionRejected              Action = "rejected"
	ActionClosedWithoutApproval Action = "closed"
)

// EvalPolicy parameterizes human-UI approval evaluation rules.
type EvalPolicy struct {
	ApproveKeys          []string
	EditKeys             []string
	CancelKeys           []string
	RequireAnswerHumanUI bool
	RejectNonUIApprove   bool
	RejectUnknownKey     bool
	CancelClosedEdits    bool
	RequireBeforeDoneAt  bool
	MissingHumanReason   string
	UnknownKeyMessageFn  func(key string) string
}

// DefaultEvalPolicy returns the standard confirm-before-doing policy with post/edit/cancel keys.
func DefaultEvalPolicy() EvalPolicy {
	return EvalPolicy{
		ApproveKeys:          []string{"post"},
		EditKeys:             []string{"edit"},
		CancelKeys:           []string{"cancel"},
		RequireAnswerHumanUI: true,
	}
}

// Verdict summarizes what the human asked for on an approval item.
type Verdict struct {
	Action        Action
	OptionKey     string
	LatestMessage decide.Message
	HasHuman      bool
	Notes         string
	LatestNote    string
	Refusal       string
}

// IsClosedState reports whether state cannot accept further human picks for an open draft.
func IsClosedState(state string) bool {
	return state == decide.StateDone || state == decide.StateDoneUnread || state == decide.StateWithdrawn
}

// IsItemClosed reports whether it is already closed (done, done-unread, or withdrawn).
func IsItemClosed(it decide.Item) bool {
	return IsClosedState(it.State)
}

// HasHumanMessage reports whether at least one message in it came from a human.
func HasHumanMessage(it decide.Item) bool {
	for _, m := range it.Messages {
		if m.FromHuman() {
			return true
		}
	}
	return false
}

// HasOptions reports whether opts contains at least one key from every key group.
func HasOptions(opts []decide.Option, groups ...[]string) bool {
	for _, group := range groups {
		found := false
		for _, opt := range opts {
			if slices.Contains(group, opt.Key) {
				found = true
				break
			}
		}
		if !found {
			return false
		}
	}
	return true
}

// EvaluateApproval evaluates the latest human message in item under policy.
// Messages sent by agents or from the command line without ViaUI never approve execution.
func EvaluateApproval(item decide.Item, policy EvalPolicy) Verdict {
	last, found, notes := scanHumanMessages(item.Messages)
	v := Verdict{
		LatestMessage: last,
		HasHuman:      found,
		OptionKey:     last.OptionKey,
		Notes:         strings.Join(notes, "\n\n"),
		LatestNote:    last.Text,
	}
	if !found {
		return evalMissingHuman(item, policy, v)
	}
	if item.State == decide.StateWithdrawn {
		v.Action = ActionClosedWithoutApproval
		v.Refusal = "decide item was withdrawn"
		return v
	}
	if slices.Contains(policy.ApproveKeys, last.OptionKey) {
		return evalApprovePick(item, last, policy, v)
	}
	return evalNonApprovePick(item, last, policy, v)
}

func scanHumanMessages(msgs []decide.Message) (last decide.Message, found bool, notes []string) {
	for _, m := range msgs {
		if !m.FromHuman() {
			continue
		}
		last, found = m, true
		if m.OptionKey == "" && strings.TrimSpace(m.Text) != "" {
			notes = append(notes, m.Text)
		}
	}
	return last, found, notes
}

func evalMissingHuman(item decide.Item, policy EvalPolicy, v Verdict) Verdict {
	if item.State == decide.StateWithdrawn {
		v.Action = ActionClosedWithoutApproval
		v.Refusal = "decide item was withdrawn"
		return v
	}
	if IsItemClosed(item) {
		v.Action = ActionClosedWithoutApproval
		if policy.MissingHumanReason != "" {
			v.Refusal = policy.MissingHumanReason
		} else {
			v.Refusal = "the approval item was " + item.State + " without a post answer"
		}
		return v
	}
	v.Action = ActionPending
	return v
}

func evalApprovePick(item decide.Item, last decide.Message, policy EvalPolicy, v Verdict) Verdict {
	closed := IsItemClosed(item)
	if !last.HumanUI() {
		return evalNonUIApprove(closed, last, policy, v)
	}
	if policy.RequireAnswerHumanUI && (!item.Answer.HumanUI() || !slices.Contains(policy.ApproveKeys, item.Answer.Key)) {
		if closed {
			v.Action = ActionClosedWithoutApproval
			v.Refusal = "the approval item was " + item.State + " without a post answer"
			return v
		}
		v.Action = ActionPending
		return v
	}
	if closed && policy.RequireBeforeDoneAt {
		if reason := closedApproveTimeReason(item, last); reason != "" {
			v.Action = ActionClosedWithoutApproval
			v.Refusal = reason
			return v
		}
	}
	v.Action = ActionApprove
	return v
}

func evalNonUIApprove(closed bool, last decide.Message, policy EvalPolicy, v Verdict) Verdict {
	if policy.RejectNonUIApprove {
		v.Refusal = fmt.Sprintf("the post pick came via %q, not from the decide UI", last.Via)
		if closed {
			v.Action = ActionClosedWithoutApproval
			return v
		}
		v.Action = ActionRejected
		return v
	}
	if closed {
		v.Action = ActionClosedWithoutApproval
		v.Refusal = "the approval item was closed without a post answer"
		return v
	}
	v.Action = ActionPending
	return v
}

func closedApproveTimeReason(item decide.Item, last decide.Message) string {
	switch {
	case item.DoneAt.IsZero():
		return fmt.Sprintf("decide item %s has no close time", item.State)
	case last.Time.IsZero():
		return "the post pick has no timestamp"
	case last.Time.After(item.DoneAt):
		return "the post pick was made after the decide item closed"
	case !last.Time.Before(item.DoneAt):
		return "the post pick was not made before the decide item closed"
	default:
		return ""
	}
}

func evalNonApprovePick(item decide.Item, last decide.Message, policy EvalPolicy, v Verdict) Verdict {
	closed := IsItemClosed(item)
	if slices.Contains(policy.CancelKeys, last.OptionKey) {
		if closed {
			v.Action = ActionClosedWithoutApproval
			v.Refusal = fmt.Sprintf("the user picked drop (%q), not a post pick", last.OptionKey)
			return v
		}
		v.Action = ActionCancel
		return v
	}
	if last.OptionKey == "" || slices.Contains(policy.EditKeys, last.OptionKey) {
		if closed && policy.CancelClosedEdits {
			v.Action = ActionClosedWithoutApproval
			if last.OptionKey == "" {
				v.Refusal = "the user's latest message is free text, not a post pick"
			} else {
				v.Refusal = "the user asked for an edit, not a post pick"
			}
			return v
		}
		v.Action = ActionEdit
		return v
	}
	return evalUnknownKey(item, last, policy, v)
}

func evalUnknownKey(item decide.Item, last decide.Message, policy EvalPolicy, v Verdict) Verdict {
	if policy.RejectUnknownKey {
		msg := fmt.Sprintf("answer %q is not an allowed option", last.OptionKey)
		if policy.UnknownKeyMessageFn != nil {
			msg = policy.UnknownKeyMessageFn(last.OptionKey)
		}
		v.Refusal = msg
		if IsItemClosed(item) {
			v.Action = ActionClosedWithoutApproval
			return v
		}
		v.Action = ActionRejected
		return v
	}
	if IsItemClosed(item) {
		v.Action = ActionClosedWithoutApproval
		v.Refusal = "the approval item was " + item.State + " without a post answer"
		return v
	}
	v.Action = ActionPending
	return v
}
