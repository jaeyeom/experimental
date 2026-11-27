package ghoutbox

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/outbox"
)

func marshalJSON(v any, label string) ([]byte, error) {
	data, err := json.Marshal(v)
	if err != nil {
		return nil, fmt.Errorf("encode %s: %w", label, err)
	}
	return data, nil
}

// Decide option keys on an approval item.
const (
	OptionPost   = "post"
	OptionEdit   = "edit"
	OptionCancel = "cancel"
)

// DecideAnswer, DecideMessage and DecideItem are decide's own types, as
// decide show --json prints them.
type (
	DecideAnswer  = decide.Answer
	DecideMessage = decide.Message
	DecideItem    = decide.Item
)

// Decider files, reads, and closes decide items.
type Decider interface {
	Ask(ctx context.Context, owner, notifyPane, question string) (string, error)
	Show(ctx context.Context, id string) (DecideItem, error)
	Done(ctx context.Context, id, outcome string) error
}

// GitHub reads pull requests and posts as the user's gh account. Each
// write returns the URL of what it posted.
type GitHub interface {
	HeadSHA(ctx context.Context, repo string, pr int) (string, error)
	CommentBody(ctx context.Context, repo, id string) (string, error)
	ReviewBody(ctx context.Context, repo string, pr int, id string) (string, error)
	ReplyTarget(ctx context.Context, repo string, pr int, commentID string) (ReplyTarget, error)
	CreateThread(ctx context.Context, repo string, pr int, sha string, c LineComment) (string, error)
	CreateReply(ctx context.Context, repo string, pr int, commentID, body string) (string, error)
	CreateReview(ctx context.Context, repo string, pr int, sha, event, body string, cs []LineComment) (string, error)
	EditComment(ctx context.Context, repo, id, body string) (string, error)
	EditReview(ctx context.Context, repo string, pr int, id, body string) (string, error)
}

// Bus sends notices to agents and lists their names.
type Bus interface {
	Send(ctx context.Context, to, text string) error
	Names(ctx context.Context) ([]string, error)
}

// Outbox submits drafts and acts on the user's answers.
type Outbox struct {
	Store     *Store
	Decide    Decider
	GH        GitHub
	Bus       Bus
	Transport outbox.Transport
	Config    Config
	// Now defaults to time.Now.
	Now func() time.Time
}

// NewOutbox constructs an Outbox wired with CLI implementations from cfg.
func NewOutbox(cfg Config) *Outbox {
	return &Outbox{
		Store:  NewStore(cfg.DataDir),
		Decide: DecideCLI{Run: Exec(cfg.DecideCommand)},
		GH:     GHCLI{Run: Exec(cfg.GHCommand)},
		Bus:    AgentbusCLI{Run: Exec(cfg.AgentbusCommand)},
		Config: cfg,
	}
}

// OpenDefault loads configuration from default XDG paths and returns a ready Outbox.
func OpenDefault(env []string) (*Outbox, error) {
	configPath, err := DefaultConfigPath(env)
	if err != nil {
		return nil, fmt.Errorf("config path: %w", err)
	}
	cfg, err := LoadConfig(configPath, env)
	if err != nil {
		return nil, fmt.Errorf("load config: %w", err)
	}
	return NewOutbox(cfg), nil
}

func (o *Outbox) now() time.Time {
	if o.Now != nil {
		return o.Now().UTC()
	}
	return time.Now().UTC()
}

func (o *Outbox) transport() outbox.Transport {
	if o.Transport != nil {
		return o.Transport
	}
	if o.Bus != nil {
		return outbox.TransportFunc(func(ctx context.Context, n outbox.Notice) error {
			return o.Bus.Send(ctx, n.Owner, n.Message)
		})
	}
	return outbox.PullTransport{}
}

// Lint checks d against the configured deny rules.
func (o *Outbox) Lint(ctx context.Context, d Draft) ([]Hit, error) {
	var names []string
	if o.Config.Lint.AgentbusNames && o.Bus != nil {
		// agentbus may not be installed; the other rules still apply.
		names, _ = o.Bus.Names(ctx)
	}
	rules, err := BuildRules(o.Config.Lint, names)
	if err != nil {
		return nil, err
	}
	return Lint(d, rules), nil
}

// Submit lints d, which must be valid, stores it, and files its decide
// item. A lint failure is a *LintError and files nothing.
func (o *Outbox) Submit(ctx context.Context, d Draft) (Record, error) {
	hits, err := o.Lint(ctx, d)
	if err != nil {
		return Record{}, err
	}
	if len(hits) > 0 {
		return Record{}, &LintError{Hits: hits}
	}
	sha, err := o.GH.HeadSHA(ctx, d.Repo, d.PR)
	if err != nil {
		return Record{}, fmt.Errorf("read head of %s: %w", d.Target(), err)
	}
	r := Record{Draft: d, State: StatePending, HeadSHA: sha, CreatedAt: o.now(), UpdatedAt: o.now()}
	if d.Edit != "" {
		if r.OldText, err = o.oldText(ctx, d); err != nil {
			return Record{}, err
		}
	} else if d.Kind == KindReply {
		target, err := o.GH.ReplyTarget(ctx, d.Repo, d.PR, d.InReplyTo)
		if err != nil {
			return Record{}, fmt.Errorf("read thread for --in-reply-to %s on %s: %w", d.InReplyTo, d.Target(), err)
		}
		if target.Resolved && !d.AllowResolved {
			return Record{}, fmt.Errorf("discussion thread for comment %s on %s is resolved; pass --allow-resolved to reply anyway", d.InReplyTo, d.Target())
		}
		r.Reply = &target
	}
	r.Digest = outbox.Digest(Question(r))
	if r, err = o.Store.Create(r); err != nil {
		return Record{}, err
	}
	if err := o.ask(ctx, &r); err != nil {
		return r, fmt.Errorf("draft %d is stored; poll will file its approval: %w", r.ID, err)
	}
	return r, nil
}

func (o *Outbox) oldText(ctx context.Context, d Draft) (string, error) {
	var (
		text string
		err  error
	)
	if d.Kind == KindThread || d.Kind == KindReply {
		text, err = o.GH.CommentBody(ctx, d.Repo, d.Edit)
	} else {
		text, err = o.GH.ReviewBody(ctx, d.Repo, d.PR, d.Edit)
	}
	if err != nil {
		return "", fmt.Errorf("read the text --edit %s replaces: %w", d.Edit, err)
	}
	return text, nil
}

// ask files r's decide item and stores its id.
func (o *Outbox) ask(ctx context.Context, r *Record) error {
	q := Question(*r)
	r.Digest = outbox.Digest(q)
	id, err := o.Decide.Ask(ctx, r.Owner, o.Config.NotifyPane, q)
	if err != nil {
		return fmt.Errorf("file approval: %w", err)
	}
	r.DecideID = id
	return o.save(r)
}

func (o *Outbox) save(r *Record) error {
	r.UpdatedAt = o.now()
	return o.Store.Save(*r)
}

const replyQuoteLines = 4

func (r Record) destinations() []string {
	if r.Kind == KindReply && r.Edit == "" && r.Reply != nil {
		if r.Reply.Line > 0 {
			return []string{fmt.Sprintf("reply in the discussion thread on %s line %d, started by %s", r.Reply.Path, r.Reply.Line, r.Reply.Starter)}
		}
		return []string{fmt.Sprintf("reply in the discussion thread on %s, started by %s", r.Reply.Path, r.Reply.Starter)}
	}
	return r.Destinations()
}

func quotePreview(body string, maxLines int) string {
	normalized := strings.ReplaceAll(body, "\r\n", "\n")
	trimmed := strings.TrimRight(normalized, "\n")
	if strings.TrimSpace(trimmed) == "" {
		return "> (empty)"
	}
	lines := strings.Split(trimmed, "\n")
	truncated := false
	if len(lines) > maxLines {
		lines = lines[:maxLines]
		truncated = true
	}
	var b strings.Builder
	for i, l := range lines {
		if i > 0 {
			b.WriteByte('\n')
		}
		b.WriteString("> ")
		b.WriteString(l)
	}
	if truncated {
		b.WriteString("\n> ...")
	}
	return b.String()
}

// Question is the decide question for r: the whole post as it will
// appear, destination first.
func Question(r Record) string {
	var b strings.Builder
	fmt.Fprintf(&b, "Post to GitHub? (gh-outbox draft %d)\n\n", r.ID)
	for _, d := range r.destinations() {
		fmt.Fprintf(&b, "Destination: %s\n", d)
	}
	fmt.Fprintf(&b, "Target: %s\nHead SHA: %s\n", r.Target(), r.HeadSHA)
	if r.Ref != "" {
		fmt.Fprintf(&b, "Ref: %s\n", r.Ref)
	}
	if r.Kind == KindReply && r.Edit == "" && r.Reply != nil && r.Reply.Resolved {
		b.WriteString("Thread: resolved (--allow-resolved)\n")
	}
	if r.Edit != "" {
		newText := r.Body
		if r.Kind == KindThread {
			newText = r.Lines[0].Text
			fmt.Fprintf(&b, "Line: %s\n", r.Lines[0].Where())
		}
		fmt.Fprintf(&b, "\nOld text:\n%s\n\nNew text:\n%s", r.OldText, newText)
		return b.String()
	}
	if r.Kind == KindReply {
		if r.Reply != nil {
			fmt.Fprintf(&b, "\nReplying to:\n%s\n", quotePreview(r.Reply.CommentBody, replyQuoteLines))
		}
		b.WriteString("\nReply:\n")
		b.WriteString(r.Body)
		return b.String()
	}
	if r.Kind != KindThread {
		b.WriteString("\nBody:\n")
		if r.Body == "" {
			b.WriteString("(empty)\n")
		} else {
			b.WriteString(r.Body)
		}
	}
	if len(r.Lines) > 0 {
		b.WriteString("\nLine comments:\n")
		for _, c := range r.Lines {
			fmt.Fprintf(&b, "\n%s\n%s", c.Where(), c.Text)
		}
	}
	return b.String()
}

// Poll acts on every draft: it files missing approvals, posts verified
// human approvals, sends redraft and cancel notices, and resumes work a
// restart interrupted. One draft's error does not stop the others.
func (o *Outbox) Poll(ctx context.Context) error {
	unlock, err := lockDir(o.Store.Dir)
	if err != nil {
		return err
	}
	defer unlock()
	recs, err := o.Store.List()
	if err != nil {
		return err
	}
	var errs []error
	for i := range recs {
		if err := o.step(ctx, &recs[i]); err != nil {
			errs = append(errs, fmt.Errorf("draft %d: %w", recs[i].ID, err))
		}
	}
	return errors.Join(errs...)
}

func (o *Outbox) step(ctx context.Context, r *Record) error {
	switch r.State {
	case StatePending:
		if r.DecideID == "" {
			return o.ask(ctx, r)
		}
		return o.pending(ctx, r)
	case StatePosting:
		if err := o.post(ctx, r); err != nil {
			return err
		}
	}
	return o.finish(ctx, r)
}

func ghOutboxEvalPolicy() outbox.EvalPolicy {
	return outbox.EvalPolicy{
		ApproveKeys:          []string{OptionPost},
		EditKeys:             []string{OptionEdit},
		CancelKeys:           []string{OptionCancel},
		RequireAnswerHumanUI: true,
	}
}

func (o *Outbox) pending(ctx context.Context, r *Record) error {
	it, err := o.Decide.Show(ctx, r.DecideID)
	if err != nil {
		return fmt.Errorf("read approval: %w", err)
	}
	v := outbox.EvaluateApproval(it, ghOutboxEvalPolicy())
	switch v.Action {
	case outbox.ActionApprove:
		return o.approved(ctx, r)
	case outbox.ActionEdit:
		r.State, r.Notes = StateNeedsRedraft, v.Notes
		r.Outcome = "edit requested; redraft notice sent to " + r.Owner
	case outbox.ActionCancel:
		r.State, r.Outcome = StateCancelled, "cancelled; nothing was posted"
	case outbox.ActionClosedWithoutApproval:
		r.State, r.DecideClosed = StateCancelled, true
		r.Outcome = "the approval item was " + it.State + " without a post answer; nothing was posted"
	default:
		return nil
	}
	if err := o.save(r); err != nil {
		return err
	}
	return o.finish(ctx, r)
}

// approved posts r unless the pull request head moved (for diff-anchored
// drafts), the draft content digest changed, or a reply's thread was deleted/resolved.
func (o *Outbox) approved(ctx context.Context, r *Record) error {
	if r.Digest != "" && r.Digest != outbox.Digest(Question(*r)) {
		return o.refuse(ctx, r, "not posted: draft content no longer matches approved digest")
	}
	done, err := o.checkTargetState(ctx, r)
	if err != nil || done {
		return err
	}
	r.State = StatePosting
	if err := o.save(r); err != nil {
		return err
	}
	if err := o.post(ctx, r); err != nil {
		return err
	}
	return o.finish(ctx, r)
}

func (o *Outbox) checkTargetState(ctx context.Context, r *Record) (bool, error) {
	if r.Kind != KindReply {
		return o.checkHeadSHA(ctx, r)
	}
	if r.Edit == "" {
		return o.checkReplyTarget(ctx, r)
	}
	return false, nil
}

func (o *Outbox) checkHeadSHA(ctx context.Context, r *Record) (bool, error) {
	sha, err := o.GH.HeadSHA(ctx, r.Repo, r.PR)
	if err != nil {
		return false, fmt.Errorf("read head of %s: %w", r.Target(), err)
	}
	if sha == r.HeadSHA {
		return false, nil
	}
	if o.Config.OnHeadMoved == HeadMovedRefuse {
		r.State = StateNeedsRedraft
		r.NewHeadSHA = sha
		r.Outcome = fmt.Sprintf("not posted: the pull request head moved from %s to %s", r.HeadSHA, sha)
		if err := o.save(r); err != nil {
			return false, err
		}
		return true, o.finish(ctx, r)
	}
	old := r.DecideID
	r.HeadSHA = sha
	if err := o.ask(ctx, r); err != nil {
		return false, fmt.Errorf("re-ask after the head moved: %w", err)
	}
	if err := o.Decide.Done(ctx, old, "not posted: the pull request head moved to "+sha+"; asked again"); err != nil {
		return false, fmt.Errorf("close the old approval: %w", err)
	}
	return true, nil
}

func (o *Outbox) checkReplyTarget(ctx context.Context, r *Record) (bool, error) {
	target, err := o.GH.ReplyTarget(ctx, r.Repo, r.PR, r.InReplyTo)
	if err != nil {
		if errors.Is(err, ErrReplyNotFound) {
			return true, o.refuse(ctx, r, "not posted: the discussion thread was deleted")
		}
		return false, fmt.Errorf("read thread for --in-reply-to %s on %s: %w", r.InReplyTo, r.Target(), err)
	}
	if target.Resolved && !r.AllowResolved {
		return true, o.refuse(ctx, r, "not posted: the discussion thread was resolved")
	}
	return false, nil
}

func (o *Outbox) refuse(ctx context.Context, r *Record, reason string) error {
	r.State = StateCancelled
	r.InFlight = nil
	r.Outcome = reason
	if err := o.save(r); err != nil {
		return err
	}
	return o.finish(ctx, r)
}

func replyWhere(r Record) string {
	if r.Reply != nil && r.Reply.Path != "" {
		if r.Reply.Line > 0 {
			return r.Reply.Path + ":" + strconv.Itoa(r.Reply.Line)
		}
		return r.Reply.Path
	}
	return "reply"
}

func (o *Outbox) steps(r Record) []outbox.Step {
	switch {
	case r.Kind == KindThread && r.Edit != "":
		c := r.Lines[0]
		return []outbox.Step{{Where: c.Where(), Run: func(ctx context.Context) (string, error) {
			return o.GH.EditComment(ctx, r.Repo, r.Edit, c.Text)
		}}}
	case r.Kind == KindThread:
		out := make([]outbox.Step, 0, len(r.Lines))
		for _, c := range r.Lines {
			out = append(out, outbox.Step{Where: c.Where(), Run: func(ctx context.Context) (string, error) {
				return o.GH.CreateThread(ctx, r.Repo, r.PR, r.HeadSHA, c)
			}})
		}
		return out
	case r.Kind == KindReply && r.Edit != "":
		return []outbox.Step{{Where: "comment " + r.Edit, Run: func(ctx context.Context) (string, error) {
			return o.GH.EditComment(ctx, r.Repo, r.Edit, r.Body)
		}}}
	case r.Kind == KindReply:
		commentID := r.InReplyTo
		if r.Reply != nil && r.Reply.RootID != "" {
			commentID = r.Reply.RootID
		}
		return []outbox.Step{{Where: replyWhere(r), Run: func(ctx context.Context) (string, error) {
			return o.GH.CreateReply(ctx, r.Repo, r.PR, commentID, r.Body)
		}}}
	case r.Edit != "":
		return []outbox.Step{{Where: "review", Run: func(ctx context.Context) (string, error) {
			return o.GH.EditReview(ctx, r.Repo, r.PR, r.Edit, r.Body)
		}}}
	default:
		return []outbox.Step{{Where: "review", Run: func(ctx context.Context) (string, error) {
			return o.GH.CreateReview(ctx, r.Repo, r.PR, r.HeadSHA, reviewEvents[r.Kind], r.Body, r.Lines)
		}}}
	}
}

// post sends the parts not yet posted. A part left in flight by a
// stopped poller may already be on GitHub, so it is never resent: the
// draft becomes uncertain. A GitHub error leaves the draft posting for
// the next poll.
func (o *Outbox) post(ctx context.Context, r *Record) error {
	steps := o.steps(*r)
	state := outbox.ExecutionState{InFlight: r.InFlight, Receipts: r.Posted}
	err := outbox.ExecuteSteps(ctx, &state, steps, func() error {
		r.InFlight = state.InFlight
		r.Posted = state.Receipts
		return o.save(r)
	})
	r.InFlight = state.InFlight
	r.Posted = state.Receipts
	var uerr *outbox.UncertainError
	if errors.As(err, &uerr) {
		r.State = StateUncertain
		r.Outcome = "the poller stopped while posting " + uerr.Where + "; it may or may not be on GitHub and is not retried"
		return o.save(r)
	}
	if err != nil {
		if r.Kind == KindReply && errors.Is(err, ErrReplyNotFound) {
			r.State = StateCancelled
			r.Outcome = "not posted: the discussion thread was deleted"
			return o.save(r)
		}
		return fmt.Errorf("post draft %d: %w", r.ID, err)
	}
	r.State, r.Outcome = StatePosted, "posted"
	return o.save(r)
}

// finish tells the owner the outcome and closes the decide item, each
// once.
func (o *Outbox) finish(ctx context.Context, r *Record) error {
	if !final(r.State) {
		return nil
	}
	if !r.Notified {
		n := outbox.Notice{
			RecordID: r.ID,
			State:    r.State,
			Kind:     r.Kind,
			Target:   r.Target(),
			Owner:    r.Owner,
			Ref:      r.Ref,
			Outcome:  r.Outcome,
			Notes:    r.Notes,
			Receipts: r.Posted,
			Message:  notice(*r),
		}
		if err := o.transport().Notify(ctx, n); err != nil {
			return fmt.Errorf("notify %s: %w", r.Owner, err)
		}
		r.Notified = true
		if err := o.save(r); err != nil {
			return err
		}
	}
	if !r.DecideClosed && r.DecideID != "" {
		if err := o.Decide.Done(ctx, r.DecideID, outcome(*r)); err != nil {
			return fmt.Errorf("close approval: %w", err)
		}
		r.DecideClosed = true
		return o.save(r)
	}
	return nil
}

func final(state string) bool {
	switch state {
	case StatePosted, StateNeedsRedraft, StateCancelled, StateUncertain:
		return true
	}
	return false
}

func linkLines(links []Link) string {
	var b strings.Builder
	for _, l := range links {
		fmt.Fprintf(&b, "\n%s %s", l.Where, l.URL)
	}
	return b.String()
}

func outcome(r Record) string {
	return r.Outcome + linkLines(r.Posted)
}

// notice is the owner notification message.
func notice(r Record) string {
	head := "gh-outbox draft " + strconv.FormatInt(r.ID, 10) + " for " + r.Target()
	if r.Ref != "" {
		head += " (ref " + r.Ref + ")"
	}
	switch r.State {
	case StatePosted:
		return head + " was posted:" + linkLines(r.Posted)
	case StateNeedsRedraft:
		if r.NewHeadSHA != "" {
			return head + ": the pull request head moved from " + r.HeadSHA + " to " + r.NewHeadSHA +
				". Nothing was posted. Re-check the diff and submit a redraft with gh-outbox submit."
		}
		notes := r.Notes
		if notes == "" {
			notes = "(no notes)"
		}
		return head + ": the user asked for a redraft. Nothing was posted. " +
			"Revise it and submit it again with gh-outbox submit. Notes:\n" + notes
	case StateUncertain:
		return head + ": " + r.Outcome + ". Check the pull request before doing anything else." + linkLines(r.Posted)
	default:
		return head + " was not posted: " + r.Outcome
	}
}
