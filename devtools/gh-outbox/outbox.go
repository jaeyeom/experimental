package ghoutbox

import (
	"context"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

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

// closed reports whether an approval item no longer takes an answer. An
// agent's done on an item the user has not read leaves it done-unread in
// the user's inbox, which is closed here too.
func closed(it DecideItem) bool {
	return it.State == decide.StateDone || it.State == decide.StateDoneUnread || it.State == decide.StateWithdrawn
}

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
	Store  *Store
	Decide Decider
	GH     GitHub
	Bus    Bus
	Config Config
	// Now defaults to time.Now.
	Now func() time.Time
}

func (o *Outbox) now() time.Time {
	if o.Now != nil {
		return o.Now().UTC()
	}
	return time.Now().UTC()
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
	id, err := o.Decide.Ask(ctx, r.Owner, o.Config.NotifyPane, Question(*r))
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

// verdict is what the user's latest own message asks for.
type verdict struct {
	action string // OptionPost, OptionEdit, OptionCancel, or "" to wait
	notes  string
}

// decideVerdict reads the user's latest message. Messages from agents,
// including ones sent as the user from the command line, do not count.
// A post pick counts only from the decide UI with a matching human UI
// answer. Free text is edit notes.
func decideVerdict(it DecideItem) verdict {
	var (
		last  DecideMessage
		found bool
		notes []string
	)
	for _, m := range it.Messages {
		if !m.FromHuman() {
			continue
		}
		last, found = m, true
		if m.OptionKey == "" && strings.TrimSpace(m.Text) != "" {
			notes = append(notes, m.Text)
		}
	}
	if !found {
		return verdict{}
	}
	switch last.OptionKey {
	case OptionPost:
		a := it.Answer
		if last.HumanUI() && a.HumanUI() && a.Key == OptionPost {
			return verdict{action: OptionPost}
		}
	case OptionCancel:
		return verdict{action: OptionCancel}
	case OptionEdit, "":
		return verdict{action: OptionEdit, notes: strings.Join(notes, "\n\n")}
	}
	return verdict{}
}

func (o *Outbox) pending(ctx context.Context, r *Record) error {
	it, err := o.Decide.Show(ctx, r.DecideID)
	if err != nil {
		return fmt.Errorf("read approval: %w", err)
	}
	v := decideVerdict(it)
	switch v.action {
	case OptionPost:
		return o.approved(ctx, r)
	case OptionEdit:
		r.State, r.Notes = StateNeedsRedraft, v.notes
		r.Outcome = "edit requested; redraft notice sent to " + r.Owner
	case OptionCancel:
		r.State, r.Outcome = StateCancelled, "cancelled; nothing was posted"
	default:
		if !closed(it) {
			return nil
		}
		r.State, r.DecideClosed = StateCancelled, true
		r.Outcome = "the approval item was " + it.State + " without a post answer; nothing was posted"
	}
	if err := o.save(r); err != nil {
		return err
	}
	return o.finish(ctx, r)
}

// approved posts r unless the pull request head moved (for diff-anchored
// drafts) or a reply's thread was deleted/resolved.
func (o *Outbox) approved(ctx context.Context, r *Record) error {
	if r.Kind != KindReply {
		sha, err := o.GH.HeadSHA(ctx, r.Repo, r.PR)
		if err != nil {
			return fmt.Errorf("read head of %s: %w", r.Target(), err)
		}
		if sha != r.HeadSHA {
			if o.Config.OnHeadMoved == HeadMovedRefuse {
				r.State = StateNeedsRedraft
				r.NewHeadSHA = sha
				r.Outcome = fmt.Sprintf("not posted: the pull request head moved from %s to %s", r.HeadSHA, sha)
				if err := o.save(r); err != nil {
					return err
				}
				return o.finish(ctx, r)
			}
			old := r.DecideID
			r.HeadSHA = sha
			if err := o.ask(ctx, r); err != nil {
				return fmt.Errorf("re-ask after the head moved: %w", err)
			}
			if err := o.Decide.Done(ctx, old, "not posted: the pull request head moved to "+sha+"; asked again"); err != nil {
				return fmt.Errorf("close the old approval: %w", err)
			}
			return nil
		}
	} else if r.Edit == "" {
		target, err := o.GH.ReplyTarget(ctx, r.Repo, r.PR, r.InReplyTo)
		if err != nil {
			if errors.Is(err, ErrReplyNotFound) {
				return o.refuse(ctx, r, "not posted: the discussion thread was deleted")
			}
			return fmt.Errorf("read thread for --in-reply-to %s on %s: %w", r.InReplyTo, r.Target(), err)
		}
		if target.Resolved && !r.AllowResolved {
			return o.refuse(ctx, r, "not posted: the discussion thread was resolved")
		}
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

func (o *Outbox) refuse(ctx context.Context, r *Record, reason string) error {
	r.State = StateCancelled
	r.InFlight = nil
	r.Outcome = reason
	if err := o.save(r); err != nil {
		return err
	}
	return o.finish(ctx, r)
}

// part is one GitHub write.
type part struct {
	where string
	send  func(context.Context) (string, error)
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

func (o *Outbox) parts(r Record) []part {
	switch {
	case r.Kind == KindThread && r.Edit != "":
		c := r.Lines[0]
		return []part{{c.Where(), func(ctx context.Context) (string, error) {
			return o.GH.EditComment(ctx, r.Repo, r.Edit, c.Text)
		}}}
	case r.Kind == KindThread:
		out := make([]part, 0, len(r.Lines))
		for _, c := range r.Lines {
			out = append(out, part{c.Where(), func(ctx context.Context) (string, error) {
				return o.GH.CreateThread(ctx, r.Repo, r.PR, r.HeadSHA, c)
			}})
		}
		return out
	case r.Kind == KindReply && r.Edit != "":
		return []part{{"comment " + r.Edit, func(ctx context.Context) (string, error) {
			return o.GH.EditComment(ctx, r.Repo, r.Edit, r.Body)
		}}}
	case r.Kind == KindReply:
		commentID := r.InReplyTo
		if r.Reply != nil && r.Reply.RootID != "" {
			commentID = r.Reply.RootID
		}
		return []part{{replyWhere(r), func(ctx context.Context) (string, error) {
			return o.GH.CreateReply(ctx, r.Repo, r.PR, commentID, r.Body)
		}}}
	case r.Edit != "":
		return []part{{"review", func(ctx context.Context) (string, error) {
			return o.GH.EditReview(ctx, r.Repo, r.PR, r.Edit, r.Body)
		}}}
	default:
		return []part{{"review", func(ctx context.Context) (string, error) {
			return o.GH.CreateReview(ctx, r.Repo, r.PR, r.HeadSHA, reviewEvents[r.Kind], r.Body, r.Lines)
		}}}
	}
}

// post sends the parts not yet posted. A part left in flight by a
// stopped poller may already be on GitHub, so it is never resent: the
// draft becomes uncertain. A GitHub error leaves the draft posting for
// the next poll.
func (o *Outbox) post(ctx context.Context, r *Record) error {
	parts := o.parts(*r)
	if r.InFlight != nil {
		where := "?"
		if *r.InFlight < len(parts) {
			where = parts[*r.InFlight].where
		}
		r.State, r.InFlight = StateUncertain, nil
		r.Outcome = "the poller stopped while posting " + where + "; it may or may not be on GitHub and is not retried"
		return o.save(r)
	}
	for i := len(r.Posted); i < len(parts); i++ {
		idx := i
		r.InFlight = &idx
		if err := o.save(r); err != nil {
			return err
		}
		url, err := parts[i].send(ctx)
		r.InFlight = nil
		if err != nil {
			if r.Kind == KindReply && errors.Is(err, ErrReplyNotFound) {
				r.State = StateCancelled
				r.Outcome = "not posted: the discussion thread was deleted"
				return o.save(r)
			}
			return errors.Join(fmt.Errorf("post %s: %w", parts[i].where, err), o.save(r))
		}
		r.Posted = append(r.Posted, Link{Where: parts[i].where, URL: url})
		if err := o.save(r); err != nil {
			return err
		}
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
		if err := o.Bus.Send(ctx, r.Owner, notice(*r)); err != nil {
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

// notice is the agentbus message to the owner.
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
