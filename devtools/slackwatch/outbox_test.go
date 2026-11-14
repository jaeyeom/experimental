package slackwatch

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"time"
)

// fakeDecider is an in-memory decide ledger.
type fakeDecider struct {
	items               map[string]*DecideItem
	questions           map[string]string
	owners              map[string]string
	outcomes            map[string]string
	replies             []string
	optionReplies       []string
	dropRestoredOptions bool
	askErr              error
	next                int
}

func newFakeDecider() *fakeDecider {
	return &fakeDecider{items: map[string]*DecideItem{}, questions: map[string]string{}, owners: map[string]string{}, outcomes: map[string]string{}}
}

func (d *fakeDecider) Ask(_ context.Context, owner, question string) (string, error) {
	if d.askErr != nil {
		return "", d.askErr
	}
	d.next++
	id := fmt.Sprintf("D-%d", d.next)
	d.items[id] = &DecideItem{
		ID: id, State: "open",
		Options: []DecideOption{
			{Key: decideKeyPost, Label: "Post as written", Recommended: true},
			{Key: decideKeyDrop, Label: "Don't post"},
		},
		Messages: []DecideMessage{{Author: owner, Text: question}},
	}
	d.questions[id] = question
	d.owners[id] = owner
	return id, nil
}

func (d *fakeDecider) Show(_ context.Context, id string) (DecideItem, error) {
	it, ok := d.items[id]
	if !ok {
		return DecideItem{}, errors.New("no such item")
	}
	return *it, nil
}

func (d *fakeDecider) Reply(_ context.Context, id, text string) error {
	it, ok := d.items[id]
	if !ok {
		return errors.New("no such item")
	}
	it.Messages = append(it.Messages, DecideMessage{Author: d.owners[id], Text: text, By: "slackwatch"})
	d.replies = append(d.replies, text)
	return nil
}

func (d *fakeDecider) ReplyOptions(_ context.Context, id, text string) error {
	it, ok := d.items[id]
	if !ok {
		return errors.New("no such item")
	}
	d.optionReplies = append(d.optionReplies, text)
	if !d.dropRestoredOptions {
		it.Options = []DecideOption{
			{Key: decideKeyPost, Label: "Post as written", Recommended: true},
			{Key: decideKeyDrop, Label: "Don't post"},
		}
	}
	it.Messages = append(it.Messages, DecideMessage{Author: d.owners[id], Text: text, By: "slackwatch"})
	return nil
}

func (d *fakeDecider) Done(_ context.Context, id, outcome string) error {
	it, ok := d.items[id]
	if !ok {
		return errors.New("no such item")
	}
	if it.State == "done" {
		return errors.New("already done")
	}
	it.State = "done"
	d.outcomes[id] = outcome
	return nil
}

// answer records a as decide answer and compose do: the item becomes
// answered and the thread gets a's message. An answer by anyone but
// human is a message sent with by.
func (d *fakeDecider) answer(id string, a DecideAnswer) {
	it := d.items[id]
	it.State = "answered"
	it.Answer = &a
	m := DecideMessage{Author: "human", Text: a.Value, OptionKey: a.Key, Via: a.Via}
	if a.AnsweredBy != "human" {
		m.By = a.AnsweredBy
	}
	it.Messages = append(it.Messages, m)
}

// reply appends an agent reply sent by by, as decide reply does: the
// item reopens and keeps its answer.
func (d *fakeDecider) reply(id, by, text string) {
	it := d.items[id]
	it.State = "open"
	it.Messages = append(it.Messages, DecideMessage{Author: d.owners[id], Text: text, By: by})
}

// fakeWriter is a Slack write API.
type fakeWriter struct {
	user   string
	auths  int
	posts  []fakePost
	err    error
	errFor map[string]error
}

type fakePost struct{ token, channel, thread, text string }

// postedBody is the direct-mode body: the approved text, a blank line,
// and the footer.
func postedBody() string { return approvedText + "\n\n" + footerText }

func (w *fakeWriter) AuthTest(_ context.Context, _ string) (AuthInfo, error) {
	w.auths++
	return AuthInfo{UserID: w.user, URL: "https://x.slack.com/"}, nil
}

func (w *fakeWriter) PostMessage(_ context.Context, token, channel, thread, text string) (string, error) {
	if w.err != nil {
		return "", w.err
	}
	if err := w.errFor[channel]; err != nil {
		return "", err
	}
	w.posts = append(w.posts, fakePost{token, channel, thread, text})
	return fmt.Sprintf("1700000100.%06d", len(w.posts)), nil
}

// fakeReader returns one canned message and a canned recent history.
type fakeReader struct {
	msg         Message
	calls       int
	recent      []Message
	recentErr   error
	recentFail  int // recentErr applies this many times; 0 means every call
	recentCalls int
	oldest      string
}

func (r *fakeReader) Message(_ context.Context, channel, ts, _ string) (Message, error) {
	r.calls++
	m := r.msg
	m.Channel, m.TS = channel, ts
	return m, nil
}

func (r *fakeReader) Recent(_ context.Context, channel, threadTS, oldest string) ([]Message, error) {
	r.recentCalls++
	r.oldest = oldest
	if r.recentErr != nil && (r.recentFail == 0 || r.recentCalls <= r.recentFail) {
		return nil, r.recentErr
	}
	out := make([]Message, 0, len(r.recent))
	for _, m := range r.recent {
		m.Channel = channel
		if threadTS != "" {
			m.ThreadTS = threadTS
		}
		out = append(out, m)
	}
	return out, nil
}

const (
	expectUser   = "UPOSTER"
	writeToken   = "xoxp-write"
	footerText   = "_sent by an agent_"
	approvedText = "Deploy is done & <@U5> can check.\nSee <https://example.com|notes>."
)

type outboxFixture struct {
	store  *Store
	dec    *fakeDecider
	rec    *recordingNotifier
	writer *fakeWriter
	reader *fakeReader
	ob     *Outbox
}

func newOutboxFixture(t *testing.T, mode string) *outboxFixture {
	t.Helper()
	s := openTestStore(t)
	if err := s.SetTeamURL("https://x.slack.com/"); err != nil {
		t.Fatal(err)
	}
	tokenFile := filepath.Join(t.TempDir(), "write-token")
	if err := os.WriteFile(tokenFile, []byte(writeToken+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	cfg := Config{
		FallbackTarget: "fallback",
		FooterPattern:  Regexp{regexp.MustCompile(`_sent by an agent_`)},
		Post: PostConfig{
			Mode: mode, Agent: "poster", ExpectUser: expectUser,
			PinSalt: "salt", TokenPin: Pin("salt", writeToken),
			WriteTokenFile: tokenFile, Footer: footerText,
		},
	}
	f := &outboxFixture{store: s, dec: newFakeDecider(), rec: &recordingNotifier{}, writer: &fakeWriter{user: expectUser}, reader: &fakeReader{}}
	f.ob = &Outbox{
		Store: s, Config: cfg, Decide: f.dec, Notifier: f.rec, Writer: f.writer, Reader: f.reader,
		Now: testNow, Logger: slog.New(slog.DiscardHandler),
	}
	return f
}

func (f *outboxFixture) submit(t *testing.T) OutboxRecord {
	t.Helper()
	r, err := f.ob.Submit(context.Background(), Draft{
		Channel: "C1", DestName: "#general", Text: approvedText,
		Owner: "agent-1", Purpose: "announce deploy", Refs: []string{"#12"},
	})
	if err != nil {
		t.Fatal(err)
	}
	return r
}

func (f *outboxFixture) get(t *testing.T, id int64) OutboxRecord {
	t.Helper()
	r, err := f.store.GetOutbox(id)
	if err != nil {
		t.Fatal(err)
	}
	return r
}

func (f *outboxFixture) poll(t *testing.T) {
	t.Helper()
	if err := f.ob.Poll(context.Background()); err != nil {
		t.Fatal(err)
	}
}

func (f *outboxFixture) events() []string {
	var out []string
	for _, n := range f.rec.notes {
		out = append(out, n.Event+">"+n.Owner)
	}
	return out
}

var uiPost = DecideAnswer{Value: "post", Key: "post", AnsweredBy: "human", Via: "ui"}

func TestSubmitFilesDecideItem(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	if r.State != OutboxPending || r.DecideID != "D-1" || r.TextHash != TextHash(approvedText) {
		t.Fatalf("record = %+v", r)
	}
	if f.dec.owners["D-1"] != DefaultDecideOwner {
		t.Errorf("decide owner = %q", f.dec.owners["D-1"])
	}
	q := f.dec.questions["D-1"]
	for _, want := range []string{"#general", "> Deploy is done & <@U5> can check.\n> See <https://example.com|notes>.", "agent-1", "announce deploy", r.TextHash[:8]} {
		if !strings.Contains(q, want) {
			t.Errorf("question missing %q:\n%s", want, q)
		}
	}
}

func TestSubmitThreadShowsParentLink(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	_, err := f.ob.Submit(context.Background(), Draft{
		Channel: "C1", ThreadTS: "1700000000.000100", DestName: "#general",
		ParentLink: "https://x.slack.com/archives/C1/p1700000000000100", Text: "hi", Owner: "a", Purpose: "p",
	})
	if err != nil {
		t.Fatal(err)
	}
	if q := f.dec.questions["D-1"]; !strings.Contains(q, "https://x.slack.com/archives/C1/p1700000000000100") {
		t.Errorf("question has no parent link:\n%s", q)
	}
}

func TestSubmitPostingOff(t *testing.T) {
	f := newOutboxFixture(t, "")
	_, err := f.ob.Submit(context.Background(), Draft{Channel: "C1", Text: "hi", Owner: "a", Purpose: "p"})
	if !errors.Is(err, ErrPostingOff) || !strings.Contains(err.Error(), "post.mode") {
		t.Fatalf("err = %v, want posting off naming post.mode", err)
	}
	if len(f.dec.items) != 0 {
		t.Error("decide item filed with posting off")
	}
}

func TestSubmitAskFailureMarksFailed(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	f.dec.askErr = errors.New("decide down")
	_, err := f.ob.Submit(context.Background(), Draft{Channel: "C1", Text: "hi", Owner: "a", Purpose: "p"})
	if err == nil {
		t.Fatal("submit succeeded")
	}
	if r := f.get(t, 1); r.State != OutboxFailed {
		t.Errorf("state = %s, want failed", r.State)
	}
}

func TestAcceptanceHandoffPostsOnce(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	f.poll(t)
	if got := f.get(t, r.ID).State; got != OutboxPending {
		t.Fatalf("unanswered state = %s", got)
	}
	f.dec.answer(r.DecideID, uiPost)
	f.poll(t)
	f.poll(t)
	if err := f.ob.Poke(context.Background(), r.DecideID); err != nil {
		t.Fatal(err)
	}
	if got := f.get(t, r.ID).State; got != OutboxAwaitingHandoff {
		t.Fatalf("state = %s, want awaiting-handoff", got)
	}
	if got := f.events(); len(got) != 1 || got[0] != EventOutboxReady+">poster" {
		t.Fatalf("events = %v, want one ready to poster", got)
	}
	if msg := f.rec.notes[0].Message; msg != fmt.Sprintf("approved post ready: %d", r.ID) {
		t.Errorf("message = %q", msg)
	}
}

func TestAcceptanceDirectPostsOnceWithFooter(t *testing.T) {
	f := newOutboxFixture(t, PostModeDirect)
	r := f.submit(t)
	f.dec.answer(r.DecideID, uiPost)
	f.poll(t)
	// A second answer to the same item does not post again.
	f.dec.items[r.DecideID].State = "answered"
	f.poll(t)
	if err := f.ob.Poke(context.Background(), r.DecideID); err != nil {
		t.Fatal(err)
	}
	if len(f.writer.posts) != 1 {
		t.Fatalf("posts = %+v, want one", f.writer.posts)
	}
	p := f.writer.posts[0]
	if p.token != writeToken || p.channel != "C1" || p.text != approvedText+"\n\n"+footerText {
		t.Errorf("post = %+v", p)
	}
	got := f.get(t, r.ID)
	if got.State != OutboxPosted || got.WatchID == 0 || got.Permalink == "" {
		t.Fatalf("record = %+v", got)
	}
	w, err := f.store.FindWatch(PostRef{WatchID: got.WatchID})
	if err != nil {
		t.Fatal(err)
	}
	if w.Owner != "agent-1" || w.Source != SourceOutbox || w.Purpose != "announce deploy" || w.State != StateOpen {
		t.Errorf("watch = %+v", w)
	}
	if f.dec.outcomes[r.DecideID] != got.Permalink {
		t.Errorf("decide outcome = %q, want %q", f.dec.outcomes[r.DecideID], got.Permalink)
	}
}

func TestAcceptanceRefusals(t *testing.T) {
	for _, tc := range []struct {
		name   string
		answer DecideAnswer
		tamper bool
		state  string
		events []string
	}{
		{
			"via cli",
			DecideAnswer{Value: "post", Key: "post", AnsweredBy: "human", Via: "cli"},
			false, OutboxRejected,
			[]string{EventOutboxRejected + ">agent-1", EventOutboxRejected + ">"},
		},
		{
			"missing via",
			DecideAnswer{Value: "post", Key: "post", AnsweredBy: "human"},
			false, OutboxRejected,
			[]string{EventOutboxRejected + ">agent-1", EventOutboxRejected + ">"},
		},
		{
			"free text",
			DecideAnswer{Value: "say tomorrow instead", AnsweredBy: "human", Via: "ui"},
			false, OutboxNeedsRedraft,
			[]string{EventOutboxRedraft + ">agent-1"},
		},
		{
			"free text post",
			DecideAnswer{Value: "post it", AnsweredBy: "human", Via: "ui"},
			false, OutboxNeedsRedraft,
			[]string{EventOutboxRedraft + ">agent-1"},
		},
		{
			"drop",
			DecideAnswer{Value: "drop", Key: "drop", AnsweredBy: "human", Via: "ui"},
			false, OutboxDropped,
			[]string{EventOutboxDropped + ">agent-1"},
		},
		{
			"changed hash", uiPost, true, OutboxRejected,
			[]string{EventOutboxRejected + ">agent-1", EventOutboxRejected + ">"},
		},
	} {
		for _, mode := range []string{PostModeHandoff, PostModeDirect} {
			t.Run(tc.name+" "+mode, func(t *testing.T) {
				f := newOutboxFixture(t, mode)
				r := f.submit(t)
				if tc.tamper {
					if _, err := f.store.DB().Exec(`UPDATE outbox SET text = 'changed' WHERE id = ?`, r.ID); err != nil {
						t.Fatal(err)
					}
				}
				f.dec.answer(r.DecideID, tc.answer)
				f.poll(t)
				f.poll(t)
				got := f.get(t, r.ID)
				if got.State != tc.state {
					t.Errorf("state = %s, want %s (reason %q)", got.State, tc.state, got.Reason)
				}
				if len(f.writer.posts) != 0 {
					t.Errorf("posted %+v", f.writer.posts)
				}
				if ev := f.events(); strings.Join(ev, ",") != strings.Join(tc.events, ",") {
					t.Errorf("events = %v, want %v", ev, tc.events)
				}
				if f.dec.items[r.DecideID].State != "done" || f.dec.outcomes[r.DecideID] == "" {
					t.Errorf("decide item not closed: %+v", f.dec.items[r.DecideID])
				}
			})
		}
	}
}

// TestAcceptanceIgnoresMessagesSentWithBy: a message sent with by is not
// the user's, whatever its via, so it neither approves nor refuses.
func TestAcceptanceIgnoresMessagesSentWithBy(t *testing.T) {
	for _, a := range []DecideAnswer{
		{Value: "post", Key: "post", AnsweredBy: "agent-1", Via: "ui"},
		{Value: "drop", Key: "drop", AnsweredBy: "agent-1", Via: "cli"},
		{Value: "shorter", AnsweredBy: "agent-1", Via: "ui"},
	} {
		t.Run(a.Value, func(t *testing.T) {
			f := newOutboxFixture(t, PostModeDirect)
			r := f.submit(t)
			f.dec.answer(r.DecideID, a)
			f.poll(t)
			if got := f.get(t, r.ID); got.State != OutboxPending {
				t.Errorf("state = %s, want pending", got.State)
			}
			if len(f.writer.posts) != 0 || len(f.rec.notes) != 0 || f.dec.items[r.DecideID].State == "done" {
				t.Errorf("posts %v, events %v, decide %+v", f.writer.posts, f.events(), f.dec.items[r.DecideID])
			}
		})
	}
}

// TestAcceptanceSurvivesLaterReplies: a ui post pick still posts after
// agent replies and messages sent with by reopen the item.
func TestAcceptanceSurvivesLaterReplies(t *testing.T) {
	for _, mode := range []string{PostModeHandoff, PostModeDirect} {
		t.Run(mode, func(t *testing.T) {
			f := newOutboxFixture(t, mode)
			r := f.submit(t)
			f.dec.answer(r.DecideID, uiPost)
			f.dec.reply(r.DecideID, "agent-1", "thanks, waiting for slackwatch")
			f.dec.answer(r.DecideID, DecideAnswer{Value: "also cc ops", AnsweredBy: "agent-2", Via: "cli"})
			f.dec.reply(r.DecideID, "", "noted")
			f.poll(t)
			want, posts := OutboxAwaitingHandoff, 0
			if mode == PostModeDirect {
				want, posts = OutboxPosted, 1
			}
			if got := f.get(t, r.ID); got.State != want || got.Answer != "post" {
				t.Fatalf("record = %+v, want %s", got, want)
			}
			if len(f.writer.posts) != posts {
				t.Errorf("posts = %+v, want %d", f.writer.posts, posts)
			}
		})
	}
}

// TestAcceptanceLaterHumanMessageWins: a human message after the post
// pick takes precedence and goes through the redraft or drop path.
func TestAcceptanceLaterHumanMessageWins(t *testing.T) {
	for _, tc := range []struct {
		name   string
		later  DecideAnswer
		state  string
		events []string
	}{
		{"free text", DecideAnswer{Value: "wait, say tomorrow", AnsweredBy: "human", Via: "ui"}, OutboxNeedsRedraft, []string{EventOutboxRedraft + ">agent-1"}},
		{"free text from cli", DecideAnswer{Value: "wait", AnsweredBy: "human", Via: "cli"}, OutboxNeedsRedraft, []string{EventOutboxRedraft + ">agent-1"}},
		{"drop", DecideAnswer{Value: "drop", Key: "drop", AnsweredBy: "human", Via: "ui"}, OutboxDropped, []string{EventOutboxDropped + ">agent-1"}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			f := newOutboxFixture(t, PostModeDirect)
			r := f.submit(t)
			f.dec.answer(r.DecideID, uiPost)
			f.dec.reply(r.DecideID, "agent-1", "ok")
			f.dec.answer(r.DecideID, tc.later)
			f.poll(t)
			got := f.get(t, r.ID)
			if got.State != tc.state {
				t.Errorf("state = %s, want %s (reason %q)", got.State, tc.state, got.Reason)
			}
			if len(f.writer.posts) != 0 {
				t.Errorf("posted %+v", f.writer.posts)
			}
			if ev := f.events(); strings.Join(ev, ",") != strings.Join(tc.events, ",") {
				t.Fatalf("events = %v, want %v", ev, tc.events)
			}
			if tc.state == OutboxNeedsRedraft && (got.Answer != tc.later.Value || f.rec.notes[0].Text != tc.later.Value) {
				t.Errorf("answer = %q, note = %+v", got.Answer, f.rec.notes[0])
			}
		})
	}
}

func TestRedraftNotifiesAnswerText(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	f.dec.answer(r.DecideID, DecideAnswer{Value: "mention the rollback plan", AnsweredBy: "human", Via: "ui"})
	f.poll(t)
	if got := f.get(t, r.ID); got.Answer != "mention the rollback plan" {
		t.Errorf("answer = %q", got.Answer)
	}
	if n := f.rec.notes[0]; !strings.Contains(n.Message, "mention the rollback plan") || n.Text != "mention the rollback plan" {
		t.Errorf("note = %+v", n)
	}
}

func TestWithdrawnDecideItemCancels(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	f.dec.items[r.DecideID].State = "withdrawn"
	f.poll(t)
	if got := f.get(t, r.ID); got.State != OutboxCancelled {
		t.Errorf("state = %s, want cancelled", got.State)
	}
}

func TestCancel(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	got, err := f.ob.Cancel(context.Background(), r.ID, "wrong channel")
	if err != nil {
		t.Fatal(err)
	}
	if got.State != OutboxCancelled || f.dec.items[r.DecideID].State != "done" {
		t.Errorf("record %+v, decide %+v", got, f.dec.items[r.DecideID])
	}
	f.dec.answer(r.DecideID, uiPost)
	f.poll(t)
	if len(f.rec.notes) != 0 {
		t.Errorf("cancelled record acted on: %v", f.events())
	}
	if _, err := f.ob.Cancel(context.Background(), r.ID, ""); err == nil {
		t.Error("cancelled twice")
	}
}

func redraft(t *testing.T, f *outboxFixture) OutboxRecord {
	t.Helper()
	r := f.submit(t)
	f.dec.answer(r.DecideID, DecideAnswer{Value: "mention the rollback plan", AnsweredBy: "human", Via: "ui"})
	f.poll(t)
	got := f.get(t, r.ID)
	if got.State != OutboxNeedsRedraft {
		t.Fatalf("state = %s, want needs-redraft", got.State)
	}
	return got
}

func TestCancelNeedsRedraft(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := redraft(t, f)
	got, err := f.ob.Cancel(context.Background(), r.ID, "dropping it")
	if err != nil {
		t.Fatal(err)
	}
	if got.State != OutboxCancelled || got.Reason != "dropping it" {
		t.Fatalf("record = %+v", got)
	}
	if f.dec.items[r.DecideID].State != "done" {
		t.Errorf("decide = %+v, want done", f.dec.items[r.DecideID])
	}
	c, err := f.store.OutboxCounts()
	if err != nil {
		t.Fatal(err)
	}
	if c.Pending != 0 || c.AwaitingHandoff != 0 || c.NeedsRedraft != 0 {
		t.Errorf("counts = %+v", c)
	}
}

func submitReplacing(t *testing.T, f *outboxFixture, old int64) (OutboxRecord, error) {
	t.Helper()
	return f.ob.Submit(context.Background(), Draft{
		Channel: "C1", DestName: "#general", Text: "revised text",
		Owner: "agent-1", Purpose: "announce deploy", Supersedes: old,
	})
}

func TestSupersedeNeedsRedraft(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	old := redraft(t, f)
	neu, err := submitReplacing(t, f, old.ID)
	if err != nil {
		t.Fatal(err)
	}
	if neu.Supersedes != old.ID || neu.State != OutboxPending {
		t.Fatalf("new = %+v", neu)
	}
	want := fmt.Sprintf("not posted: superseded by %d", neu.ID)
	got := f.get(t, old.ID)
	if got.State != OutboxCancelled || got.Reason != want {
		t.Fatalf("old = %+v, want reason %q", got, want)
	}
	if f.dec.items[old.DecideID].State != "done" {
		t.Errorf("decide = %+v, want done", f.dec.items[old.DecideID])
	}
	by, err := f.store.SupersededBy(old.ID)
	if err != nil || by != neu.ID {
		t.Errorf("SupersededBy = %d, %v", by, err)
	}
	if back, err := f.store.SupersededBy(neu.ID); err != nil || back != 0 {
		t.Errorf("SupersededBy(new) = %d, %v", back, err)
	}
}

func TestSupersedePendingAndAwaitingHandoff(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	old := f.submit(t)
	neu, err := submitReplacing(t, f, old.ID)
	if err != nil {
		t.Fatal(err)
	}
	want := fmt.Sprintf("not posted: superseded by %d", neu.ID)
	if got := f.get(t, old.ID); got.State != OutboxCancelled || got.Reason != want {
		t.Fatalf("pending old = %+v", got)
	}
	if f.dec.outcomes[old.DecideID] != want || f.dec.items[old.DecideID].State != "done" {
		t.Errorf("decide outcome = %q, item %+v", f.dec.outcomes[old.DecideID], f.dec.items[old.DecideID])
	}

	f = newOutboxFixture(t, PostModeHandoff)
	old = approveHandoff(t, f)
	if got := f.get(t, old.ID); got.State != OutboxAwaitingHandoff {
		t.Fatalf("state = %s, want awaiting-handoff", got.State)
	}
	if _, err := submitReplacing(t, f, old.ID); err != nil {
		t.Fatal(err)
	}
	if got := f.get(t, old.ID); got.State != OutboxCancelled {
		t.Errorf("awaiting-handoff old state = %s", got.State)
	}
}

func TestSupersedeRefusesFinished(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	old := approveHandoff(t, f)
	moved, err := f.store.moveOutbox(old.ID, []string{OutboxAwaitingHandoff}, outboxUpdate{state: OutboxPosted}, testNow())
	if err != nil || !moved {
		t.Fatalf("move = %v, %v", moved, err)
	}
	if _, err := submitReplacing(t, f, old.ID); err == nil || !strings.Contains(err.Error(), OutboxPosted) {
		t.Fatalf("err = %v, want a refusal naming posted", err)
	}
	recs, err := f.store.ListOutbox("")
	if err != nil {
		t.Fatal(err)
	}
	if len(recs) != 1 || recs[0].State != OutboxPosted {
		t.Fatalf("records = %+v", recs)
	}

	approved := newOutboxFixture(t, PostModeDirect)
	r := approved.submit(t)
	moved, err = approved.store.moveOutbox(r.ID, []string{OutboxPending}, outboxUpdate{state: OutboxApproved}, testNow())
	if err != nil || !moved {
		t.Fatalf("move = %v, %v", moved, err)
	}
	if _, err := submitReplacing(t, approved, r.ID); err == nil || !strings.Contains(err.Error(), OutboxApproved) {
		t.Fatalf("err = %v, want a refusal naming approved", err)
	}
	if recs, err = approved.store.ListOutbox(""); err != nil || len(recs) != 1 {
		t.Fatalf("records = %+v, %v", recs, err)
	}
}

func TestSupersedeFailureLeavesOld(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	old := redraft(t, f)
	_, err := f.ob.Submit(context.Background(), Draft{
		Text: "revised text", Owner: "agent-1", Purpose: "announce deploy", Supersedes: old.ID,
	})
	if err == nil {
		t.Fatal("empty destination succeeded")
	}
	if got := f.get(t, old.ID); got.State != OutboxNeedsRedraft || got.Reason != "" {
		t.Fatalf("old changed: %+v", got)
	}
	recs, err := f.store.ListOutbox("")
	if err != nil {
		t.Fatal(err)
	}
	if len(recs) != 1 {
		t.Fatalf("records = %d, want 1", len(recs))
	}

	f.dec.askErr = errors.New("decide down")
	_, err = submitReplacing(t, f, old.ID)
	if err == nil {
		t.Fatal("submit succeeded with decide down")
	}
	if got := f.get(t, old.ID); got.State != OutboxNeedsRedraft {
		t.Fatalf("old = %+v, want needs-redraft", got)
	}
	neu := f.get(t, 2)
	if neu.State != OutboxFailed || neu.Supersedes != 0 {
		t.Fatalf("failed submit = %+v", neu)
	}
	by, err := f.store.SupersededBy(old.ID)
	if err != nil || by != 0 {
		t.Errorf("SupersededBy = %d, %v", by, err)
	}
}

func approveHandoff(t *testing.T, f *outboxFixture) OutboxRecord {
	t.Helper()
	r := f.submit(t)
	f.dec.answer(r.DecideID, uiPost)
	f.poll(t)
	return r
}

const postedLink = "https://x.slack.com/archives/C1/p1700000100000001"

func TestHandoffPostedMatches(t *testing.T) {
	for name, text := range map[string]string{
		"exact":    approvedText,
		"escaped":  "Deploy is done &amp; <@U5> can check.  \nSee <https://example.com|notes>.\n\n",
		"footer":   approvedText + "\n" + footerText,
		"trailing": approvedText + " \t\n",
	} {
		t.Run(name, func(t *testing.T) {
			f := newOutboxFixture(t, PostModeHandoff)
			r := approveHandoff(t, f)
			f.rec.notes = nil
			f.reader.msg = Message{User: expectUser, Text: text}
			got, err := f.ob.Posted(context.Background(), r.ID, postedLink)
			if err != nil {
				t.Fatal(err)
			}
			if got.State != OutboxPosted || got.PostTS != "1700000100.000001" {
				t.Fatalf("record = %+v", got)
			}
			w, err := f.store.FindWatch(PostRef{WatchID: got.WatchID})
			if err != nil {
				t.Fatal(err)
			}
			if w.Owner != "agent-1" || w.Source != SourceOutbox || w.Flag != "" || w.Permalink != postedLink {
				t.Errorf("watch = %+v", w)
			}
			if f.dec.items[r.DecideID].State != "done" || f.dec.outcomes[r.DecideID] != postedLink {
				t.Errorf("decide = %+v outcome %q", f.dec.items[r.DecideID], f.dec.outcomes[r.DecideID])
			}
			if len(f.rec.notes) != 0 {
				t.Errorf("notified %v", f.events())
			}
		})
	}
}

func TestHandoffPostedMismatch(t *testing.T) {
	for name, tc := range map[string]struct {
		link string
		msg  Message
	}{
		"rewritten link": {postedLink, Message{User: expectUser, Text: "Deploy is done & <@U5> can check.\nSee <https://example.com/|notes>."}},
		"markdown":       {postedLink, Message{User: expectUser, Text: "*Deploy* is done & <@U5> can check.\nSee <https://example.com|notes>."}},
		"wrong author":   {postedLink, Message{User: "UOTHER", Text: approvedText}},
		"other channel":  {"https://x.slack.com/archives/C9/p1700000100000001", Message{User: expectUser, Text: approvedText}},
	} {
		t.Run(name, func(t *testing.T) {
			f := newOutboxFixture(t, PostModeHandoff)
			r := approveHandoff(t, f)
			f.rec.notes = nil
			f.reader.msg = tc.msg
			got, err := f.ob.Posted(context.Background(), r.ID, tc.link)
			if err != nil {
				t.Fatal(err)
			}
			if got.State != OutboxMismatch || got.WatchID == 0 {
				t.Fatalf("record = %+v", got)
			}
			w, err := f.store.FindWatch(PostRef{WatchID: got.WatchID})
			if err != nil {
				t.Fatal(err)
			}
			if w.Owner != "agent-1" || w.Flag == "" {
				t.Errorf("watch = %+v, want owned and flagged", w)
			}
			if ev := f.events(); strings.Join(ev, ",") != EventOutboxMismatch+">agent-1,"+EventOutboxMismatch+">" {
				t.Fatalf("events = %v", ev)
			}
			for _, n := range f.rec.notes {
				if !strings.Contains(n.Message, "Deploy is done & <@U5> can check.") || !strings.Contains(n.Message, tc.msg.Text[:10]) {
					t.Errorf("alert lacks both texts: %q", n.Message)
				}
			}
		})
	}
}

func TestRecheck(t *testing.T) {
	mismatch := func(t *testing.T) (*outboxFixture, OutboxRecord) {
		t.Helper()
		f := newOutboxFixture(t, PostModeHandoff)
		r, err := f.ob.Submit(context.Background(), Draft{
			Channel: "C1", DestName: "#general", Text: "Ship it",
			Owner: "agent-1", Purpose: "announce deploy",
		})
		if err != nil {
			t.Fatal(err)
		}
		f.dec.answer(r.DecideID, uiPost)
		f.poll(t)
		f.reader.msg = Message{User: expectUser, Text: "Ship it *Sent using* <@U1>"}
		got, err := f.ob.Posted(context.Background(), r.ID, postedLink)
		if err != nil {
			t.Fatal(err)
		}
		if got.State != OutboxMismatch {
			t.Fatalf("record = %+v, want mismatch", got)
		}
		return f, got
	}

	t.Run("fixed footer pattern", func(t *testing.T) {
		f, r := mismatch(t)
		posts := len(f.writer.posts)
		f.ob.Config.FooterPattern = Regexp{regexp.MustCompile(`Sent using\*? <@U1`)}
		got, err := f.ob.Recheck(context.Background(), r.ID)
		if err != nil {
			t.Fatal(err)
		}
		if got.State != OutboxPosted || got.Reason != "" {
			t.Fatalf("record = %+v, want posted", got)
		}
		if len(f.writer.posts) != posts {
			t.Fatalf("posts = %d, want %d", len(f.writer.posts), posts)
		}
		if len(f.dec.replies) != 1 || !strings.Contains(f.dec.replies[0], "rechecked") {
			t.Fatalf("replies = %q", f.dec.replies)
		}
		w, err := f.store.FindWatch(PostRef{WatchID: got.WatchID})
		if err != nil {
			t.Fatal(err)
		}
		if w.Flag != "" {
			t.Errorf("watch flag = %q", w.Flag)
		}
	})

	t.Run("still different", func(t *testing.T) {
		f, r := mismatch(t)
		f.reader.msg = Message{User: expectUser, Text: "Ship that"}
		_, err := f.ob.Recheck(context.Background(), r.ID)
		if err == nil || !strings.Contains(err.Error(), "the posted text differs from the approved text") {
			t.Fatalf("recheck = %v", err)
		}
		if got := f.get(t, r.ID); got.State != OutboxMismatch {
			t.Fatalf("record = %+v, want mismatch", got)
		}
	})

	t.Run("refuses other states", func(t *testing.T) {
		f := newOutboxFixture(t, PostModeHandoff)
		pending := f.submit(t)
		if _, err := f.ob.Recheck(context.Background(), pending.ID); err == nil || !strings.Contains(err.Error(), "not mismatch") {
			t.Fatalf("pending recheck = %v", err)
		}
		f.reader.msg = Message{User: expectUser, Text: approvedText}
		posted, err := f.ob.Posted(context.Background(), approveHandoff(t, f).ID, postedLink)
		if err != nil {
			t.Fatal(err)
		}
		if _, err := f.ob.Recheck(context.Background(), posted.ID); err == nil || !strings.Contains(err.Error(), "not mismatch") {
			t.Fatalf("posted recheck = %v", err)
		}
	})
}

func TestHandoffPostedRequiresAwaiting(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	if _, err := f.ob.Posted(context.Background(), r.ID, postedLink); err == nil {
		t.Error("posted accepted for a pending record")
	}
	if f.reader.calls != 0 {
		t.Error("read back a pending record")
	}
}

func TestManualPostedPendingWithUIApproval(t *testing.T) {
	for _, mode := range []string{PostModeHandoff, PostModeDirect} {
		t.Run(mode, func(t *testing.T) {
			f := newOutboxFixture(t, mode)
			r := f.submit(t)
			f.dec.answer(r.DecideID, uiPost)
			f.reader.msg = Message{User: expectUser, Text: approvedText}
			got, err := f.ob.PostedManual(context.Background(), r.ID, postedLink)
			if err != nil {
				t.Fatal(err)
			}
			if got.State != OutboxPosted || !got.Manual || got.Permalink != postedLink {
				t.Fatalf("record = %+v", got)
			}
			w, err := f.store.FindWatch(PostRef{WatchID: got.WatchID})
			if err != nil {
				t.Fatal(err)
			}
			if w.Owner != "agent-1" || w.Source != SourceOutbox || w.Flag != "" {
				t.Errorf("watch = %+v", w)
			}
			if f.dec.items[r.DecideID].State != "done" || f.dec.outcomes[r.DecideID] != postedLink {
				t.Errorf("decide = %+v outcome %q", f.dec.items[r.DecideID], f.dec.outcomes[r.DecideID])
			}
			f.poll(t)
			if len(f.writer.posts) != 0 || len(f.rec.notes) != 0 {
				t.Errorf("posts %v, events %v", f.writer.posts, f.events())
			}
		})
	}
}

func TestManualPostedAfterLaterReply(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	f.dec.answer(r.DecideID, uiPost)
	f.dec.reply(r.DecideID, "agent-1", "posting by hand")
	f.reader.msg = Message{User: expectUser, Text: approvedText}
	got, err := f.ob.PostedManual(context.Background(), r.ID, postedLink)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != OutboxPosted || !got.Manual {
		t.Fatalf("record = %+v", got)
	}
}

func TestManualPostedRefusesChangedHash(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	if _, err := f.store.DB().Exec(`UPDATE outbox SET text = 'changed' WHERE id = ?`, r.ID); err != nil {
		t.Fatal(err)
	}
	f.dec.answer(r.DecideID, uiPost)
	f.reader.msg = Message{User: expectUser, Text: "changed"}
	_, err := f.ob.PostedManual(context.Background(), r.ID, postedLink)
	if err == nil || !strings.Contains(err.Error(), "no longer matches its hash") {
		t.Fatalf("err = %v, want a hash refusal", err)
	}
	if got := f.get(t, r.ID); got.State != OutboxPending || f.reader.calls != 0 {
		t.Errorf("record = %+v, reads %d", got, f.reader.calls)
	}
}

func TestManualPostedAwaitingHandoff(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := approveHandoff(t, f)
	f.reader.msg = Message{User: expectUser, Text: approvedText}
	got, err := f.ob.PostedManual(context.Background(), r.ID, postedLink)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != OutboxPosted || !got.Manual {
		t.Fatalf("record = %+v", got)
	}
}

func TestManualPostedRequiresUIApproval(t *testing.T) {
	for name, a := range map[string]*DecideAnswer{
		"unanswered": nil,
		"cli post":   {Value: "post", Key: "post", AnsweredBy: "human", Via: "cli"},
		"agent post": {Value: "post", Key: "post", AnsweredBy: "agent", Via: "ui"},
		"no via":     {Value: "post", Key: "post", AnsweredBy: "human"},
		"drop":       {Value: "drop", Key: "drop", AnsweredBy: "human", Via: "ui"},
		"free text":  {Value: "shorter please", AnsweredBy: "human", Via: "ui"},
	} {
		t.Run(name, func(t *testing.T) {
			f := newOutboxFixture(t, PostModeHandoff)
			r := f.submit(t)
			if a != nil {
				f.dec.answer(r.DecideID, *a)
			}
			f.reader.msg = Message{User: expectUser, Text: approvedText}
			_, err := f.ob.PostedManual(context.Background(), r.ID, postedLink)
			if want := fmt.Sprintf("no ui approval found for %d: ", r.ID); err == nil || !strings.HasPrefix(err.Error(), want) {
				t.Fatalf("err = %v, want %q", err, want)
			}
			if got := f.get(t, r.ID); got.State != OutboxPending || got.Manual || got.WatchID != 0 {
				t.Errorf("record = %+v", got)
			}
			if f.reader.calls != 0 {
				t.Error("read back an unapproved record")
			}
		})
	}
}

func TestManualPostedRefusesOtherStates(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	if _, err := f.ob.Cancel(context.Background(), r.ID, ""); err != nil {
		t.Fatal(err)
	}
	if _, err := f.ob.PostedManual(context.Background(), r.ID, postedLink); err == nil {
		t.Error("manual posted accepted a cancelled record")
	}
}

func TestManualPostedMismatchAlerts(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	f.dec.answer(r.DecideID, uiPost)
	f.reader.msg = Message{User: expectUser, Text: "*Deploy* is done & <@U5> can check."}
	got, err := f.ob.PostedManual(context.Background(), r.ID, postedLink)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != OutboxMismatch || !got.Manual || got.WatchID == 0 {
		t.Fatalf("record = %+v", got)
	}
	if ev := f.events(); strings.Join(ev, ",") != EventOutboxMismatch+">agent-1,"+EventOutboxMismatch+">" {
		t.Fatalf("events = %v", ev)
	}
}

func TestHandoffPostedIsNotManual(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := approveHandoff(t, f)
	f.reader.msg = Message{User: expectUser, Text: approvedText}
	got, err := f.ob.Posted(context.Background(), r.ID, postedLink)
	if err != nil {
		t.Fatal(err)
	}
	if got.Manual {
		t.Errorf("handoff post recorded as manual: %+v", got)
	}
}

func TestNormalizePosted(t *testing.T) {
	re := regexp.MustCompile(`_sent by an agent_`)
	for _, tc := range []struct{ posted, stored string }{
		{"a &amp; b &lt;c&gt;", "a & b <c>"},
		{"a &amp;lt;", "a &lt;"},
		{"line  \nnext\t\n\n", "line\nnext"},
		{"text\n\n_sent by an agent_", "text"},
		{"Ship it", "Ship it"},
	} {
		if got, want := NormalizePosted(tc.posted, re), NormalizeStored(tc.stored); got != want {
			t.Errorf("NormalizePosted(%q) = %q, want %q", tc.posted, got, want)
		}
	}
	inline := regexp.MustCompile(`Sent using\*? <@U1`)
	if got := NormalizePosted("Ship it *Sent using* <@U1>", inline); got != "Ship it" {
		t.Errorf("inline footer = %q, want the text alone", got)
	}
}

func TestInlineFooterComparePasses(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	f.ob.Config.FooterPattern = Regexp{regexp.MustCompile(`Sent using\*? <@U1`)}
	f.ob.Config.Post.Footer = " *Sent using* <@U1>"
	r := approveHandoff(t, f)
	f.rec.notes = nil
	f.reader.msg = Message{User: expectUser, Text: approvedText + " *Sent using* <@U1>"}
	got, err := f.ob.Posted(context.Background(), r.ID, postedLink)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != OutboxPosted {
		t.Fatalf("record = %+v, want posted", got)
	}
}

func TestHandoffReadBack(t *testing.T) {
	cases := []struct {
		name   string
		stored string
		posted string
		match  bool
	}{
		{
			name:   "connector link and dropped blank line",
			stored: "*Heading*\n\nSee https://example.com/a/very/long/path",
			posted: "*Heading*\nSee <https://example.com/a/very/long/path|example.com/a/very/lo…>",
			match:  true,
		},
		{
			name:   "label is the url",
			stored: "See https://example.com/a",
			posted: "See <https://example.com/a|https://example.com/a>",
			match:  true,
		},
		{
			name:   "label is the url without its scheme",
			stored: "See https://example.com/a",
			posted: "See <https://example.com/a|example.com/a>",
			match:  true,
		},
		{
			name:   "truncated url prefix",
			stored: "See https://example.com/a/very/long/path",
			posted: "See <https://example.com/a/very/long/path|https://example.com/a/very/lo…>",
			match:  true,
		},
		{
			name:   "unrelated label",
			stored: "See https://example.com/a",
			posted: "See <https://example.com/a|notes>",
			match:  false,
		},
		{
			name:   "bold versus italic",
			stored: "*Heading*",
			posted: "_Heading_",
			match:  false,
		},
		{
			name:   "changed word",
			stored: "Ship it",
			posted: "Ship that",
			match:  false,
		},
	}
	if len(handoffRules) != 2 || handoffRules[0].name != "link label" || handoffRules[1].name != "blank lines" {
		t.Fatalf("handoff rules = %+v", handoffRules)
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			match := sameText(NormalizePosted(tc.posted, nil), NormalizeStored(tc.stored), true)
			if match != tc.match {
				t.Fatalf("handoff match = %v, want %v", match, tc.match)
			}
		})
	}
	dropped := sameText(
		NormalizePosted("*Heading*\nSee https://example.com/a", nil),
		NormalizeStored("*Heading*\n\nSee https://example.com/a"),
		false,
	)
	if dropped {
		t.Fatal("direct mode treated a dropped blank line as a match")
	}
}

func TestHandoffConnectorLinkComparePasses(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	text := "*Heading*\n\nSee https://example.com/a/very/long/path"
	r, err := f.ob.Submit(context.Background(), Draft{
		Channel: "C1", DestName: "#general", Text: text,
		Owner: "agent-1", Purpose: "announce deploy",
	})
	if err != nil {
		t.Fatal(err)
	}
	f.dec.answer(r.DecideID, uiPost)
	f.poll(t)
	f.rec.notes = nil
	f.reader.msg = Message{User: expectUser, Text: "*Heading*\nSee <https://example.com/a/very/long/path|example.com/a/very/lo…>"}
	got, err := f.ob.Posted(context.Background(), r.ID, postedLink)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != OutboxPosted {
		t.Fatalf("record = %+v, want posted", got)
	}
}

func TestFooterRuleLinksAwaitingHandoff(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := approveHandoff(t, f)
	f.rec.notes = nil
	proc := &Processor{
		Store: f.store, Notifier: f.rec, Config: f.ob.Config, Self: expectUser,
		Now: testNow, AfterFunc: func(time.Duration, func()) {},
	}
	m := Message{
		Channel: "C1", ChannelType: "channel", TS: "1700000100.000001", User: expectUser,
		Text: "Deploy is done &amp; <@U5> can check.\nSee <https://example.com|notes>.\n" + footerText,
	}
	if err := proc.HandleMessage(context.Background(), m); err != nil {
		t.Fatal(err)
	}
	got := f.get(t, r.ID)
	if got.PostTS != m.TS || got.WatchID == 0 || got.State != OutboxAwaitingHandoff {
		t.Fatalf("record = %+v", got)
	}
	w, err := f.store.FindWatch(PostRef{Channel: "C1", TS: m.TS})
	if err != nil {
		t.Fatal(err)
	}
	if w.State != StateOpen || w.Owner != "agent-1" || w.Source != SourceOutbox {
		t.Errorf("watch = %+v, want an open outbox watch", w)
	}
	if len(f.rec.notes) != 0 {
		t.Errorf("notified %v", f.events())
	}

	// outbox posted confirms the link.
	f.reader.msg = Message{User: expectUser, Text: m.Text}
	got, err = f.ob.Posted(context.Background(), r.ID, postedLink)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != OutboxPosted || got.WatchID != w.ID {
		t.Errorf("record = %+v, want posted on watch %d", got, w.ID)
	}
}

func TestFooterRuleLinkedThenDifferentMessageMismatch(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := approveHandoff(t, f)
	proc := &Processor{Store: f.store, Notifier: f.rec, Config: f.ob.Config, Self: expectUser, Now: testNow, AfterFunc: func(time.Duration, func()) {}}
	if err := proc.HandleMessage(context.Background(), Message{Channel: "C1", TS: "1700000100.000001", User: expectUser, Text: approvedText}); err != nil {
		t.Fatal(err)
	}
	f.reader.msg = Message{User: expectUser, Text: approvedText}
	got, err := f.ob.Posted(context.Background(), r.ID, "https://x.slack.com/archives/C1/p1700000100000002")
	if err != nil {
		t.Fatal(err)
	}
	if got.State != OutboxMismatch {
		t.Errorf("state = %s, want mismatch", got.State)
	}
}

func TestFooterRuleNonMatchingOwnPostStaysFooterWatch(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := approveHandoff(t, f)
	proc := &Processor{Store: f.store, Notifier: f.rec, Config: f.ob.Config, Self: expectUser, Now: testNow, AfterFunc: func(time.Duration, func()) {}}
	if err := proc.HandleMessage(context.Background(), Message{Channel: "C1", TS: "1700000100.000001", User: expectUser, Text: "other\n" + footerText}); err != nil {
		t.Fatal(err)
	}
	if got := f.get(t, r.ID); got.PostTS != "" {
		t.Errorf("linked a different text: %+v", got)
	}
	w, err := f.store.FindWatch(PostRef{Channel: "C1", TS: "1700000100.000001"})
	if err != nil || w.State != StateUnowned {
		t.Errorf("watch = %+v, %v, want unowned footer watch", w, err)
	}
}

func TestDirectPinMismatchDisables(t *testing.T) {
	f := newOutboxFixture(t, PostModeDirect)
	f.ob.Config.Post.TokenPin = Pin("salt", "xoxp-other")
	r := f.submit(t)
	f.dec.answer(r.DecideID, uiPost)
	f.poll(t)
	f.poll(t)
	if len(f.writer.posts) != 0 {
		t.Fatalf("posted with a bad pin: %+v", f.writer.posts)
	}
	if f.writer.auths != 0 {
		t.Errorf("auth.test called %d times before the pin check", f.writer.auths)
	}
	if got := f.get(t, r.ID).State; got != OutboxApproved {
		t.Errorf("state = %s, want approved", got)
	}
	reason, err := f.store.PostingDisabled()
	if err != nil || reason == "" {
		t.Fatalf("posting disabled = %q, %v", reason, err)
	}
	if ev := f.events(); strings.Join(ev, ",") != EventPostingDisabled+">" {
		t.Errorf("events = %v, want one fallback alert", ev)
	}

	// Re-pinning re-enables posting and the approved record posts.
	f.ob.Config.Post.TokenPin = Pin("salt", writeToken)
	if err := f.store.SetPostingDisabled(""); err != nil {
		t.Fatal(err)
	}
	f.poll(t)
	if len(f.writer.posts) != 1 || f.get(t, r.ID).State != OutboxPosted {
		t.Errorf("posts = %+v after re-pin", f.writer.posts)
	}
}

func TestDirectWrongUserDisables(t *testing.T) {
	f := newOutboxFixture(t, PostModeDirect)
	f.writer.user = "UOTHER"
	r := f.submit(t)
	f.dec.answer(r.DecideID, uiPost)
	f.poll(t)
	if len(f.writer.posts) != 0 {
		t.Fatalf("posted as the wrong user: %+v", f.writer.posts)
	}
	if reason, _ := f.store.PostingDisabled(); !strings.Contains(reason, "UOTHER") {
		t.Errorf("disabled reason = %q", reason)
	}
	if ev := f.events(); strings.Join(ev, ",") != EventPostingDisabled+">" {
		t.Errorf("events = %v", ev)
	}
}

func TestDirectAuthTestOncePerToken(t *testing.T) {
	f := newOutboxFixture(t, PostModeDirect)
	for range 3 {
		r := f.submit(t)
		f.dec.answer(r.DecideID, uiPost)
		f.poll(t)
	}
	if len(f.writer.posts) != 3 || f.writer.auths != 1 {
		t.Errorf("posts %d auths %d, want 3 posts and one auth.test", len(f.writer.posts), f.writer.auths)
	}
}

func TestDirectFooterPatternMustMatchFooter(t *testing.T) {
	f := newOutboxFixture(t, PostModeDirect)
	f.ob.Config.FooterPattern = Regexp{regexp.MustCompile(`via bot`)}
	_, err := f.ob.Submit(context.Background(), Draft{Channel: "C1", Text: "hi", Owner: "a", Purpose: "p"})
	if err == nil || !strings.Contains(err.Error(), "footerPattern") {
		t.Fatalf("submit err = %v, want footerPattern refusal", err)
	}
	if len(f.dec.items) != 0 {
		t.Error("filed a decide item")
	}
}

func TestStaleCachedChannelIsResolvedAgain(t *testing.T) {
	f := newOutboxFixture(t, PostModeDirect)
	if err := f.store.PutChannel("general", "COLD", f.ob.Now()); err != nil {
		t.Fatal(err)
	}
	var listed int
	f.ob.Channels = &ChannelLookup{
		Store: f.store, Now: f.ob.Now, TTL: DefaultChannelCacheTTL, MaxWait: DefaultChannelLookupWait,
		List: listFunc(func(context.Context, string) (ChannelPage, error) {
			listed++
			return ChannelPage{Channels: []NamedChannel{{Name: "general", ID: "CNEW"}}}, nil
		}),
	}
	f.writer.errFor = map[string]error{"COLD": errors.New("channel_not_found")}
	r, err := f.ob.Submit(context.Background(), Draft{
		Channel: "COLD", DestName: "#general", Text: approvedText,
		Owner: "agent-1", Purpose: "announce deploy",
	})
	if err != nil {
		t.Fatal(err)
	}
	f.dec.answer(r.DecideID, uiPost)
	f.poll(t)
	if listed != 1 {
		t.Errorf("lookups = %d, want one resolve after channel_not_found", listed)
	}
	if len(f.writer.posts) != 1 || f.writer.posts[0].channel != "CNEW" {
		t.Fatalf("posts = %+v", f.writer.posts)
	}
	if got := f.get(t, r.ID); got.State != OutboxPosted || got.Channel != "CNEW" {
		t.Errorf("record = %+v", got)
	}
	id, _, ok, err := f.store.ChannelByName("general")
	if err != nil || !ok || id != "CNEW" {
		t.Errorf("cache = %q, %v, %v", id, ok, err)
	}
	if name, ok, err := f.store.ChannelName("COLD"); err != nil || ok {
		t.Errorf("stale id still cached as %q, %v, %v", name, ok, err)
	}
}

func TestStaleCachedChannelReadResolvesAgain(t *testing.T) {
	f, r := failedDirect(t)
	if err := f.store.PutChannel("general", r.Channel, f.ob.Now()); err != nil {
		t.Fatal(err)
	}
	f.reader.recentErr = errors.New("channel_not_found")
	f.reader.recentFail = 1
	f.reader.msg = Message{User: expectUser, Text: postedBody()}
	var listed int
	f.ob.Channels = &ChannelLookup{
		Store: f.store, Now: f.ob.Now, TTL: DefaultChannelCacheTTL, MaxWait: DefaultChannelLookupWait,
		List: listFunc(func(context.Context, string) (ChannelPage, error) {
			listed++
			return ChannelPage{Channels: []NamedChannel{{Name: "general", ID: "C2"}}}, nil
		}),
	}
	got, err := f.ob.Retry(context.Background(), r.ID)
	if err != nil {
		t.Fatal(err)
	}
	if listed != 1 {
		t.Errorf("lookups = %d, want one", listed)
	}
	if got.State != OutboxPosted || got.Channel != "C2" || len(f.writer.posts) != 1 || f.writer.posts[0].channel != "C2" {
		t.Fatalf("record %+v posts %+v", got, f.writer.posts)
	}
	id, _, ok, err := f.store.ChannelByName("general")
	if err != nil || !ok || id != "C2" {
		t.Errorf("cache = %q, %v, %v", id, ok, err)
	}
}

func TestDirectPostErrorFails(t *testing.T) {
	f := newOutboxFixture(t, PostModeDirect)
	f.writer.err = errors.New("channel_not_found")
	r := f.submit(t)
	f.dec.answer(r.DecideID, uiPost)
	f.poll(t)
	f.poll(t)
	if got := f.get(t, r.ID); got.State != OutboxFailed || !strings.Contains(got.Reason, "channel_not_found") {
		t.Errorf("record = %+v", got)
	}
	if ev := f.events(); strings.Join(ev, ",") != EventOutboxFailed+">agent-1,"+EventOutboxFailed+">" {
		t.Errorf("events = %v", ev)
	}
}

func TestCheckPost(t *testing.T) {
	footer := Regexp{regexp.MustCompile(`agent`)}
	for _, tc := range []struct {
		name string
		cfg  Config
		want string
	}{
		{"off", Config{}, "post.mode"},
		{"bad mode", Config{Post: PostConfig{Mode: "auto"}}, "post.mode"},
		{"handoff no agent", Config{Post: PostConfig{Mode: "handoff", ExpectUser: "U1"}}, "post.agent"},
		{"handoff no user", Config{Post: PostConfig{Mode: "handoff", Agent: "a"}}, "post.expectUser"},
		{"handoff ok", Config{Post: PostConfig{Mode: "handoff", Agent: "a", ExpectUser: "U1"}}, ""},
		{"direct no token", Config{FooterPattern: footer, Post: PostConfig{Mode: "direct", ExpectUser: "U1", Footer: "by agent"}}, "post.writeTokenFile"},
		{"direct no footer", Config{FooterPattern: footer, Post: PostConfig{Mode: "direct", ExpectUser: "U1", WriteTokenFile: "f"}}, "post.footer"},
		{"direct footer mismatch", Config{FooterPattern: footer, Post: PostConfig{Mode: "direct", ExpectUser: "U1", WriteTokenFile: "f", Footer: "by me"}}, "footerPattern"},
		{"direct no pattern", Config{Post: PostConfig{Mode: "direct", ExpectUser: "U1", WriteTokenFile: "f", Footer: "by agent"}}, "footerPattern"},
		{"direct footer partly removed", Config{FooterPattern: footer, Post: PostConfig{Mode: "direct", ExpectUser: "U1", WriteTokenFile: "f", Footer: "by agent"}}, "fully remove"},
		{"handoff footer partly removed", Config{FooterPattern: footer, Post: PostConfig{Mode: "handoff", Agent: "a", ExpectUser: "U1", Footer: "by agent"}}, "fully remove"},
		{"direct ok", Config{FooterPattern: footer, Post: PostConfig{Mode: "direct", ExpectUser: "U1", WriteTokenFile: "f", Footer: "agent"}}, ""},
	} {
		err := tc.cfg.CheckPost()
		if tc.want == "" {
			if err != nil {
				t.Errorf("%s: %v", tc.name, err)
			}
			continue
		}
		if err == nil || !strings.Contains(err.Error(), tc.want) {
			t.Errorf("%s: err = %v, want it to mention %q", tc.name, err, tc.want)
		}
	}
}

func TestOutboxCounts(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	approveHandoff(t, f)
	f.submit(t)
	c, err := f.store.OutboxCounts()
	if err != nil {
		t.Fatal(err)
	}
	if c.Pending != 1 || c.AwaitingHandoff != 1 || c.NeedsRedraft != 0 {
		t.Errorf("counts = %+v", c)
	}
	recs, err := f.store.ListOutbox(OutboxPending)
	if err != nil || len(recs) != 1 {
		t.Errorf("ListOutbox(pending) = %v, %v", recs, err)
	}
}

func TestDecideCLI(t *testing.T) {
	var calls [][]string
	out := map[string]string{
		"ask": `{"id":"D-7","state":"open"}`,
		"show": `{"id":"D-7","state":"open","answer":{"value":"post","key":"post","answeredBy":"human","via":"ui"},` +
			`"messages":[{"author":"slackwatch","text":"Post?"},{"author":"human","text":"post: Post as written","optionKey":"post","via":"ui"},` +
			`{"author":"slackwatch","text":"ok","by":"agent-1"}]}`,
		"done": `{"id":"D-7","state":"done"}`,
	}
	d := DecideCLI{Run: func(_ context.Context, args ...string) ([]byte, error) {
		calls = append(calls, args)
		return []byte(out[args[0]]), nil
	}}
	id, err := d.Ask(context.Background(), "slackwatch", "Post?")
	if err != nil || id != "D-7" {
		t.Fatalf("Ask = %q, %v", id, err)
	}
	it, err := d.Show(context.Background(), "D-7")
	wantMsgs := []DecideMessage{
		{Author: "slackwatch", Text: "Post?"},
		{Author: "human", Text: "post: Post as written", OptionKey: "post", Via: "ui"},
		{Author: "slackwatch", Text: "ok", By: "agent-1"},
	}
	if err != nil || it.State != "open" || fmt.Sprint(it.Messages) != fmt.Sprint(wantMsgs) {
		t.Fatalf("Show = %+v, %v", it, err)
	}
	if err := d.Done(context.Background(), "D-7", "posted"); err != nil {
		t.Fatal(err)
	}
	want := [][]string{
		{"ask", "--owner", "slackwatch", "--owner-replies-only", "--kind", "relay", "--option", "post:Post as written", "--option", "drop:Don't post", "--rec", "post", "--free-text", "--json", "Post?"},
		{"show", "--json", "D-7"},
		{"done", "--json", "D-7", "posted"},
	}
	if fmt.Sprint(calls) != fmt.Sprint(want) {
		t.Errorf("calls = %q\nwant %q", calls, want)
	}
}

// failedDirect is a direct-mode record whose post failed, with the
// writer ready for another attempt.
func failedDirect(t *testing.T) (*outboxFixture, OutboxRecord) {
	t.Helper()
	f := newOutboxFixture(t, PostModeDirect)
	f.writer.err = errors.New("not_in_channel")
	r := f.submit(t)
	f.dec.answer(r.DecideID, uiPost)
	f.poll(t)
	f.writer.err = nil
	if got := f.get(t, r.ID); got.State != OutboxFailed {
		t.Fatalf("state = %s, want failed", got.State)
	}
	return f, f.get(t, r.ID)
}

func TestRetryPostsOnce(t *testing.T) {
	f, r := failedDirect(t)
	f.reader.msg = Message{User: expectUser, Text: postedBody()}
	got, err := f.ob.Retry(context.Background(), r.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != OutboxPosted || len(f.writer.posts) != 1 || f.reader.calls != 1 {
		t.Fatalf("state %s posts %d read-backs %d", got.State, len(f.writer.posts), f.reader.calls)
	}
	if f.writer.posts[0].text != postedBody() || f.writer.posts[0].channel != "C1" {
		t.Errorf("post = %+v", f.writer.posts[0])
	}
	if _, err := f.ob.Retry(context.Background(), r.ID); err == nil || !strings.Contains(err.Error(), OutboxPosted) {
		t.Fatalf("second retry err = %v", err)
	}
	if len(f.writer.posts) != 1 || f.get(t, r.ID).State != OutboxPosted {
		t.Errorf("second retry posted again: %d posts, state %s", len(f.writer.posts), f.get(t, r.ID).State)
	}
}

func TestRetryRefusesChangedHash(t *testing.T) {
	f, r := failedDirect(t)
	if _, err := f.store.DB().Exec(`UPDATE outbox SET text = ? WHERE id = ?`, "tampered", r.ID); err != nil {
		t.Fatal(err)
	}
	_, err := f.ob.Retry(context.Background(), r.ID)
	if err == nil || !strings.Contains(err.Error(), "hash") {
		t.Fatalf("err = %v", err)
	}
	if got := f.get(t, r.ID); got.State != OutboxFailed || len(f.writer.posts) != 0 {
		t.Errorf("state %s posts %d", got.State, len(f.writer.posts))
	}
}

func TestRetryRefusesNonUIAnswer(t *testing.T) {
	f, r := failedDirect(t)
	item := f.dec.items[r.DecideID]
	for i := range item.Messages {
		if item.Messages[i].OptionKey == decideKeyPost {
			item.Messages[i].Via = "cli"
		}
	}
	_, err := f.ob.Retry(context.Background(), r.ID)
	if err == nil || !strings.Contains(err.Error(), "via ui") {
		t.Fatalf("err = %v", err)
	}
	if f.get(t, r.ID).State != OutboxFailed || len(f.writer.posts) != 0 {
		t.Errorf("state %s posts %d", f.get(t, r.ID).State, len(f.writer.posts))
	}
}

func TestRetryRefusesOtherStates(t *testing.T) {
	f := newOutboxFixture(t, PostModeDirect)
	r := f.submit(t)
	_, err := f.ob.Retry(context.Background(), r.ID)
	if err == nil || !strings.Contains(err.Error(), OutboxPending) || !strings.Contains(err.Error(), OutboxFailed) {
		t.Fatalf("err = %v", err)
	}
	if f.get(t, r.ID).State != OutboxPending || len(f.writer.posts) != 0 {
		t.Errorf("pending record changed")
	}
}

func TestRetryRegistersExistingPost(t *testing.T) {
	f, r := failedDirect(t)
	const matchTS = "1700000099.000100"
	f.reader.recent = []Message{
		{User: "UOTHER", TS: "1700000098.000100", Text: postedBody()},
		{User: expectUser, TS: "1700000097.000100", Text: "a different message"},
		{User: expectUser, TS: matchTS, Text: postedBody()},
	}
	got, err := f.ob.Retry(context.Background(), r.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != OutboxPosted || got.PostTS != matchTS || len(f.writer.posts) != 0 || f.reader.calls != 0 {
		t.Fatalf("state %s ts %s posts %d read-backs %d", got.State, got.PostTS, len(f.writer.posts), f.reader.calls)
	}
	if f.reader.oldest != unixTS(testNow().Add(-retryLookback)) {
		t.Errorf("oldest = %q", f.reader.oldest)
	}
}

func TestRetrySearchErrorStaysFailed(t *testing.T) {
	f, r := failedDirect(t)
	f.reader.recentErr = errors.New("slack down")
	_, err := f.ob.Retry(context.Background(), r.ID)
	if err == nil || !strings.Contains(err.Error(), "slack down") {
		t.Fatalf("err = %v", err)
	}
	if f.get(t, r.ID).State != OutboxFailed || len(f.writer.posts) != 0 {
		t.Errorf("state %s posts %d", f.get(t, r.ID).State, len(f.writer.posts))
	}
}

func TestRetryRefusesChangedDestination(t *testing.T) {
	f, r := failedDirect(t)
	if _, err := f.store.DB().Exec(`UPDATE outbox SET channel = ? WHERE id = ?`, "C9", r.ID); err != nil {
		t.Fatal(err)
	}
	_, err := f.ob.Retry(context.Background(), r.ID)
	if err == nil || !strings.Contains(err.Error(), "destination") {
		t.Fatalf("err = %v", err)
	}
	if got := f.get(t, r.ID); got.State != OutboxFailed || got.Channel != "C9" || len(f.writer.posts) != 0 {
		t.Errorf("state %s channel %s posts %d", got.State, got.Channel, len(f.writer.posts))
	}
}

func TestRetryRefusesIdentityMismatch(t *testing.T) {
	f, r := failedDirect(t)
	f.ob.authed = ""
	f.writer.user = "UOTHER"
	_, err := f.ob.Retry(context.Background(), r.ID)
	if err == nil || !strings.Contains(err.Error(), "UOTHER") {
		t.Fatalf("err = %v", err)
	}
	if f.get(t, r.ID).State != OutboxFailed || len(f.writer.posts) != 0 {
		t.Errorf("state %s posts %d", f.get(t, r.ID).State, len(f.writer.posts))
	}
}

func TestRetryHandoffReturnsToAwaiting(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	f.dec.answer(r.DecideID, uiPost)
	f.poll(t)
	if f.get(t, r.ID).State != OutboxAwaitingHandoff {
		t.Fatalf("state = %s", f.get(t, r.ID).State)
	}
	if _, err := f.store.DB().Exec(`UPDATE outbox SET state = ?, reason = ? WHERE id = ?`, OutboxFailed, "chat.postMessage: not_in_channel", r.ID); err != nil {
		t.Fatal(err)
	}
	got, err := f.ob.Retry(context.Background(), r.ID)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != OutboxAwaitingHandoff || got.Reason != "" || len(f.writer.posts) != 0 {
		t.Fatalf("state %s reason %q posts %d", got.State, got.Reason, len(f.writer.posts))
	}
	ready := 0
	for _, n := range f.rec.notes {
		if n.Event == EventOutboxReady && n.Owner == "poster" && n.Message == fmt.Sprintf("approved post ready: %d", r.ID) {
			ready++
		}
	}
	if ready != 2 {
		t.Errorf("ready notices = %d, want 2", ready)
	}
}

func TestNotInChannelNotice(t *testing.T) {
	f := newOutboxFixture(t, PostModeDirect)
	f.writer.err = errors.New("not_in_channel")
	r := f.submit(t)
	f.dec.answer(r.DecideID, uiPost)
	f.poll(t)
	want := fmt.Sprintf("posting account is not a member of %s; join it, then: slackwatch outbox retry %d", r.Channel, r.ID)
	var notified bool
	for _, n := range f.rec.notes {
		if n.Event == EventOutboxFailed && strings.Contains(n.Message, want) {
			notified = true
		}
	}
	if !notified {
		t.Errorf("notify missing %q", want)
	}
	if !strings.Contains(strings.Join(f.dec.replies, "\n"), want) {
		t.Errorf("replies = %v", f.dec.replies)
	}
	if got := f.get(t, r.ID); got.State != OutboxFailed || !strings.Contains(got.Reason, "chat.postMessage: not_in_channel") {
		t.Errorf("record = %+v", got)
	}
}

func TestPostFailureNotice(t *testing.T) {
	r := OutboxRecord{ID: 7, Channel: "C1"}
	for _, tc := range []struct{ reason, want string }{
		{"chat.postMessage: not_in_channel", "posting account is not a member of C1; join it, then: slackwatch outbox retry 7"},
		{"chat.postMessage: channel_not_found", "channel C1 was not found; check the destination, then: slackwatch outbox retry 7"},
		{"chat.postMessage: is_archived", "channel C1 is archived; unarchive it, then: slackwatch outbox retry 7"},
		{"chat.postMessage: restricted_action", "posting is restricted in C1; fix that, then: slackwatch outbox retry 7"},
		{"chat.postMessage: fatal_error", "chat.postMessage: fatal_error"},
	} {
		if got := postFailureNotice(r, tc.reason); got != tc.want {
			t.Errorf("postFailureNotice(%q) = %q, want %q", tc.reason, got, tc.want)
		}
	}
}

func TestAskPassesOwnerOnlyWhenDecideSupportsIt(t *testing.T) {
	var calls [][]string
	d := DecideCLI{Run: func(_ context.Context, args ...string) ([]byte, error) {
		calls = append(calls, append([]string(nil), args...))
		if args[0] == "ask" && !slicesHas(args, "--owner-replies-only") {
			return nil, errors.New("missing flag")
		}
		return []byte(`{"id":"D-1","state":"open"}`), nil
	}}
	if id, err := d.Ask(context.Background(), "slackwatch", "Post?"); err != nil || id != "D-1" {
		t.Fatalf("Ask = %q, %v", id, err)
	}
	if len(calls) != 1 || !slicesHas(calls[0], "--owner-replies-only") {
		t.Fatalf("calls = %q", calls)
	}
}

func TestAskOmitsOwnerOnlyWhenDecideRejectsIt(t *testing.T) {
	var calls [][]string
	d := DecideCLI{Run: func(_ context.Context, args ...string) ([]byte, error) {
		calls = append(calls, append([]string(nil), args...))
		if slicesHas(args, "--owner-replies-only") {
			return nil, errors.New("decide ask: exit status 2: flag provided but not defined: -owner-replies-only")
		}
		return []byte(`{"id":"D-2","state":"open"}`), nil
	}}
	if id, err := d.Ask(context.Background(), "slackwatch", "Post?"); err != nil || id != "D-2" {
		t.Fatalf("Ask = %q, %v", id, err)
	}
	if len(calls) != 2 || !slicesHas(calls[0], "--owner-replies-only") || slicesHas(calls[1], "--owner-replies-only") {
		t.Fatalf("calls = %q", calls)
	}
}

func slicesHas(args []string, want string) bool {
	for _, a := range args {
		if a == want {
			return true
		}
	}
	return false
}

func TestOutboxReattachesLostOptionsOnce(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	f.dec.items[r.DecideID].Options = nil
	f.dec.dropRestoredOptions = true
	var logs bytes.Buffer
	f.ob.Logger = slog.New(slog.NewTextHandler(&logs, nil))
	f.poll(t)
	f.poll(t)
	if len(f.dec.optionReplies) != 1 {
		t.Fatalf("reattach replies = %d, want 1: %q", len(f.dec.optionReplies), f.dec.optionReplies)
	}
	if f.dec.optionReplies[0] != f.dec.questions[r.DecideID] {
		t.Errorf("reattach text differs from the approval question\nreply %q\nquestion %q", f.dec.optionReplies[0], f.dec.questions[r.DecideID])
	}
	want := fmt.Sprintf("outbox %d: decide item lost its options; re-attached", r.ID)
	var notices int
	for _, n := range f.rec.notes {
		if n.Message == want && n.Owner == r.Owner {
			notices++
		}
	}
	if notices != 1 {
		t.Fatalf("notices = %d, events %v", notices, f.events())
	}
	if !strings.Contains(logs.String(), "re-attached") {
		t.Errorf("log = %q", logs.String())
	}
	if got := f.get(t, r.ID); got.State != OutboxPending {
		t.Fatalf("state = %s, want pending", got.State)
	}
}

func TestOutboxReattachesAgainAfterOptionsReturn(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	f.dec.items[r.DecideID].Options = nil
	f.poll(t)
	if len(f.dec.optionReplies) != 1 {
		t.Fatalf("first loss replies = %d", len(f.dec.optionReplies))
	}
	f.dec.items[r.DecideID].Options = nil
	f.poll(t)
	if len(f.dec.optionReplies) != 2 {
		t.Fatalf("second loss replies = %d, want 2", len(f.dec.optionReplies))
	}
	want := fmt.Sprintf("outbox %d: decide item lost its options; re-attached", r.ID)
	var notices int
	for _, n := range f.rec.notes {
		if n.Message == want {
			notices++
		}
	}
	if notices != 2 {
		t.Fatalf("notices = %d, want 2", notices)
	}
}

func TestOutboxLeavesOptionsAlone(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	f.dec.items[r.DecideID].Options = []DecideOption{
		{Key: decideKeyPost, Label: "Post as written"},
		{Key: decideKeyDrop, Label: "Don't post"},
	}
	var logs bytes.Buffer
	f.ob.Logger = slog.New(slog.NewTextHandler(&logs, nil))
	f.poll(t)
	if len(f.dec.optionReplies) != 0 || len(f.rec.notes) != 0 || logs.Len() != 0 {
		t.Fatalf("replies %q events %v log %q", f.dec.optionReplies, f.events(), logs.String())
	}
	if got := f.get(t, r.ID); got.State != OutboxPending {
		t.Fatalf("state = %s", got.State)
	}
}

func TestFreeTextPostStillNeedsRedraftWhenOptionsAreGone(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	f.dec.items[r.DecideID].Options = nil
	f.dec.answer(r.DecideID, DecideAnswer{Value: "Post", AnsweredBy: "human", Via: "ui"})
	f.poll(t)
	if got := f.get(t, r.ID); got.State != OutboxNeedsRedraft {
		t.Fatalf("state = %s, want needs-redraft", got.State)
	}
	if len(f.dec.optionReplies) != 0 {
		t.Fatalf("reattached an answered item: %q", f.dec.optionReplies)
	}
}

func TestManualPostedNeedsRedraftWithVerifiedPost(t *testing.T) {
	f := newOutboxFixture(t, PostModeHandoff)
	r := f.submit(t)
	f.dec.answer(r.DecideID, DecideAnswer{Value: "shorter", AnsweredBy: "human", Via: "ui"})
	f.poll(t)
	if got := f.get(t, r.ID); got.State != OutboxNeedsRedraft {
		t.Fatalf("state = %s, want needs-redraft", got.State)
	}
	f.reader.msg = Message{User: expectUser, Text: approvedText}
	if _, err := f.ob.PostedManual(context.Background(), r.ID, postedLink); err == nil {
		t.Fatal("accepted needs-redraft without a post pick")
	}
	if got := f.get(t, r.ID); got.State != OutboxNeedsRedraft || f.reader.calls != 0 {
		t.Fatalf("record = %+v reads %d", got, f.reader.calls)
	}
	f.dec.items[r.DecideID].Messages = append(f.dec.items[r.DecideID].Messages, DecideMessage{
		Author: "human", Text: "post", OptionKey: decideKeyPost, Via: decideViaUI,
	})
	got, err := f.ob.PostedManual(context.Background(), r.ID, postedLink)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != OutboxPosted || !got.Manual || got.Permalink != postedLink {
		t.Fatalf("record = %+v", got)
	}
}

func TestManualPostedNeedsRedraftRefusesUnverified(t *testing.T) {
	for _, tc := range []struct {
		name string
		msgs []DecideMessage
	}{
		{name: "free text post", msgs: []DecideMessage{{Author: "human", Text: "Post", Via: "ui"}}},
		{name: "cli post", msgs: []DecideMessage{{Author: "human", Text: "post", OptionKey: "post", Via: "cli"}}},
		{name: "agent post", msgs: []DecideMessage{{Author: "human", Text: "post", OptionKey: "post", Via: "ui", By: "agent"}}},
		{name: "stale pick before the draft text", msgs: nil},
	} {
		t.Run(tc.name, func(t *testing.T) {
			f := newOutboxFixture(t, PostModeHandoff)
			r := f.submit(t)
			f.dec.answer(r.DecideID, DecideAnswer{Value: "shorter", AnsweredBy: "human", Via: "ui"})
			f.poll(t)
			it := f.dec.items[r.DecideID]
			if tc.name == "stale pick before the draft text" {
				q := it.Messages[0].Text
				it.Messages = []DecideMessage{
					{Author: DefaultDecideOwner, Text: q},
					{Author: "human", Text: "post", OptionKey: decideKeyPost, Via: decideViaUI},
					{Author: DefaultDecideOwner, Text: q, By: "slackwatch"},
				}
			} else {
				it.Messages = append(it.Messages, tc.msgs...)
			}
			f.reader.msg = Message{User: expectUser, Text: approvedText}
			_, err := f.ob.PostedManual(context.Background(), r.ID, postedLink)
			if err == nil || !strings.Contains(err.Error(), "no ui approval found") {
				t.Fatalf("err = %v", err)
			}
			if got := f.get(t, r.ID); got.State != OutboxNeedsRedraft || f.reader.calls != 0 {
				t.Fatalf("record = %+v reads %d", got, f.reader.calls)
			}
		})
	}
}
