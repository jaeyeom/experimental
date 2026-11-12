package slackwatch

import (
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
	items     map[string]*DecideItem
	questions map[string]string
	owners    map[string]string
	outcomes  map[string]string
	askErr    error
	next      int
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
	d.items[id] = &DecideItem{ID: id, State: "open"}
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

func (d *fakeDecider) answer(id string, a DecideAnswer) {
	d.items[id].State = "answered"
	d.items[id].Answer = &a
}

// fakeWriter is a Slack write API.
type fakeWriter struct {
	user  string
	auths int
	posts []fakePost
	err   error
}

type fakePost struct{ token, channel, thread, text string }

func (w *fakeWriter) AuthTest(_ context.Context, _ string) (AuthInfo, error) {
	w.auths++
	return AuthInfo{UserID: w.user, URL: "https://x.slack.com/"}, nil
}

func (w *fakeWriter) PostMessage(_ context.Context, token, channel, thread, text string) (string, error) {
	if w.err != nil {
		return "", w.err
	}
	w.posts = append(w.posts, fakePost{token, channel, thread, text})
	return fmt.Sprintf("1700000100.%06d", len(w.posts)), nil
}

// fakeReader returns one canned message.
type fakeReader struct {
	msg   Message
	calls int
}

func (r *fakeReader) Message(_ context.Context, channel, ts, _ string) (Message, error) {
	r.calls++
	m := r.msg
	m.Channel, m.TS = channel, ts
	return m, nil
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
			"agent answer",
			DecideAnswer{Value: "post", Key: "post", AnsweredBy: "agent-1", Via: "ui"},
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

func TestNormalizePosted(t *testing.T) {
	re := regexp.MustCompile(`_sent by an agent_`)
	for _, tc := range []struct{ posted, stored string }{
		{"a &amp; b &lt;c&gt;", "a & b <c>"},
		{"a &amp;lt;", "a &lt;"},
		{"line  \nnext\t\n\n", "line\nnext"},
		{"text\n\n_sent by an agent_", "text"},
	} {
		if got, want := NormalizePosted(tc.posted, re), NormalizeStored(tc.stored); got != want {
			t.Errorf("NormalizePosted(%q) = %q, want %q", tc.posted, got, want)
		}
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
		{"direct ok", Config{FooterPattern: footer, Post: PostConfig{Mode: "direct", ExpectUser: "U1", WriteTokenFile: "f", Footer: "by agent"}}, ""},
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
		"ask":  `{"id":"D-7","state":"open"}`,
		"show": `{"id":"D-7","state":"answered","answer":{"value":"post","key":"post","answeredBy":"human","via":"ui"}}`,
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
	if err != nil || it.Answer == nil || it.Answer.Via != "ui" || it.Answer.Key != "post" || it.State != "answered" {
		t.Fatalf("Show = %+v, %v", it, err)
	}
	if err := d.Done(context.Background(), "D-7", "posted"); err != nil {
		t.Fatal(err)
	}
	want := [][]string{
		{"ask", "--owner", "slackwatch", "--kind", "relay", "--option", "post:Post as written", "--option", "drop:Don't post", "--rec", "post", "--free-text", "--json", "Post?"},
		{"show", "--json", "D-7"},
		{"done", "--json", "D-7", "posted"},
	}
	if fmt.Sprint(calls) != fmt.Sprint(want) {
		t.Errorf("calls = %q\nwant %q", calls, want)
	}
}
