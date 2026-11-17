package ghoutbox

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"
)

type askCall struct{ owner, pane, question string }

type fakeDecide struct {
	items map[string]*DecideItem
	asks  []askCall
	done  map[string]string
}

func newFakeDecide() *fakeDecide {
	return &fakeDecide{items: map[string]*DecideItem{}, done: map[string]string{}}
}

func (f *fakeDecide) Ask(_ context.Context, owner, pane, question string) (string, error) {
	f.asks = append(f.asks, askCall{owner, pane, question})
	id := fmt.Sprintf("D-%d", len(f.asks))
	f.items[id] = &DecideItem{ID: id, State: "open"}
	return id, nil
}

func (f *fakeDecide) Show(_ context.Context, id string) (DecideItem, error) {
	it, ok := f.items[id]
	if !ok {
		return DecideItem{}, errors.New("no item")
	}
	return *it, nil
}

func (f *fakeDecide) Done(_ context.Context, id, outcome string) error {
	f.done[id] = outcome
	f.items[id].State = "done"
	return nil
}

// pick records a message on item id. by marks a message an agent sent
// from the command line; author is "human" for the user.
func (f *fakeDecide) pick(id, author, key, text, via, by string) {
	it := f.items[id]
	it.Messages = append(it.Messages, DecideMessage{Author: author, Time: time.Now(), Text: text, OptionKey: key, Via: via, By: by})
	if author == "human" {
		it.State = "answered"
		it.Answer = &DecideAnswer{Value: text, Key: key, AnsweredBy: "human", Via: via}
		if key != "" {
			it.Answer.Value = key
		}
	}
}

func (f *fakeDecide) last() string { return fmt.Sprintf("D-%d", len(f.asks)) }

type postedReview struct {
	sha, event, body string
	lines            []LineComment
}

type fakeGH struct {
	head     string
	threads  []LineComment
	shas     []string
	reviews  []postedReview
	edits    []string
	oldText  string
	calls    int
	failPost error
}

func (g *fakeGH) HeadSHA(context.Context, string, int) (string, error) { return g.head, nil }

func (g *fakeGH) CommentBody(context.Context, string, string) (string, error) { return g.oldText, nil }

func (g *fakeGH) ReviewBody(context.Context, string, int, string) (string, error) {
	return g.oldText, nil
}

func (g *fakeGH) CreateThread(_ context.Context, _ string, _ int, sha string, c LineComment) (string, error) {
	g.calls++
	if g.failPost != nil {
		return "", g.failPost
	}
	g.threads = append(g.threads, c)
	g.shas = append(g.shas, sha)
	return fmt.Sprintf("https://gh/thread/%d", len(g.threads)), nil
}

func (g *fakeGH) CreateReview(_ context.Context, _ string, _ int, sha, event, body string, cs []LineComment) (string, error) {
	g.calls++
	g.reviews = append(g.reviews, postedReview{sha, event, body, cs})
	return fmt.Sprintf("https://gh/review/%d", len(g.reviews)), nil
}

func (g *fakeGH) EditComment(_ context.Context, _, id, body string) (string, error) {
	g.calls++
	g.edits = append(g.edits, "comment "+id+": "+body)
	return "https://gh/comment/" + id, nil
}

func (g *fakeGH) EditReview(_ context.Context, _ string, _ int, id, body string) (string, error) {
	g.calls++
	g.edits = append(g.edits, "review "+id+": "+body)
	return "https://gh/review/" + id, nil
}

type busMsg struct{ to, text string }

type fakeBus struct {
	names []string
	sent  []busMsg
}

func (b *fakeBus) Send(_ context.Context, to, text string) error {
	b.sent = append(b.sent, busMsg{to, text})
	return nil
}

func (b *fakeBus) Names(context.Context) ([]string, error) { return b.names, nil }

type harness struct {
	ob  *Outbox
	dec *fakeDecide
	gh  *fakeGH
	bus *fakeBus
	dir string
}

func newHarness(t *testing.T) *harness {
	t.Helper()
	h := &harness{dec: newFakeDecide(), gh: &fakeGH{head: "sha1"}, bus: &fakeBus{}, dir: t.TempDir()}
	h.ob = h.outbox()
	return h
}

// outbox returns a fresh Outbox over the same store, as after a restart.
func (h *harness) outbox() *Outbox {
	cfg := DefaultConfig()
	cfg.NotifyPane = "pane-1"
	return &Outbox{Store: NewStore(h.dir), Decide: h.dec, GH: h.gh, Bus: h.bus, Config: cfg}
}

func (h *harness) submit(t *testing.T, d Draft) Record {
	t.Helper()
	if err := d.Validate(); err != nil {
		t.Fatal(err)
	}
	rec, err := h.ob.Submit(context.Background(), d)
	if err != nil {
		t.Fatal(err)
	}
	return rec
}

func (h *harness) poll(t *testing.T) {
	t.Helper()
	if err := h.ob.Poll(context.Background()); err != nil {
		t.Fatal(err)
	}
}

func (h *harness) state(t *testing.T, id int64) Record {
	t.Helper()
	rec, err := h.ob.Store.Get(id)
	if err != nil {
		t.Fatal(err)
	}
	return rec
}

func threadDraft() Draft {
	return Draft{Repo: "o/r", PR: 7, Owner: "pm/x", Ref: "task-1", Lines: []LineComment{
		{Path: "a.go", Line: 3, Text: "first"},
		{Path: "b/c.go", Line: 10, Text: "second"},
	}}
}

func TestSubmitRejectsLintAndFilesNothing(t *testing.T) {
	h := newHarness(t)
	h.bus.names = []string{"coord/primary"}
	d := threadDraft()
	d.Lines[1].Text = "coord/primary wants this"
	if err := d.Validate(); err != nil {
		t.Fatal(err)
	}
	_, err := h.ob.Submit(context.Background(), d)
	var le *LintError
	if !errors.As(err, &le) || len(le.Hits) != 1 || le.Hits[0].Location != "b/c.go:10" {
		t.Fatalf("err = %v, want one lint hit at b/c.go:10", err)
	}
	if len(h.dec.asks) != 0 {
		t.Errorf("asks = %d, want none", len(h.dec.asks))
	}
	if recs, _ := h.ob.Store.List(); len(recs) != 0 {
		t.Errorf("records = %d, want none", len(recs))
	}
}

func TestLineBreaksSurviveSubmitApprovalAndPost(t *testing.T) {
	h := newHarness(t)
	text := "line one\n\n\n  indented two\r\nthree  \n\n"
	body := "review\n\n\nbody  \n"
	d := Draft{Repo: "o/r", PR: 7, Owner: "pm/x", Kind: KindComment, Body: body, Lines: []LineComment{{Path: "a.go", Line: 1, Text: text}}}
	rec := h.submit(t, d)
	q := h.dec.asks[0].question
	if !strings.Contains(q, text) || !strings.Contains(q, body) {
		t.Errorf("question does not hold the text byte for byte:\n%q", q)
	}
	if got := h.state(t, rec.ID); got.Body != body || got.Lines[0].Text != text {
		t.Errorf("stored = %q / %q", got.Body, got.Lines[0].Text)
	}
	h.dec.pick(rec.DecideID, "human", "post", "", "ui", "")
	h.poll(t)
	if len(h.gh.reviews) != 1 || h.gh.reviews[0].body != body || h.gh.reviews[0].lines[0].Text != text {
		t.Fatalf("posted = %+v", h.gh.reviews)
	}
}

func TestQuestionShowsWholePost(t *testing.T) {
	h := newHarness(t)
	d := Draft{Repo: "o/r", PR: 7, Owner: "pm/x", Kind: KindRequestChanges, Body: "Please fix.", Lines: []LineComment{
		{Path: "a.go", Line: 3, Text: "first note"}, {Path: "b.go", Line: 4, Text: "second note"},
	}}
	h.submit(t, d)
	call := h.dec.asks[0]
	if call.owner != "pm/x" || call.pane != "pane-1" {
		t.Errorf("ask owner/pane = %q/%q", call.owner, call.pane)
	}
	q := call.question
	for _, want := range []string{"review (main conversation): request changes", "o/r#7", "sha1", "Please fix.", "a.go:3", "first note", "b.go:4", "second note"} {
		if !strings.Contains(q, want) {
			t.Errorf("question lacks %q:\n%s", want, q)
		}
	}
	if strings.Index(q, "review (main conversation)") > strings.Index(q, "o/r#7") {
		t.Errorf("destination is not first:\n%s", q)
	}

	h.submit(t, threadDraft())
	q = h.dec.asks[1].question
	for _, want := range []string{"discussion thread on a.go line 3", "discussion thread on b/c.go line 10", "a.go:3", "first", "b/c.go:10", "second", "sha1"} {
		if !strings.Contains(q, want) {
			t.Errorf("thread question lacks %q:\n%s", want, q)
		}
	}
}

func TestDefaultKindPostsThreadsOnly(t *testing.T) {
	h := newHarness(t)
	rec := h.submit(t, threadDraft())
	h.dec.pick(rec.DecideID, "human", "post", "", "ui", "")
	h.poll(t)
	if len(h.gh.reviews) != 0 {
		t.Errorf("reviews = %+v, want nothing in the main conversation", h.gh.reviews)
	}
	if len(h.gh.threads) != 2 || h.gh.threads[0].Path != "a.go" || h.gh.threads[1].Line != 10 || h.gh.shas[0] != "sha1" {
		t.Errorf("threads = %+v shas = %v", h.gh.threads, h.gh.shas)
	}
	got := h.state(t, rec.ID)
	if got.State != StatePosted || len(got.Posted) != 2 {
		t.Fatalf("record = %+v", got)
	}
	if len(h.bus.sent) != 1 || h.bus.sent[0].to != "pm/x" || !strings.Contains(h.bus.sent[0].text, "https://gh/thread/2") ||
		!strings.Contains(h.bus.sent[0].text, "task-1") {
		t.Errorf("notices = %+v", h.bus.sent)
	}
	if out := h.dec.done[rec.DecideID]; !strings.Contains(out, "posted") || !strings.Contains(out, "https://gh/thread/1") {
		t.Errorf("decide outcome = %q", out)
	}
}

func TestOnlyHumanUIAnswerPostsOnce(t *testing.T) {
	h := newHarness(t)
	rec := h.submit(t, threadDraft())
	steps := []struct{ author, via, by string }{
		{"human", "cli", ""},          // the user's pick from the command line
		{"human", "ui", "pm/x"},       // an agent sending as the user
		{"coord/primary", "", ""},     // a peer agent message
		{"human", "ui", "coord/peer"}, // a peer with ui
	}
	for _, s := range steps {
		h.dec.pick(rec.DecideID, s.author, "post", "post", s.via, s.by)
		h.poll(t)
		if h.gh.calls != 0 {
			t.Fatalf("%+v posted", s)
		}
	}
	h.dec.pick(rec.DecideID, "human", "post", "", "ui", "")
	h.poll(t)
	h.poll(t)
	if h.gh.calls != 2 {
		t.Errorf("calls = %d, want one post per thread", h.gh.calls)
	}
}

func TestMovedHeadReasks(t *testing.T) {
	h := newHarness(t)
	rec := h.submit(t, threadDraft())
	h.dec.pick(rec.DecideID, "human", "post", "", "ui", "")
	h.gh.head = "sha2"
	h.poll(t)
	if h.gh.calls != 0 {
		t.Fatal("posted on a moved head")
	}
	if len(h.dec.asks) != 2 || !strings.Contains(h.dec.asks[1].question, "sha2") {
		t.Fatalf("asks = %+v, want a fresh approval with sha2", h.dec.asks)
	}
	if _, ok := h.dec.done[rec.DecideID]; !ok {
		t.Error("old decide item left open")
	}
	got := h.state(t, rec.ID)
	if got.State != StatePending || got.HeadSHA != "sha2" || got.DecideID != h.dec.last() {
		t.Errorf("record = %+v", got)
	}
	h.dec.pick(got.DecideID, "human", "post", "", "ui", "")
	h.poll(t)
	if len(h.gh.shas) != 2 || h.gh.shas[0] != "sha2" {
		t.Errorf("shas = %v", h.gh.shas)
	}
}

func TestEditSendsRedraftNotice(t *testing.T) {
	h := newHarness(t)
	rec := h.submit(t, threadDraft())
	h.dec.pick(rec.DecideID, "human", "edit", "", "ui", "")
	h.dec.pick(rec.DecideID, "human", "", "say it\nmore kindly", "ui", "")
	h.poll(t)
	h.poll(t)
	if h.gh.calls != 0 {
		t.Fatal("edit posted")
	}
	if len(h.bus.sent) != 1 || h.bus.sent[0].to != "pm/x" || !strings.Contains(h.bus.sent[0].text, "say it\nmore kindly") ||
		!strings.Contains(h.bus.sent[0].text, "redraft") {
		t.Errorf("notices = %+v", h.bus.sent)
	}
	if got := h.state(t, rec.ID); got.State != StateNeedsRedraft {
		t.Errorf("state = %q", got.State)
	}
}

func TestCancelClosesDraft(t *testing.T) {
	h := newHarness(t)
	rec := h.submit(t, threadDraft())
	h.dec.pick(rec.DecideID, "human", "cancel", "", "ui", "")
	h.poll(t)
	if h.gh.calls != 0 {
		t.Fatal("cancel posted")
	}
	if got := h.state(t, rec.ID); got.State != StateCancelled {
		t.Errorf("state = %q", got.State)
	}
	if _, ok := h.dec.done[rec.DecideID]; !ok {
		t.Error("decide item left open")
	}
}

func TestClosedItemWithoutAnswerCancels(t *testing.T) {
	h := newHarness(t)
	rec := h.submit(t, threadDraft())
	h.dec.items[rec.DecideID].State = "withdrawn"
	h.poll(t)
	if got := h.state(t, rec.ID); got.State != StateCancelled || h.gh.calls != 0 {
		t.Errorf("state = %q calls = %d", got.State, h.gh.calls)
	}
}

// An agent's done on an approval the user has not read leaves it in the
// inbox as done-unread. It is closed for the outbox.
func TestDoneUnreadItemWithoutAnswerCancels(t *testing.T) {
	h := newHarness(t)
	rec := h.submit(t, threadDraft())
	h.dec.items[rec.DecideID].State = "done-unread"
	h.poll(t)
	if got := h.state(t, rec.ID); got.State != StateCancelled || h.gh.calls != 0 {
		t.Errorf("state = %q calls = %d", got.State, h.gh.calls)
	}
}

func TestRestartDoesNotDoublePost(t *testing.T) {
	h := newHarness(t)
	rec := h.submit(t, threadDraft())
	h.dec.pick(rec.DecideID, "human", "post", "", "ui", "")
	h.poll(t)
	h.ob = h.outbox()
	h.poll(t)
	if h.gh.calls != 2 {
		t.Errorf("calls = %d, want 2", h.gh.calls)
	}
}

func TestRestartMidPostDoesNotRepost(t *testing.T) {
	h := newHarness(t)
	rec := h.submit(t, threadDraft())
	h.dec.pick(rec.DecideID, "human", "post", "", "ui", "")
	// A crash after the first thread was sent but before it was recorded.
	got := h.state(t, rec.ID)
	got.State = StatePosting
	inFlight := 0
	got.InFlight = &inFlight
	if err := h.ob.Store.Save(got); err != nil {
		t.Fatal(err)
	}
	h.ob = h.outbox()
	h.poll(t)
	if h.gh.calls != 0 {
		t.Fatalf("calls = %d, want none", h.gh.calls)
	}
	if got := h.state(t, rec.ID); got.State != StateUncertain {
		t.Errorf("state = %q", got.State)
	}
	if len(h.bus.sent) != 1 || !strings.Contains(h.bus.sent[0].text, "a.go:3") {
		t.Errorf("notices = %+v", h.bus.sent)
	}
}

func TestRestartResumesRemainingThreads(t *testing.T) {
	h := newHarness(t)
	rec := h.submit(t, threadDraft())
	h.dec.pick(rec.DecideID, "human", "post", "", "ui", "")
	// A crash after the first thread was recorded.
	got := h.state(t, rec.ID)
	got.State = StatePosting
	got.Posted = []Link{{Where: "a.go:3", URL: "https://gh/thread/0"}}
	if err := h.ob.Store.Save(got); err != nil {
		t.Fatal(err)
	}
	h.ob = h.outbox()
	h.poll(t)
	if h.gh.calls != 1 || h.gh.threads[0].Path != "b/c.go" {
		t.Errorf("threads = %+v", h.gh.threads)
	}
	if got := h.state(t, rec.ID); got.State != StatePosted || len(got.Posted) != 2 {
		t.Errorf("record = %+v", got)
	}
}

func TestPostFailureKeepsDraftForRetry(t *testing.T) {
	h := newHarness(t)
	rec := h.submit(t, threadDraft())
	h.dec.pick(rec.DecideID, "human", "post", "", "ui", "")
	h.gh.failPost = errors.New("HTTP 422")
	if err := h.ob.Poll(context.Background()); err == nil {
		t.Fatal("want the post error")
	}
	got := h.state(t, rec.ID)
	if got.State != StatePosting || got.InFlight != nil {
		t.Fatalf("record = %+v", got)
	}
	h.gh.failPost = nil
	h.poll(t)
	if got := h.state(t, rec.ID); got.State != StatePosted || len(h.gh.threads) != 2 {
		t.Errorf("record = %+v threads = %d", got, len(h.gh.threads))
	}
}

func TestEditOfExistingComment(t *testing.T) {
	h := newHarness(t)
	h.gh.oldText = "old words"
	d := Draft{Repo: "o/r", PR: 7, Owner: "pm/x", Edit: "55", Lines: []LineComment{{Path: "a.go", Line: 3, Text: "new words"}}}
	rec := h.submit(t, d)
	q := h.dec.asks[0].question
	if !strings.Contains(q, "old words") || !strings.Contains(q, "new words") || !strings.Contains(q, "edit of comment 55") {
		t.Errorf("question:\n%s", q)
	}
	h.dec.pick(rec.DecideID, "human", "post", "", "ui", "")
	h.poll(t)
	if len(h.gh.edits) != 1 || h.gh.edits[0] != "comment 55: new words" || len(h.gh.threads) != 0 {
		t.Errorf("edits = %v threads = %v", h.gh.edits, h.gh.threads)
	}

	h.gh.oldText = "old review"
	d = Draft{Repo: "o/r", PR: 7, Owner: "pm/x", Edit: "66", Kind: KindComment, Body: "new review"}
	rec = h.submit(t, d)
	h.dec.pick(rec.DecideID, "human", "post", "", "ui", "")
	h.poll(t)
	if len(h.gh.edits) != 2 || h.gh.edits[1] != "review 66: new review" || len(h.gh.reviews) != 0 {
		t.Errorf("edits = %v reviews = %v", h.gh.edits, h.gh.reviews)
	}
}

func TestEditIsLinted(t *testing.T) {
	h := newHarness(t)
	d := Draft{Repo: "o/r", PR: 7, Owner: "pm/x", Edit: "55", Lines: []LineComment{{Path: "a.go", Line: 3, Text: "per the user"}}}
	if err := d.Validate(); err != nil {
		t.Fatal(err)
	}
	var le *LintError
	if _, err := h.ob.Submit(context.Background(), d); !errors.As(err, &le) {
		t.Fatalf("err = %v, want a lint error", err)
	}
}
