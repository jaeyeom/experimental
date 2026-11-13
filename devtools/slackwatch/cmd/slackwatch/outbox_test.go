package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"net/http/httptest"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
)

// cmdFakeDecider is an in-memory decide ledger.
type cmdFakeDecider struct {
	items     map[string]*slackwatch.DecideItem
	questions map[string]string
	outcomes  map[string]string
	replies   []string
}

func (d *cmdFakeDecider) Ask(_ context.Context, _, question string) (string, error) {
	id := fmt.Sprintf("D-%d", len(d.items)+1)
	d.items[id] = &slackwatch.DecideItem{ID: id, State: "open"}
	d.questions[id] = question
	return id, nil
}

func (d *cmdFakeDecider) Show(_ context.Context, id string) (slackwatch.DecideItem, error) {
	it, ok := d.items[id]
	if !ok {
		return slackwatch.DecideItem{}, errors.New("no such item")
	}
	return *it, nil
}

func (d *cmdFakeDecider) Reply(_ context.Context, id, text string) error {
	it, ok := d.items[id]
	if !ok {
		return errors.New("no such item")
	}
	it.Messages = append(it.Messages, slackwatch.DecideMessage{Text: text, By: "slackwatch"})
	d.replies = append(d.replies, text)
	return nil
}

func (d *cmdFakeDecider) Done(_ context.Context, id, outcome string) error {
	d.items[id].State = "done"
	d.outcomes[id] = outcome
	return nil
}

func (d *cmdFakeDecider) answerPost(id string) {
	d.items[id].State = "answered"
	d.items[id].Answer = &slackwatch.DecideAnswer{Value: "post", Key: "post", AnsweredBy: "human", Via: "ui"}
	d.items[id].Messages = append(d.items[id].Messages, slackwatch.DecideMessage{Author: "human", Text: "post: Post as written", OptionKey: "post", Via: "ui"})
}

func (d *cmdFakeDecider) answerNotes(id, notes string) {
	d.items[id].State = "answered"
	d.items[id].Answer = &slackwatch.DecideAnswer{Value: notes, AnsweredBy: "human", Via: "ui"}
	d.items[id].Messages = append(d.items[id].Messages, slackwatch.DecideMessage{Author: "human", Text: notes, Via: "ui"})
}

const (
	cmdWriteToken = "xoxp-write-token" //nolint:gosec // G101: a fake token for the fake Slack server.
	cmdExpectUser = "UPOSTER"
	cmdText       = "Release is out & ready.\nNotes: <https://example.com|here>"
	cmdFooter     = "_sent by an agent_"
	cmdPostedLink = "https://acme.slack.com/archives/C0123/p1700000100000001"
)

// postEnv is a config with posting set up, a fake Slack API, and a fake
// decide.
type postEnv struct {
	t         *testing.T
	dir, cfg  string
	textFile  string
	tokenFile string
	dec       *cmdFakeDecider

	mu       sync.Mutex
	authUser string
	readUser string
	readText string
	posts    []url.Values
	cursors  []string
	// list answers conversations.list. code 0 means 200. It must not lock e.
	list func(cursor string) (code int, retryAfter, body string)
}

// newPostEnv writes a config whose "post" object is post(dir), or none
// when post is nil.
func newPostEnv(t *testing.T, post func(dir string) map[string]any) *postEnv {
	t.Helper()
	dir := isolateEnv(t)
	e := &postEnv{
		t: t, dir: dir, authUser: cmdExpectUser, readUser: cmdExpectUser, readText: cmdText + "\n" + cmdFooter,
		dec: &cmdFakeDecider{items: map[string]*slackwatch.DecideItem{}, questions: map[string]string{}, outcomes: map[string]string{}},
	}
	old := newDecider
	newDecider = func(slackwatch.Config) slackwatch.Decider { return e.dec }
	t.Cleanup(func() { newDecider = old })

	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if err := r.ParseForm(); err != nil {
			t.Error(err)
		}
		e.mu.Lock()
		defer e.mu.Unlock()
		w.Header().Set("Content-Type", "application/json")
		switch r.URL.Path[1:] {
		case "auth.test":
			fmt.Fprintf(w, `{"ok":true,"url":"https://acme.slack.com/","user_id":%q}`, e.authUser)
		case "chat.postMessage":
			e.posts = append(e.posts, r.Form)
			fmt.Fprintf(w, `{"ok":true,"channel":%q,"ts":"1700000100.000001"}`, r.Form.Get("channel"))
		case "conversations.info":
			fmt.Fprint(w, `{"ok":true,"channel":{"id":"C0123","name":"general"}}`)
		case "conversations.list":
			cursor := r.Form.Get("cursor")
			e.cursors = append(e.cursors, cursor)
			if e.list == nil {
				t.Errorf("unexpected Slack call %s", r.URL.Path)
				fmt.Fprint(w, `{"ok":false,"error":"unknown_method"}`)
				return
			}
			code, retry, body := e.list(cursor)
			if code != 0 && code != http.StatusOK {
				if retry != "" {
					w.Header().Set("Retry-After", retry)
				}
				w.WriteHeader(code)
			}
			fmt.Fprint(w, body)
		case "conversations.history":
			msg, _ := json.Marshal(map[string]string{"type": "message", "user": e.readUser, "text": e.readText, "ts": r.Form.Get("latest")})
			fmt.Fprintf(w, `{"ok":true,"messages":[%s]}`, msg)
		default:
			t.Errorf("unexpected Slack call %s", r.URL.Path)
			fmt.Fprint(w, `{"ok":false,"error":"unknown_method"}`)
		}
	}))
	t.Cleanup(srv.Close)
	oldURL := slackAPIURL
	slackAPIURL = srv.URL + "/"
	t.Cleanup(func() { slackAPIURL = oldURL })

	e.tokenFile = filepath.Join(dir, "write-token")
	writeFile(t, e.tokenFile, cmdWriteToken+"\n")
	readTokens := filepath.Join(dir, "tokens.json")
	writeFile(t, readTokens, `{"appToken":"xapp-1","userToken":"xoxp-read"}`)
	e.textFile = filepath.Join(dir, "draft.txt")
	writeFile(t, e.textFile, cmdText)
	cfg := map[string]any{
		"dbPath":         filepath.Join(dir, "slackwatch.db"),
		"tokenFile":      readTokens,
		"notifyTemplate": `printf '%s %s %s\n' "$SLACKWATCH_EVENT" "$SLACKWATCH_TARGET" "$SLACKWATCH_MESSAGE" >> ` + filepath.Join(dir, "notes"),
		"fallbackTarget": "fallback",
		"footerPattern":  "_sent by an agent_",
		"unownedGrace":   "0s",
	}
	if post != nil {
		cfg["post"] = post(dir)
	}
	b, err := json.Marshal(cfg)
	if err != nil {
		t.Fatal(err)
	}
	e.cfg = filepath.Join(dir, "config.json")
	writeFile(t, e.cfg, string(b))
	return e
}

func writeFile(t *testing.T, path, body string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
}

func handoffPost(string) map[string]any {
	return map[string]any{"mode": "handoff", "agent": "poster", "expectUser": cmdExpectUser}
}

func directPost(dir, pin string) map[string]any {
	return map[string]any{
		"mode": "direct", "expectUser": cmdExpectUser, "pinSalt": "s4lt", "tokenPin": pin,
		"writeTokenFile": filepath.Join(dir, "write-token"), "footer": cmdFooter,
	}
}

// newDirectEnv sets up direct mode with pin, or with the write token
// pinned when pin is empty.
func newDirectEnv(t *testing.T, pin string) *postEnv {
	t.Helper()
	if pin == "" {
		pin = slackwatch.Pin("s4lt", cmdWriteToken)
	}
	return newPostEnv(t, func(dir string) map[string]any { return directPost(dir, pin) })
}

func (e *postEnv) run(args ...string) (string, string, int) {
	var stdout, stderr bytes.Buffer
	code := run(append([]string{"--config", e.cfg}, args...), &stdout, &stderr)
	return stdout.String(), stderr.String(), code
}

func (e *postEnv) ok(args ...string) string {
	e.t.Helper()
	out, errOut, code := e.run(args...)
	if code != 0 {
		e.t.Fatalf("slackwatch %q = %d, stderr %q", args, code, errOut)
	}
	return out
}

func (e *postEnv) submit() {
	e.t.Helper()
	out := e.ok("post", "--to", "C0123", "--text-file", e.textFile, "--owner", "agent-1", "--purpose", "announce release", "--ref", "#7")
	if !strings.Contains(out, "outbox 1") {
		e.t.Fatalf("post printed %q, want outbox 1", out)
	}
}

func (e *postEnv) record() slackwatch.OutboxRecord {
	e.t.Helper()
	var r slackwatch.OutboxRecord
	if err := json.Unmarshal([]byte(e.ok("outbox", "show", "1", "--json")), &r); err != nil {
		e.t.Fatal(err)
	}
	return r
}

func (e *postEnv) putConfig(key string, value any) {
	e.t.Helper()
	b, err := os.ReadFile(e.cfg)
	if err != nil {
		e.t.Fatal(err)
	}
	var cfg map[string]any
	if err := json.Unmarshal(b, &cfg); err != nil {
		e.t.Fatal(err)
	}
	cfg[key] = value
	out, err := json.Marshal(cfg)
	if err != nil {
		e.t.Fatal(err)
	}
	writeFile(e.t, e.cfg, string(out))
}

func useAgents(t *testing.T, names []string, err error) {
	t.Helper()
	old := listAgents
	listAgents = func(context.Context) ([]string, error) { return names, err }
	t.Cleanup(func() { listAgents = old })
}

func (e *postEnv) notes() string {
	b, err := os.ReadFile(filepath.Join(e.dir, "notes"))
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		e.t.Fatal(err)
	}
	return string(b)
}

func TestPostRefusedWhenPostingOff(t *testing.T) {
	e := newPostEnv(t, nil)
	_, errOut, code := e.run("post", "--to", "C0123", "--text-file", e.textFile, "--owner", "a", "--purpose", "p")
	if code == 0 || !strings.Contains(errOut, "posting is disabled") || !strings.Contains(errOut, "post.mode") {
		t.Errorf("post = %d, stderr %q, want a refusal naming post.mode", code, errOut)
	}
	if len(e.dec.items) != 0 {
		t.Error("filed a decide item")
	}
}

func TestPostRequiresFlags(t *testing.T) {
	e := newPostEnv(t, handoffPost)
	for _, args := range [][]string{
		{"post", "--text-file", e.textFile, "--owner", "a", "--purpose", "p"},
		{"post", "--to", "C0123", "--owner", "a", "--purpose", "p"},
		{"post", "--to", "C0123", "--text-file", e.textFile, "--purpose", "p"},
		{"post", "--to", "C0123", "--text-file", e.textFile, "--owner", "a"},
	} {
		if _, _, code := e.run(args...); code != 2 {
			t.Errorf("%q exit = %d, want 2", args, code)
		}
	}
}

func TestPostHandoffFlow(t *testing.T) {
	e := newPostEnv(t, handoffPost)
	e.submit()
	q := e.dec.questions["D-1"]
	for _, want := range []string{"#general (C0123)", "> Release is out & ready.", "agent-1", "announce release", slackwatch.TextHash(cmdText)[:8]} {
		if !strings.Contains(q, want) {
			t.Errorf("question missing %q:\n%s", want, q)
		}
	}
	if r := e.record(); r.State != slackwatch.OutboxPending || r.Text != cmdText || r.DestName != "#general" {
		t.Fatalf("record = %+v", r)
	}

	e.dec.answerPost("D-1")
	e.ok("outbox", "poke", "D-1")
	if r := e.record(); r.State != slackwatch.OutboxAwaitingHandoff {
		t.Fatalf("state = %s", r.State)
	}
	if notes := e.notes(); !strings.Contains(notes, "outbox-ready poster approved post ready: 1\n") {
		t.Errorf("notes = %q", notes)
	}
	if show := e.ok("outbox", "show", "1"); !strings.HasSuffix(show, "\n"+cmdText+"\n") {
		t.Errorf("show does not end with the exact text:\n%s", show)
	}

	if out := e.ok("outbox", "posted", "1", cmdPostedLink); !strings.Contains(out, "posted") {
		t.Errorf("posted printed %q", out)
	}
	r := e.record()
	if r.State != slackwatch.OutboxPosted || r.WatchID == 0 {
		t.Fatalf("record = %+v", r)
	}
	if e.dec.outcomes["D-1"] != cmdPostedLink {
		t.Errorf("decide outcome = %q", e.dec.outcomes["D-1"])
	}
	if ls := e.ok("ls", "--owner", "agent-1"); !strings.Contains(ls, "announce release") {
		t.Errorf("ls = %q", ls)
	}
	if len(e.posts) != 0 {
		t.Errorf("handoff mode posted itself: %v", e.posts)
	}
}

func TestOutboxPostedManual(t *testing.T) {
	e := newPostEnv(t, handoffPost)
	e.submit()
	if _, errOut, code := e.run("outbox", "posted", "1", cmdPostedLink); code == 0 || !strings.Contains(errOut, "pending, not awaiting-handoff") {
		t.Errorf("posted without --manual = %d, stderr %q", code, errOut)
	}
	if _, errOut, code := e.run("outbox", "posted", "--manual", "1", cmdPostedLink); code == 0 || !strings.Contains(errOut, "no ui approval found for 1") {
		t.Errorf("unapproved --manual = %d, stderr %q", code, errOut)
	}
	if r := e.record(); r.State != slackwatch.OutboxPending {
		t.Fatalf("state = %s", r.State)
	}

	e.dec.answerPost("D-1")
	if out := e.ok("outbox", "posted", "--manual", "1", cmdPostedLink); !strings.Contains(out, "outbox 1 posted") {
		t.Errorf("posted printed %q", out)
	}
	r := e.record()
	if r.State != slackwatch.OutboxPosted || r.WatchID == 0 || !r.Manual {
		t.Fatalf("record = %+v", r)
	}
	if e.dec.outcomes["D-1"] != cmdPostedLink {
		t.Errorf("decide outcome = %q", e.dec.outcomes["D-1"])
	}
	if show := e.ok("outbox", "show", "1"); !strings.Contains(show, "\nmanual: true\n") {
		t.Errorf("show does not note manual:\n%s", show)
	}
	if ls := e.ok("ls", "--owner", "agent-1"); !strings.Contains(ls, "announce release") {
		t.Errorf("ls = %q", ls)
	}
}

func TestPostHandoffMismatchAlerts(t *testing.T) {
	e := newPostEnv(t, handoffPost)
	e.submit()
	e.dec.answerPost("D-1")
	e.ok("outbox", "poke", "D-1")
	e.readText = strings.Replace(cmdText, "https://example.com", "https://example.com/", 1)
	e.ok("outbox", "posted", "1", cmdPostedLink)
	if r := e.record(); r.State != slackwatch.OutboxMismatch {
		t.Fatalf("state = %s", r.State)
	}
	notes := e.notes()
	for _, target := range []string{"agent-1", "fallback"} {
		if !strings.Contains(notes, "outbox-mismatch "+target+" ") {
			t.Errorf("no mismatch alert to %s: %q", target, notes)
		}
	}
	if !strings.Contains(notes, "https://example.com/|here") || !strings.Contains(notes, "https://example.com|here") {
		t.Errorf("alert lacks both texts: %q", notes)
	}
	if show := e.ok("show", "1"); !strings.Contains(show, "flag: outbox 1 mismatch") {
		t.Errorf("watch not flagged:\n%s", show)
	}
}

func TestOutboxRecheck(t *testing.T) {
	var help bytes.Buffer
	usage(&help)
	if !strings.Contains(help.String(), "slackwatch outbox recheck <id>") {
		t.Fatalf("usage missing recheck:\n%s", help.String())
	}

	e := newPostEnv(t, handoffPost)
	e.submit()
	if _, errOut, code := e.run("outbox", "recheck", "1"); code == 0 || !strings.Contains(errOut, "not mismatch") {
		t.Fatalf("pending recheck = %d, stderr %q", code, errOut)
	}

	e.dec.answerPost("D-1")
	e.ok("outbox", "poke", "D-1")
	e.readText = cmdText + " *Sent using* <@U1>"
	e.ok("outbox", "posted", "1", cmdPostedLink)
	if r := e.record(); r.State != slackwatch.OutboxMismatch {
		t.Fatalf("state = %s", r.State)
	}
	_, errOut, code := e.run("outbox", "recheck", "1")
	if code == 0 || !strings.Contains(errOut, "the posted text differs from the approved text") {
		t.Fatalf("recheck = %d, stderr %q", code, errOut)
	}
	if r := e.record(); r.State != slackwatch.OutboxMismatch {
		t.Fatalf("state = %s", r.State)
	}

	body, err := os.ReadFile(e.cfg)
	if err != nil {
		t.Fatal(err)
	}
	next := strings.Replace(string(body), `"footerPattern":"_sent by an agent_"`, `"footerPattern":"Sent using"`, 1)
	if next == string(body) {
		t.Fatal("config had no footerPattern to replace")
	}
	if err := os.WriteFile(e.cfg, []byte(next), 0o600); err != nil {
		t.Fatal(err)
	}
	posts := len(e.posts)
	out := e.ok("outbox", "recheck", "1")
	if !strings.Contains(out, "outbox 1 posted") {
		t.Fatalf("recheck printed %q", out)
	}
	if r := e.record(); r.State != slackwatch.OutboxPosted {
		t.Fatalf("state = %s", r.State)
	}
	if len(e.posts) != posts {
		t.Fatalf("posts = %d, want %d", len(e.posts), posts)
	}
	if len(e.dec.replies) != 1 || !strings.Contains(e.dec.replies[0], "rechecked") {
		t.Fatalf("replies = %q", e.dec.replies)
	}
}

func TestPostDirectPostsWithFooter(t *testing.T) {
	e := newDirectEnv(t, "")
	e.submit()
	e.dec.answerPost("D-1")
	e.ok("outbox", "poke", "D-1")
	e.ok("outbox", "poke", "D-1")
	if len(e.posts) != 1 {
		t.Fatalf("posts = %v, want one", e.posts)
	}
	p := e.posts[0]
	if p.Get("channel") != "C0123" || p.Get("text") != cmdText+"\n\n"+cmdFooter || p.Get("token") != cmdWriteToken {
		t.Errorf("chat.postMessage form = %v", p)
	}
	r := e.record()
	if r.State != slackwatch.OutboxPosted || r.Permalink != cmdPostedLink {
		t.Fatalf("record = %+v", r)
	}
	if show := e.ok("show", "1"); !strings.Contains(show, "owner: agent-1") || !strings.Contains(show, "source: outbox") {
		t.Errorf("watch:\n%s", show)
	}
}

func TestPostDirectPinMismatchDisables(t *testing.T) {
	e := newDirectEnv(t, slackwatch.Pin("s4lt", "xoxp-some-other-token"))
	e.submit()
	e.dec.answerPost("D-1")
	e.ok("outbox", "poke", "D-1")
	if len(e.posts) != 0 {
		t.Fatalf("posted with a pin mismatch: %v", e.posts)
	}
	if !strings.Contains(e.notes(), "posting-disabled fallback ") {
		t.Errorf("notes = %q, want a fallback alert", e.notes())
	}
	out, _, code := e.run("status")
	if code == 0 || !strings.Contains(out, "posting: direct (disabled: the write token does not match post.tokenPin)") {
		t.Errorf("status = %d:\n%s", code, out)
	}
}

func TestPostDirectWrongUserDisables(t *testing.T) {
	e := newDirectEnv(t, "")
	e.authUser = "USOMEONE"
	e.submit()
	e.dec.answerPost("D-1")
	e.ok("outbox", "poke", "D-1")
	if len(e.posts) != 0 {
		t.Fatalf("posted as the wrong user: %v", e.posts)
	}
	if !strings.Contains(e.notes(), "posting-disabled fallback ") {
		t.Errorf("notes = %q", e.notes())
	}
	if r := e.record(); r.State != slackwatch.OutboxApproved {
		t.Errorf("state = %s, want approved", r.State)
	}
}

func TestPostDirectFooterPatternMismatchRefuses(t *testing.T) {
	e := newPostEnv(t, func(dir string) map[string]any {
		post := directPost(dir, "x")
		post["footer"] = "posted by me"
		return post
	})
	_, errOut, code := e.run("post", "--to", "C0123", "--text-file", e.textFile, "--owner", "a", "--purpose", "p")
	if code == 0 || !strings.Contains(errOut, "footerPattern") {
		t.Errorf("post = %d, stderr %q", code, errOut)
	}
}

func TestPinWritesConfigAndReenables(t *testing.T) {
	e := newDirectEnv(t, "wrong")
	e.submit()
	e.dec.answerPost("D-1")
	e.ok("outbox", "poke", "D-1")
	if len(e.posts) != 0 {
		t.Fatal("posted before pin")
	}

	e.authUser = "USOMEONE"
	if _, errOut, code := e.run("pin"); code == 0 || !strings.Contains(errOut, "USOMEONE") {
		t.Errorf("pin with the wrong user = %d, %q", code, errOut)
	}
	e.authUser = cmdExpectUser
	if out := e.ok("pin"); !strings.Contains(out, cmdExpectUser) {
		t.Errorf("pin printed %q", out)
	}
	cfg, err := slackwatch.LoadConfig(e.cfg)
	if err != nil {
		t.Fatal(err)
	}
	if cfg.Post.TokenPin != slackwatch.Pin(cfg.Post.PinSalt, cmdWriteToken) || cfg.Post.Footer != cmdFooter {
		t.Errorf("config post = %+v", cfg.Post)
	}
	if cfg.FallbackTarget != "fallback" {
		t.Errorf("pin lost other config: %+v", cfg)
	}
	// poke posts an approved record once pin has re-enabled posting.
	e.ok("outbox", "poke", "D-1")
	e.ok("outbox", "poke", "D-1")
	if len(e.posts) != 1 || e.record().State != slackwatch.OutboxPosted {
		t.Errorf("posts = %v after pin", e.posts)
	}
}

func TestOutboxLsCancelAndStatus(t *testing.T) {
	e := newPostEnv(t, handoffPost)
	e.submit()
	if ls := e.ok("outbox", "ls"); !strings.Contains(ls, "pending") || !strings.Contains(ls, "agent-1") {
		t.Errorf("ls = %q", ls)
	}
	out, _, _ := e.run("status")
	if !strings.Contains(out, "posting: handoff") || !strings.Contains(out, "outbox: 1 pending, 0 awaiting-handoff, 0 needs-redraft") {
		t.Errorf("status:\n%s", out)
	}
	e.ok("outbox", "cancel", "1", "--reason", "wrong channel")
	if r := e.record(); r.State != slackwatch.OutboxCancelled || e.dec.items["D-1"].State != "done" {
		t.Errorf("record %+v decide %+v", r, e.dec.items["D-1"])
	}
	if ls := e.ok("outbox", "ls", "--state", "pending"); strings.Contains(ls, "agent-1") {
		t.Errorf("cancelled record listed as pending: %q", ls)
	}
	if _, _, code := e.run("outbox", "bogus"); code != 2 {
		t.Errorf("unknown outbox command exit = %d", code)
	}
}

func (e *postEnv) show(id string) (string, slackwatch.OutboxRecord) {
	e.t.Helper()
	out := e.ok("outbox", "show", id)
	var r slackwatch.OutboxRecord
	if err := json.Unmarshal([]byte(e.ok("outbox", "show", id, "--json")), &r); err != nil {
		e.t.Fatal(err)
	}
	return out, r
}

func TestCancelNeedsRedraft(t *testing.T) {
	e := newPostEnv(t, handoffPost)
	e.submit()
	e.dec.answerNotes("D-1", "say tomorrow")
	e.ok("outbox", "poke", "D-1")
	if e.record().State != slackwatch.OutboxNeedsRedraft {
		t.Fatalf("state = %s", e.record().State)
	}
	e.ok("outbox", "cancel", "1", "--reason", "drop it")
	if r := e.record(); r.State != slackwatch.OutboxCancelled || e.dec.items["D-1"].State != "done" {
		t.Errorf("record %+v decide %+v", r, e.dec.items["D-1"])
	}
	out, _, _ := e.run("status")
	if !strings.Contains(out, "outbox: 0 pending, 0 awaiting-handoff, 0 needs-redraft") {
		t.Errorf("status:\n%s", out)
	}
}

func TestPostSupersedes(t *testing.T) {
	e := newPostEnv(t, handoffPost)
	e.submit()
	e.dec.answerNotes("D-1", "say tomorrow")
	e.ok("outbox", "poke", "D-1")

	_, errOut, code := e.run("post", "--to", "nope", "--text-file", e.textFile, "--owner", "agent-1", "--purpose", "again", "--supersedes", "1")
	if code == 0 {
		t.Fatal("invalid destination succeeded")
	}
	if r := e.record(); r.State != slackwatch.OutboxNeedsRedraft || r.SupersededBy != 0 {
		t.Fatalf("old changed: %+v, stderr %q", r, errOut)
	}
	if _, _, code := e.run("outbox", "show", "2"); code == 0 {
		t.Fatal("invalid destination stored a record")
	}

	out := e.ok("post", "--to", "C0123", "--text-file", e.textFile, "--owner", "agent-1", "--purpose", "again", "--supersedes", "1")
	if !strings.Contains(out, "outbox 2") {
		t.Fatalf("post printed %q", out)
	}
	text, neu := e.show("2")
	if neu.Supersedes != 1 || !strings.Contains(text, "supersedes 1") {
		t.Fatalf("show new = %q %+v", text, neu)
	}
	text, old := e.show("1")
	want := "not posted: superseded by 2"
	if old.State != slackwatch.OutboxCancelled || old.Reason != want || old.SupersededBy != 2 || !strings.Contains(text, "superseded by 2") {
		t.Fatalf("show old = %q %+v", text, old)
	}
	if e.dec.items["D-1"].State != "done" {
		t.Errorf("decide = %+v", e.dec.items["D-1"])
	}
}

func TestPostSupersedesPostedRefused(t *testing.T) {
	e := newPostEnv(t, handoffPost)
	e.submit()
	e.dec.answerPost("D-1")
	e.ok("outbox", "poke", "D-1")
	e.ok("outbox", "posted", "1", cmdPostedLink)
	if e.record().State != slackwatch.OutboxPosted {
		t.Fatalf("state = %s", e.record().State)
	}
	_, errOut, code := e.run("post", "--to", "C0123", "--text-file", e.textFile, "--owner", "agent-1", "--purpose", "again", "--supersedes", "1")
	if code == 0 || !strings.Contains(errOut, slackwatch.OutboxPosted) {
		t.Fatalf("post = %d, stderr %q", code, errOut)
	}
	if _, _, code := e.run("outbox", "show", "2"); code == 0 {
		t.Fatal("posted record was superseded")
	}
	if e.record().State != slackwatch.OutboxPosted {
		t.Errorf("old state = %s", e.record().State)
	}
}

func TestPostNameRateLimitResumesCursor(t *testing.T) {
	e := newPostEnv(t, handoffPost)
	var calls int
	e.list = func(cursor string) (int, string, string) {
		calls++
		switch cursor {
		case "":
			return http.StatusOK, "", `{"ok":true,"channels":[{"id":"C1","name":"general"}],"response_metadata":{"next_cursor":"p2"}}`
		case "p2":
			if calls == 2 {
				return http.StatusTooManyRequests, "30", `{"ok":false,"error":"ratelimited"}`
			}
			return http.StatusOK, "", `{"ok":true,"channels":[{"id":"C9","name":"deploys"}]}`
		default:
			t.Errorf("cursor %q", cursor)
			return http.StatusInternalServerError, "", `{"ok":false,"error":"bad_cursor"}`
		}
	}
	old := waitFor
	var waits []time.Duration
	waitFor = func(_ context.Context, d time.Duration) error {
		waits = append(waits, d)
		return nil
	}
	t.Cleanup(func() { waitFor = old })

	out, errOut, code := e.run("post", "--to", "#deploys", "--text-file", e.textFile, "--owner", "agent-1", "--purpose", "announce")
	if code != 0 {
		t.Fatalf("post = %d, stderr %q", code, errOut)
	}
	if !strings.Contains(out, "outbox 1") {
		t.Fatalf("post printed %q", out)
	}
	if !strings.Contains(errOut, "rate limited, waiting 30s…") {
		t.Errorf("stderr = %q, want a 30s wait", errOut)
	}
	if len(waits) != 1 || waits[0] != 30*time.Second {
		t.Errorf("waits = %v, want [30s]", waits)
	}
	if got := strings.Join(e.cursors, ","); got != ",p2,p2" {
		t.Errorf("cursors = %q, want ,p2,p2", got)
	}
	r := e.record()
	if r.Channel != "C9" || r.DestName != "#deploys" {
		t.Errorf("record channel %q dest %q", r.Channel, r.DestName)
	}
	store, err := slackwatch.OpenStore(filepath.Join(e.dir, "slackwatch.db"))
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { store.Close() })
	id, _, ok, err := store.ChannelByName("deploys")
	if err != nil || !ok || id != "C9" {
		t.Fatalf("cache deploys = %q ok %v err %v", id, ok, err)
	}
	listed := len(e.cursors)
	e.ok("post", "--to", "#deploys", "--text-file", e.textFile, "--owner", "agent-1", "--purpose", "again")
	if len(e.cursors) != listed {
		t.Errorf("cache hit listed again: %v", e.cursors)
	}
}

func TestPostNameRateLimitPastBoundWritesNoOutbox(t *testing.T) {
	e := newPostEnv(t, handoffPost)
	e.putConfig("channelLookupWait", "0s")
	e.list = func(string) (int, string, string) {
		return http.StatusTooManyRequests, "30", `{"ok":false,"error":"ratelimited"}`
	}
	old := waitFor
	waitFor = func(context.Context, time.Duration) error {
		t.Fatal("slept past the wait bound")
		return nil
	}
	t.Cleanup(func() { waitFor = old })

	_, errOut, code := e.run("post", "--to", "#deploys", "--text-file", e.textFile, "--owner", "agent-1", "--purpose", "announce")
	if code == 0 {
		t.Fatal("post succeeded past the rate-limit bound")
	}
	if !strings.Contains(errOut, "could not resolve #deploys") || !strings.Contains(errOut, "pass the channel id (C…) to --to") {
		t.Fatalf("stderr = %q", errOut)
	}
	out := e.ok("outbox", "ls", "--json")
	if strings.TrimSpace(out) != "[]" {
		t.Fatalf("outbox = %q, want no record", out)
	}
}

func TestPostOwnerChecks(t *testing.T) {
	useAgents(t, []string{"alpha"}, nil)
	e := newPostEnv(t, handoffPost)
	e.putConfig("targets", map[string]string{"zeta": "x"})
	_, errOut, code := e.run("post", "--to", "C0123", "--text-file", e.textFile, "--owner", "nope", "--purpose", "p")
	if code == 0 || !strings.Contains(errOut, `unknown owner "nope"`) || !strings.Contains(errOut, "known names: alpha, zeta") {
		t.Fatalf("post = %d, stderr %q", code, errOut)
	}
	if out := strings.TrimSpace(e.ok("outbox", "ls", "--json")); out != "[]" {
		t.Fatalf("outbox = %q", out)
	}
	out := e.ok("post", "--to", "C0123", "--text-file", e.textFile, "--owner", "nope", "--owner-unchecked", "--purpose", "p")
	if !strings.Contains(out, "outbox 1") {
		t.Fatalf("unchecked post printed %q", out)
	}

	useAgents(t, nil, errors.New("agentbus down"))
	e = newPostEnv(t, handoffPost)
	_, errOut, code = e.run("post", "--to", "C0123", "--text-file", e.textFile, "--owner", "nope", "--purpose", "p")
	if code != 0 || !strings.Contains(errOut, "agentbus") || !strings.Contains(errOut, "accepting --owner nope") {
		t.Fatalf("agentbus down post = %d, stderr %q", code, errOut)
	}
}

func TestSubOwnerChecks(t *testing.T) {
	useAgents(t, []string{"alpha"}, nil)
	dir := t.TempDir()
	cfg := filepath.Join(dir, "config.json")
	writeFile(t, cfg, `{"dbPath":"`+filepath.Join(dir, "slackwatch.db")+`","targets":{"zeta":"x"}}`)
	var stdout, stderr bytes.Buffer
	if code := run([]string{"--config", cfg, "sub", "#deploys", "--owner", "nope"}, &stdout, &stderr); code == 0 || !strings.Contains(stderr.String(), "known names: alpha, zeta") {
		t.Fatalf("sub = %d, stderr %q", code, stderr.String())
	}
	if out := strings.TrimSpace(sw(t, cfg, "subs", "--json")); out != "[]" {
		t.Fatalf("subs = %q", out)
	}
	if !strings.Contains(sw(t, cfg, "sub", "C0", "--owner", "nope", "--owner-unchecked"), "created") {
		t.Fatal("owner-unchecked was refused")
	}

	useAgents(t, nil, errors.New("agentbus down"))
	stdout.Reset()
	stderr.Reset()
	if code := run([]string{"--config", cfg, "sub", "C1", "--owner", "other"}, &stdout, &stderr); code != 0 || !strings.Contains(stderr.String(), "agentbus") || !strings.Contains(stderr.String(), "accepting --owner other") {
		t.Fatalf("agentbus down sub = %d, stderr %q", code, stderr.String())
	}
}
