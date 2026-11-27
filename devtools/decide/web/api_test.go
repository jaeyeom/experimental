package web

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

func TestMain(m *testing.M) {
	dir, err := os.MkdirTemp("", "web-config")
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	// No test may run a live notify command from the user's config.
	if err := os.Setenv("XDG_CONFIG_HOME", dir); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	code := m.Run()
	_ = os.RemoveAll(dir)
	os.Exit(code)
}

// api is a handler over a temporary ledger with recording fakes.
type api struct {
	t      *testing.T
	dir    string
	h      http.Handler
	server *Server
	mu     sync.Mutex
	sends  [][]string
	bus    [][]string
	org    int
	sendOK bool
}

func newAPI(t *testing.T) *api {
	t.Helper()
	a := &api{t: t, dir: t.TempDir(), sendOK: true}
	a.server = &Server{
		Backend: &tui.LocalBackend{
			Dir: a.dir,
			Now: testNow,
			RunSend: func(argv []string) error {
				a.mu.Lock()
				defer a.mu.Unlock()
				a.sends = append(a.sends, argv)
				if !a.sendOK {
					return errors.New("pane gone")
				}
				return nil
			},
			BusRun: func(_ context.Context, args []string) ([]byte, error) {
				a.mu.Lock()
				defer a.mu.Unlock()
				a.bus = append(a.bus, args)
				switch args[0] {
				case "list":
					return []byte(`[{"name":"coord/primary","when":"reviews PRs","liveness":"live","handle":"herdr:hub/s/p9","liveness_age":42}]`), nil
				case "health":
					return []byte(`{"ok":true}`), nil
				}
				return []byte("m-1 queued"), nil
			},
		},
		Config:   decide.Config{},
		Targets:  []decide.ConfigTarget{{Name: "builder", Pane: "w2:p3"}},
		Agentbus: true,
		Clock:    testNow,
		Display: func() (decide.DisplaySettings, error) {
			return decide.DisplaySettings{Keys: map[string]json.RawMessage{
				"favoriteContacts": json.RawMessage(`["ada"]`),
				"hiddenContacts":   json.RawMessage(`["ghost"]`),
				"contactAliases":   json.RawMessage(`{"ada-old":"ada"}`),
			}}, nil
		},
		OrgConfig: func() (bool, string, error) { return true, filepath.Join(a.dir, "org.yaml"), nil },
		Orgctl: func(_ context.Context, args []string) ([]byte, error) {
			a.mu.Lock()
			defer a.mu.Unlock()
			a.org++
			return []byte(`{"label":"root","args":"` + strings.Join(args[:2], " ") + `"}`), nil
		},
	}
	if err := os.WriteFile(filepath.Join(a.dir, "org.yaml"), []byte("x"), 0o600); err != nil {
		t.Fatal(err)
	}
	h, err := a.server.Handler()
	if err != nil {
		t.Fatal(err)
	}
	a.h = h
	return a
}

func (a *api) create(item *decide.Item) decide.Item {
	a.t.Helper()
	led := a.open()
	if item.Owner == "" {
		item.Owner = "ada"
	}
	if item.Kind == "" {
		item.Kind = decide.KindRelay
	}
	item.State = decide.StateOpen
	got, err := led.Create(item)
	if err != nil {
		a.t.Fatal(err)
	}
	return got
}

func (a *api) get(it decide.Item) decide.Item {
	a.t.Helper()
	led := a.open()
	got, err := led.Get(it.ID)
	if err != nil {
		a.t.Fatal(err)
	}
	return got
}

func (a *api) do(method, path, body string) *httpResult {
	return &httpResult{do(a.h, req{method, path, body, sameOrigin})}
}

type httpResult struct {
	*httptest.ResponseRecorder
}

func (a *api) post(path, body string) *httpResult { return a.do("POST", path, body) }

func (a *api) wantOK(r *httpResult, code int) map[string]any {
	a.t.Helper()
	wantStatus(a.t, r.ResponseRecorder, code)
	return jsonBody(a.t, r.ResponseRecorder)
}

func (a *api) wantErr(r *httpResult, code int) map[string]any {
	a.t.Helper()
	m := a.wantOK(r, code)
	if s, _ := m["error"].(string); s == "" {
		a.t.Fatalf("no error in body %s", r.Body)
	}
	return m
}

func (a *api) sendCount() int {
	a.mu.Lock()
	defer a.mu.Unlock()
	return len(a.sends)
}

func TestSnapshot(t *testing.T) {
	a := newAPI(t)
	relay := a.create(&decide.Item{Question: "ship?", Options: []decide.Option{{Key: "y", Label: "Yes"}}, Unread: true})
	a.create(&decide.Item{Question: "fyi", Class: decide.ClassNote})
	board, err := decide.OpenStatus(a.dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := board.SetAgent("builder", "working", "compiling"); err != nil {
		t.Fatal(err)
	}
	if err := a.server.refreshBus(context.Background()); err != nil {
		t.Fatal(err)
	}
	m := a.wantOK(a.do("GET", "/api/v1/snapshot", ""), 200)
	if items, _ := m["items"].([]any); len(items) != 2 {
		t.Errorf("items = %v", m["items"])
	}
	if inbox, _ := m["inbox"].([]any); len(inbox) != 2 {
		t.Errorf("inbox = %v", m["inbox"])
	}
	if m["unreadCount"] != float64(1) {
		t.Errorf("unreadCount = %v (relay %s)", m["unreadCount"], relay.ID)
	}
	if m["seq"].(float64) < 1 {
		t.Errorf("seq = %v", m["seq"])
	}
	for _, k := range []string{"status", "panels", "dashboardPanels", "drafts", "draftSeq", "display", "targets", "agentbus", "busHealth", "orgTreeEnabled", "serverTime"} {
		if _, ok := m[k]; !ok {
			t.Errorf("snapshot lacks %q: %v", k, m)
		}
	}
	if m["agentbus"] != true || m["orgTreeEnabled"] != true {
		t.Errorf("agentbus/orgTreeEnabled = %v/%v", m["agentbus"], m["orgTreeEnabled"])
	}
	if bh, _ := m["busHealth"].(map[string]any); bh["ok"] != true {
		t.Errorf("busHealth = %v", m["busHealth"])
	}
	if m["serverTime"] != "2026-01-02T03:04:05Z" {
		t.Errorf("serverTime = %v", m["serverTime"])
	}
}

func TestSnapshotNeverRunsBus(t *testing.T) {
	a := newAPI(t)
	a.wantOK(a.do("GET", "/api/v1/snapshot", ""), 200)
	if len(a.bus) != 0 {
		t.Errorf("snapshot ran agentbus: %v", a.bus)
	}
}

func TestRefreshBusChecksDeliveriesOnlyWhenWaiting(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{Question: "q", NotifyPane: "agentbus:ops"})
	if err := a.server.refreshBus(context.Background()); err != nil {
		t.Fatal(err)
	}
	for _, args := range a.bus {
		if args[0] == "status" {
			t.Errorf("checked deliveries with none waiting: %v", args)
		}
	}
	led := a.open()
	if _, err := led.RecordCompose(it.ID, testNow(), decide.Delivery{Target: "agentbus:ops", Outcome: decide.OutcomeWaiting, BusID: "m-1"}); err != nil {
		t.Fatalf("seed waiting delivery: %v", err)
	}
	a.bus = nil
	_ = a.server.refreshBus(context.Background())
	var status bool
	for _, args := range a.bus {
		status = status || args[0] == "status"
	}
	if !status {
		t.Errorf("did not check waiting delivery: %v", a.bus)
	}
}

func TestGetItem(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{Question: "ship?"})
	m := a.wantOK(a.do("GET", "/api/v1/items/"+it.ID, ""), 200)
	if m["question"] != "ship?" {
		t.Errorf("item = %v", m)
	}
	a.wantErr(a.do("GET", "/api/v1/items/D999", ""), 404)
	a.wantErr(a.do("GET", "/api/v1/items/bad%20id", ""), 400)
}

func TestStatus(t *testing.T) {
	a := newAPI(t)
	board, err := decide.OpenStatus(a.dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := board.SetAgent("builder", "working", "compiling"); err != nil {
		t.Fatal(err)
	}
	m := a.wantOK(a.do("GET", "/api/v1/status", ""), 200)
	if agents, _ := m["agents"].([]any); len(agents) != 1 {
		t.Errorf("status = %v", m)
	}
}

func TestContacts(t *testing.T) {
	a := newAPI(t)
	a.create(&decide.Item{Question: "q1", Owner: "ada-old"})
	a.create(&decide.Item{Question: "q2", Owner: "ada"})
	done := a.create(&decide.Item{Question: "q3", Owner: "bob"})
	led := a.open()
	if _, err := led.Withdraw(done.ID, "moot"); err != nil {
		t.Fatal(err)
	}
	m := a.wantOK(a.do("GET", "/api/v1/contacts", ""), 200)
	by := map[string]map[string]any{}
	for _, c := range m["contacts"].([]any) {
		c := c.(map[string]any)
		by[c["name"].(string)] = c
	}
	ada := by["ada"]
	if ada == nil || ada["favorite"] != true || ada["counts"].(map[string]any)["open"] != float64(2) {
		t.Errorf("ada = %v (all %v)", ada, by)
	}
	if _, dup := by["ada-old"]; dup {
		t.Errorf("alias not merged: %v", by)
	}
	if bob := by["bob"]; bob == nil || bob["counts"].(map[string]any)["withdrawn"] != float64(1) {
		t.Errorf("bob = %v", bob)
	}
	if cp := by["coord/primary"]; cp == nil || cp["bus"] != true || cp["liveness"] != "live" {
		t.Errorf("bus contact = %v", cp)
	} else if cp["when"] != "reviews PRs" || cp["livenessAge"] != float64(42) {
		t.Errorf("bus contact when/livenessAge = %v", cp)
	}
	if ada["last"] == nil || ada["last"] == "" {
		t.Errorf("ada last activity missing: %v", ada)
	}
	if by["ghost"] != nil && by["ghost"]["hidden"] != true {
		t.Errorf("ghost = %v", by["ghost"])
	}
}

func TestContactsDegradeWithoutBus(t *testing.T) {
	a := newAPI(t)
	a.server.Backend.BusRun = func(context.Context, []string) ([]byte, error) { return nil, errors.New("no bus") }
	a.create(&decide.Item{Question: "q"})
	m := a.wantOK(a.do("GET", "/api/v1/contacts", ""), 200)
	if m["busError"] == nil || len(m["contacts"].([]any)) != 1 {
		t.Errorf("contacts = %v", m)
	}
}

func TestOrgTreeCachesBySpecModTime(t *testing.T) {
	a := newAPI(t)
	m := a.wantOK(a.do("GET", "/api/v1/org-tree", ""), 200)
	tree, _ := m["tree"].(map[string]any)
	if tree["label"] != "root" || tree["args"] != "tree --json" || m["modTime"] == nil {
		t.Errorf("org tree = %v", m)
	}
	a.wantOK(a.do("GET", "/api/v1/org-tree", ""), 200)
	if a.org != 1 {
		t.Errorf("orgctl ran %d times, want 1", a.org)
	}
	future := time.Now().Add(time.Hour)
	if err := os.Chtimes(filepath.Join(a.dir, "org.yaml"), future, future); err != nil {
		t.Fatal(err)
	}
	a.wantOK(a.do("GET", "/api/v1/org-tree", ""), 200)
	if a.org != 2 {
		t.Errorf("orgctl ran %d times after spec change, want 2", a.org)
	}
}

func TestOrgTreeOffAndFailing(t *testing.T) {
	a := newAPI(t)
	a.server.OrgConfig = func() (bool, string, error) { return false, "", nil }
	h, _ := a.server.Handler()
	a.h = h
	a.wantErr(a.do("GET", "/api/v1/org-tree", ""), 404)

	b := newAPI(t)
	b.server.Orgctl = func(context.Context, []string) ([]byte, error) { return nil, errors.New("orgctl: boom") }
	h, _ = b.server.Handler()
	b.h = h
	b.wantErr(b.do("GET", "/api/v1/org-tree", ""), 502)
}

func TestSearch(t *testing.T) {
	a := newAPI(t)
	a.create(&decide.Item{Question: "deploy the server"})
	a.create(&decide.Item{Question: "lunch plans", Class: decide.ClassNote})
	snoozed := a.create(&decide.Item{Question: "deploy later"})
	led := a.open()
	if _, err := led.Snooze(snoozed.ID, testNow().Add(time.Hour)); err != nil {
		t.Fatal(err)
	}
	ids := func(path string) int {
		m := a.wantOK(a.do("GET", path, ""), 200)
		return len(m["items"].([]any))
	}
	if n := ids("/api/v1/search?q=deploy"); n != 1 {
		t.Errorf("inbox deploy = %d", n)
	}
	if n := ids("/api/v1/search?q=deploy&folder=snoozed"); n != 1 {
		t.Errorf("snoozed deploy = %d", n)
	}
	if n := ids("/api/v1/search?class=note"); n != 1 {
		t.Errorf("note class = %d", n)
	}
	if n := ids("/api/v1/search?q=nothing"); n != 0 {
		t.Errorf("no match = %d", n)
	}
	a.wantErr(a.do("GET", "/api/v1/search?folder=bogus", ""), 400)
	a.wantErr(a.do("GET", "/api/v1/search?class=bogus", ""), 400)
	a.wantErr(a.do("GET", "/api/v1/search?mode=bogus", ""), 400)
	a.wantErr(a.do("GET", "/api/v1/search?q="+strings.Repeat("x", 2000), ""), 400)
}

func TestSearchLogMode(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{Question: "use postgres?", Class: decide.ClassDecision, Options: []decide.Option{{Key: "y", Label: "Yes"}}})
	a.create(&decide.Item{Question: "use postgres too?", Class: decide.ClassDecision})
	a.wantOK(a.post("/api/v1/items/"+it.ID+"/answer", `{"value":"y"}`), 200)
	m := a.wantOK(a.do("GET", "/api/v1/search?mode=log&q=postgres&class=decision", ""), 200)
	if items := m["items"].([]any); len(items) != 1 {
		t.Errorf("log search = %v", items)
	}
}

func TestMarkRead(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{Question: "q", Unread: true})
	m := a.wantOK(a.post("/api/v1/items/"+it.ID+"/read", ""), 200)
	if m["item"].(map[string]any)["unread"] != false || a.get(it).Unread {
		t.Errorf("not read: %v", m)
	}
	a.wantErr(a.post("/api/v1/items/D999/read", ""), 404)
}

func TestAnswerRecordsViaUIAndNote(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{Question: "ship?", Options: []decide.Option{{Key: "y", Label: "Yes"}}, NotifyPane: "w1:p1"})
	writtenAt := "2026-01-02T03:00:00Z"
	m := a.wantOK(a.post("/api/v1/items/"+it.ID+"/answer", `{"value":"y","note":"but be careful","writtenAt":"`+writtenAt+`"}`), 200)
	if _, ok := m["item"]; !ok || m["notifyError"] != nil {
		t.Fatalf("body = %v", m)
	}
	got := a.get(it)
	if got.Answer == nil || got.Answer.Via != decide.ViaUI {
		t.Fatalf("answer = %+v", got.Answer)
	}
	last := got.Messages[len(got.Messages)-1]
	if last.Text != "but be careful" || last.Via != decide.ViaUI {
		t.Errorf("note message = %+v", last)
	}
	if last.WrittenAt == nil || last.WrittenAt.Format(time.RFC3339) != writtenAt {
		t.Errorf("writtenAt = %v", last.WrittenAt)
	}
}

func TestAnswerValidation(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{Question: "q", Options: []decide.Option{{Key: "y", Label: "Yes"}}})
	p := "/api/v1/items/" + it.ID + "/answer"
	a.wantErr(a.post(p, `{}`), 400)
	a.wantErr(a.post(p, `{"value":""}`), 400)
	a.wantErr(a.post(p, `{"value":`), 400)
	a.wantErr(a.post(p, `{"value":"y","bogus":1}`), 400)
	a.wantErr(a.post(p, `{"value":"y","writtenAt":"yesterday"}`), 400)
	a.wantErr(a.post(p, `{"value":"y"} {"value":"y"}`), 400)
	a.wantErr(a.post("/api/v1/items/D999/answer", `{"value":"y"}`), 404)
	big := `{"value":"` + strings.Repeat("a", 2<<20) + `"}`
	a.wantErr(a.post(p, big), 413)
	if a.get(it).Answer != nil {
		t.Error("a rejected request answered the item")
	}
}

func TestAnswerClosedItemIsRefused(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{
		Question:   "post it?",
		Owner:      "ada",
		Options:    []decide.Option{{Key: "yes", Label: "Yes"}, {Key: "no", Label: "No"}},
		NotifyPane: "w1:p1",
	})
	led, err := decide.Open(a.dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Done(it.ID, "Not posted: cancelled."); err != nil {
		t.Fatal(err)
	}
	p := "/api/v1/items/" + it.ID + "/answer"
	// Direct answer without conflictCheck.
	w := a.post(p, `{"value":"yes"}`)
	if w.Code != http.StatusBadRequest || !strings.Contains(w.Body.String(), "already closed by ada") {
		t.Fatalf("direct answer = %d %s, want 400 with already closed by ada", w.Code, w.Body.String())
	}
	// Answer with stale open conflictCheck.
	w = a.post(p, `{"value":"yes","conflictCheck":{"baseState":"open","baseMessageCount":1,"baseAnswered":false}}`)
	if w.Code != http.StatusConflict || !strings.Contains(w.Body.String(), "already closed by ada") {
		t.Fatalf("stale answer = %d %s, want 409 with already closed by ada", w.Code, w.Body.String())
	}
	if a.sendCount() != 0 {
		t.Fatalf("sendCount = %d, want 0", a.sendCount())
	}
	if got := a.get(it); got.State != decide.StateDoneUnread || got.Answer != nil {
		t.Fatalf("item = state %s answer %+v", got.State, got.Answer)
	}
}

func TestAnsweringNestedApprovalNotesTheParent(t *testing.T) {
	a := newAPI(t)
	parent := a.create(&decide.Item{Question: "deploy?", NotifyPane: "w1:p1"})
	card := a.create(&decide.Item{
		Question: "run rm -rf?", Verbatim: "rm -rf /tmp/x", Class: decide.ClassApproval, Parent: parent.ID,
		Options: []decide.Option{{Key: "yes", Label: "Approve"}, {Key: "no", Label: "Decline"}}, NotifyPane: "w1:p1",
	})
	before := len(a.get(parent).Messages)
	a.wantOK(a.post("/api/v1/items/"+card.ID+"/answer", `{"value":"yes"}`), 200)
	got := a.get(parent)
	if len(got.Messages) <= before {
		t.Fatalf("parent has no approval line: %+v", got)
	}
}

func TestReplyRouteAndNotifyError(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{Question: "q", NotifyPane: "w1:p1"})
	p := "/api/v1/items/" + it.ID + "/reply"
	m := a.wantOK(a.post(p, `{"text":"why?"}`), 200)
	if m["item"] == nil || a.sendCount() != 1 {
		t.Fatalf("reply = %v, sends %d", m, a.sendCount())
	}
	if msgs := a.get(it).Messages; msgs[len(msgs)-1].Via != decide.ViaUI {
		t.Errorf("via = %+v", msgs[len(msgs)-1])
	}
	a.sendOK = false
	m = a.wantOK(a.post(p, `{"text":"again","route":"owner"}`), 200)
	if s, _ := m["notifyError"].(string); !strings.Contains(s, "pane gone") || m["item"] == nil {
		t.Errorf("notify failure body = %v", m)
	}
	if msgs := a.get(it).Messages; msgs[len(msgs)-1].Text != "again" {
		t.Error("notify failure lost the saved message")
	}
	a.wantErr(a.post(p, `{"text":""}`), 400)
	a.wantErr(a.post(p, `{"text":"x","route":"nobody"}`), 400)
}

func TestArchive(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{Question: "fyi", Class: decide.ClassNote})
	p := "/api/v1/items/" + it.ID + "/archive"
	a.wantErr(a.post(p, `{}`), 400)
	stale := it.CreatedAt.Add(-time.Hour).Format(time.RFC3339)
	led := a.open()
	if _, err := led.Reply(it.ID, "ops/bot", "newer", nil, false); err != nil {
		t.Fatal(err)
	}
	m := a.wantErr(a.post(p, `{"seen":"`+stale+`"}`), 409)
	if m["code"] != "newer_message" || m["error"] != "new message: not archived" {
		t.Errorf("409 body = %v", m)
	}
	got := a.get(it)
	seen := got.LastMessageAt().Format(time.RFC3339Nano)
	m = a.wantOK(a.post(p, `{"seen":"`+seen+`"}`), 200)
	if m["item"].(map[string]any)["outcome"] != "archived" {
		t.Errorf("archive = %v", m)
	}
}

func TestDone(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{Question: "q", NotifyPane: "w1:p1"})
	p := "/api/v1/items/" + it.ID + "/done"
	a.wantErr(a.post(p, `{"outcome":"`+strings.Repeat("x", maxText+1)+`"}`), 400)
	m := a.wantOK(a.post(p, `{"outcome":"shipped"}`), 200)
	if i := m["item"].(map[string]any); i["state"] != decide.StateDone || i["outcome"] != "shipped" {
		t.Errorf("done = %v", i)
	}
}

func TestSnooze(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{Question: "q"})
	p := "/api/v1/items/" + it.ID + "/snooze"
	until := testNow().Add(2 * time.Hour).Format(time.RFC3339)
	a.wantOK(a.post(p, `{"until":"`+until+`"}`), 200)
	if got := a.get(it); got.SnoozedUntil == nil || got.SnoozedUntil.Format(time.RFC3339) != until {
		t.Errorf("snoozedUntil = %v", got.SnoozedUntil)
	}
	for preset, want := range map[string]time.Time{
		"1h": testNow().Add(time.Hour), "3h": testNow().Add(3 * time.Hour),
	} {
		a.wantOK(a.post(p, `{"preset":"`+preset+`"}`), 200)
		if got := a.get(it); got.SnoozedUntil == nil || !got.SnoozedUntil.Equal(want) {
			t.Errorf("preset %s: %v, want %v", preset, got.SnoozedUntil, want)
		}
	}
	presets := decide.SnoozePresets(testNow().In(time.Local))
	for preset, want := range map[string]time.Time{"tomorrow9": presets[2].At, "nextmon9": presets[3].At} {
		a.wantOK(a.post(p, `{"preset":"`+preset+`"}`), 200)
		if got := a.get(it); got.SnoozedUntil == nil || !got.SnoozedUntil.Equal(want) {
			t.Errorf("preset %s: %v, want %v", preset, got.SnoozedUntil, want)
		}
	}
	a.wantErr(a.post(p, `{}`), 400)
	a.wantErr(a.post(p, `{"until":"`+until+`","preset":"1h"}`), 400)
	a.wantErr(a.post(p, `{"preset":"soon"}`), 400)
	a.wantErr(a.post(p, `{"until":"tomorrow"}`), 400)
	a.wantErr(a.post(p, `{"until":"`+testNow().Add(-time.Hour).Format(time.RFC3339)+`"}`), 400)
	m := a.wantOK(a.post("/api/v1/items/"+it.ID+"/unsnooze", ""), 200)
	if m["item"].(map[string]any)["snoozedUntil"] != nil {
		t.Errorf("unsnooze = %v", m)
	}
}

func TestArchiveAllNotesRemoved(t *testing.T) {
	a := newAPI(t)
	n := a.create(&decide.Item{Question: "n1", Class: decide.ClassNote})
	seen := testNow().Add(time.Hour).Format(time.RFC3339)
	wantStatus(t, a.post("/api/v1/notes/archive-all", `{"seen":"`+seen+`"}`).ResponseRecorder, 404)
	if a.get(n).State != decide.StateOpen {
		t.Error("removed archive-all endpoint changed note state")
	}
}

func TestSend(t *testing.T) {
	a := newAPI(t)
	m := a.wantOK(a.post("/api/v1/send", `{"to":"builder","text":"build it"}`), 200)
	if m["ok"] != true || a.sendCount() != 1 {
		t.Errorf("send = %v, sends %d", m, a.sendCount())
	}
	a.wantOK(a.post("/api/v1/send", `{"to":"coord/primary","bus":true,"text":"hello"}`), 200)
	a.wantErr(a.post("/api/v1/send", `{"to":"stranger","text":"hi"}`), 400)
	a.wantErr(a.post("/api/v1/send", `{"text":"hi"}`), 400)
	a.wantErr(a.post("/api/v1/send", `{"to":"builder","text":""}`), 400)
}

func TestFavorites(t *testing.T) {
	a := newAPI(t)
	m := a.wantOK(a.post("/api/v1/contacts/favorites", `{"names":["ada","bob","ada"," "]}`), 200)
	if m["display"] == nil {
		t.Errorf("favorites = %v", m)
	}
	f, err := decide.LoadConfigFile()
	if err != nil {
		t.Fatal(err)
	}
	var got []string
	if err := json.Unmarshal(f.Keys["favoriteContacts"], &got); err != nil || strings.Join(got, ",") != "ada,bob" {
		t.Errorf("stored favorites = %s (%v)", f.Keys["favoriteContacts"], err)
	}
	a.wantErr(a.post("/api/v1/contacts/favorites", `{}`), 400)
	a.wantErr(a.post("/api/v1/contacts/favorites", `{"names":"ada"}`), 400)
}

func TestConflictCheck(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{Question: "q", Options: []decide.Option{{Key: "y", Label: "Yes"}}, NotifyPane: "w1:p1"})
	led := a.open()
	if _, err := led.Reply(it.ID, "ops/bot", "news", nil, false); err != nil {
		t.Fatal(err)
	}
	body := `{"value":"y","conflictCheck":{"baseState":"open","baseMessageCount":0}}`
	m := a.wantErr(a.post("/api/v1/items/"+it.ID+"/answer", body), 409)
	if m["conflict"] != "new message arrived" || m["item"] == nil {
		t.Errorf("conflict body = %v", m)
	}
	if a.get(it).Answer != nil {
		t.Error("conflicting answer applied")
	}
	fresh := fmt.Sprintf(`{"value":"y","conflictCheck":{"baseState":"open","baseMessageCount":%d}}`, len(a.get(it).Messages))
	a.wantOK(a.post("/api/v1/items/"+it.ID+"/answer", fresh), 200)
}

func TestDrafts(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{Question: "q"})
	key := "reply:" + it.ID
	put := func(text string, base int) *httpResult {
		b, _ := json.Marshal(map[string]any{"draft": map[string]any{"key": key, "text": text}, "baseRev": base})
		return a.do("PUT", "/api/v1/drafts/"+key, string(b))
	}
	m := a.wantOK(put("first", 0), 200)
	d := m["draft"].(map[string]any)
	if m["draftConflict"] != false || d["rev"] != float64(1) || d["text"] != "first" || d["itemId"] != it.ID {
		t.Errorf("put = %v", m)
	}
	a.wantOK(put("second", 1), 200)
	m = a.wantOK(put("stale", 1), 409)
	d = m["draft"].(map[string]any)
	if m["draftConflict"] != true || len(d["conflicts"].([]any)) != 1 {
		t.Errorf("conflict = %v", m)
	}
	conflictID := d["conflicts"].([]any)[0].(map[string]any)["id"].(string)
	m = a.wantOK(a.do("DELETE", "/api/v1/drafts/"+key+"?conflictId="+conflictID, ""), 200)
	left, ok := m["draft"].(map[string]any)
	if !ok || left["text"] != "second" || left["rev"] == nil || left["conflicts"] != nil {
		t.Errorf("conflict delete should return the draft left behind, got %v", m)
	}
	ds, _ := decide.OpenDraftStore(a.dir)
	if _, drafts, _ := ds.List(); len(drafts) != 1 || len(drafts[0].Conflicts) != 0 {
		t.Errorf("after conflict delete = %+v", drafts)
	}
	if m = a.wantOK(a.do("DELETE", "/api/v1/drafts/"+key, ""), 200); m["draft"] != nil {
		t.Errorf("whole delete returned a draft: %v", m)
	}
	if _, drafts, _ := ds.List(); len(drafts) != 0 {
		t.Errorf("after delete = %+v", drafts)
	}
	b, _ := json.Marshal(map[string]any{"draft": map[string]any{"key": "other", "text": "x"}})
	a.wantErr(a.do("PUT", "/api/v1/drafts/"+key, string(b)), 400)
	a.wantErr(a.do("PUT", "/api/v1/drafts/"+key, `{}`), 400)
	a.wantErr(a.do("PUT", "/api/v1/drafts/"+key, `{"draft":{"text":"x"},"baseRev":-1}`), 400)
	huge, _ := json.Marshal(map[string]any{"draft": map[string]any{"key": key, "text": strings.Repeat("x", decide.MaxDraftTextBytes+1)}})
	a.wantErr(a.do("PUT", "/api/v1/drafts/"+key, string(huge)), 413)
}

// every protected route, with a body that would otherwise be valid.
var apiRoutes = []req{
	{"GET", "/api/v1/snapshot", "", nil},
	{"GET", "/api/v1/items/D1", "", nil},
	{"GET", "/api/v1/status", "", nil},
	{"GET", "/api/v1/contacts", "", nil},
	{"GET", "/api/v1/org-tree", "", nil},
	{"GET", "/api/v1/search?q=x", "", nil},
	{"GET", "/api/v1/keymap", "", nil},
	{"POST", "/api/v1/items/D1/read", "", nil},
	{"POST", "/api/v1/items/D1/answer", `{"value":"y"}`, nil},
	{"POST", "/api/v1/items/D1/reply", `{"text":"x"}`, nil},
	{"POST", "/api/v1/items/D1/archive", `{"seen":"2026-01-02T03:04:05Z"}`, nil},
	{"POST", "/api/v1/items/D1/done", `{"outcome":"x"}`, nil},
	{"POST", "/api/v1/items/D1/snooze", `{"preset":"1h"}`, nil},
	{"POST", "/api/v1/items/D1/unsnooze", "", nil},
	{"POST", "/api/v1/send", `{"to":"builder","text":"x"}`, nil},
	{"POST", "/api/v1/contacts/favorites", `{"names":[]}`, nil},
	{"PUT", "/api/v1/drafts/k", `{"draft":{"key":"k","text":"x"}}`, nil},
	{"DELETE", "/api/v1/drafts/k", "", nil},
}

func TestAPIRequiresAuth(t *testing.T) {
	a := newAPI(t)
	t.Setenv(TokenEnv, testToken)
	a.server.Config.Web.Auth.Mode = decide.WebAuthToken
	h, err := a.server.Handler()
	if err != nil {
		t.Fatal(err)
	}
	for _, r := range apiRoutes {
		r.header = sameOrigin
		w := do(h, r)
		if w.Code != 401 {
			t.Errorf("%s %s unauthenticated = %d, want 401", r.method, r.path, w.Code)
		}
		r.header = with(sameOrigin, "Authorization", "Bearer "+testToken)
		if w := do(h, r); w.Code == 401 || w.Code == 403 {
			t.Errorf("%s %s with bearer = %d", r.method, r.path, w.Code)
		}
	}
}

func TestMutationsRequireSameOrigin(t *testing.T) {
	a := newAPI(t)
	it := a.create(&decide.Item{Question: "q", Unread: true})
	for _, r := range apiRoutes {
		if r.method == "GET" {
			continue
		}
		w := do(a.h, r)
		if w.Code != 403 {
			t.Errorf("%s %s without Sec-Fetch-Site = %d, want 403", r.method, r.path, w.Code)
		}
	}
	w := do(a.h, req{"POST", "/api/v1/items/" + it.ID + "/read", "", map[string]string{"Origin": "https://evil.example"}})
	wantStatus(t, w, 403)
	if !a.get(it).Unread {
		t.Error("a cross-origin write changed the item")
	}
}

func TestWrongMethodIs405(t *testing.T) {
	a := newAPI(t)
	wantStatus(t, do(a.h, req{"GET", "/api/v1/items/D1/read", "", sameOrigin}), 405)
}

func TestKeymapDefaultAndCustom(t *testing.T) {
	a := newAPI(t)
	m := a.wantOK(a.do("GET", "/api/v1/keymap", ""), 200)
	if m["chordTimeoutMs"] != float64(1000) {
		t.Errorf("chordTimeoutMs = %v, want 1000", m["chordTimeoutMs"])
	}
	if m["asciiKeys"] != false {
		t.Errorf("asciiKeys = %v, want false", m["asciiKeys"])
	}
	actions, _ := m["actions"].([]any)
	catalog := decide.ActionCatalog()
	if len(actions) != len(catalog) {
		t.Fatalf("len(actions) = %d, want %d", len(actions), len(catalog))
	}
	byName := map[string]map[string]any{}
	for _, raw := range actions {
		act := raw.(map[string]any)
		byName[act["name"].(string)] = act
	}
	if got := byName["goto.inbox"]["keys"].([]any); len(got) != 1 || got[0] != "g i" {
		t.Errorf("goto.inbox keys = %v, want [g i]", got)
	}
	if got := byName["compose.send"]["buttonKeys"].([]any); len(got) == 0 || got[0] != "^⏎" {
		t.Errorf("compose.send buttonKeys = %v, want [^⏎ ...]", got)
	}

	// Custom bindings and asciiKeys from config apply on the web endpoint.
	a.server.Config.UI.Keys = map[string][]string{
		"list.archive": {"x"},
		"goto.inbox":   {"z i"},
	}
	a.server.Config.UI.ASCIIKeys = true
	m = a.wantOK(a.do("GET", "/api/v1/keymap", ""), 200)
	if m["asciiKeys"] != true {
		t.Errorf("asciiKeys = %v, want true", m["asciiKeys"])
	}
	byName = map[string]map[string]any{}
	for _, raw := range m["actions"].([]any) {
		act := raw.(map[string]any)
		byName[act["name"].(string)] = act
	}
	if got := byName["list.archive"]["keys"].([]any); len(got) != 1 || got[0] != "x" {
		t.Errorf("custom list.archive keys = %v, want [x]", got)
	}
	if got := byName["goto.inbox"]["keys"].([]any); len(got) != 1 || got[0] != "z i" {
		t.Errorf("custom goto.inbox keys = %v, want [z i]", got)
	}
	if got := byName["compose.send"]["buttonKeys"].([]any); len(got) == 0 || got[0] != "C-Enter" {
		t.Errorf("ascii compose.send buttonKeys = %v, want [C-Enter ...]", got)
	}
}

func TestKeymapRequiresSession(t *testing.T) {
	a := newAPI(t)
	t.Setenv(TokenEnv, testToken)
	a.server.Config.Web.Auth.Mode = decide.WebAuthToken
	h, err := a.server.Handler()
	if err != nil {
		t.Fatal(err)
	}
	// Unauthenticated request is rejected with 401.
	w := do(h, req{"GET", "/api/v1/keymap", "", sameOrigin})
	wantStatus(t, w, http.StatusUnauthorized)

	// Log in to obtain a session cookie, then fetch keymap with that session.
	login := do(h, req{"POST", "/api/v1/auth/login", `{"token":"` + testToken + `"}`, sameOrigin})
	wantStatus(t, login, http.StatusOK)
	cookies := login.Result().Cookies()
	if len(cookies) == 0 {
		t.Fatal("login did not set session cookie")
	}
	r := httptest.NewRequest(http.MethodGet, "/api/v1/keymap", nil)
	r.AddCookie(cookies[0])
	rec := httptest.NewRecorder()
	h.ServeHTTP(rec, r)
	wantStatus(t, rec, http.StatusOK)
}

func TestLongUnbreakableWordsThroughWebAPI(t *testing.T) {
	a := newAPI(t)
	url300 := "https://example.com/" + strings.Repeat("a", 280)
	longPath := "/Users/developer/workspace/github.com/org/project/internal/service/handler/very_long_file_name_for_testing_paths.go"
	tok41 := strings.Repeat("c", 41)

	it := a.create(&decide.Item{Question: "Review " + longPath, NotifyPane: "w1:p1"})

	// Web API writes are human UI writes (ViaUI) and accept long URLs, paths, and tokens.
	for _, tc := range []struct {
		name string
		path string
		body map[string]any
	}{
		{name: "reply 300-char URL", path: "/api/v1/items/" + it.ID + "/reply", body: map[string]any{"text": url300}},
		{name: "reply long path first line", path: "/api/v1/items/" + it.ID + "/reply", body: map[string]any{"text": "See " + longPath}},
		{name: "reply 41-char token", path: "/api/v1/items/" + it.ID + "/reply", body: map[string]any{"text": "token " + tok41}},
		{name: "send 300-char URL", path: "/api/v1/send", body: map[string]any{"to": "builder", "text": url300}},
		{name: "done long path", path: "/api/v1/items/" + it.ID + "/done", body: map[string]any{"outcome": "Fixed " + longPath}},
	} {
		raw, err := json.Marshal(tc.body)
		if err != nil {
			t.Fatalf("%s: %v", tc.name, err)
		}
		a.wantOK(a.post(tc.path, string(raw)), 200)
	}
}

func TestInlineCodeSpansThroughWebAPI(t *testing.T) {
	a := newAPI(t)
	cmdSpan := "`decide send --to alpha --text-file /path/to/message.txt --flag-one value --flag-two other`"
	firstLineWithSpan := "Run " + cmdSpan + " to notify alpha"

	it := a.create(&decide.Item{Question: firstLineWithSpan, NotifyPane: "w1:p1"})

	for _, tc := range []struct {
		name string
		path string
		body map[string]any
	}{
		{name: "reply inline code span", path: "/api/v1/items/" + it.ID + "/reply", body: map[string]any{"text": firstLineWithSpan}},
		{name: "send inline code span", path: "/api/v1/send", body: map[string]any{"to": "builder", "text": firstLineWithSpan}},
		{name: "done inline code span", path: "/api/v1/items/" + it.ID + "/done", body: map[string]any{"outcome": "Ran " + cmdSpan}},
	} {
		raw, err := json.Marshal(tc.body)
		if err != nil {
			t.Fatalf("%s: %v", tc.name, err)
		}
		a.wantOK(a.post(tc.path, string(raw)), 200)
	}
}

func TestWebAPIRecordsAllFourAnswerTypes(t *testing.T) {
	a := newAPI(t)

	optItem := a.create(&decide.Item{
		Question:   "pick?",
		Owner:      "ada",
		FreeText:   true,
		Options:    []decide.Option{{Key: "y", Label: "Yes"}, {Key: "n", Label: "No"}},
		NotifyPane: "w1:p1",
	})
	ftOptItem := a.create(&decide.Item{
		Question:   "pick or type?",
		Owner:      "bea",
		FreeText:   true,
		Options:    []decide.Option{{Key: "a", Label: "Alpha"}, {Key: "b", Label: "Beta"}},
		NotifyPane: "w1:p1",
	})
	ftOnlyItem := a.create(&decide.Item{
		Question:   "which env?",
		Owner:      "carl",
		Class:      decide.ClassFact,
		FreeText:   true,
		NotifyPane: "w1:p1",
	})
	noteItem := a.create(&decide.Item{
		Question:   "fyi",
		Owner:      "dana",
		Class:      decide.ClassNote,
		NotifyPane: "w1:p1",
	})

	// 1. option
	a.wantOK(a.post("/api/v1/items/"+optItem.ID+"/answer", `{"value":"y"}`), 200)
	gotOpt := a.get(optItem)
	if gotOpt.Answer == nil || gotOpt.Answer.AnswerType != decide.AnswerTypeOption || gotOpt.Answer.AskingAgent != "ada" {
		t.Fatalf("web option Answer = %+v", gotOpt.Answer)
	}
	if last := gotOpt.Messages[len(gotOpt.Messages)-1]; last.AnswerType != decide.AnswerTypeOption || last.AskingAgent != "ada" {
		t.Fatalf("web option Message = %+v", last)
	}

	// 2. free-text-over-options
	a.wantOK(a.post("/api/v1/items/"+ftOptItem.ID+"/answer", `{"value":"neither"}`), 200)
	gotFTO := a.get(ftOptItem)
	if gotFTO.Answer == nil || gotFTO.Answer.AnswerType != decide.AnswerTypeFreeTextOverOptions || gotFTO.Answer.AskingAgent != "bea" {
		t.Fatalf("web free-text-over-options Answer = %+v", gotFTO.Answer)
	}
	if last := gotFTO.Messages[len(gotFTO.Messages)-1]; last.AnswerType != decide.AnswerTypeFreeTextOverOptions || last.AskingAgent != "bea" {
		t.Fatalf("web free-text-over-options Message = %+v", last)
	}

	// 3. free-text-only
	a.wantOK(a.post("/api/v1/items/"+ftOnlyItem.ID+"/answer", `{"value":"prod"}`), 200)
	gotFTOnly := a.get(ftOnlyItem)
	if gotFTOnly.Answer == nil || gotFTOnly.Answer.AnswerType != decide.AnswerTypeFreeTextOnly || gotFTOnly.Answer.AskingAgent != "carl" {
		t.Fatalf("web free-text-only Answer = %+v", gotFTOnly.Answer)
	}
	if last := gotFTOnly.Messages[len(gotFTOnly.Messages)-1]; last.AnswerType != decide.AnswerTypeFreeTextOnly || last.AskingAgent != "carl" {
		t.Fatalf("web free-text-only Message = %+v", last)
	}

	// 4. reply-no-question
	a.wantOK(a.post("/api/v1/items/"+noteItem.ID+"/reply", `{"text":"noted"}`), 200)
	gotNote := a.get(noteItem)
	if gotNote.Answer != nil {
		t.Fatalf("web note Answer = %+v, want nil", gotNote.Answer)
	}
	if last := gotNote.Messages[len(gotNote.Messages)-1]; last.AnswerType != decide.AnswerTypeReplyNoQuestion || last.AskingAgent != "" {
		t.Fatalf("web reply-no-question Message = %+v", last)
	}
}

// open is the ledger on the test clock.
func (a *api) open() *decide.Ledger {
	a.t.Helper()
	led, err := decide.Open(a.dir)
	if err != nil {
		a.t.Fatal(err)
	}
	led.SetClock(testNow)
	return led
}
