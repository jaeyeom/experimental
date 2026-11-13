package main

import (
	"context"
	"encoding/json"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/client"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

func TestQueueHoldsThenDeliversAndSurvivesRestart(t *testing.T) {
	runtime := shortRuntime(t)
	stateHome := t.TempDir()
	home := t.TempDir()
	base := []string{
		"XDG_RUNTIME_DIR=" + runtime,
		"XDG_STATE_HOME=" + stateHome,
		"HOME=" + home,
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	holder := "herdr:local/default/w1:p1"
	reviewer := "herdr:local/default/w1:p2"
	bare := "herdr:local/default/w9:p9"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach: map[string]adapter.State{
			holder:   adapter.StateIdle,
			reviewer: adapter.StateBlocked,
			bare:     adapter.StateNoAgent,
		},
		Result: map[string]adapter.Result{
			holder:   adapter.ResultAccepted,
			reviewer: adapter.ResultAccepted,
		},
		PaneHandles: map[string]string{"p1": holder, "p2": reviewer},
		InventoryAgents: []adapter.Agent{
			{Handle: holder, PaneID: "p1", SessionID: "default", Status: "idle"},
			{Handle: reviewer, PaneID: "p2", SessionID: "default", Status: "blocked"},
		},
	}
	cfg := daemon.Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour}
	stop := startDaemon(t, dirs, cfg)
	env := func(pane string) []string {
		return append(append([]string{}, base...), "HERDR_PANE_ID="+pane)
	}
	mustCmd(t, []string{"register", "ops/inbox", "--when", "reviews"}, env("p1"))
	mustCmd(t, []string{"register", "eng/reviewer", "--when", "reviews"}, env("p2"))

	if code, _, stderr := runCmd([]string{"send", "missing/name", "nope"}, env("p1")); code == 0 || !strings.Contains(stderr, "try agentbus search") {
		t.Fatalf("unknown name = %d %q", code, stderr)
	}
	bareSent, _ := mustCmd(t, []string{"send", bare, "bare-shell-body"}, env("p1"))
	if !strings.Contains(bareSent, "held: no-agent") || len(ad.Texts) != 0 {
		t.Fatalf("bare send = %q texts %q", bareSent, ad.Texts)
	}

	first, _ := mustCmd(t, []string{"send", "eng/reviewer", "first-body"}, env("p1"))
	second, _ := mustCmd(t, []string{"send", "eng/reviewer", "second-body"}, env("p1"))
	if !strings.Contains(first, "held: blocked") || !strings.Contains(second, "held: waiting for an earlier message") {
		t.Fatalf("queued = %q / %q", first, second)
	}
	if len(ad.Texts) != 0 {
		t.Fatalf("pasted while blocked: %q", ad.Texts)
	}
	firstID, _, ok := strings.Cut(strings.TrimSpace(first), " ")
	if !ok {
		t.Fatalf("first send = %q", first)
	}
	listed, _ := mustCmd(t, []string{"list"}, base)
	if !strings.Contains(listed, "held "+firstID) || !strings.Contains(listed, "waited") || !strings.Contains(listed, "name: ops/inbox") {
		t.Fatalf("list = %q", listed)
	}
	status, _ := mustCmd(t, []string{"status", firstID}, base)
	if !strings.Contains(status, "state: held: blocked") || !strings.Contains(status, "waited:") {
		t.Fatalf("status = %q", status)
	}
	if !logHas(t, dirs, eventlog.EventHeld, "waited") {
		t.Fatal("held log has no waited detail")
	}

	ad.Reach[reviewer] = adapter.StateIdle
	status, _ = mustCmd(t, []string{"status", firstID}, base)
	if !strings.Contains(status, "state: delivered") {
		t.Fatalf("status after idle = %q", status)
	}
	if len(ad.Texts) != 2 || !strings.Contains(ad.Texts[0], "first-body") || !strings.Contains(ad.Texts[1], "second-body") {
		t.Fatalf("delivery order = %q", ad.Texts)
	}

	ad.Reach[reviewer] = adapter.StateBlocked
	expired, _ := mustCmd(t, []string{"send", "--ttl", "200ms", "--wait", "eng/reviewer", "ttl-body"}, env("p1"))
	expiredID, expiredState, ok := strings.Cut(strings.TrimSpace(expired), " ")
	if !ok || expiredState != "expired" {
		t.Fatalf("ttl send = %q", expired)
	}
	if textsContain(ad.Texts, "ttl-body") || !textsContain(ad.Texts, "message "+expiredID+" to eng/reviewer expired") {
		t.Fatalf("ttl texts = %q", ad.Texts)
	}
	if !logHas(t, dirs, eventlog.EventExpired, "ttl") {
		t.Fatal("expired log missing")
	}

	held, _ := mustCmd(t, []string{"send", "eng/reviewer", "survive-body"}, env("p1"))
	heldID, heldState, ok := strings.Cut(strings.TrimSpace(held), " ")
	if !ok || !strings.HasPrefix(heldState, "held:") {
		t.Fatalf("survive send = %q", held)
	}
	stop()
	before := len(ad.Texts)
	stop = startDaemon(t, dirs, cfg)
	defer stop()
	status, _ = mustCmd(t, []string{"status", heldID}, base)
	if !strings.Contains(status, "state: held:") || len(ad.Texts) != before || textsContain(ad.Texts, "survive-body") {
		t.Fatalf("after restart status %q texts %q", status, ad.Texts)
	}
	ad.Reach[reviewer] = adapter.StateIdle
	status, _ = mustCmd(t, []string{"status", heldID}, base)
	if !strings.Contains(status, "state: delivered") || !textsContain(ad.Texts, "survive-body") {
		t.Fatalf("delivered after restart = %q texts %q", status, ad.Texts)
	}

	ad.Result[reviewer] = adapter.ResultRefusedRetryable
	code, stdout, stderr := runCmd([]string{"send", "eng/reviewer", "refused-body"}, env("p1"))
	refusedID, refusedState, ok := strings.Cut(strings.TrimSpace(stdout), " ")
	if code != 0 || !ok || refusedState != "queued for delivery" {
		t.Fatalf("refusal = %d %q %q", code, stdout, stderr)
	}
	refused, _ := mustCmd(t, []string{"status", refusedID}, base)
	if !strings.Contains(refused, "state: held: transport refused") {
		t.Fatalf("refusal status = %q", refused)
	}
}

func TestQueueHoldsForUserDraft(t *testing.T) {
	runtime := shortRuntime(t)
	base := []string{
		"XDG_RUNTIME_DIR=" + runtime,
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	sender := "herdr:local/default/w1:p1"
	target := "herdr:local/default/w1:p2"
	var mu sync.Mutex
	composer := map[string][]string{} // handle -> hold reasons for the next reads; "" is empty
	setComposer := func(handle string, reasons ...string) {
		mu.Lock()
		defer mu.Unlock()
		composer[handle] = reasons
	}
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{sender: adapter.StateIdle, target: adapter.StateIdle},
		Result:      map[string]adapter.Result{sender: adapter.ResultAccepted, target: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": sender, "p2": target},
		InventoryAgents: []adapter.Agent{
			{Handle: sender, PaneID: "p1", SessionID: "default", Status: "idle"},
			{Handle: target, PaneID: "p2", SessionID: "default", Status: "idle"},
		},
		Composer: func(handle string) (bool, string, error) {
			mu.Lock()
			defer mu.Unlock()
			reasons := composer[handle]
			if len(reasons) == 0 {
				return true, "", nil
			}
			reason := reasons[0]
			if len(reasons) > 1 {
				composer[handle] = reasons[1:]
			}
			return reason == "", reason, nil
		},
	}
	stop := startDaemon(t, dirs, daemon.Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour})
	defer stop()
	env := append(append([]string{}, base...), "HERDR_PANE_ID=p1")
	mustCmd(t, []string{"register", "eng/reviewer", "--when", "reviews"}, append(append([]string{}, base...), "HERDR_PANE_ID=p2"))

	sendHeld := func(body, want string) string {
		t.Helper()
		out, _ := mustCmd(t, []string{"send", "eng/reviewer", body}, env)
		id, state, _ := strings.Cut(strings.TrimSpace(out), " ")
		if state != want {
			t.Fatalf("send %s = %q, want state %q", body, out, want)
		}
		status, _ := mustCmd(t, []string{"status", id}, base)
		if !strings.Contains(status, "state: "+want) {
			t.Fatalf("status = %q, want %q", status, want)
		}
		return id
	}

	// Draft present: no paste, no retry, then delivery with the hold duration.
	setComposer(target, adapter.HoldDraft)
	draftID := sendHeld("draft-body", "held: "+adapter.HoldDraft)
	mustCmd(t, []string{"status", draftID}, base)
	if len(ad.Texts) != 0 {
		t.Fatalf("pasted over a draft: %q", ad.Texts)
	}
	if logHas(t, dirs, eventlog.EventRetry, draftID) {
		t.Fatal("a draft hold counted as a retry")
	}
	if !logHas(t, dirs, eventlog.EventHeld, adapter.HoldDraft) {
		t.Fatal("hold start was not logged")
	}
	setComposer(target)
	status, _ := mustCmd(t, []string{"status", draftID}, base)
	if !strings.Contains(status, "state: delivered") || !textsContain(ad.Texts, "draft-body") {
		t.Fatalf("after draft cleared: status %q texts %q", status, ad.Texts)
	}
	if !logHas(t, dirs, eventlog.EventDelivered, "after hold of") {
		t.Fatal("delivery did not log the hold duration")
	}
	if !logHas(t, dirs, eventlog.EventDeliverAttempt, "composer-empty-at-check") {
		t.Fatal("deliver_attempt detail=composer-empty-at-check missing")
	}

	// The adapter cannot tell, or the pane is focused.
	for _, reason := range []string{adapter.HoldDraftUnknown, adapter.HoldFocused} {
		setComposer(target, reason)
		id := sendHeld(reason+"-body", "held: "+reason)
		setComposer(target)
		if status, _ := mustCmd(t, []string{"status", id}, base); !strings.Contains(status, "state: delivered") {
			t.Fatalf("after %s: status %q", reason, status)
		}
	}

	// Race: empty at the precheck, a draft at the pre-paste re-read.
	// The reply follows the precheck, so it says queued. The paste holds.
	before := len(ad.Texts)
	setComposer(target, "", adapter.HoldDraft)
	raceOut, _ := mustCmd(t, []string{"send", "eng/reviewer", "race-body"}, env)
	raceID, raceState, _ := strings.Cut(strings.TrimSpace(raceOut), " ")
	if raceState != "queued for delivery" {
		t.Fatalf("race send = %q, want queued for delivery", raceOut)
	}
	raceStatus, _ := mustCmd(t, []string{"status", raceID}, base)
	if !strings.Contains(raceStatus, "state: held: "+adapter.HoldDraft) {
		t.Fatalf("race status = %q", raceStatus)
	}
	if len(ad.Texts) != before {
		t.Fatalf("pasted while the user typed: %q", ad.Texts)
	}
	setComposer(target)
	status, _ = mustCmd(t, []string{"status", raceID}, base)
	if !strings.Contains(status, "state: delivered") || len(ad.Texts) != before+1 {
		t.Fatalf("after race: status %q texts %q", status, ad.Texts)
	}

	// TTL still applies to a draft hold, and the sender is told.
	setComposer(target, adapter.HoldDraft)
	expired, _ := mustCmd(t, []string{"send", "--ttl", "200ms", "--wait", "eng/reviewer", "ttl-body"}, env)
	expiredID, state, _ := strings.Cut(strings.TrimSpace(expired), " ")
	if state != "expired" || textsContain(ad.Texts, "ttl-body") ||
		!textsContain(ad.Texts, "message "+expiredID+" to eng/reviewer expired") {
		t.Fatalf("ttl send = %q texts %q", expired, ad.Texts)
	}
}

func TestClientIDAcceptIsIdempotent(t *testing.T) {
	runtime := shortRuntime(t)
	base := []string{
		"XDG_RUNTIME_DIR=" + runtime,
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	sender := "herdr:local/default/w1:p1"
	reviewer := "herdr:local/default/w1:p2"
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{sender: adapter.StateIdle, reviewer: adapter.StateIdle},
		Result:      map[string]adapter.Result{sender: adapter.ResultAccepted, reviewer: adapter.ResultAccepted},
		PaneHandles: map[string]string{"p1": sender, "p2": reviewer},
		InventoryAgents: []adapter.Agent{
			{Handle: sender, PaneID: "p1", SessionID: "default", Status: "idle"},
			{Handle: reviewer, PaneID: "p2", SessionID: "default", Status: "idle"},
		},
	}
	cfg := daemon.Config{Adapters: []adapter.Adapter{ad}, PollInterval: time.Hour}
	stop := startDaemon(t, dirs, cfg)
	env := func(pane string) []string {
		return append(append([]string{}, base...), "HERDR_PANE_ID="+pane)
	}
	mustCmd(t, []string{"register", "ops/inbox", "--when", "reviews"}, env("p1"))
	mustCmd(t, []string{"register", "eng/reviewer", "--when", "reviews"}, env("p2"))
	clientID := "mac:01ARZ3NDEKTSV4RRFFQ69G5FAV"

	first, _ := mustCmd(t, []string{"send", "--client-id", clientID, "eng/reviewer", "same-body"}, env("p1"))
	id, state, ok := strings.Cut(strings.TrimSpace(first), " ")
	if !ok || !strings.HasPrefix(id, "m-") || state != "queued for delivery" || strings.Contains(first, "duplicate") {
		t.Fatalf("first accept = %q", first)
	}
	awaitDelivered(t, base, id)
	if len(ad.Texts) != 1 || !textsContain(ad.Texts, "same-body") {
		t.Fatalf("first delivery = %q", ad.Texts)
	}

	fresh, _ := mustCmd(t, []string{"send", "--json", "eng/reviewer", "json-body"}, env("p1"))
	if strings.Contains(fresh, "duplicate") || !strings.Contains(fresh, `"state":"queued for delivery"`) {
		t.Fatalf("first json = %q", fresh)
	}
	var freshMsg struct {
		ID string `json:"id"`
	}
	if err := json.Unmarshal([]byte(fresh), &freshMsg); err != nil {
		t.Fatal(err)
	}
	awaitDelivered(t, base, freshMsg.ID)

	again, _ := mustCmd(t, []string{"send", "--json", "--client-id", clientID, "eng/reviewer", "same-body"}, env("p1"))
	if !strings.Contains(again, `"id":"`+id+`"`) || !strings.Contains(again, `"duplicate":true`) || !strings.Contains(again, `"state":"delivered"`) {
		t.Fatalf("duplicate json = %q", again)
	}
	if len(ad.Texts) != 2 {
		t.Fatalf("duplicate changed deliveries: %q", ad.Texts)
	}

	code, stdout, stderr := runCmd([]string{"send", "--client-id", clientID, "eng/reviewer", "other-body"}, env("p1"))
	if code == 0 || !strings.Contains(stderr, "conflict") {
		t.Fatalf("different body = %d %q %q", code, stdout, stderr)
	}
	code, stdout, stderr = runCmd([]string{"send", "--client-id", clientID, reviewer, "same-body"}, env("p1"))
	if code == 0 || !strings.Contains(stderr, "conflict") {
		t.Fatalf("different recipient = %d %q %q", code, stdout, stderr)
	}
	if len(ad.Texts) != 2 || textsContain(ad.Texts, "other-body") {
		t.Fatalf("conflict stored a delivery: %q", ad.Texts)
	}
	if !logHas(t, dirs, "dedupe", clientID) {
		t.Fatal("dedupe event missing")
	}

	stop()
	before := len(ad.Texts)
	stop = startDaemon(t, dirs, cfg)
	defer stop()
	third, _ := mustCmd(t, []string{"send", "--client-id", clientID, "eng/reviewer", "same-body"}, env("p1"))
	thirdID, _, ok := strings.Cut(strings.TrimSpace(third), " ")
	if !ok || thirdID != id || len(ad.Texts) != before {
		t.Fatalf("after restart = %q texts %q", third, ad.Texts)
	}

	byClient, _ := mustCmd(t, []string{"status", clientID}, base)
	byID, _ := mustCmd(t, []string{"status", id}, base)
	for _, text := range []string{byClient, byID} {
		if !strings.Contains(text, "id: "+id) || !strings.Contains(text, "client_id: "+clientID) {
			t.Fatalf("status = %q", text)
		}
	}
	shown, _ := mustCmd(t, []string{"show", id}, base)
	if !strings.Contains(shown, "client_id: "+clientID) || !strings.Contains(shown, "same-body") {
		t.Fatalf("show = %q", shown)
	}

	plain, _ := mustCmd(t, []string{"send", "eng/reviewer", "plain-body"}, env("p1"))
	plainID, _, ok := strings.Cut(strings.TrimSpace(plain), " ")
	if !ok || plainID == id {
		t.Fatalf("send without client id = %q", plain)
	}
	awaitDelivered(t, base, plainID)
	if !textsContain(ad.Texts, "plain-body") {
		t.Fatalf("send without client id = %q texts %q", plain, ad.Texts)
	}

	replyID := "mac:01ARZ3NDEKTSV4RRFFQ69G5FAW"
	reply, _ := mustCmd(t, []string{"reply", "--client-id", replyID, id, "thanks"}, env("p2"))
	gotReply, _, ok := strings.Cut(strings.TrimSpace(reply), " ")
	if !ok || gotReply == id {
		t.Fatalf("reply = %q", reply)
	}
	awaitDelivered(t, base, gotReply)
	replyAgain, _ := mustCmd(t, []string{"reply", "--json", "--client-id", replyID, id, "thanks"}, env("p2"))
	if !strings.Contains(replyAgain, `"id":"`+gotReply+`"`) || !strings.Contains(replyAgain, `"duplicate":true`) {
		t.Fatalf("duplicate reply = %q", replyAgain)
	}
	if !textsContain(ad.Texts, "thanks") {
		t.Fatalf("reply was not delivered: %q", ad.Texts)
	}
	thanks := 0
	for _, text := range ad.Texts {
		if strings.Contains(text, "thanks") {
			thanks++
		}
	}
	if thanks != 1 {
		t.Fatalf("reply delivered %d times: %q", thanks, ad.Texts)
	}

	code, _, stderr = runCmd([]string{"send", "--client-id", "nocolon", "eng/reviewer", "x"}, env("p1"))
	if code == 0 || !strings.Contains(stderr, "client id") {
		t.Fatalf("invalid client id = %d %q", code, stderr)
	}
}

func TestClientLogAppendKeepsTimeAndMarksOrigin(t *testing.T) {
	runtime := shortRuntime(t)
	base := []string{
		"XDG_RUNTIME_DIR=" + runtime,
		"XDG_STATE_HOME=" + t.TempDir(),
		"HOME=" + t.TempDir(),
	}
	dirs, err := paths.FromEnv(base)
	if err != nil {
		t.Fatal(err)
	}
	stop := startDaemon(t, dirs, daemon.Config{PollInterval: time.Hour})
	defer stop()
	when := time.Date(2020, 1, 2, 3, 4, 5, 0, time.UTC)
	resp, err := client.Call(context.Background(), dirs.SocketPath, daemon.Request{
		Op: "log-append",
		Events: []daemon.ClientEvent{{
			Time:   when,
			Event:  eventlog.EventSend,
			Actor:  "outbox",
			Detail: "queued locally",
			Origin: "spoof",
		}},
	})
	if err != nil || !resp.OK {
		t.Fatalf("log-append = %+v, %v", resp, err)
	}
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{Event: eventlog.EventSend})
	if err != nil {
		t.Fatal(err)
	}
	if len(lines) != 1 {
		t.Fatalf("send lines = %v", lines)
	}
	if strings.Contains(lines[0], "spoof") {
		t.Fatalf("client origin was trusted: %s", lines[0])
	}
	ev, err := eventlog.Parse(lines[0])
	if err != nil {
		t.Fatal(err)
	}
	if !ev.Time.Equal(when) || ev.Origin != eventlog.OriginClient || ev.Detail != "queued locally" {
		t.Fatalf("event = %+v", ev)
	}
	text, err := eventlog.Format(lines[0])
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(text, `origin="client"`) {
		t.Fatalf("format = %q", text)
	}
}

func logHas(t *testing.T, dirs paths.Dirs, event, text string) bool {
	t.Helper()
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{Event: event})
	if err != nil {
		t.Fatal(err)
	}
	return strings.Contains(strings.Join(lines, "\n"), text)
}

func textsContain(texts []string, fragment string) bool {
	for _, text := range texts {
		if strings.Contains(text, fragment) {
			return true
		}
	}
	return false
}
