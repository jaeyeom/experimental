package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/outbox"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

const (
	idA = "linux2:01ARZ3NDEKTSV4RRFFQ69G5FAV"
	idB = "linux2:01ARZ3NDEKTSV4RRFFQ69G5FAW"
	idC = "linux2:01ARZ3NDEKTSV4RRFFQ69G5FAX"

	hubTarget = "herdr:local/default/w1:p2"
)

func TestHubDownQueuesSend(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`)
	h.ssh.Write("locked", "")
	id := pinClientID(t, idA)
	code, stdout, stderr := h.run(t, "", "send", "ops/x", "hi")
	if code != 0 || stdout != "queued (hub unreachable): "+id+"\n" {
		t.Fatalf("send = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	if strings.Contains(stderr, "queued") {
		t.Fatalf("stderr = %q", stderr)
	}
	if n := pendingCount(t, h.env); n != 1 {
		t.Fatalf("pending = %d", n)
	}
}

func TestFlushDeliversOnce(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("locked", "")
	id := pinClientID(t, idA)
	code, stdout, stderr := s.run(t, s.env, "", "send", hubTarget, "once-body")
	if code != 0 || stdout != "queued (hub unreachable): "+id+"\n" {
		t.Fatalf("send = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	_, dirs := s.startHubDaemon(t)
	if code, _, stderr = s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("flush = %d, stderr %q", code, stderr)
	}
	if code, _, stderr = s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("second flush = %d, stderr %q", code, stderr)
	}
	msgs := readMessages(t, dirs)
	if countBody(msgs, "once-body") != 1 || countField(msgs, "clientId", id) != 1 {
		t.Fatalf("messages = %s", msgs)
	}
	code, stdout, stderr = s.run(t, s.env, "", "status", id)
	if code != 0 || !strings.HasPrefix(stdout, "delivered → m-") {
		t.Fatalf("status = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	if pendingCount(t, s.env) != 0 {
		t.Fatal("delivered entry is still pending")
	}
	if code, stdout, stderr = s.run(t, s.env, "", "outbox", "ls"); code != 0 || stdout != "" {
		t.Fatalf("ls = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
}

func TestLostAckRetriesOnce(t *testing.T) {
	s := newTunnelSetup(t)
	_, dirs := s.startHubDaemon(t)
	loseFirst(t, "send")
	id := pinClientID(t, idA)
	code, stdout, stderr := s.run(t, s.env, "", "send", hubTarget, "lost-body")
	if code != 0 || stdout != "queued (hub unreachable): "+id+"\n" {
		t.Fatalf("send = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	if countBody(readMessages(t, dirs), "lost-body") != 1 {
		t.Fatalf("hub did not accept the first send: %s", readMessages(t, dirs))
	}
	if code, _, stderr = s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("flush = %d, stderr %q", code, stderr)
	}
	if code, _, stderr = s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("second flush = %d, stderr %q", code, stderr)
	}
	msgs := readMessages(t, dirs)
	if countBody(msgs, "lost-body") != 1 || countField(msgs, "clientId", id) != 1 {
		t.Fatalf("messages = %s", msgs)
	}
}

func TestPendingSenderStaysOrdered(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("locked", "")
	pinClientIDs(t, idA, idB, idC)
	code, stdout, stderr := s.run(t, s.env, "", "send", hubTarget, "first-body")
	if code != 0 || stdout != "queued (hub unreachable): "+idA+"\n" {
		t.Fatalf("first = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	_, dirs := s.startHubDaemon(t)
	box := outbox.Open(stateDirOf(t, s.env))
	unlock, ok, err := box.TryLock()
	if err != nil || !ok {
		t.Fatalf("lock = %v, %v", ok, err)
	}

	code, stdout, stderr = s.run(t, s.env, "", "send", hubTarget, "second-body")
	if code != 0 || stdout != "queued: "+idB+"\n" {
		t.Fatalf("second = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	if strings.Contains(readMessages(t, dirs), "first-body") || strings.Contains(readMessages(t, dirs), "second-body") {
		t.Fatalf("a locked outbox still delivered: %s", readMessages(t, dirs))
	}

	p2 := withEnv(s.env, "HERDR_PANE_ID", "w1:p2")
	code, stdout, stderr = s.run(t, p2, "", "send", "herdr:local/default/w1:p1", "p2-body")
	if code != 0 || !strings.Contains(stdout, "queued for delivery") || strings.Contains(stdout, "queued:") || strings.Contains(stdout, "queued (hub") {
		t.Fatalf("other sender = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	if countBody(readMessages(t, dirs), "p2-body") != 1 {
		t.Fatalf("other sender was blocked: %s", readMessages(t, dirs))
	}

	unlock()
	if code, _, stderr = s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("flush = %d, stderr %q", code, stderr)
	}
	msgs := readMessages(t, dirs)
	first, second := sentAt(t, msgs, "first-body"), sentAt(t, msgs, "second-body")
	if !first.Before(second) || countBody(msgs, "first-body") != 1 || countBody(msgs, "second-body") != 1 {
		t.Fatalf("order first %s second %s\n%s", first, second, msgs)
	}
}

func TestSecondFlushSkipsWhileLocked(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("locked", "")
	pinClientID(t, idA)
	if code, _, stderr := s.run(t, s.env, "", "send", hubTarget, "held-body"); code != 0 {
		t.Fatalf("send = %d, stderr %q", code, stderr)
	}
	_, dirs := s.startHubDaemon(t)
	box := outbox.Open(stateDirOf(t, s.env))
	unlock, ok, err := box.TryLock()
	if err != nil || !ok {
		t.Fatalf("lock = %v, %v", ok, err)
	}
	if code, _, stderr := s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("flush = %d, stderr %q", code, stderr)
	}
	if readMessages(t, dirs) != "" {
		t.Fatalf("the second flusher delivered: %s", readMessages(t, dirs))
	}
	unlock()
	if code, _, stderr := s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("flush after unlock = %d, stderr %q", code, stderr)
	}
	if countBody(readMessages(t, dirs), "held-body") != 1 {
		t.Fatalf("messages = %s", readMessages(t, dirs))
	}
}

func TestAnonReplayKeepsTheStoredHandle(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("locked", "")
	env := withoutEnv(s.env, "HERDR_PANE_ID")
	id := pinClientID(t, idA)
	code, stdout, stderr := s.run(t, env, "", "send", hubTarget, "anon-body")
	if code != 0 || stdout != "queued (hub unreachable): "+id+"\n" {
		t.Fatalf("send = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	box := outbox.Open(stateDirOf(t, env))
	entry, ok, err := box.Get(id)
	if err != nil || !ok {
		t.Fatalf("get = %v, %v", ok, err)
	}
	entry.Handle = "anon:testhost-42"
	if err := box.Put(entry); err != nil {
		t.Fatal(err)
	}
	_, dirs := s.startHubDaemon(t)
	loseFirst(t, "send")
	if code, _, stderr = s.run(t, env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("flush = %d, stderr %q", code, stderr)
	}
	if code, _, stderr = s.run(t, env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("second flush = %d, stderr %q", code, stderr)
	}
	msgs := readMessages(t, dirs)
	if countBody(msgs, "anon-body") != 1 || countField(msgs, "senderHandle", "anon:testhost-42") != 1 {
		t.Fatalf("messages = %s", msgs)
	}
}

func TestHubRefusalIsNotQueued(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`)
	h.ssh.Write("agentbus.stderr", "agentbus: unknown name ops/x\n")
	h.ssh.Write("agentbus.code", "1")
	pinClientID(t, idA)
	code, stdout, stderr := h.run(t, "", "send", "ops/x", "nope")
	if code == 0 || strings.Contains(stdout, "queued") || strings.Contains(stderr, "queued") {
		t.Fatalf("send = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	if !strings.Contains(stderr, "unknown name") {
		t.Fatalf("stderr = %q", stderr)
	}
	if pendingCount(t, h.env) != 0 {
		t.Fatal("a refusal was queued")
	}
}

func TestProtocolMismatchIsNotQueued(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("locked", "")
	serveProtocol(t, s.tunnel, 9)
	pinClientID(t, idA)
	code, stdout, stderr := s.run(t, s.env, "", "send", hubTarget, "nope")
	if code == 0 || strings.Contains(stdout, "queued") || strings.Contains(stderr, "queued") {
		t.Fatalf("send = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	if !strings.Contains(stderr, "speaks protocol 9") {
		t.Fatalf("stderr = %q", stderr)
	}
	if pendingCount(t, s.env) != 0 {
		t.Fatal("a protocol mismatch was queued")
	}
}

func TestOutboxWriteFailureIsLoud(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`)
	h.ssh.Write("locked", "")
	file := filepath.Join(t.TempDir(), "state")
	if err := os.WriteFile(file, []byte("x"), 0o600); err != nil {
		t.Fatal(err)
	}
	env := withEnv(h.env, "XDG_STATE_HOME", file)
	pinClientID(t, idA)
	code, stdout, stderr := h.runEnv(t, env, "", "send", "ops/x", "hi")
	if code == 0 || strings.Contains(stdout, "queued") || strings.Contains(stderr, "queued") || !strings.Contains(stderr, "sandbox") {
		t.Fatalf("send = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
}

func TestLeaseHubDownIsNotQueued(t *testing.T) {
	h := newHubLeaseWorld(t)
	h.ssh.Write("locked", "")
	code, stdout, stderr := h.lease(t, "w1:p1", "claim", "--for", "40m")
	if code == 0 || strings.Contains(stdout, "queued") || strings.Contains(stderr, "queued") {
		t.Fatalf("claim = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	if pendingCount(t, h.base) != 0 {
		t.Fatal("a lease was queued")
	}
}

func TestTTLExpiryNotifiesAndLogs(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("locked", "")
	when := time.Now().UTC().Truncate(time.Millisecond)
	now := pinClock(t, when)
	id := pinClientID(t, idA)
	code, stdout, stderr := s.run(t, s.env, "", "send", hubTarget, "expiring", "--ttl", "1ms")
	if code != 0 || stdout != "queued (hub unreachable): "+id+"\n" {
		t.Fatalf("send = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	*now = when.Add(time.Second)
	s.herdr.Argv()
	if code, _, stderr = s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("flush = %d, stderr %q", code, stderr)
	}
	if !textsContain(s.herdr.Argv(), "--machine linux2 agent prompt w1:p1 ") {
		t.Fatal("the sender was not told the message expired")
	}
	if pendingCount(t, s.env) != 0 {
		t.Fatal("an expired entry is still pending")
	}
	code, stdout, stderr = s.run(t, s.env, "", "status", id)
	if code != 0 || stdout != "expired\n" {
		t.Fatalf("status = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	_, dirs := s.startHubDaemon(t)
	if code, _, stderr = s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("forward = %d, stderr %q", code, stderr)
	}
	ev := oneEvent(t, dirs, eventlog.EventExpired)
	if ev.Origin != eventlog.OriginClient || ev.Detail != "ttl elapsed" || !ev.Time.Equal(*now) {
		t.Fatalf("expired event = %+v", ev)
	}
}

func TestNoTTLStaysAndIsFlaggedAfter24h(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("locked", "")
	when := time.Now().UTC().Truncate(time.Millisecond)
	now := pinClock(t, when)
	id := pinClientID(t, idA)
	if code, _, stderr := s.run(t, s.env, "", "send", hubTarget, "stays"); code != 0 {
		t.Fatalf("send = %d, stderr %q", code, stderr)
	}
	*now = when.Add(25 * time.Hour)
	if code, _, stderr := s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("flush = %d, stderr %q", code, stderr)
	}
	if pendingCount(t, s.env) != 1 {
		t.Fatal("an entry with no ttl was dropped")
	}
	code, stdout, stderr := s.run(t, s.env, "", "status", id)
	if code != 0 || !strings.Contains(stdout, "retrying (attempt ") {
		t.Fatalf("status = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	code, stdout, stderr = s.run(t, s.env, "", "outbox", "ls")
	if code != 0 || !strings.Contains(stdout, id+" ") || !strings.Contains(stdout, " pending for more than 24h") {
		t.Fatalf("ls = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	code, stdout, stderr = s.run(t, s.env, "", "doctor")
	if code != 0 || !strings.Contains(stdout, "outbox: "+id+" pending for more than 24h") {
		t.Fatalf("doctor = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
}

func TestWaitPastTheDeadlineStaysQueued(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("locked", "")
	id := pinClientID(t, idA)
	ctx, cancel := context.WithTimeout(context.Background(), 400*time.Millisecond)
	defer cancel()
	var stdout, stderr bytes.Buffer
	args := []string{"send", hubTarget, "waiting", "--wait", "--ttl", "1h"}
	code := run(ctx, args, s.env, strings.NewReader(""), &stdout, &stderr)
	if code == 0 || !strings.Contains(stdout.String(), "still queued: "+id) {
		t.Fatalf("send = %d, stdout %q, stderr %q", code, stdout.String(), stderr.String())
	}
	if strings.Contains(stdout.String(), "delivered") {
		t.Fatalf("stdout claimed delivery: %q", stdout.String())
	}
	if pendingCount(t, s.env) != 1 {
		t.Fatal("the wait removed the queued message")
	}
}

func TestSpooledLogKeepsTheClientTime(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("locked", "")
	when := time.Now().UTC().Truncate(time.Millisecond)
	pinClock(t, when)
	id := pinClientID(t, idA)
	if code, stdout, stderr := s.run(t, s.env, "", "send", hubTarget, "logged-body"); code != 0 || !strings.Contains(stdout, id) {
		t.Fatalf("send = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	_, dirs := s.startHubDaemon(t)
	if code, _, stderr := s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("flush = %d, stderr %q", code, stderr)
	}
	ev := oneEvent(t, dirs, eventlog.EventQueued)
	if ev.Origin != eventlog.OriginClient || ev.Detail != "hub unreachable" || !ev.Time.Equal(when) || ev.MsgID != id {
		t.Fatalf("queued event = %+v", ev)
	}
}

func TestBypassIsSpooledOnce(t *testing.T) {
	s := newTunnelSetup(t)
	s.ssh.Write("locked", "")
	raw := `{"session_id":"s1","tool_input":{"to":"x","message":"bypass-body"}}`
	code, stdout, stderr := s.run(t, s.env, raw, "hook")
	if code != 0 || stdout != "" || stderr != "" {
		t.Fatalf("hook = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	pending := pendingEntries(t, s.env)
	if len(pending) != 1 || pending[0].Kind != outbox.KindBypass || pending[0].Body != "bypass-body" {
		t.Fatalf("pending = %+v", pending)
	}
	if code, _, stderr = s.run(t, s.env, `{"prompt":"x"}`, "hook"); code != 0 {
		t.Fatalf("invalid hook = %d, stderr %q", code, stderr)
	}
	if n := pendingCount(t, s.env); n != 1 {
		t.Fatalf("invalid hook queued, pending = %d", n)
	}
	_, dirs := s.startHubDaemon(t)
	loseFirst(t, "bypass")
	if code, _, stderr = s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("flush = %d, stderr %q", code, stderr)
	}
	if code, _, stderr = s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("second flush = %d, stderr %q", code, stderr)
	}
	if n := bypassCount(t, dirs); n != 1 {
		t.Fatalf("bypass events = %d", n)
	}
	if pendingCount(t, s.env) != 0 {
		t.Fatal("the bypass is still pending")
	}
}

func TestTunnelHookSuccessDoesNotSpool(t *testing.T) {
	s := newTunnelSetup(t)
	_, dirs := s.startHubDaemon(t)
	raw := `{"session_id":"s1","tool_input":{"to":"x","message":"hi"}}`
	if code, _, stderr := s.run(t, s.env, raw, "hook"); code != 0 || s.usedSSH() {
		t.Fatalf("hook = %d, stderr %q, ssh %q", code, stderr, s.ssh.Lines("argv"))
	}
	if pendingCount(t, s.env) != 0 || bypassCount(t, dirs) != 1 {
		t.Fatalf("pending %d, bypass %d", pendingCount(t, s.env), bypassCount(t, dirs))
	}
	if code, _, stderr := s.run(t, s.env, "", "outbox", "flush"); code != 0 {
		t.Fatalf("flush = %d, stderr %q", code, stderr)
	}
	if bypassCount(t, dirs) != 1 {
		t.Fatalf("flush added a bypass, count %d", bypassCount(t, dirs))
	}
}

func TestEmptyOutboxDoesNotSSH(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`)
	if code, _, stderr := h.run(t, "", "doctor"); code != 0 {
		t.Fatalf("doctor = %d, stderr %q", code, stderr)
	}
	if h.ssh.Read("argv") != "" {
		t.Fatal("an empty outbox opened ssh")
	}
}

func pinClientIDs(t *testing.T, ids ...string) {
	t.Helper()
	var n int
	orig := mintClientID
	mintClientID = func(string) (string, error) {
		if n >= len(ids) {
			return "", errors.New("unexpected client id")
		}
		id := ids[n]
		n++
		return id, nil
	}
	t.Cleanup(func() { mintClientID = orig })
}

func pinClock(t *testing.T, start time.Time) *time.Time {
	t.Helper()
	now := start
	orig := outboxNow
	outboxNow = func() time.Time { return now }
	t.Cleanup(func() { outboxNow = orig })
	return &now
}

func loseFirst(t *testing.T, op string) {
	t.Helper()
	orig := dialDaemon
	var once sync.Once
	dialDaemon = func(ctx context.Context, socket string, req daemon.Request) (daemon.Response, error) {
		resp, err := orig(ctx, socket, req)
		if err != nil || req.Op != op {
			return resp, err
		}
		lost := false
		once.Do(func() { lost = true })
		if lost {
			return resp, errors.New("connection reset")
		}
		return resp, nil
	}
	t.Cleanup(func() { dialDaemon = orig })
}

func serveProtocol(t *testing.T, socket string, protocol int) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(socket), 0o755); err != nil {
		t.Fatal(err)
	}
	ln, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = ln.Close() })
	go func() {
		for {
			conn, err := ln.Accept()
			if err != nil {
				return
			}
			var req daemon.Request
			_ = json.NewDecoder(conn).Decode(&req)
			_ = json.NewEncoder(conn).Encode(daemon.Response{Error: "mismatch", Protocol: protocol})
			_ = conn.Close()
		}
	}()
}

func stateDirOf(t *testing.T, env []string) string {
	t.Helper()
	dir, err := paths.StateDir(env)
	if err != nil {
		t.Fatal(err)
	}
	return dir
}

func pendingEntries(t *testing.T, env []string) []outbox.Entry {
	t.Helper()
	entries, err := outbox.Open(stateDirOf(t, env)).Pending()
	if err != nil {
		t.Fatal(err)
	}
	return entries
}

func pendingCount(t *testing.T, env []string) int {
	t.Helper()
	return len(pendingEntries(t, env))
}

func readMessages(t *testing.T, dirs paths.Dirs) string {
	t.Helper()
	data, err := os.ReadFile(filepath.Join(dirs.StateDir, "messages.json"))
	if errors.Is(err, os.ErrNotExist) {
		return ""
	}
	if err != nil {
		t.Fatal(err)
	}
	return string(data)
}

type storedMessage struct {
	Body         string    `json:"body"`
	SentAt       time.Time `json:"sentAt"`
	ClientID     string    `json:"clientId"`
	SenderHandle string    `json:"senderHandle"`
}

func sentAt(t *testing.T, raw, body string) time.Time {
	t.Helper()
	var doc struct {
		Messages []storedMessage `json:"messages"`
	}
	if err := json.Unmarshal([]byte(raw), &doc); err != nil {
		t.Fatal(err)
	}
	for _, msg := range doc.Messages {
		if msg.Body == body {
			return msg.SentAt
		}
	}
	t.Fatalf("no message %q in %s", body, raw)
	return time.Time{}
}

func countBody(raw, body string) int {
	return strings.Count(raw, fmt.Sprintf("%q", body))
}

func countField(raw, field, value string) int {
	return strings.Count(raw, fmt.Sprintf("%q: %q", field, value))
}

func oneEvent(t *testing.T, dirs paths.Dirs, name string) eventlog.Event {
	t.Helper()
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{Event: name})
	if err != nil {
		t.Fatal(err)
	}
	if len(lines) != 1 {
		t.Fatalf("%s events = %d %q", name, len(lines), lines)
	}
	ev, err := eventlog.Parse(lines[0])
	if err != nil {
		t.Fatal(err)
	}
	return ev
}

func bypassCount(t *testing.T, dirs paths.Dirs) int {
	t.Helper()
	lines, err := eventlog.Open(dirs.StateDir).Read(eventlog.Filter{Event: eventlog.EventBypass})
	if err != nil {
		t.Fatal(err)
	}
	return len(lines)
}

func withEnv(env []string, key, value string) []string {
	return append(withoutEnv(env, key), key+"="+value)
}

func withoutEnv(env []string, key string) []string {
	prefix := key + "="
	out := make([]string, 0, len(env))
	for _, kv := range env {
		if !strings.HasPrefix(kv, prefix) {
			out = append(out, kv)
		}
	}
	return out
}
