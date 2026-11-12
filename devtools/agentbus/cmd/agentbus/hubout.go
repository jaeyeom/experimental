package main

import (
	"bytes"
	"context"
	"crypto/rand"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"math/big"
	"net"
	"os"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/outbox"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
)

const (
	attemptOK        = "ok"
	attemptTransport = "transport"
	attemptRefusal   = "refusal"
	attemptHard      = "hard"

	outboxInitialBackoff = 5 * time.Second
	outboxMaxBackoff     = 5 * time.Minute
	outboxWaitPoll       = 20 * time.Millisecond
)

// mintClientID builds <machine>:<ulid>. Tests replace it.
var mintClientID = func(machine string) (string, error) {
	label := machineLabel(machine)
	var buf [16]byte
	if _, err := rand.Read(buf[:]); err != nil {
		return "", fmt.Errorf("client id: %w", err)
	}
	return label + ":" + crockford(new(big.Int).SetBytes(buf[:])), nil
}

// outboxNow is the clock the outbox uses. Tests replace it.
var outboxNow = time.Now

type flushingKey struct{}

func withFlushing(ctx context.Context) context.Context {
	return context.WithValue(ctx, flushingKey{}, true)
}

func isFlushing(ctx context.Context) bool {
	on, _ := ctx.Value(flushingKey{}).(bool)
	return on
}

type attempt struct {
	kind        string
	code        int
	resp        daemon.Response
	err         error
	passthrough bool
}

// hubDeliver runs one hub-mode send or reply. A transport failure is
// queued. A hub refusal is not.
func hubDeliver(ctx context.Context, cfg config.Config, args, env []string, stdin io.Reader, stdout, stderr io.Writer) int {
	e, wait, asJSON, code := prepareHubSend(cfg, args, stdin, stderr)
	if code != 0 {
		return code
	}
	handle, err := outboxHandle(ctx, cfg, env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
		return 1
	}
	e.Handle = handle
	e.EnqueuedAt = outboxNow().UTC()
	return dispatchHub(ctx, cfg, env, e, wait, asJSON, stdout, stderr)
}

func dispatchHub(ctx context.Context, cfg config.Config, env []string, e outbox.Entry, wait, asJSON bool, stdout, stderr io.Writer) int {
	if queuedBehind(env, e) {
		return enqueueReport(ctx, env, e, false, wait, cfg, stdout, stderr)
	}
	att := attemptHub(ctx, cfg, env, e, wait, stdout, stderr)
	switch att.kind {
	case attemptOK:
		if att.passthrough {
			return 0
		}
		return printAccepted(stdout, stderr, att.resp.Name, att.resp.State, asJSON, att.resp.Duplicate)
	case attemptRefusal:
		return refuseHub(stderr, att)
	case attemptHard:
		return hardHub(env, cfg, stderr, att)
	default:
		return enqueueReport(ctx, env, e, true, wait, cfg, stdout, stderr)
	}
}

func queuedBehind(env []string, e outbox.Entry) bool {
	box, err := openBox(env)
	if err != nil {
		return false
	}
	pending, err := box.PendingHandle(e.Handle)
	if err != nil || len(pending) == 0 {
		return false
	}
	return pending[0].ClientID != e.ClientID
}

func refuseHub(stderr io.Writer, att attempt) int {
	if msg := strings.TrimSpace(att.resp.Error); msg != "" {
		fmt.Fprintf(stderr, "agentbus: %s\n", msg)
	}
	if att.code == 0 {
		return 1
	}
	return att.code
}

func hardHub(env []string, cfg config.Config, stderr io.Writer, att attempt) int {
	if att.err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", sshHint(env, cfg, att.err))
	}
	if att.code == 0 {
		return 1
	}
	return att.code
}

func enqueueReport(ctx context.Context, env []string, e outbox.Entry, down, wait bool, cfg config.Config, stdout, stderr io.Writer) int {
	box, err := openBox(env)
	if err != nil {
		return outboxWriteFailed(stderr, err)
	}
	e.State = outbox.StatePending
	if e.EnqueuedAt.IsZero() {
		e.EnqueuedAt = outboxNow().UTC()
	}
	if _, err = box.Enqueue(e); err != nil {
		return outboxWriteFailed(stderr, err)
	}
	detail := "waiting for an earlier message"
	if down {
		detail = "hub unreachable"
	}
	_ = box.Spool(outbox.LogEvent{
		Time: e.EnqueuedAt, Event: eventlog.EventQueued, Actor: logActor(e.Handle),
		Name: e.Target, MsgID: e.ClientID, Detail: detail,
	})
	if wait {
		return waitQueued(ctx, cfg, env, e, stdout, stderr)
	}
	sayQueued(stdout, e.ClientID, down)
	return 0
}

func outboxWriteFailed(stderr io.Writer, err error) int {
	fmt.Fprintf(stderr, "agentbus: write the outbox: %s (sandbox? the state directory needs to be writable; see agentbus doctor --sandbox)\n", err)
	return 1
}

func sayQueued(stdout io.Writer, id string, down bool) {
	if down {
		fmt.Fprintf(stdout, "queued (hub unreachable): %s\n", id)
		return
	}
	fmt.Fprintf(stdout, "queued: %s\n", id)
}

func waitQueued(ctx context.Context, cfg config.Config, env []string, e outbox.Entry, stdout, stderr io.Writer) int {
	deadline, limited := waitDeadline(e)
	for {
		if limited && !outboxNow().Before(deadline) {
			fmt.Fprintf(stdout, "still queued: %s\n", e.ClientID)
			return 1
		}
		flushOutbox(ctx, cfg, env, io.Discard, io.Discard)
		if code, done := waitDelivered(env, e.ClientID, stdout, stderr); done {
			return code
		}
		if limited && !outboxNow().Before(deadline) {
			fmt.Fprintf(stdout, "still queued: %s\n", e.ClientID)
			return 1
		}
		if !sleepWait(ctx, limited, deadline) {
			fmt.Fprintf(stdout, "still queued: %s\n", e.ClientID)
			return 1
		}
	}
}

func waitDelivered(env []string, id string, stdout, stderr io.Writer) (int, bool) {
	box, err := openBox(env)
	if err != nil {
		return outboxWriteFailed(stderr, err), true
	}
	got, ok, err := box.Get(id)
	if err != nil {
		return outboxWriteFailed(stderr, err), true
	}
	if !ok || got.State == outbox.StatePending {
		return 0, false
	}
	if got.State == outbox.StateExpired {
		fmt.Fprintf(stdout, "expired: %s\n", id)
		return 1, true
	}
	state := got.HubState
	if state == "" {
		state = "delivered"
	}
	return printAccepted(stdout, stderr, got.MsgID, state, false, false), true
}

func sleepWait(ctx context.Context, limited bool, deadline time.Time) bool {
	d := outboxWaitPoll
	if limited {
		if remain := deadline.Sub(outboxNow()); remain < d {
			d = remain
		}
		if d <= 0 {
			return false
		}
	}
	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case <-ctx.Done():
		return false
	case <-timer.C:
		return true
	}
}

func waitDeadline(e outbox.Entry) (time.Time, bool) {
	if e.TTL == "" {
		return time.Time{}, false
	}
	d, err := time.ParseDuration(e.TTL)
	if err != nil || d <= 0 {
		return time.Time{}, false
	}
	return e.EnqueuedAt.Add(d), true
}

func attemptHub(ctx context.Context, cfg config.Config, env []string, e outbox.Entry, wait bool, stdout, stderr io.Writer) attempt {
	tun, ok, err := openTunnel(ctx, cfg, e.Kind, env, stderr)
	mismatch := errors.Is(err, errProtocolMismatch)
	if err != nil && !mismatch {
		return attempt{kind: attemptHard, code: 1, err: err}
	}
	if ok {
		if att, done := attemptTunnel(tun, e, wait); done {
			return att
		}
	}
	return attemptSSH(ctx, cfg, env, e, wait, mismatch, stdout, stderr)
}

func attemptTunnel(ctx context.Context, e outbox.Entry, wait bool) (attempt, bool) {
	t, ok := tunnelFrom(ctx)
	if !ok {
		return attempt{}, false
	}
	resp, err := dialDaemon(ctx, t.socket, busRequest(t.handle, e, wait))
	if err != nil {
		return attempt{kind: classifyDial(err), code: 1, err: err}, true
	}
	if !resp.OK {
		return attempt{kind: attemptRefusal, code: 1, resp: resp}, true
	}
	return attempt{kind: attemptOK, resp: resp}, true
}

// busRequest uses handle, which is the stored sender on a replay and the
// tunnel's caller on a live send. The tunnel stamp would otherwise
// replace a stored anon handle with the flusher's.
func busRequest(handle string, e outbox.Entry, wait bool) daemon.Request {
	op := e.Kind
	if op == outbox.KindBypass {
		op = "bypass"
	}
	return daemon.Request{
		Op: op, Name: e.Target, Body: e.Body, Session: e.Session,
		ClientID: e.ClientID, TTL: e.TTL, Wait: wait,
		Protocol: daemon.ProtocolVersion, Handle: handle,
	}
}

func classifyDial(err error) string {
	if errors.Is(err, context.Canceled) {
		return attemptHard
	}
	return attemptTransport
}

func attemptSSH(ctx context.Context, cfg config.Config, _ []string, e outbox.Entry, wait, mismatch bool, stdout, stderr io.Writer) attempt {
	args := e.Args
	if wait {
		args = withBoolFlag(args, "--wait")
	}
	var errBuf bytes.Buffer
	call := hubcall.Call{
		Args: args, Stdout: stdout, Stderr: &errBuf,
		Env: []string{herdr.AsHandleEnv + "=" + e.Handle},
	}
	if e.Stdin != "" {
		call.Stdin = strings.NewReader(e.Stdin)
	}
	hub := hubcall.Hub{Target: cfg.Hub, Tool: "agentbus", ConnectTimeout: cfg.RemoteTimeout}
	code, err := hub.Run(ctx, call)
	// A cancelled ssh comes back as a bare non-zero code. That is not a
	// hub refusal, and dropping the queued message would lose it.
	if err == nil && code != 0 && ctx.Err() != nil {
		err = ctx.Err()
	}
	if err == nil {
		_, _ = io.Copy(stderr, &errBuf)
		if code != 0 {
			return attempt{kind: attemptRefusal, code: code}
		}
		return attempt{kind: attemptOK, passthrough: true}
	}
	kind := classifySSH(err, mismatch)
	if kind != attemptTransport {
		_, _ = io.Copy(stderr, &errBuf)
	}
	return attempt{kind: kind, code: code, err: err}
}

func classifySSH(err error, mismatch bool) string {
	if mismatch || errors.Is(err, hubcall.ErrNoSSH) || errors.Is(err, context.Canceled) {
		return attemptHard
	}
	return attemptTransport
}

func prepareHubSend(cfg config.Config, args []string, stdin io.Reader, stderr io.Writer) (outbox.Entry, bool, bool, int) {
	if len(args) == 0 {
		return outbox.Entry{}, false, false, 2
	}
	if args[0] == "reply" {
		return prepareReply(cfg, args[1:], stderr)
	}
	return prepareSend(cfg, args[1:], stdin, stderr)
}

func prepareSend(cfg config.Config, args []string, stdin io.Reader, stderr io.Writer) (outbox.Entry, bool, bool, int) {
	parsed, err := parseSend(args)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus send: %s\n", err)
		return outbox.Entry{}, false, false, 2
	}
	body, fwd, code := sendBody(parsed, args, stdin, stderr)
	if code != 0 {
		return outbox.Entry{}, false, false, code
	}
	if strings.TrimSpace(body) == "" {
		fmt.Fprintf(stderr, "agentbus: message body is required\n")
		return outbox.Entry{}, false, false, 2
	}
	id, err := chooseClientID(cfg.Machine, parsed.clientID)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus send: %s\n", err)
		return outbox.Entry{}, false, false, 1
	}
	fwd = withClientID(fwd, id)
	return outbox.Entry{
		Kind: outbox.KindSend, Args: withoutBool(fwd, "--wait"), Stdin: stdinOf(parsed, body),
		ClientID: id, TTL: parsed.ttl, Target: parsed.target, Body: body,
	}, parsed.wait, parsed.asJSON, 0
}

func sendBody(parsed sendParse, args []string, stdin io.Reader, stderr io.Writer) (string, []string, int) {
	fwd := append([]string{"send"}, args...)
	if parsed.file != "" {
		data, err := os.ReadFile(parsed.file)
		if err != nil {
			fmt.Fprintf(stderr, "agentbus send: %s\n", err)
			return "", nil, 1
		}
		_, rest, _, _ := takeFlag(args, "--file")
		return string(data), append([]string{"send"}, rest...), 0
	}
	if parsed.body != "" {
		return parsed.body, fwd, 0
	}
	if stdin == nil {
		return "", fwd, 0
	}
	data, err := io.ReadAll(stdin)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus send: %s\n", err)
		return "", nil, 1
	}
	return string(data), fwd, 0
}

func stdinOf(parsed sendParse, body string) string {
	if parsed.body != "" && parsed.file == "" {
		return ""
	}
	return body
}

func prepareReply(cfg config.Config, args []string, stderr io.Writer) (outbox.Entry, bool, bool, int) {
	id, body, clientID, asJSON, err := parseReply(args)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus reply: %s\n", err)
		return outbox.Entry{}, false, false, 2
	}
	cid, err := chooseClientID(cfg.Machine, clientID)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus reply: %s\n", err)
		return outbox.Entry{}, false, false, 1
	}
	fwd := []string{"reply", id, body}
	if asJSON {
		fwd = append(fwd, "--json")
	}
	fwd = withClientID(fwd, cid)
	return outbox.Entry{
		Kind: outbox.KindReply, Args: fwd, ClientID: cid, Target: id, Body: body,
	}, false, asJSON, 0
}

func chooseClientID(machine, given string) (string, error) {
	if given != "" {
		return given, nil
	}
	return mintClientID(machine)
}

func withClientID(args []string, id string) []string {
	if _, _, found, _ := takeFlag(args, "--client-id"); found {
		return args
	}
	out := append([]string{}, args...)
	return append(out, "--client-id", id)
}

func withBoolFlag(args []string, flagName string) []string {
	for _, arg := range args {
		if arg == flagName {
			return args
		}
	}
	return append(append([]string{}, args...), flagName)
}

func withoutBool(args []string, flagName string) []string {
	var out []string
	for _, arg := range args {
		if arg != flagName {
			out = append(out, arg)
		}
	}
	return out
}

func outboxHandle(ctx context.Context, cfg config.Config, env []string) (string, error) {
	if envGet(env, "HERDR_PANE_ID") != "" {
		return callerHandle(ctx, cfg, env)
	}
	return localAnon(), nil
}

func localAnon() string {
	host, err := os.Hostname()
	if err != nil || strings.TrimSpace(host) == "" {
		host = "unknown"
	}
	return fmt.Sprintf("anon:%s-%d", host, os.Getpid())
}

func machineLabel(machine string) string {
	if machine == "" || len(machine) > 32 || strings.ContainsAny(machine, ": \t") {
		return "client"
	}
	for _, r := range machine {
		switch {
		case r >= 'A' && r <= 'Z', r >= 'a' && r <= 'z', r >= '0' && r <= '9':
		case r == '.' || r == '_' || r == '-':
		default:
			return "client"
		}
	}
	return machine
}

func crockford(n *big.Int) string {
	const alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
	mod := big.NewInt(32)
	rem := new(big.Int)
	out := make([]byte, 26)
	for i := 25; i >= 0; i-- {
		n.DivMod(n, mod, rem)
		out[i] = alphabet[rem.Int64()]
	}
	return string(out)
}

func openBox(env []string) (outbox.Box, error) {
	dir, err := paths.StateDir(env)
	if err != nil {
		return outbox.Box{}, fmt.Errorf("locate the outbox: %w", err)
	}
	return outbox.Open(dir), nil
}

func logActor(handle string) string {
	if handle == "" {
		return "client"
	}
	return handle
}

// maybeFlush delivers a pending outbox before the command runs. A
// missing hub config is the caller's problem, not the flusher's.
func maybeFlush(ctx context.Context, cfg config.Config, cfgErr error, env []string) {
	if cfgErr != nil || cfg.Hub == "" || isFlushing(ctx) {
		return
	}
	opportunisticFlush(ctx, cfg, env)
}

// opportunisticFlush delivers anything already queued. An empty outbox
// does not open ssh. Errors stay local so the command the user ran still
// proceeds.
func opportunisticFlush(ctx context.Context, cfg config.Config, env []string) {
	if isFlushing(ctx) {
		return
	}
	box, err := openBox(env)
	if err != nil {
		return
	}
	work, err := box.HasWork()
	if err != nil || !work {
		return
	}
	flushOutbox(withFlushing(ctx), cfg, env, io.Discard, io.Discard)
}

func flushOutbox(ctx context.Context, cfg config.Config, env []string, _, _ io.Writer) {
	box, err := openBox(env)
	if err != nil {
		return
	}
	work, err := box.HasWork()
	if err != nil || !work {
		return
	}
	unlock, ok, err := box.TryLock()
	if err != nil || !ok {
		return
	}
	defer unlock()
	entries, err := box.Pending()
	if err != nil {
		return
	}
	handle := flushHandle(entries)
	if handle == "" {
		handle = spoolActor(box)
	}
	if sock, up := tunnelUp(ctx, env, handle); up {
		forwardSpool(ctx, box, sock, handle)
	}
	flushSenders(ctx, cfg, env, box, entries)
	if sock, up := tunnelUp(ctx, env, handle); up {
		forwardSpool(ctx, box, sock, handle)
	}
}

func spoolActor(box outbox.Box) string {
	events, err := box.SpoolEvents()
	if err != nil || len(events) == 0 {
		return ""
	}
	return events[0].Actor
}

func flushHandle(entries []outbox.Entry) string {
	for _, e := range entries {
		if e.Handle != "" {
			return e.Handle
		}
	}
	return ""
}

func flushSenders(ctx context.Context, cfg config.Config, env []string, box outbox.Box, entries []outbox.Entry) {
	groups, order := groupHandles(entries)
	for _, handle := range order {
		flushSender(ctx, cfg, env, box, groups[handle])
	}
}

func groupHandles(entries []outbox.Entry) (map[string][]outbox.Entry, []string) {
	groups := map[string][]outbox.Entry{}
	var order []string
	for _, e := range entries {
		if _, ok := groups[e.Handle]; !ok {
			order = append(order, e.Handle)
		}
		groups[e.Handle] = append(groups[e.Handle], e)
	}
	return groups, order
}

func flushSender(ctx context.Context, cfg config.Config, env []string, box outbox.Box, entries []outbox.Entry) {
	for _, e := range entries {
		if ctx.Err() != nil {
			return
		}
		if stop := flushOne(ctx, cfg, env, box, e); stop {
			return
		}
	}
}

// flushOne expires, delivers, or stops the sender on a transport failure.
// It returns true when later messages for this sender must wait.
func flushOne(ctx context.Context, cfg config.Config, env []string, box outbox.Box, e outbox.Entry) bool {
	now := outboxNow().UTC()
	if expired(e, now) {
		expireEntry(ctx, env, box, e, now)
		return false
	}
	resp, kind, err := deliverStored(ctx, cfg, env, e)
	switch kind {
	case attemptOK:
		e.State = outbox.StateDelivered
		e.MsgID = resp.Name
		e.HubState = resp.State
		_ = box.Finish(e)
		return false
	case attemptRefusal:
		_ = box.Spool(outbox.LogEvent{
			Time: now, Event: eventlog.EventRejected, Actor: logActor(e.Handle),
			Name: e.Target, MsgID: e.ClientID, Detail: refusalDetail(resp, err),
		})
		_ = box.Drop(e.ClientID)
		return false
	default:
		noteRetry(box, e, now, err)
		return true
	}
}

func refusalDetail(resp daemon.Response, err error) string {
	if msg := strings.TrimSpace(resp.Error); msg != "" {
		return msg
	}
	if err != nil {
		return err.Error()
	}
	return "hub refused"
}

func noteRetry(box outbox.Box, e outbox.Entry, now time.Time, err error) {
	e.Attempts++
	if err != nil {
		e.LastError = err.Error()
	}
	e.NextAt = now.Add(backoffFor(e.Attempts))
	_ = box.Put(e)
	_ = box.Spool(outbox.LogEvent{
		Time: now, Event: eventlog.EventRetry, Actor: logActor(e.Handle),
		Name: e.Target, MsgID: e.ClientID, Detail: e.LastError,
	})
}

func expired(e outbox.Entry, now time.Time) bool {
	if e.Kind == outbox.KindBypass || e.TTL == "" {
		return false
	}
	d, err := time.ParseDuration(e.TTL)
	if err != nil || d <= 0 {
		return false
	}
	return !now.Before(e.EnqueuedAt.Add(d))
}

func expireEntry(ctx context.Context, env []string, box outbox.Box, e outbox.Entry, now time.Time) {
	text := fmt.Sprintf("agentbus: message %s to %s expired\n", e.ClientID, e.Target)
	notifyExpiry(ctx, env, e.Handle, e.ClientID, text)
	_ = box.Spool(outbox.LogEvent{
		Time: now, Event: eventlog.EventExpired, Actor: logActor(e.Handle),
		Name: e.Target, MsgID: e.ClientID, Detail: "ttl elapsed",
	})
	e.State = outbox.StateExpired
	_ = box.Finish(e)
}

func notifyExpiry(ctx context.Context, env []string, handle, id, text string) {
	if strings.HasPrefix(handle, "anon:") || handle == "" {
		return
	}
	ad := &herdr.Adapter{Timeout: 5 * time.Second}
	state, err := ad.State(ctx, handle)
	if err != nil || !canPaste(state) {
		return
	}
	empty, reason, err := ad.ComposerEmpty(ctx, handle)
	if err != nil || !empty {
		box, boxErr := openBox(env)
		if boxErr == nil {
			detail := reason
			if err != nil {
				detail = err.Error()
			}
			_ = box.Spool(outbox.LogEvent{
				Time: outboxNow().UTC(), Event: eventlog.EventDeliverAttempt,
				Actor: "agentbus", Name: handle, MsgID: id, Detail: "expiry notice not sent", Reason: detail,
			})
		}
		return
	}
	_, _ = ad.Deliver(ctx, handle, text)
}

func canPaste(state adapter.State) bool {
	switch state {
	case adapter.StateIdle, adapter.StateWorking, adapter.StateDone:
		return true
	default:
		return false
	}
}

func deliverStored(ctx context.Context, cfg config.Config, env []string, e outbox.Entry) (daemon.Response, string, error) {
	if sock, up := tunnelUp(ctx, env, e.Handle); up {
		resp, err := dialDaemon(ctx, sock, busRequest(e.Handle, e, false))
		if err != nil {
			return daemon.Response{}, classifyDial(err), err
		}
		if !resp.OK {
			return resp, attemptRefusal, nil
		}
		return resp, attemptOK, nil
	}
	var stdout, stderr bytes.Buffer
	replay := e
	replay.Args = withoutBool(e.Args, "--json")
	att := attemptSSH(ctx, cfg, env, replay, false, false, &stdout, &stderr)
	if att.kind == attemptOK {
		id, state, _ := strings.Cut(strings.TrimSpace(stdout.String()), " ")
		return daemon.Response{OK: true, Name: id, State: state}, attemptOK, nil
	}
	if att.kind == attemptRefusal {
		att.resp.Error = strings.TrimSpace(stderr.String())
		return att.resp, attemptRefusal, att.err
	}
	return daemon.Response{}, att.kind, att.err
}

func tunnelUp(ctx context.Context, env []string, handle string) (string, bool) {
	if handle == "" {
		return "", false
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		return "", false
	}
	if _, err := os.Stat(dirs.TunnelSocketPath); err != nil {
		return "", false
	}
	ctx, cancel := context.WithTimeout(ctx, tunnelProbeTimeout)
	defer cancel()
	resp, err := dialDaemon(ctx, dirs.TunnelSocketPath, daemon.Request{
		Op: "ping", Protocol: daemon.ProtocolVersion, Handle: handle,
	})
	if err != nil || !resp.OK || resp.Protocol != daemon.ProtocolVersion {
		return "", false
	}
	return dirs.TunnelSocketPath, true
}

func forwardSpool(ctx context.Context, box outbox.Box, sock, handle string) {
	events, err := box.SpoolEvents()
	if err != nil || len(events) == 0 || handle == "" {
		return
	}
	req := daemon.Request{
		Op: "log-append", Protocol: daemon.ProtocolVersion, Handle: handle,
		Events: clientEvents(events),
	}
	resp, err := dialDaemon(ctx, sock, req)
	if err != nil || !resp.OK {
		return
	}
	_ = box.ClearSpool()
}

func clientEvents(events []outbox.LogEvent) []daemon.ClientEvent {
	out := make([]daemon.ClientEvent, len(events))
	for i, ev := range events {
		out[i] = daemon.ClientEvent{
			Time: ev.Time, Event: ev.Event, Actor: ev.Actor, Name: ev.Name,
			MsgID: ev.MsgID, Detail: ev.Detail, Reason: ev.Reason,
			TargetState: ev.TargetState, Resolved: ev.Resolved,
		}
	}
	return out
}

func backoffFor(attempt int) time.Duration {
	d := outboxInitialBackoff
	for i := 1; i < attempt && d < outboxMaxBackoff; i++ {
		d *= 2
	}
	if d > outboxMaxBackoff {
		return outboxMaxBackoff
	}
	return d
}

func jitter(d time.Duration) time.Duration {
	span := int64(d / 5)
	if span <= 0 {
		return d
	}
	n, err := rand.Int(rand.Reader, big.NewInt(2*span+1))
	if err != nil {
		return d
	}
	return d + time.Duration(n.Int64()-span)
}

func cmdOutbox(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, "usage: agentbus outbox <ls|flush>")
		return 2
	}
	switch args[0] {
	case "ls":
		return cmdOutboxLs(args[1:], env, stdout, stderr)
	case "flush":
		return cmdOutboxFlush(ctx, args[1:], env, stdout, stderr)
	default:
		fmt.Fprintln(stderr, "usage: agentbus outbox <ls|flush>")
		return 2
	}
}

func cmdOutboxLs(args, env []string, stdout, stderr io.Writer) int {
	if len(args) != 0 {
		fmt.Fprintln(stderr, "usage: agentbus outbox ls")
		return 2
	}
	box, err := openBox(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus outbox: %s\n", err)
		return 1
	}
	pending, err := box.Pending()
	if err != nil {
		fmt.Fprintf(stderr, "agentbus outbox: %s\n", err)
		return 1
	}
	now := outboxNow()
	for _, e := range pending {
		line := e.ClientID + " " + outbox.StatusLine(e, now)
		if outbox.Stale(e, now) {
			line += " pending for more than 24h"
		}
		fmt.Fprintln(stdout, line)
	}
	return 0
}

func cmdOutboxFlush(ctx context.Context, args, env []string, stdout, stderr io.Writer) int {
	watch, rest, err := takeBool(args, "--watch")
	if err != nil || len(rest) != 0 {
		fmt.Fprintln(stderr, "usage: agentbus outbox flush [--watch]")
		return 2
	}
	cfg, err := hubSettings(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus outbox: %s\n", err)
		return 1
	}
	if watch {
		return watchOutbox(ctx, cfg, env, stdout, stderr)
	}
	flushOutbox(withFlushing(ctx), cfg, env, stdout, stderr)
	return 0
}

func watchOutbox(ctx context.Context, cfg config.Config, env []string, stdout, stderr io.Writer) int {
	backoff := outboxInitialBackoff
	for ctx.Err() == nil {
		flushOutbox(withFlushing(ctx), cfg, env, stdout, stderr)
		if !waitWake(ctx, env, jitter(backoff)) {
			return 0
		}
		if backoff < outboxMaxBackoff {
			backoff *= 2
		}
	}
	return 0
}

func waitWake(ctx context.Context, env []string, backoff time.Duration) bool {
	timer := time.NewTimer(backoff)
	defer timer.Stop()
	tick := time.NewTicker(200 * time.Millisecond)
	defer tick.Stop()
	sock := tunnelSocket(env)
	for {
		select {
		case <-ctx.Done():
			return false
		case <-timer.C:
			return true
		case <-tick.C:
			if socketAnswers(sock) {
				return true
			}
		}
	}
}

func tunnelSocket(env []string) string {
	dirs, err := paths.FromEnv(env)
	if err != nil {
		return ""
	}
	return dirs.TunnelSocketPath
}

func socketAnswers(path string) bool {
	if path == "" {
		return false
	}
	conn, err := net.DialTimeout("unix", path, 50*time.Millisecond)
	if err != nil {
		return false
	}
	_ = conn.Close()
	return true
}

func outboxStatus(env []string, id string) (string, bool) {
	box, err := openBox(env)
	if err != nil {
		return "", false
	}
	e, ok, err := box.Get(id)
	if err != nil || !ok {
		return "", false
	}
	return outbox.StatusLine(e, outboxNow()), true
}

func reportStaleOutbox(env []string, stdout io.Writer) {
	box, err := openBox(env)
	if err != nil {
		return
	}
	pending, err := box.Pending()
	if err != nil {
		return
	}
	now := outboxNow()
	for _, e := range pending {
		if outbox.Stale(e, now) {
			fmt.Fprintf(stdout, "outbox: %s pending for more than 24h\n", e.ClientID)
		}
	}
}

// hubRecordHook runs the SendMessage hook. A transport failure spools the
// bypass record instead of dropping it, and still exits 0.
func hubRecordHook(ctx context.Context, cfg config.Config, args, env []string, stdin io.Reader, stdout, stderr io.Writer) int {
	raw, err := io.ReadAll(io.LimitReader(stdin, 1<<20))
	if err != nil || len(bytes.TrimSpace(raw)) == 0 {
		return 0
	}
	id, err := chooseClientID(cfg.Machine, "")
	if err != nil {
		return 0
	}
	tun, ok, openErr := openTunnel(ctx, cfg, "hook", env, stderr)
	mismatch := errors.Is(openErr, errProtocolMismatch)
	if openErr != nil && !mismatch {
		return 0
	}
	if ok {
		if hookViaTunnel(tun, env, id, raw, stdout) {
			return 0
		}
		if !mismatch {
			spoolBypass(cfg, env, id, raw)
		}
		return 0
	}
	call := hubcall.Call{Args: append([]string{"hook"}, args...), Stdin: bytes.NewReader(raw), Stderr: io.Discard}
	total := hubHookTimeout(env)
	hub := hookHub(hubcall.Hub{Target: cfg.Hub, Tool: "agentbus", ConnectTimeout: cfg.RemoteTimeout}, total)
	ctx, cancel := context.WithTimeout(ctx, total)
	defer cancel()
	code, err := hub.Run(ctx, call)
	if err != nil && !mismatch && classifySSH(err, false) == attemptTransport {
		spoolBypass(cfg, env, id, raw)
	}
	if err != nil || ctx.Err() != nil {
		return 0
	}
	return code
}

func hookViaTunnel(ctx context.Context, _ []string, id string, raw []byte, stdout io.Writer) bool {
	session, target, body, ok := parseHook(raw)
	if !ok {
		return true
	}
	t, ok := tunnelFrom(ctx)
	if !ok {
		return false
	}
	callCtx, cancel := context.WithTimeout(ctx, hookTimeout)
	defer cancel()
	resp, err := dialDaemon(callCtx, t.socket, t.stamp(daemon.Request{
		Op: "bypass", Name: target, Body: body, Session: session, ClientID: id,
	}))
	if err != nil {
		return false
	}
	if resp.OK && resp.Name != "" {
		out, err := json.Marshal(busHint(resp.Name))
		if err == nil {
			fmt.Fprintf(stdout, "%s\n", out)
		}
	}
	return true
}

func spoolBypass(cfg config.Config, env []string, id string, raw []byte) {
	session, target, body, ok := parseHook(raw)
	if !ok || strings.TrimSpace(body) == "" {
		return
	}
	handle, err := outboxHandle(context.Background(), cfg, env)
	if err != nil {
		handle = ""
	}
	box, err := openBox(env)
	if err != nil {
		return
	}
	e := outbox.Entry{
		Kind: outbox.KindBypass, ClientID: id, Handle: handle, Target: target,
		Body: body, Session: session, EnqueuedAt: outboxNow().UTC(), State: outbox.StatePending,
	}
	if _, err := box.Enqueue(e); err != nil {
		return
	}
	_ = box.Spool(outbox.LogEvent{
		Time: e.EnqueuedAt, Event: eventlog.EventQueued, Actor: logActor(handle),
		Name: target, MsgID: id, Detail: "hub unreachable",
	})
}
