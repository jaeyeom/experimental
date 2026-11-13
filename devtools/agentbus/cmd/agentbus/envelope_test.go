package main

import (
	"bytes"
	"context"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

func TestEnvelopeStampsFromAndPreviewsLongBodies(t *testing.T) {
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
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach: map[string]adapter.State{
			holder:   adapter.StateIdle,
			reviewer: adapter.StateIdle,
		},
		Result: map[string]adapter.Result{
			holder:   adapter.ResultAccepted,
			reviewer: adapter.ResultAccepted,
		},
		PaneHandles: map[string]string{"p1": holder, "p2": reviewer},
		InventoryAgents: []adapter.Agent{
			{Handle: holder, PaneID: "p1", SessionID: "default", Status: "idle"},
			{Handle: reviewer, PaneID: "p2", SessionID: "default", Status: "idle"},
		},
	}
	stop := startDaemon(t, dirs, daemon.Config{
		Adapters:    []adapter.Adapter{ad},
		InlineBytes: 32,
	})
	t.Cleanup(stop)
	env := func(pane string) []string {
		if pane == "" {
			return append([]string{}, base...)
		}
		return append(append([]string{}, base...), "HERDR_PANE_ID="+pane)
	}
	mustCmd(t, []string{"register", "ops/inbox", "--when", "reviews"}, env("p1"))
	mustCmd(t, []string{"register", "eng/reviewer", "--when", "reviews"}, env("p2"))
	if code, _, stderr := runCmd([]string{"register", "eng/other", "--when", "nope"}, env("")); code == 0 || !strings.Contains(stderr, "cannot register") {
		t.Fatalf("anon register = %d %q", code, stderr)
	}

	sent, _ := mustCmd(t, []string{"send", "eng/reviewer", "hello"}, env("p1"))
	id, state, ok := strings.Cut(strings.TrimSpace(sent), " ")
	if !ok || !strings.HasPrefix(id, "m-") || state != "queued for delivery" {
		t.Fatalf("send = %q", sent)
	}
	awaitDelivered(t, base, id)
	if !strings.Contains(ad.LastText, "from ops/inbox → eng/reviewer") || strings.Contains(ad.LastText, holder) {
		t.Fatalf("delivery = %q", ad.LastText)
	}
	shown, _ := mustCmd(t, []string{"show", id}, base)
	if !strings.Contains(shown, "from: ops/inbox") || !strings.Contains(shown, "to: eng/reviewer") || !strings.Contains(shown, "hello") {
		t.Fatalf("show = %q", shown)
	}
	status, _ := mustCmd(t, []string{"status", id}, base)
	if !strings.Contains(status, "state: delivered") {
		t.Fatalf("status = %q", status)
	}

	replied, _ := mustCmd(t, []string{"reply", id, "ack"}, env("p2"))
	replyID, _, ok := strings.Cut(strings.TrimSpace(replied), " ")
	if !ok || !strings.HasPrefix(replyID, "m-") {
		t.Fatalf("reply = %q", replied)
	}
	replyShow, _ := mustCmd(t, []string{"show", replyID}, base)
	if !strings.Contains(replyShow, "from: eng/reviewer") || !strings.Contains(replyShow, "to: ops/inbox") || !strings.Contains(replyShow, "re: "+id) {
		t.Fatalf("reply show = %q", replyShow)
	}
	chain, _ := mustCmd(t, []string{"thread", replyID}, base)
	if !strings.Contains(chain, "id: "+id) || strings.Index(chain, "id: "+replyID) < strings.Index(chain, "id: "+id) {
		t.Fatalf("thread = %q", chain)
	}

	host, err := os.Hostname()
	if err != nil || strings.TrimSpace(host) == "" {
		host = "unknown"
	}
	wantFrom := "anon:" + host + "-" + strconv.Itoa(os.Getpid())
	anonSent, _ := mustCmd(t, []string{"send", "ops/inbox", "ping"}, env(""))
	anonID, _, ok := strings.Cut(strings.TrimSpace(anonSent), " ")
	if !ok {
		t.Fatalf("anon send = %q", anonSent)
	}
	awaitDelivered(t, base, anonID)
	if !strings.Contains(ad.LastText, "from "+wantFrom) || !strings.Contains(ad.LastText, "no reply address") {
		t.Fatalf("anon delivery = %q, want from %s", ad.LastText, wantFrom)
	}
	if code, _, stderr := runCmd([]string{"reply", anonID, "nope"}, env("p1")); code == 0 || !strings.Contains(stderr, "no reply address") {
		t.Fatalf("reply to anon = %d %q", code, stderr)
	}

	body := strings.Repeat("x", 80) + "END-OF-BODY"
	longSent, _ := mustCmd(t, []string{"send", "eng/reviewer", body}, env("p1"))
	longID, _, ok := strings.Cut(strings.TrimSpace(longSent), " ")
	if !ok {
		t.Fatalf("long send = %q", longSent)
	}
	awaitDelivered(t, base, longID)
	if strings.Contains(ad.LastText, "END-OF-BODY") || !strings.Contains(ad.LastText, "agentbus show "+longID) {
		t.Fatalf("long delivery id %q text %q", longID, ad.LastText)
	}
	longShow, _ := mustCmd(t, []string{"show", longID}, base)
	if !strings.Contains(longShow, "END-OF-BODY") {
		t.Fatalf("show long = %q", longShow)
	}

	file := filepath.Join(t.TempDir(), "body.txt")
	if err := os.WriteFile(file, []byte("from-file"), 0o600); err != nil {
		t.Fatal(err)
	}
	filed, _ := mustCmd(t, []string{"send", "eng/reviewer", "--file", file}, env("p1"))
	fileID, _, ok := strings.Cut(strings.TrimSpace(filed), " ")
	if !ok {
		t.Fatalf("file send = %q", filed)
	}
	fileShow, _ := mustCmd(t, []string{"show", fileID}, base)
	if !strings.Contains(fileShow, "from-file") {
		t.Fatalf("show file = %q", fileShow)
	}

	code, stdinSent, stderr := runCmdIn([]string{"send", "eng/reviewer"}, env("p1"), strings.NewReader("from-stdin"))
	if code != 0 || !strings.Contains(stdinSent, "queued for delivery") {
		t.Fatalf("stdin send = %d %q %q", code, stdinSent, stderr)
	}
}

func TestReplyAndSendBodySources(t *testing.T) {
	oldDial, oldEnsure := dialDaemon, ensureDaemon
	t.Cleanup(func() { dialDaemon, ensureDaemon = oldDial, oldEnsure })
	var got []daemon.Request
	dialDaemon = func(_ context.Context, _ string, req daemon.Request) (daemon.Response, error) {
		got = append(got, req)
		return daemon.Response{OK: true, Name: "m-9", State: "queued"}, nil
	}
	ensureDaemon = func([]string, io.Writer) int { return 0 }
	env := []string{"HOME=" + t.TempDir(), "XDG_RUNTIME_DIR=" + t.TempDir(), "XDG_STATE_HOME=" + t.TempDir()}
	runBody := func(stdin string, args ...string) (int, string) {
		t.Helper()
		got = nil
		var stdout, stderr bytes.Buffer
		code := run(context.Background(), args, env, strings.NewReader(stdin), &stdout, &stderr)
		return code, stderr.String()
	}
	replyBody := func() string {
		t.Helper()
		for _, req := range got {
			if req.Op == "reply" {
				return req.Body
			}
		}
		return ""
	}
	file := filepath.Join(t.TempDir(), "msg.txt")
	if err := os.WriteFile(file, []byte("from-file"), 0o600); err != nil {
		t.Fatal(err)
	}
	if code, stderr := runBody("", "reply", "m-1", "--file", file); code != 0 || replyBody() != "from-file" {
		t.Fatalf("reply --file = %d %q body %q", code, stderr, replyBody())
	}
	if code, stderr := runBody("from-stdin", "reply", "m-1"); code != 0 || replyBody() != "from-stdin" {
		t.Fatalf("reply stdin = %d %q body %q", code, stderr, replyBody())
	}
	if code, stderr := runBody("dash-stdin", "reply", "m-1", "-"); code != 0 || replyBody() != "dash-stdin" {
		t.Fatalf("reply - = %d %q body %q", code, stderr, replyBody())
	}
	if code, stderr := runBody("nope", "reply", "m-1", "--bogus", "x"); code == 0 || !strings.Contains(stderr, "unknown flag --bogus") || len(got) != 0 {
		t.Fatalf("reply --bogus = %d %q calls %d", code, stderr, len(got))
	}
	if code, stderr := runBody("", "reply", "m-1", "--", "--bogus", "x"); code != 0 || replyBody() != "--bogus x" {
		t.Fatalf("reply -- --bogus = %d %q body %q", code, stderr, replyBody())
	}
	if code, stderr := runBody("nope", "send", "ops/x", "--bogus", "x"); code == 0 || !strings.Contains(stderr, "unknown flag --bogus") || len(got) != 0 {
		t.Fatalf("send --bogus = %d %q calls %d", code, stderr, len(got))
	}
	if code, stderr := runBody("", "send", "ops/x", "--", "--bogus", "x"); code != 0 {
		t.Fatalf("send -- --bogus = %d %q", code, stderr)
	}
	var sent string
	for _, req := range got {
		if req.Op == "send" {
			sent = req.Body
		}
	}
	if sent != "--bogus x" {
		t.Fatalf("send body = %q", sent)
	}
}
