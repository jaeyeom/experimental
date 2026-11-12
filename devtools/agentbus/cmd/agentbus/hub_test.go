package main

import (
	"bytes"
	"context"
	"io"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall/sshtest"
)

type hubSetup struct {
	ssh     *sshtest.Fake
	env     []string
	runtime string
}

// startHub puts a fake ssh and a fake hub agentbus on PATH and writes
// config with the given lines. Any local daemon call fails the test.
func startHub(t *testing.T, configLines ...string) *hubSetup {
	t.Helper()
	fake := sshtest.Install(t)
	fake.InstallTool("agentbus")
	xdg := t.TempDir()
	if len(configLines) > 0 {
		dir := filepath.Join(xdg, "agentbus")
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(dir, "config.toml"), []byte(strings.Join(configLines, "\n")+"\n"), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	origEnsure, origDial := ensureDaemon, dialDaemon
	ensureDaemon = func([]string, io.Writer) int {
		t.Error("hub mode started a local daemon")
		return 1
	}
	dialDaemon = func(context.Context, string, daemon.Request) (daemon.Response, error) {
		t.Error("hub mode dialed a local daemon")
		return daemon.Response{}, os.ErrNotExist
	}
	t.Cleanup(func() { ensureDaemon, dialDaemon = origEnsure, origDial })
	runtime := t.TempDir()
	return &hubSetup{
		ssh:     fake,
		runtime: runtime,
		env: []string{
			"HOME=" + t.TempDir(),
			"XDG_CONFIG_HOME=" + xdg,
			"XDG_RUNTIME_DIR=" + runtime,
			"XDG_STATE_HOME=" + t.TempDir(),
		},
	}
}

func (h *hubSetup) run(t *testing.T, stdin string, args ...string) (int, string, string) {
	t.Helper()
	return h.runEnv(t, h.env, stdin, args...)
}

func (h *hubSetup) runEnv(t *testing.T, env []string, stdin string, args ...string) (int, string, string) {
	t.Helper()
	var stdout, stderr bytes.Buffer
	code := run(context.Background(), args, env, strings.NewReader(stdin), &stdout, &stderr)
	return code, stdout.String(), stderr.String()
}

func TestHubSendQuotesEachArgument(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`)
	h.ssh.Write("agentbus.stdout", "m-1 delivered\n")
	code, stdout, stderr := h.run(t, "", "send", "ops/x", "a b; c")
	if code != 0 || stdout != "m-1 delivered\n" {
		t.Fatalf("send = %d, stdout %q, stderr %q", code, stdout, stderr)
	}
	if got := h.ssh.Lines("agentbus.args"); !slices.Equal(got, []string{"send", "ops/x", "a b; c"}) {
		t.Fatalf("hub args = %q", got)
	}
	argv := h.ssh.Lines("argv")
	if !slices.Contains(argv, "me@hub") || !slices.Contains(argv, "BatchMode=yes") || !slices.Contains(argv, "ConnectTimeout=5") {
		t.Fatalf("ssh argv = %q", argv)
	}
	if last := argv[len(argv)-1]; !strings.HasSuffix(last, "agentbus send ops/x 'a b; c'") {
		t.Fatalf("remote command = %q", last)
	}
}

func TestHubEnvOverridesConfig(t *testing.T) {
	h := startHub(t, `hub = "me@other"`)
	env := append(append([]string{}, h.env...), "AGENTBUS_HUB=me@hub")
	if code, _, stderr := h.runEnv(t, env, "", "list"); code != 0 {
		t.Fatalf("list = %d, stderr %q", code, stderr)
	}
	if argv := h.ssh.Lines("argv"); !slices.Contains(argv, "me@hub") {
		t.Fatalf("ssh argv = %q", argv)
	}
}

func TestHubSendForwardsStdinAndReadsFileLocally(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`)
	if code, _, stderr := h.run(t, "from stdin\n", "send", "ops/x"); code != 0 {
		t.Fatalf("send = %d, stderr %q", code, stderr)
	}
	if got := h.ssh.Read("agentbus.stdin"); got != "from stdin\n" {
		t.Fatalf("hub stdin = %q", got)
	}

	path := filepath.Join(t.TempDir(), "body.txt")
	if err := os.WriteFile(path, []byte("from file\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if code, _, stderr := h.run(t, "ignored", "send", "ops/x", "--file", path, "--ttl", "30m"); code != 0 {
		t.Fatalf("send --file = %d, stderr %q", code, stderr)
	}
	if got := h.ssh.Lines("agentbus.args"); !slices.Equal(got, []string{"send", "ops/x", "--ttl", "30m"}) {
		t.Fatalf("hub args = %q", got)
	}
	if got := h.ssh.Read("agentbus.stdin"); got != "from file\n" {
		t.Fatalf("hub stdin = %q", got)
	}
}

func TestHubSendForwardsClientIDWithFile(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`)
	path := filepath.Join(t.TempDir(), "body.txt")
	if err := os.WriteFile(path, []byte("from file\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	id := "mac:01ARZ3NDEKTSV4RRFFQ69G5FAV"
	if code, _, stderr := h.run(t, "ignored", "send", "ops/x", "--file", path, "--client-id", id, "--ttl", "30m"); code != 0 {
		t.Fatalf("send --file = %d, stderr %q", code, stderr)
	}
	if got := h.ssh.Lines("agentbus.args"); !slices.Equal(got, []string{"send", "ops/x", "--client-id", id, "--ttl", "30m"}) {
		t.Fatalf("hub args = %q", got)
	}
	if got := h.ssh.Read("agentbus.stdin"); got != "from file\n" {
		t.Fatalf("hub stdin = %q", got)
	}
}

func TestHubStdinIsNotForwardedToOtherCommands(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`)
	if code, _, stderr := h.run(t, "keep me", "status", "m-1"); code != 0 {
		t.Fatalf("status = %d, stderr %q", code, stderr)
	}
	if got := h.ssh.Read("agentbus.stdin"); got != "" {
		t.Fatalf("hub stdin = %q", got)
	}
}

func TestHubReturnsRemoteExitCodeAndStderr(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`)
	h.ssh.Write("agentbus.stderr", "agentbus: unknown name ops/x\n")
	h.ssh.Write("agentbus.code", "3")
	code, _, stderr := h.run(t, "", "status", "m-1")
	if code != 3 || stderr != "agentbus: unknown name ops/x\n" {
		t.Fatalf("status = %d, stderr %q", code, stderr)
	}
}

func TestHubRegisterSendsTheCallerHandle(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`, `machine = "linux2"`)
	herdrFake := herdrtest.Install(t)
	herdrFake.Write("local.agents", agentsJSON("w1:p1=idle"))
	env := append(append([]string{}, h.env...), "HERDR_PANE_ID=w1:p1")
	if code, _, stderr := h.runEnv(t, env, "", "register", "ops/x", "--when", "testing"); code != 0 {
		t.Fatalf("register = %d, stderr %q", code, stderr)
	}
	if got := h.ssh.Read("agentbus.handle"); got != "herdr:linux2/default/w1:p1" {
		t.Fatalf("hub caller = %q", got)
	}
	if got := h.ssh.Lines("agentbus.args"); !slices.Equal(got, []string{"register", "ops/x", "--when", "testing"}) {
		t.Fatalf("hub args = %q", got)
	}
}

func TestHubRegisterNeedsMachine(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`)
	env := append(append([]string{}, h.env...), "HERDR_PANE_ID=w1:p1")
	code, _, stderr := h.runEnv(t, env, "", "register", "ops/x", "--when", "testing")
	if code == 0 || !strings.Contains(stderr, `hub mode needs machine = "<label>"`) {
		t.Fatalf("register = %d, stderr %q", code, stderr)
	}
	if h.ssh.Read("argv") != "" {
		t.Fatal("register ran ssh without a caller handle")
	}
}

func TestHubUnreachableFailsFast(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`, `remote_timeout = "1s"`)
	h.ssh.Write("down", "")
	h.ssh.Unreachable()
	start := time.Now()
	code, _, stderr := h.run(t, "", "send", "ops/x", "hi")
	if took := time.Since(start); took > 5*time.Second {
		t.Fatalf("send took %s", took)
	}
	if code == 0 || !strings.Contains(stderr, "agentbus: hub me@hub unreachable: host unreachable") {
		t.Fatalf("send = %d, stderr %q", code, stderr)
	}
	if entries, _ := os.ReadDir(h.runtime); len(entries) != 0 {
		t.Fatalf("hub mode created local runtime state: %v", entries)
	}
}

func TestHubHooksExitZeroWhenUnreachable(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`)
	h.ssh.Write("locked", "")
	for _, name := range []string{"hook", "prompt-hook"} {
		code, stdout, stderr := h.run(t, `{"prompt":"x"}`, name)
		if code != 0 || stdout != "" || stderr != "" {
			t.Fatalf("%s = %d, stdout %q, stderr %q", name, code, stdout, stderr)
		}
	}
}

func TestHubHooksUseAShortConnectTimeout(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`, `remote_timeout = "9s"`)
	if code, _, _ := h.run(t, `{}`, "hook"); code != 0 {
		t.Fatalf("hook = %d", code)
	}
	if argv := h.ssh.Lines("argv"); !slices.Contains(argv, "ConnectTimeout=2") {
		t.Fatalf("hook ssh argv = %q, want ConnectTimeout=2", argv)
	}
}

func TestHubHookTimeoutBoundsTheWholeCall(t *testing.T) {
	for _, tt := range []struct {
		name, file, body string
	}{
		{"hub never answers", "hang", ""},
		{"hub hook is slow", "agentbus.sleep", "5"},
	} {
		t.Run(tt.name, func(t *testing.T) {
			h := startHub(t, `hub = "me@hub"`)
			h.ssh.Write(tt.file, tt.body)
			h.ssh.Reachable()
			env := append(slices.Clone(h.env), "AGENTBUS_HOOK_TIMEOUT=500ms")
			for _, name := range []string{"hook", "prompt-hook"} {
				start := time.Now()
				code, stdout, stderr := h.runEnv(t, env, `{"prompt":"x"}`, name)
				if took := time.Since(start); took > 2500*time.Millisecond {
					t.Fatalf("%s took %s", name, took)
				}
				if code != 0 || stdout != "" || stderr != "" {
					t.Fatalf("%s = %d, stdout %q, stderr %q", name, code, stdout, stderr)
				}
				// Under load the deadline can kill ssh before it records
				// argv; TestHookHubCapsConnectTimeout covers the options.
				if argv := h.ssh.Lines("argv"); argv != nil && !slices.Contains(argv, "ConnectTimeout=1") {
					t.Fatalf("%s ssh argv = %q, want ConnectTimeout=1", name, argv)
				}
			}
		})
	}
}

func TestHookHubCapsConnectTimeout(t *testing.T) {
	for _, tt := range []struct {
		total, connect time.Duration
	}{
		{500 * time.Millisecond, 500 * time.Millisecond},
		{hookConnectTimeout, hookConnectTimeout},
		{defaultHookTimeout, hookConnectTimeout},
		{9 * time.Second, hookConnectTimeout},
	} {
		hub := hookHub(hubcall.Hub{Target: "me@hub", Tool: "agentbus", ConnectTimeout: 9 * time.Second}, tt.total)
		if hub.ConnectTimeout != tt.connect || hub.Deadline != tt.total || hub.Target != "me@hub" || hub.Tool != "agentbus" {
			t.Fatalf("hookHub(%s) = %+v, want ConnectTimeout %s, Deadline %s", tt.total, hub, tt.connect, tt.total)
		}
	}
}

func TestHubHelpStaysLocal(t *testing.T) {
	h := startHub(t, `hub = "me@hub"`)
	if code, stdout, _ := h.run(t, "", "send", "--help"); code != 0 || !strings.Contains(stdout, "agentbus send") {
		t.Fatalf("send --help = %d, %q", code, stdout)
	}
	if h.ssh.Read("argv") != "" {
		t.Fatal("help ran ssh")
	}
}
