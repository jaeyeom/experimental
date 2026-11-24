package decide

import (
	"context"
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/herdrtest"
)

func writePluginScript(t *testing.T, dir, name, body string) string {
	t.Helper()
	script := filepath.Join(dir, name)
	if err := os.WriteFile(script, []byte("#!/bin/sh\n"+body), 0o755); err != nil { //nolint:gosec // test helper script
		t.Fatal(err)
	}
	return script
}

func writePluginConfig(t *testing.T, transports map[string][]string) {
	t.Helper()
	data, err := json.Marshal(Config{Transports: transports})
	if err != nil {
		t.Fatal(err)
	}
	writeConfig(t, string(data))
}

func TestPluginTransportDeliverDelivered(t *testing.T) {
	dir := t.TempDir()
	stdinFile := filepath.Join(dir, "stdin.json")
	argsFile := filepath.Join(dir, "args.txt")
	shellMarker := filepath.Join(dir, "shell-ran")

	script := writePluginScript(t, dir, "plugin",
		"printf '%s\\n' \"$@\" > '"+argsFile+"'\n"+
			"cat > '"+stdinFile+"'\n"+
			"printf '%s\\n' '{\"outcome\":\"delivered\",\"externalId\":\"ext-42\"}'\n",
	)
	literalArg := "$(touch " + shellMarker + ")"
	writePluginConfig(t, map[string][]string{
		"tmux": {script, "--flag", literalArg},
	})

	clock := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	item := answeredRelay(t, led, "tmux:%1", "")

	got, err := led.DeliverAnswer(&item, nil)
	if err != nil {
		t.Fatalf("DeliverAnswer error = %v", err)
	}
	if len(got.Delivery) != 1 {
		t.Fatalf("Delivery = %+v, want 1 record", got.Delivery)
	}
	d := got.Delivery[0]
	if d.Outcome != OutcomeDelivered || d.Target != "tmux:%1" || d.BusID != "ext-42" {
		t.Fatalf("Delivery[0] = %+v, want delivered target=tmux:%%1 busId=ext-42", d)
	}
	if _, statErr := os.Stat(shellMarker); !errors.Is(statErr, fs.ErrNotExist) {
		t.Fatal("plugin command was executed through a shell")
	}
	if gotArgs := readText(t, argsFile); gotArgs != "--flag\n"+literalArg+"\n" {
		t.Fatalf("args = %q, want %q", gotArgs, "--flag\n"+literalArg+"\n")
	}

	var req PluginRequest
	if err := json.Unmarshal([]byte(readText(t, stdinFile)), &req); err != nil {
		t.Fatalf("unmarshal plugin request: %v", err)
	}
	if req.Version != PluginProtocolVersion || req.Op != pluginOpDeliver {
		t.Fatalf("req header = version:%d op:%q", req.Version, req.Op)
	}
	if req.Target != (PluginTarget{Scheme: "tmux", Address: "%1"}) {
		t.Fatalf("req.Target = %+v", req.Target)
	}
	if req.DeliveryKey != DeliveryKey(&item) {
		t.Fatalf("req.DeliveryKey = %q, want %q", req.DeliveryKey, DeliveryKey(&item))
	}
	if req.Item == nil || req.Item.ID != item.ID || req.Item.Answer == nil || req.Item.Answer.Value != "a" {
		t.Fatalf("req.Item = %+v", req.Item)
	}
	if !strings.Contains(req.Message, item.ID) {
		t.Fatalf("req.Message = %q, want item id %s", req.Message, item.ID)
	}
}

func TestPluginTransportDeliverNote(t *testing.T) {
	dir := t.TempDir()
	stdinFile := filepath.Join(dir, "stdin.json")
	script := writePluginScript(t, dir, "plugin",
		"cat > '"+stdinFile+"'\n"+
			"printf '%s\\n' '{\"outcome\":\"delivered\",\"externalId\":\"note-9\"}'\n",
	)
	writePluginConfig(t, map[string][]string{"webhook": {script}})

	item := &Item{ID: "D-9", Owner: "bot", NotifyPane: "webhook:https://example.com/h"}
	d, err := NotifyItemVerbatim(item, "webhook:https://example.com/h", "", "please check", "")
	if err != nil {
		t.Fatalf("NotifyItemVerbatim error = %v", err)
	}
	if d.Outcome != OutcomeDelivered || d.Target != "webhook:https://example.com/h" || d.BusID != "note-9" {
		t.Fatalf("Delivery = %+v", d)
	}
	var req PluginRequest
	if err := json.Unmarshal([]byte(readText(t, stdinFile)), &req); err != nil {
		t.Fatalf("unmarshal plugin request: %v", err)
	}
	if req.Version != PluginProtocolVersion || req.Op != pluginOpDeliver {
		t.Fatalf("req = %+v", req)
	}
	if req.Target.Scheme != "webhook" || req.Target.Address != "https://example.com/h" {
		t.Fatalf("req.Target = %+v", req.Target)
	}
	if !strings.Contains(req.Message, "please check") {
		t.Fatalf("req.Message = %q", req.Message)
	}
}

func TestPluginTransportDeliverWaiting(t *testing.T) {
	dir := t.TempDir()
	script := writePluginScript(t, dir, "plugin",
		"cat >/dev/null\n"+
			"printf '%s\\n' '{\"outcome\":\"waiting\",\"reason\":\"pane busy\",\"externalId\":\"q-1\"}'\n",
	)
	writePluginConfig(t, map[string][]string{"tmux": {script}})

	clock := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	item := answeredRelay(t, led, "tmux:%2", "")

	got, err := led.DeliverAnswer(&item, nil)
	if err != nil {
		t.Fatalf("DeliverAnswer error = %v, want nil for waiting", err)
	}
	if len(got.Delivery) != 1 {
		t.Fatalf("Delivery = %+v, want 1 record", got.Delivery)
	}
	d := got.Delivery[0]
	if d.Outcome != OutcomeWaiting || d.Reason != "pane busy" || d.BusID != "q-1" || d.Target != "tmux:%2" {
		t.Fatalf("Delivery[0] = %+v", d)
	}
	if got.NotifyError != "" {
		t.Fatalf("NotifyError = %q, want empty", got.NotifyError)
	}
}

func TestPluginTransportDeliverFailures(t *testing.T) {
	tests := []struct {
		name       string
		scriptBody string
		wantErr    string
	}{
		{
			name:       "reported failed",
			scriptBody: "cat >/dev/null\nprintf '%s\\n' '{\"outcome\":\"failed\",\"error\":\"pane not found\"}'\n",
			wantErr:    "pane not found",
		},
		{
			name:       "non-zero exit",
			scriptBody: "cat >/dev/null\necho 'plugin crashed' >&2\nexit 3\n",
			wantErr:    "plugin crashed",
		},
		{
			name:       "invalid json",
			scriptBody: "cat >/dev/null\nprintf 'not-json'\n",
			wantErr:    "invalid plugin response JSON",
		},
		{
			name:       "invalid outcome",
			scriptBody: "cat >/dev/null\nprintf '%s\\n' '{\"outcome\":\"mystery\"}'\n",
			wantErr:    "invalid plugin outcome",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			dir := t.TempDir()
			script := writePluginScript(t, dir, "plugin", tc.scriptBody)
			writePluginConfig(t, map[string][]string{"tmux": {script}})

			clock := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
			led := openDeliveryLedger(t, dir, &clock)
			item := answeredRelay(t, led, "tmux:%9", "")

			got, derr := led.DeliverAnswer(&item, nil)
			if derr == nil || !strings.Contains(derr.Error(), tc.wantErr) {
				t.Fatalf("DeliverAnswer error = %v, want substring %q", derr, tc.wantErr)
			}
			if len(got.Delivery) != 1 || got.Delivery[0].Outcome != OutcomeFailed || got.Delivery[0].Target != "tmux:%9" {
				t.Fatalf("Delivery = %+v, want one failed record", got.Delivery)
			}
		})
	}
}

func TestPluginTransportDeliverTimeout(t *testing.T) {
	dir := t.TempDir()
	script := writePluginScript(t, dir, "slow-plugin", "cat >/dev/null\nsleep 5\n")
	cfg := Config{Transports: map[string][]string{"tmux": {script}}}
	item := &Item{
		ID:         "D-1",
		Owner:      "dev",
		NotifyPane: "tmux:%9",
		Answer:     &Answer{Value: "a", AnsweredBy: "human"},
		AnsweredAt: time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC),
	}
	target := ParseTarget("tmux:%9", "")
	d, derr := (pluginTransport{timeout: 150 * time.Millisecond}).DeliverAnswer(item, target, cfg, nil)
	if derr == nil || !strings.Contains(derr.Error(), "timed out") {
		t.Fatalf("DeliverAnswer error = %v, want timed out", derr)
	}
	if d.Outcome != OutcomeFailed || d.Target != "tmux:%9" {
		t.Fatalf("Delivery = %+v, want failed record for tmux:%%9", d)
	}
}

func TestPluginTransportStatusDoesNotInvokeHerdr(t *testing.T) {
	f := herdrtest.Install(t)
	dir := t.TempDir()
	stdinFile := filepath.Join(dir, "status-req.json")
	script := writePluginScript(t, dir, "tmux-plugin",
		"cat > '"+stdinFile+"'\n"+
			"printf '%s\\n' '{\"state\":\"live\",\"label\":\"dev:0.1\"}'\n",
	)
	writePluginConfig(t, map[string][]string{"tmux": {script}})

	loc := resolveOne(t, &Item{NotifyPane: "tmux:%5"}, "")
	if loc.State != LocationLive || loc.Label != "dev:0.1" || loc.Pane != "tmux:%5" {
		t.Fatalf("location = %+v, want live label=dev:0.1 pane=tmux:%%5", loc)
	}
	if got := loc.Text(); got != "dev:0.1 (tmux:%5)" {
		t.Fatalf("Text() = %q, want %q", got, "dev:0.1 (tmux:%5)")
	}
	if argv := f.Argv(); len(argv) != 0 {
		t.Fatalf("herdr ran %q, want none", argv)
	}
	var req PluginRequest
	if err := json.Unmarshal([]byte(readText(t, stdinFile)), &req); err != nil {
		t.Fatalf("unmarshal status request: %v", err)
	}
	if req.Version != PluginProtocolVersion || req.Op != pluginOpStatus {
		t.Fatalf("req = %+v", req)
	}
	if req.Target != (PluginTarget{Scheme: "tmux", Address: "%5"}) {
		t.Fatalf("req.Target = %+v", req.Target)
	}
	if req.Item != nil || req.DeliveryKey != "" || req.Message != "" {
		t.Fatalf("status request should omit deliver-only fields: %+v", req)
	}
}

func TestPluginTransportUnregisteredSchemeFailsWithoutExecutingPATH(t *testing.T) {
	dir := t.TempDir()
	marker := filepath.Join(dir, "path-bin-ran")
	// Install an executable with the exact scheme name on PATH to verify
	// decide never executes unregistered schemes from PATH.
	installProgram(t, "unreg", "#!/bin/sh\ntouch '"+marker+"'\nprintf '%s\\n' '{\"outcome\":\"delivered\"}'\n")
	writeConfig(t, `{}`)

	clock := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	item := answeredRelay(t, led, "unreg:addr1", "")

	got, derr := led.DeliverAnswer(&item, nil)
	if derr == nil || !strings.Contains(derr.Error(), "unknown transport") {
		t.Fatalf("DeliverAnswer error = %v, want unknown transport error", derr)
	}
	if len(got.Delivery) != 1 || got.Delivery[0].Outcome != OutcomeFailed || got.Delivery[0].Target != "unreg:addr1" {
		t.Fatalf("Delivery = %+v, want one failed record for unreg:addr1", got.Delivery)
	}
	if _, statErr := os.Stat(marker); !errors.Is(statErr, fs.ErrNotExist) {
		t.Fatal("executable on PATH was invoked for unregistered scheme")
	}

	// Verify LocationResolver also leaves unregistered scheme unknown without executing PATH.
	f := herdrtest.Install(t)
	loc := LocationResolver{}.Resolve(context.Background(), []OwnerLocation{OwnerPane(&item, "")})[0]
	if loc.State != LocationUnknown || loc.Pane != "unreg:addr1" {
		t.Fatalf("location = %+v, want unknown", loc)
	}
	if _, statErr := os.Stat(marker); !errors.Is(statErr, fs.ErrNotExist) {
		t.Fatal("LocationResolver invoked executable on PATH for unregistered scheme")
	}
	if argv := f.Argv(); len(argv) != 0 {
		t.Fatalf("herdr ran %q, want none", argv)
	}
}

func TestFollowStalePluginTransportUsesStatusWithoutHerdr(t *testing.T) {
	t.Run("closed marks owner gone", func(t *testing.T) {
		f := herdrtest.Install(t)
		dir := t.TempDir()
		logPath := filepath.Join(dir, "deliver.log")
		script := writePluginScript(t, dir, "tmux-plugin",
			"input=$(cat)\n"+
				"case \"$input\" in\n"+
				"  *'\"op\":\"status\"'*)\n"+
				"    printf '%s\\n' '{\"state\":\"closed\",\"label\":\"dev:0\"}'\n"+
				"    ;;\n"+
				"  *)\n"+
				"    printf 'deliver\\n' >> '"+logPath+"'\n"+
				"    printf '%s\\n' '{\"outcome\":\"delivered\"}'\n"+
				"    ;;\n"+
				"esac\n",
		)
		writePluginConfig(t, map[string][]string{"tmux": {script}})

		clock := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
		led := openDeliveryLedger(t, dir, &clock)
		item := answeredRelay(t, led, "tmux:%3", "")
		if _, err := led.DeliverAnswer(&item, nil); err != nil {
			t.Fatal(err)
		}

		clock = item.AnsweredAt.Add(30 * time.Minute)
		if err := led.FollowStale(clock, nil, nil); err != nil {
			t.Fatal(err)
		}
		got := mustGet(t, led, item.ID)
		if got.FollowUp != FollowUpOwnerGone {
			t.Fatalf("FollowUp = %q, want %q", got.FollowUp, FollowUpOwnerGone)
		}
		if lines := keyLog(t, logPath); len(lines) != 1 {
			t.Fatalf("deliver count = %d, want 1 (no resend)", len(lines))
		}
		if argv := f.Argv(); len(argv) != 0 {
			t.Fatalf("herdr ran %q, want none", argv)
		}
	})

	t.Run("live resends without calling herdr", func(t *testing.T) {
		f := herdrtest.Install(t)
		dir := t.TempDir()
		logPath := filepath.Join(dir, "deliver.log")
		script := writePluginScript(t, dir, "tmux-plugin",
			"input=$(cat)\n"+
				"case \"$input\" in\n"+
				"  *'\"op\":\"status\"'*)\n"+
				"    printf '%s\\n' '{\"state\":\"live\",\"label\":\"dev:0\"}'\n"+
				"    ;;\n"+
				"  *)\n"+
				"    printf 'deliver\\n' >> '"+logPath+"'\n"+
				"    printf '%s\\n' '{\"outcome\":\"delivered\"}'\n"+
				"    ;;\n"+
				"esac\n",
		)
		writePluginConfig(t, map[string][]string{"tmux": {script}})

		clock := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
		led := openDeliveryLedger(t, dir, &clock)
		item := answeredRelay(t, led, "tmux:%4", "")
		if _, err := led.DeliverAnswer(&item, nil); err != nil {
			t.Fatal(err)
		}

		clock = item.AnsweredAt.Add(30 * time.Minute)
		if err := led.FollowStale(clock, nil, nil); err != nil {
			t.Fatal(err)
		}
		got := mustGet(t, led, item.ID)
		if got.FollowUp != "" {
			t.Fatalf("FollowUp = %q, want empty", got.FollowUp)
		}
		if len(got.Delivery) != 2 || got.Delivery[1].Outcome != OutcomeResend || got.Delivery[1].Target != "tmux:%4" {
			t.Fatalf("Delivery = %+v", got.Delivery)
		}
		if lines := keyLog(t, logPath); len(lines) != 2 {
			t.Fatalf("deliver count = %d, want 2", len(lines))
		}
		if argv := f.Argv(); len(argv) != 0 {
			t.Fatalf("herdr ran %q, want none", argv)
		}
	})

	t.Run("unknown status resends safely without calling herdr", func(t *testing.T) {
		f := herdrtest.Install(t)
		dir := t.TempDir()
		logPath := filepath.Join(dir, "deliver.log")
		script := writePluginScript(t, dir, "webhook-plugin",
			"input=$(cat)\n"+
				"case \"$input\" in\n"+
				"  *'\"op\":\"status\"'*)\n"+
				"    printf '%s\\n' '{\"state\":\"unknown\"}'\n"+
				"    ;;\n"+
				"  *)\n"+
				"    printf 'deliver\\n' >> '"+logPath+"'\n"+
				"    printf '%s\\n' '{\"outcome\":\"delivered\"}'\n"+
				"    ;;\n"+
				"esac\n",
		)
		writePluginConfig(t, map[string][]string{"webhook": {script}})

		clock := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
		led := openDeliveryLedger(t, dir, &clock)
		item := answeredRelay(t, led, "webhook:https://example.com/cb", "")
		if _, err := led.DeliverAnswer(&item, nil); err != nil {
			t.Fatal(err)
		}

		clock = item.AnsweredAt.Add(30 * time.Minute)
		if err := led.FollowStale(clock, nil, nil); err != nil {
			t.Fatal(err)
		}
		if lines := keyLog(t, logPath); len(lines) != 2 {
			t.Fatalf("deliver count = %d, want 2", len(lines))
		}
		if argv := f.Argv(); len(argv) != 0 {
			t.Fatalf("herdr ran %q, want none", argv)
		}
	})
}
