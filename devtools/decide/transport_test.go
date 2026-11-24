package decide

import (
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

func TestParseTarget(t *testing.T) {
	tests := []struct {
		name       string
		pane       string
		machine    string
		want       Target
		wantString string
	}{
		{
			name:       "empty",
			pane:       "",
			machine:    "mac",
			want:       Target{},
			wantString: "",
		},
		{
			name:       "whitespace only",
			pane:       "   ",
			machine:    "mac",
			want:       Target{},
			wantString: "",
		},
		{
			name:       "pull",
			pane:       "pull",
			machine:    "mac",
			want:       Target{Scheme: SchemePull},
			wantString: "pull",
		},
		{
			name:       "pull with detail",
			pane:       "pull:desktop",
			machine:    "mac",
			want:       Target{Scheme: SchemePull, Address: "desktop"},
			wantString: "pull:desktop",
		},
		{
			name:       "agentbus",
			pane:       "agentbus:foo",
			machine:    "mac",
			want:       Target{Scheme: SchemeAgentbus, Address: "foo"},
			wantString: "agentbus:foo",
		},
		{
			name:       "agentbus trimmed",
			pane:       " agentbus: ops/x ",
			machine:    "",
			want:       Target{Scheme: SchemeAgentbus, Address: "ops/x"},
			wantString: "agentbus:ops/x",
		},
		{
			name:       "agentbus empty name",
			pane:       "agentbus:",
			machine:    "",
			want:       Target{Scheme: SchemeAgentbus, Address: ""},
			wantString: "agentbus:",
		},
		{
			name:       "explicit herdr local",
			pane:       "herdr:w1:p2",
			machine:    "",
			want:       Target{Scheme: SchemeHerdr, Address: "w1:p2"},
			wantString: "w1:p2",
		},
		{
			name:       "explicit herdr remote",
			pane:       "herdr:w1:p2",
			machine:    "mac",
			want:       Target{Scheme: SchemeHerdr, Address: "w1:p2", Machine: "mac"},
			wantString: "w1:p2",
		},
		{
			name:       "bare pane local",
			pane:       "w1:p2",
			machine:    "",
			want:       Target{Scheme: SchemeHerdr, Address: "w1:p2"},
			wantString: "w1:p2",
		},
		{
			name:       "bare pane remote",
			pane:       "w1:p2",
			machine:    "mac",
			want:       Target{Scheme: SchemeHerdr, Address: "w1:p2", Machine: "mac"},
			wantString: "w1:p2",
		},
		{
			name:       "target",
			pane:       "target:mytool",
			machine:    "mac",
			want:       Target{Scheme: SchemeTarget, Address: "mytool"},
			wantString: "target:mytool",
		},
		{
			name:       "target trimmed",
			pane:       " target: slackwatch ",
			machine:    "",
			want:       Target{Scheme: SchemeTarget, Address: "slackwatch"},
			wantString: "target:slackwatch",
		},
		{
			name:       "target empty name",
			pane:       "target:",
			machine:    "",
			want:       Target{Scheme: SchemeTarget, Address: ""},
			wantString: "target:",
		},
		{
			name:       "custom plugin scheme",
			pane:       "tmux:%1",
			machine:    "",
			want:       Target{Scheme: "tmux", Address: "%1"},
			wantString: "tmux:%1",
		},
		{
			name:       "custom plugin scheme with colons in address",
			pane:       "webhook:https://example.com/hook",
			machine:    "mac",
			want:       Target{Scheme: "webhook", Address: "https://example.com/hook", Machine: "mac"},
			wantString: "webhook:https://example.com/hook",
		},
		{
			name:       "bare herdr pane with letters",
			pane:       "w6:pM",
			machine:    "",
			want:       Target{Scheme: SchemeHerdr, Address: "w6:pM"},
			wantString: "w6:pM",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := ParseTarget(tc.pane, tc.machine)
			if got != tc.want {
				t.Errorf("ParseTarget(%q, %q) = %+v, want %+v", tc.pane, tc.machine, got, tc.want)
			}
			if s := got.String(); s != tc.wantString {
				t.Errorf("Target.String() = %q, want %q", s, tc.wantString)
			}
		})
	}
}

func TestDeliverAnswerPullIsNoOp(t *testing.T) {
	for _, pane := range []string{"pull", "pull:ci"} {
		t.Run(pane, func(t *testing.T) {
			dir := t.TempDir()
			marker := filepath.Join(dir, "template-ran")
			writeConfig(t, `{"notifyTemplate":"touch `+marker+`","notifyPane":"w1:p1"}`)

			clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
			led := openDeliveryLedger(t, dir, &clock)
			item := answeredRelay(t, led, pane, "")

			got, err := led.DeliverAnswer(&item, nil)
			if err != nil {
				t.Fatalf("DeliverAnswer(%q) error = %v, want nil", pane, err)
			}
			if got.NotifyError != "" {
				t.Fatalf("NotifyError = %q, want empty", got.NotifyError)
			}
			if len(got.Delivery) != 0 {
				t.Fatalf("Delivery = %+v, want empty", got.Delivery)
			}
			if _, statErr := os.Stat(marker); !errors.Is(statErr, fs.ErrNotExist) {
				t.Fatalf("notify template ran for pull target %q", pane)
			}
		})
	}
}

func TestNotifyOwnerPullIsNoOp(t *testing.T) {
	dir := t.TempDir()
	marker := filepath.Join(dir, "send-ran")
	writeConfig(t, `{"sendTemplate":"touch `+marker+`"}`)

	d, err := NotifyOwner("pull", "mac", "D-7", "hello")
	if err != nil {
		t.Fatalf("NotifyOwner(pull) error = %v, want nil", err)
	}
	if d != (Delivery{}) {
		t.Fatalf("Delivery = %+v, want zero", d)
	}
	if _, statErr := os.Stat(marker); !errors.Is(statErr, fs.ErrNotExist) {
		t.Fatal("send command ran for pull target")
	}
}

func TestNotifyHerdrPrefixRoutesLikeBarePane(t *testing.T) {
	t.Run("local", func(t *testing.T) {
		writeConfig(t, `{"notifyTemplate":"printf '%s' \"$DECIDE_PANE\""}`)
		item := &Item{ID: "D-7", NotifyPane: "herdr:w1:p1", Answer: &Answer{Value: "ship", AnsweredBy: "human"}}
		var out strings.Builder
		if err := Notify(item, &out); err != nil {
			t.Fatal(err)
		}
		if out.String() != "w1:p1" {
			t.Fatalf("DECIDE_PANE = %q, want %q", out.String(), "w1:p1")
		}
	})

	t.Run("remote", func(t *testing.T) {
		writeConfig(t, `{"notifyTemplate":"false"}`)
		herdr := herdrtest.Install(t)
		item := &Item{
			ID:            "D-7",
			NotifyPane:    "herdr:p3",
			NotifyMachine: "m2",
			Answer:        &Answer{Value: "ship", AnsweredBy: "human"},
		}
		if err := Notify(item, &strings.Builder{}); err != nil {
			t.Fatal(err)
		}
		header, _, _ := strings.Cut(DeliveryNote("D-7", "human", "ship"), "\n")
		want := "--machine m2 agent prompt p3 " + header
		argv := herdr.Argv()
		if len(argv) == 0 || argv[0] != want {
			t.Fatalf("herdr argv = %q, want %q", argv, want)
		}
	})
}

func TestDeliverAnswerCommandTarget(t *testing.T) {
	dir := t.TempDir()
	marker := filepath.Join(dir, "template-ran")
	shellMarker := filepath.Join(dir, "shell-ran")
	argsFile := filepath.Join(dir, "args")
	envFile := filepath.Join(dir, "env")
	stdinFile := filepath.Join(dir, "stdin")

	script := filepath.Join(dir, "mytool")
	body := "#!/bin/sh\n" +
		"printf '%s\\n' \"$@\" > '" + argsFile + "'\n" +
		"printf 'ID=%s\\nANSWER=%s\\nLABEL=%s\\nOWNER=%s\\nPANE=%s\\nKEY=%s\\n' " +
		"\"$DECIDE_ID\" \"$DECIDE_ANSWER\" \"$DECIDE_ANSWER_LABEL\" \"$DECIDE_OWNER\" \"$DECIDE_PANE\" \"$DECIDE_DELIVERY_KEY\" > '" + envFile + "'\n" +
		"cat > '" + stdinFile + "'\n"
	if err := os.WriteFile(script, []byte(body), 0o755); err != nil { //nolint:gosec // test helper script
		t.Fatal(err)
	}

	// Include a shell metacharacter arg to verify exec.Command runs directly without sh -c.
	literalArg := "$(touch " + shellMarker + ")"
	cfgBytes, err := json.Marshal(Config{
		NotifyTemplate: "touch " + marker,
		Targets: []ConfigTarget{
			{Name: "mytool", Command: []string{script, "--mode", literalArg}},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	writeConfig(t, string(cfgBytes))

	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	item := answeredRelay(t, led, "target:mytool", "")

	got, err := led.DeliverAnswer(&item, nil)
	if err != nil {
		t.Fatalf("DeliverAnswer error = %v", err)
	}
	if len(got.Delivery) != 1 || got.Delivery[0].Outcome != OutcomeDelivered || got.Delivery[0].Target != "target:mytool" {
		t.Fatalf("Delivery = %+v, want one delivered record for target:mytool", got.Delivery)
	}
	if _, statErr := os.Stat(marker); !errors.Is(statErr, fs.ErrNotExist) {
		t.Fatal("notifyTemplate ran for command target")
	}
	if _, statErr := os.Stat(shellMarker); !errors.Is(statErr, fs.ErrNotExist) {
		t.Fatal("command target was executed through a shell")
	}
	if gotArgs := readText(t, argsFile); gotArgs != "--mode\n"+literalArg+"\n" {
		t.Fatalf("args = %q, want %q", gotArgs, "--mode\n"+literalArg+"\n")
	}
	wantEnv := "ID=" + item.ID + "\nANSWER=a\nLABEL=Alpha\nOWNER=" + item.Owner + "\nPANE=target:mytool\nKEY=" + DeliveryKey(&item) + "\n"
	if gotEnv := readText(t, envFile); gotEnv != wantEnv {
		t.Fatalf("env = %q, want %q", gotEnv, wantEnv)
	}
	var stdinItem Item
	if err := json.Unmarshal([]byte(readText(t, stdinFile)), &stdinItem); err != nil {
		t.Fatalf("unmarshal stdin JSON: %v", err)
	}
	if stdinItem.ID != item.ID || stdinItem.Owner != item.Owner || stdinItem.Answer == nil || stdinItem.Answer.Value != "a" {
		t.Fatalf("stdin item = %+v, want id=%s owner=%s answer=a", stdinItem, item.ID, item.Owner)
	}
}

func TestDeliverAnswerCommandTargetNonZeroExit(t *testing.T) {
	dir := t.TempDir()
	script := filepath.Join(dir, "failtool")
	if err := os.WriteFile(script, []byte("#!/bin/sh\necho 'boom on stderr' >&2\nexit 2\n"), 0o755); err != nil { //nolint:gosec // test helper script
		t.Fatal(err)
	}
	cfgBytes, err := json.Marshal(Config{
		Targets: []ConfigTarget{
			{Name: "failtool", Command: []string{script}},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	writeConfig(t, string(cfgBytes))

	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	item := answeredRelay(t, led, "target:failtool", "")

	got, derr := led.DeliverAnswer(&item, nil)
	if derr == nil || !strings.Contains(derr.Error(), "boom on stderr") {
		t.Fatalf("DeliverAnswer error = %v, want stderr detail", derr)
	}
	if len(got.Delivery) != 1 || got.Delivery[0].Outcome != OutcomeFailed || got.Delivery[0].Target != "target:failtool" {
		t.Fatalf("Delivery = %+v, want one failed record for target:failtool", got.Delivery)
	}
	if !strings.Contains(got.Delivery[0].Error, "boom on stderr") {
		t.Fatalf("Delivery[0].Error = %q, want stderr detail", got.Delivery[0].Error)
	}
}

func TestDeliverAnswerUnknownTarget(t *testing.T) {
	dir := t.TempDir()
	writeConfig(t, `{"notifyTemplate":"false"}`)

	clock := time.Date(2026, 11, 14, 12, 0, 0, 0, time.UTC)
	led := openDeliveryLedger(t, dir, &clock)
	item := answeredRelay(t, led, "target:missing", "")

	got, derr := led.DeliverAnswer(&item, nil)
	if derr == nil || !strings.Contains(derr.Error(), "unknown target") {
		t.Fatalf("DeliverAnswer error = %v, want unknown target error", derr)
	}
	if len(got.Delivery) != 1 || got.Delivery[0].Outcome != OutcomeFailed || got.Delivery[0].Target != "target:missing" {
		t.Fatalf("Delivery = %+v, want one failed record for target:missing", got.Delivery)
	}
}

func TestNotifyItemCommandTarget(t *testing.T) {
	dir := t.TempDir()
	envFile := filepath.Join(dir, "env")
	stdinFile := filepath.Join(dir, "stdin")
	script := filepath.Join(dir, "replytool")
	body := "#!/bin/sh\n" +
		"printf 'ID=%s\\nOWNER=%s\\nPANE=%s\\n' \"$DECIDE_ID\" \"$DECIDE_OWNER\" \"$DECIDE_PANE\" > '" + envFile + "'\n" +
		"cat > '" + stdinFile + "'\n"
	if err := os.WriteFile(script, []byte(body), 0o755); err != nil { //nolint:gosec // test helper script
		t.Fatal(err)
	}
	cfgBytes, err := json.Marshal(Config{
		Targets: []ConfigTarget{
			{Name: "replytool", Command: []string{script}},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	writeConfig(t, string(cfgBytes))

	item := &Item{
		ID:         "D-42",
		Owner:      "slackwatch",
		NotifyPane: "target:replytool",
		Question:   "Post to #eng?",
		Messages: []Message{
			{Author: "slackwatch", Text: "Post to #eng?"},
			{Author: "human", Text: "please tweak the wording"},
		},
	}
	d, err := NotifyItemVerbatim(item, "target:replytool", "", "please tweak the wording", "")
	if err != nil {
		t.Fatalf("NotifyItemVerbatim error = %v", err)
	}
	if d.Outcome != OutcomeDelivered || d.Target != "target:replytool" {
		t.Fatalf("Delivery = %+v", d)
	}
	if gotEnv := readText(t, envFile); gotEnv != "ID=D-42\nOWNER=slackwatch\nPANE=target:replytool\n" {
		t.Fatalf("env = %q", gotEnv)
	}
	var stdinItem Item
	if err := json.Unmarshal([]byte(readText(t, stdinFile)), &stdinItem); err != nil {
		t.Fatalf("unmarshal stdin JSON: %v", err)
	}
	if stdinItem.ID != "D-42" || len(stdinItem.Messages) != 2 || stdinItem.Messages[1].Text != "please tweak the wording" {
		t.Fatalf("stdin item = %+v", stdinItem)
	}

	d, err = NotifyOwner("target:unknown", "", "D-42", "hello")
	if err == nil || !strings.Contains(err.Error(), "unknown target") {
		t.Fatalf("NotifyOwner unknown error = %v, want unknown target", err)
	}
	if d.Outcome != OutcomeFailed || d.Target != "target:unknown" {
		t.Fatalf("Delivery = %+v", d)
	}
}
