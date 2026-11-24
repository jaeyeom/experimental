package decide

import (
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
