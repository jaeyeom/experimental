package herdr

import (
	"context"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
)

func TestIdentifyTrustsAHubCallerHandleOnlyOverSSH(t *testing.T) {
	ad := &Adapter{Run: func(_ context.Context, args []string) (string, string, int, error) {
		t.Fatalf("Identify ran herdr %q", args)
		return "", "", 1, nil
	}}
	ctx := context.Background()
	got, err := ad.Identify(ctx, []string{"SSH_CONNECTION=10.0.0.2 5000 10.0.0.1 22", "AGENTBUS_AS_HANDLE=herdr:linux2/default/w1:p1"})
	if err != nil || got != "herdr:linux2/default/w1:p1" {
		t.Fatalf("Identify = %q, %v", got, err)
	}
	for _, tt := range []struct {
		env  []string
		want string
	}{
		{[]string{"AGENTBUS_AS_HANDLE=herdr:linux2/default/w1:p1"}, "only trusted over ssh"},
		{[]string{"SSH_CONNECTION=x", "AGENTBUS_AS_HANDLE=herdr:local/default/w1:p1"}, "must name a remote machine"},
		{[]string{"SSH_CONNECTION=x", "AGENTBUS_AS_HANDLE=fake:x"}, "not a herdr handle"},
	} {
		_, err := ad.Identify(ctx, tt.env)
		if err == nil || !strings.Contains(err.Error(), tt.want) {
			t.Errorf("Identify(%q) = %v, want %q", tt.env, err, tt.want)
		}
	}
}

func TestRemoteCallerAcceptsOnlyRemoteHerdrHandles(t *testing.T) {
	if got, err := RemoteCaller("herdr:linux2/default/w1:p1"); err != nil || got != "herdr:linux2/default/w1:p1" {
		t.Fatalf("RemoteCaller = %q, %v", got, err)
	}
	for raw, want := range map[string]string{
		"herdr:local/default/w1:p1": "must name a remote machine",
		"fake:x":                    "not a herdr handle",
	} {
		if _, err := RemoteCaller(raw); err == nil || !strings.Contains(err.Error(), want) {
			t.Errorf("RemoteCaller(%q) = %v, want %q", raw, err, want)
		}
	}
}

func TestIdentifyUsesTheForwardedCallerOverEnv(t *testing.T) {
	ad := &Adapter{Run: func(_ context.Context, args []string) (string, string, int, error) {
		t.Fatalf("Identify ran herdr %q", args)
		return "", "", 1, nil
	}}
	ctx := adapter.WithForwardedCaller(context.Background(), "herdr:linux2/default/w1:p1")
	got, err := ad.Identify(ctx, []string{"HERDR_PANE_ID=w9:p9", "SSH_CONNECTION=x", "AGENTBUS_AS_HANDLE=herdr:m3/default/w3:p3"})
	if err != nil || got != "herdr:linux2/default/w1:p1" {
		t.Fatalf("Identify = %q, %v", got, err)
	}
}

func TestOnMachine(t *testing.T) {
	got, err := OnMachine("herdr:local/default/w1:p1", "linux2")
	if err != nil || got != "herdr:linux2/default/w1:p1" {
		t.Fatalf("OnMachine = %q, %v", got, err)
	}
	if _, err := OnMachine("herdr:m2/default/w1:p1", "linux2"); err == nil {
		t.Fatal("OnMachine accepted a remote handle")
	}
}
