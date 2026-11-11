package hubcall

import (
	"strings"
	"testing"
)

func TestTrustedCallerOnlyOverSSHAndOnlyRemote(t *testing.T) {
	const key = "TOOL_AS_HANDLE"
	got, err := TrustedCaller([]string{"SSH_CONNECTION=10.0.0.2 5000 10.0.0.1 22", key + "=herdr:m2/s/p3"}, key)
	if err != nil || got != "herdr:m2/s/p3" {
		t.Fatalf("TrustedCaller = %q, %v", got, err)
	}
	if got, err := TrustedCaller([]string{"SSH_CONNECTION=x"}, key); err != nil || got != "" {
		t.Fatalf("TrustedCaller without a handle = %q, %v", got, err)
	}
	for _, tt := range []struct {
		env  []string
		want string
	}{
		{[]string{key + "=herdr:m2/s/p3"}, key + " is only trusted over ssh"},
		{[]string{"SSH_CONNECTION=x", key + "=herdr:local/s/p3"}, "must name a remote machine"},
		{[]string{"SSH_CONNECTION=x", key + "=fake:x"}, "not a herdr handle"},
	} {
		if _, err := TrustedCaller(tt.env, key); err == nil || !strings.Contains(err.Error(), tt.want) {
			t.Errorf("TrustedCaller(%q) = %v, want %q", tt.env, err, tt.want)
		}
	}
}

func TestParseHandle(t *testing.T) {
	machine, session, pane, err := ParseHandle("herdr:m2/s/w1:p3")
	if err != nil || machine != "m2" || session != "s" || pane != "w1:p3" {
		t.Fatalf("ParseHandle = %q %q %q, %v", machine, session, pane, err)
	}
	for raw, want := range map[string]string{
		"fake:x":       "not a herdr handle",
		"herdr:/s/p":   "missing a machine",
		"herdr:m2/s":   "missing a session or pane",
		"herdr:m2//p3": "missing a session or pane",
	} {
		if _, _, _, err := ParseHandle(raw); err == nil || !strings.Contains(err.Error(), want) {
			t.Errorf("ParseHandle(%q) = %v, want %q", raw, err, want)
		}
	}
}

func TestOnMachine(t *testing.T) {
	got, err := OnMachine("herdr:local/s/p3", "m2")
	if err != nil || got != "herdr:m2/s/p3" {
		t.Fatalf("OnMachine = %q, %v", got, err)
	}
	if _, err := OnMachine("herdr:m3/s/p3", "m2"); err == nil {
		t.Fatal("OnMachine accepted a remote handle")
	}
}
