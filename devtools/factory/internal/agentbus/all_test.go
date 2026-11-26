package agentbus

import (
	"context"
	"strings"
	"testing"
)

func TestAll(t *testing.T) {
	mock := leaseMock(t)
	mock.ExpectCommandWithArgs("agentbus", "lease", "status", "--all", "--json").
		WillSucceed(`[`+freeJSON+`, `+strings.Replace(heldJSON, "local:/src/repo", "mac:/src/repo", 1)+`]`, 0).Once().Build()
	got, err := NewClient(mock, "agentbus").All(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 2 || got[0].Checkout != "local:/src/repo" || got[1].Checkout != "mac:/src/repo" || got[1].State != StateHeld {
		t.Fatalf("statuses = %+v", got)
	}
}

func TestDecodeStatusListRejects(t *testing.T) {
	for _, data := range []string{"", "{}", "null", `[{"checkout": "x", "state": "bogus", "queue": []}]`} {
		if _, err := DecodeStatusList([]byte(data)); err == nil {
			t.Fatalf("DecodeStatusList(%q): expected an error", data)
		}
	}
}
