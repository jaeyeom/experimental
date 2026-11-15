package agentbus

import (
	"context"
	"errors"
	"strings"
	"testing"
)

func TestListParsesFixture(t *testing.T) {
	const fixture = `[
		{"name":"ops/a","when":"2026-11-14T10:00:00Z","liveness":"alive","transport":"herdr","handle":"herdr:local/11111111-1111-1111-1111-111111111111/w1:p1"},
		{"name":"ops/b","when":"2026-11-14T10:01:00Z","liveness":"dead","transport":"herdr","handle":"herdr:linux2/22222222-2222-2222-2222-222222222222/w2:p3"},
		{"name":"ops/c","when":"2026-11-14T10:02:00Z","liveness":"unknown","transport":"slack","handle":"slack:C1"}
	]`
	var gotArgs []string
	names, warnings, err := List(context.Background(), func(_ context.Context, args []string) ([]byte, error) {
		gotArgs = append([]string(nil), args...)
		return []byte(fixture), nil
	})
	if err != nil {
		t.Fatalf("List: %v", err)
	}
	if strings.Join(gotArgs, " ") != "list --json" {
		t.Fatalf("args = %q, want list --json", gotArgs)
	}
	if len(warnings) != 0 {
		t.Fatalf("warnings = %#v, want none", warnings)
	}
	if len(names) != 3 {
		t.Fatalf("names = %d, want 3", len(names))
	}
	local := names[0]
	if local.Name != "ops/a" || local.When != "2026-11-14T10:00:00Z" || local.Liveness != "alive" || local.Transport != "herdr" {
		t.Fatalf("local row = %#v", local)
	}
	if local.Server != "local" || local.SessionID != "11111111-1111-1111-1111-111111111111" || local.Pane != "w1:p1" || local.Raw {
		t.Fatalf("local handle = %#v", local)
	}
	remote := names[1]
	if remote.Server != "linux2" || remote.SessionID != "22222222-2222-2222-2222-222222222222" || remote.Pane != "w2:p3" || remote.Raw {
		t.Fatalf("remote handle = %#v", remote)
	}
	other := names[2]
	if !other.Raw || other.Handle != "slack:C1" || other.Server != "" || other.SessionID != "" || other.Pane != "" {
		t.Fatalf("other transport = %#v, want a raw handle", other)
	}
}

func TestListMalformedHandleStaysRaw(t *testing.T) {
	const fixture = `[{"name":"ops/d","when":"t","liveness":"alive","transport":"herdr","handle":"herdr:local/not-split"}]`
	names, warnings, err := List(context.Background(), func(context.Context, []string) ([]byte, error) {
		return []byte(fixture), nil
	})
	if err != nil {
		t.Fatalf("List: %v", err)
	}
	if len(names) != 1 || !names[0].Raw || names[0].Handle != "herdr:local/not-split" {
		t.Fatalf("name = %#v, want the raw handle", names)
	}
	if len(warnings) != 1 || !strings.Contains(warnings[0], "herdr:local/not-split") {
		t.Fatalf("warnings = %#v, want one that names the handle", warnings)
	}
}

func TestListUnreachable(t *testing.T) {
	_, _, err := List(context.Background(), func(context.Context, []string) ([]byte, error) {
		return nil, errors.New("agentbus unreachable")
	})
	var nr *NotReadError
	if !errors.As(err, &nr) {
		t.Fatalf("err = %v, want NotReadError", err)
	}
	if err.Error() != "agentbus: not read (agentbus unreachable)" {
		t.Fatalf("err = %q", err.Error())
	}
}
