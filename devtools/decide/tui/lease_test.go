package tui

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestStatusBoardShowsCheckoutLease(t *testing.T) {
	dir := t.TempDir()
	if _, err := decide.Open(dir); err != nil {
		t.Fatal(err)
	}
	board, err := decide.OpenStatus(dir)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := board.SetResource("checkout", decide.ResourceFree, "mine"); err != nil {
		t.Fatal(err)
	}
	var busErr error
	m := New(dir)
	m.Checkouts = []decide.ConfigCheckout{{Name: "checkout", Path: "/repo"}}
	m.busRun = func(_ context.Context, args []string) ([]byte, error) {
		if busErr != nil {
			return nil, busErr
		}
		if slices.Contains(args, "--guarded") {
			return []byte(`{"guarded": true, "checkout": "/repo"}`), nil
		}
		return []byte(`{"checkout":"/repo","state":"needs-adopt","holder":{"handle":"%7"},"queue":[],"backup_ref":"refs/b/1"}`), nil
	}
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	m.width, m.height = 120, 24
	press(t, m, "gs")
	row := visibleLine(lineContaining(m.View(), "checkout"))
	if !strings.Contains(row, "busy") || !strings.Contains(row, "needs adopt (backup refs/b/1)") {
		t.Fatalf("lease row %q\n%s", row, m.View())
	}

	busErr = errors.New("agentbus: not found")
	if err := m.loadStatus(); err != nil {
		t.Fatal(err)
	}
	row = visibleLine(lineContaining(m.View(), "checkout"))
	if !strings.Contains(row, "free") || !strings.Contains(row, "mine (guess)") {
		t.Fatalf("guess row %q\n%s", row, m.View())
	}
}
