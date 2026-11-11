package client

import (
	"context"
	"encoding/json"
	"net"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
)

// serveOnce answers one connection with replies, then closes it.
func serveOnce(t *testing.T, replies ...daemon.Response) string {
	t.Helper()
	path := filepath.Join(shortDir(t), "s.sock")
	ln, err := net.Listen("unix", path)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { ln.Close() })
	go func() {
		conn, err := ln.Accept()
		if err != nil {
			return
		}
		defer conn.Close()
		var req daemon.Request
		_ = json.NewDecoder(conn).Decode(&req)
		enc := json.NewEncoder(conn)
		for _, r := range replies {
			_ = enc.Encode(r)
		}
	}()
	return path
}

func TestStreamPassesRepliesUntilTheEnd(t *testing.T) {
	path := serveOnce(t,
		daemon.Response{OK: true, Text: "a"},
		daemon.Response{OK: true, Text: "b"},
		daemon.Response{OK: true, End: true},
		daemon.Response{OK: true, Text: "after the end"},
	)
	var got []string
	err := Stream(context.Background(), path, daemon.Request{Op: "log"}, func(r daemon.Response) error {
		got = append(got, r.Text)
		return nil
	})
	if err != nil || !slices.Equal(got, []string{"a", "b"}) {
		t.Fatalf("Stream = %v, got %q", err, got)
	}
}

func TestStreamReportsAnErrorOrAnEarlyClose(t *testing.T) {
	path := serveOnce(t, daemon.Response{OK: true, Text: "a"}, daemon.Response{Error: "boom"})
	err := Stream(context.Background(), path, daemon.Request{Op: "log"}, func(daemon.Response) error { return nil })
	if err == nil || err.Error() != "boom" {
		t.Fatalf("Stream = %v, want boom", err)
	}
	path = serveOnce(t, daemon.Response{OK: true, Text: "a"})
	err = Stream(context.Background(), path, daemon.Request{Op: "log"}, func(daemon.Response) error { return nil })
	if err == nil || !strings.Contains(err.Error(), "closed the stream") {
		t.Fatalf("Stream = %v, want closed", err)
	}
}
