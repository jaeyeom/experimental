package ghask

import (
	"strings"
	"testing"
)

func TestListenRefusesNonLoopback(t *testing.T) {
	for _, addr := range []string{
		"0.0.0.0:0",
		":0",
		"localhost:0",
		"[::1]:0",
		"[::]:0",
		"192.168.1.10:0",
		"127.0.0.2:0",
		"127.0.0.1",
	} {
		t.Run(addr, func(t *testing.T) {
			ln, err := Listen(addr)
			if err == nil {
				_ = ln.Close()
				t.Fatalf("Listen(%q) error = nil, want refusal", addr)
			}
		})
	}
}

func TestListenLoopback(t *testing.T) {
	ln, err := Listen("127.0.0.1:0")
	if err != nil {
		t.Fatalf("Listen() error = %v", err)
	}
	defer ln.Close()
	if got := ln.Addr().String(); !strings.HasPrefix(got, "127.0.0.1:") {
		t.Errorf("Addr() = %q, want 127.0.0.1:<port>", got)
	}
}
