package ghcli

import "testing"

func TestBinDefaultsToGH(t *testing.T) {
	t.Setenv(BinEnv, "")
	if got := Bin(); got != "gh" {
		t.Fatalf("Bin() = %q, want gh", got)
	}
}

func TestBinReadsEnv(t *testing.T) {
	t.Setenv(BinEnv, "/opt/bin/gh-throttle")
	if got := Bin(); got != "/opt/bin/gh-throttle" {
		t.Fatalf("Bin() = %q, want /opt/bin/gh-throttle", got)
	}
}
