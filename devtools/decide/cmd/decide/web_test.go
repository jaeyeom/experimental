package main

import (
	"bytes"
	"strings"
	"testing"
)

func TestWebRefusesOpenPublicBind(t *testing.T) {
	t.Setenv("HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	var stdout, stderr bytes.Buffer
	code := run([]string{"web", "--addr", "0.0.0.0:0"}, &stdout, &stderr)
	if code == 0 || !strings.Contains(stderr.String(), "allow-insecure-unauth") {
		t.Errorf("code = %d, stderr = %q", code, stderr.String())
	}
	if strings.Contains(stdout.String(), "listening") {
		t.Errorf("announced listening: %q", stdout.String())
	}
}

func TestWebRejectsArguments(t *testing.T) {
	var stderr bytes.Buffer
	if code := run([]string{"web", "extra"}, &bytes.Buffer{}, &stderr); code != 2 {
		t.Errorf("code = %d, want 2", code)
	}
}
