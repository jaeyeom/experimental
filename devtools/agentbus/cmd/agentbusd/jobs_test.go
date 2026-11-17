package main

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

func TestDecideHoldIsNilWithoutDecide(t *testing.T) {
	hold := decideHold(func(string) (string, error) { return "", errors.New("not found") },
		func(context.Context, []string) ([]byte, error) { t.Fatal("ran decide"); return nil, nil })
	if hold != nil {
		t.Fatal("a hold check without decide on PATH")
	}
}

func TestDecideHoldReadsTheJobsResource(t *testing.T) {
	out := `{"agents":[],"resources":[{"name":"jobs","availability":"busy","note":"release day"}]}`
	var argv []string
	hold := decideHold(func(name string) (string, error) { return "/bin/" + name, nil },
		func(_ context.Context, a []string) ([]byte, error) { argv = a; return []byte(out), nil })
	held, why := hold()
	if !held || !strings.Contains(why, "release day") {
		t.Fatalf("hold = %v %q, want held with the note", held, why)
	}
	if !slices.Equal(argv, []string{"/bin/decide", "status", "list", "--json"}) {
		t.Fatalf("argv = %q", argv)
	}
	out = `{"agents":[],"resources":[{"name":"jobs","availability":"free"},{"name":"other","availability":"busy"}]}`
	if held, _ := hold(); held {
		t.Fatal("held while the jobs resource is free")
	}
}

func TestDecideHoldHoldsWhenDecideFails(t *testing.T) {
	hold := decideHold(func(name string) (string, error) { return "/bin/" + name, nil },
		func(context.Context, []string) ([]byte, error) { return nil, errors.New("exit status 1") })
	if held, why := hold(); !held || !strings.Contains(why, "decide status") {
		t.Fatalf("hold = %v %q, want held because decide status cannot be read", held, why)
	}
}

func TestJobsConfigBesideTheAgentbusConfig(t *testing.T) {
	dir := t.TempDir()
	env := []string{"XDG_CONFIG_HOME=" + dir}
	cfg, err := jobsConfig(env)
	if err != nil || len(cfg.Kinds) != 0 {
		t.Fatalf("missing jobs.json = %+v, %v", cfg, err)
	}
	if err := os.MkdirAll(filepath.Join(dir, "agentbus"), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "agentbus", "jobs.json"), []byte(`{"kinds":{"rebase":{"idempotent":true}}}`), 0o600); err != nil {
		t.Fatal(err)
	}
	if cfg, err := jobsConfig(env); err != nil || !cfg.Kind("rebase").Idempotent {
		t.Fatalf("jobs.json = %+v, %v", cfg, err)
	}
}
