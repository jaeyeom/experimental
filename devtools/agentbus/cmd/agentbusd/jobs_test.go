package main

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/jobs"
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

func TestGateDeciderIsNilWithoutDecide(t *testing.T) {
	d := gateDecider(func(string) (string, error) { return "", errors.New("not found") },
		func(context.Context, []string) ([]byte, error) { t.Fatal("ran decide"); return nil, nil })
	if d != nil {
		t.Fatal("a gate decider without decide on PATH")
	}
}

func TestGateDeciderAsksWithoutAPaneAndReadsTheItem(t *testing.T) {
	var argvs [][]string
	out := map[string]string{
		"ask":  `{"id":"d7"}`,
		"show": `{"id":"d7","state":"answered","messages":[{"author":"human","text":"approve","optionKey":"approve","via":"ui"}],"answer":{"value":"approve","key":"approve","answeredBy":"human","via":"ui"}}`,
		"done": `{"id":"d7","state":"done"}`,
	}
	d := gateDecider(func(name string) (string, error) { return "/bin/" + name, nil },
		func(_ context.Context, a []string) ([]byte, error) {
			argvs = append(argvs, a)
			return []byte(out[a[1]]), nil
		})
	ctx := context.Background()
	id, err := d.Ask(ctx, "agentbus job j-1 waits for you")
	if err != nil || id != "d7" {
		t.Fatalf("Ask = %q, %v", id, err)
	}
	want := []string{
		"/bin/decide", "ask", "--owner", "agentbus", "--kind", "relay",
		"--option", "approve:Run it", "--option", "reject:Reject", "--free-text", "--json", "agentbus job j-1 waits for you",
	}
	if !slices.Equal(argvs[0], want) {
		t.Fatalf("ask argv = %q, want %q", argvs[0], want)
	}
	if slices.Contains(argvs[0], "--notify-pane") {
		t.Fatal("the gate item prompts a pane")
	}
	it, err := d.Show(ctx, "d7")
	if err != nil || it.Answer == nil || !it.Answer.HumanUI() || len(it.Messages) != 1 {
		t.Fatalf("Show = %+v, %v", it, err)
	}
	if err := d.Done(ctx, "d7", "approved"); err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(argvs[2], []string{"/bin/decide", "done", "--json", "d7", "approved"}) {
		t.Fatalf("done argv = %q", argvs[2])
	}
}

func TestJobsPolicyMissingAsksForEveryKind(t *testing.T) {
	dir := t.TempDir()
	var stderr strings.Builder
	p := jobsPolicy([]string{"XDG_CONFIG_HOME=" + dir}, config.Config{}, &stderr, nil)
	if p.Gate("rebase") != jobs.GateAsk {
		t.Fatalf("gate = %s, want ask", p.Gate("rebase"))
	}
	if !strings.Contains(stderr.String(), filepath.Join(dir, "agentbus", "jobs-policy.json")) || !strings.Contains(stderr.String(), "missing") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestJobsPolicyRefusedIsLoudAndAsksForEveryKind(t *testing.T) {
	path := filepath.Join(t.TempDir(), "policy.json")
	if err := os.WriteFile(path, []byte(`{"default":"auto"}`), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(path, 0o666); err != nil {
		t.Fatal(err)
	}
	var (
		stderr strings.Builder
		notes  []string
	)
	p := jobsPolicy(nil, config.Config{JobsPolicy: path}, &stderr, func(text string) { notes = append(notes, text) })
	if p.Gate("rebase") != jobs.GateAsk {
		t.Fatalf("gate = %s, want ask", p.Gate("rebase"))
	}
	if !strings.Contains(stderr.String(), "refused") || len(notes) != 1 || !strings.Contains(notes[0], "writable") {
		t.Fatalf("stderr = %q, notes = %q", stderr.String(), notes)
	}
}

func TestJobsPolicyLoadsTheConfiguredFile(t *testing.T) {
	path := filepath.Join(t.TempDir(), "policy.json")
	if err := os.WriteFile(path, []byte(`{"default":"auto"}`), 0o600); err != nil {
		t.Fatal(err)
	}
	var stderr strings.Builder
	if p := jobsPolicy(nil, config.Config{JobsPolicy: path}, &stderr, nil); p.Gate("rebase") != jobs.GateAuto {
		t.Fatalf("gate = %s, want auto", p.Gate("rebase"))
	}
}
