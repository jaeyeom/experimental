package runner

import (
	"os"
	"slices"
	"strings"
	"testing"
	"time"
)

func TestParseConfigExample(t *testing.T) {
	data, err := os.ReadFile("../../docs/runner/runner.json")
	if err != nil {
		t.Fatal(err)
	}
	cfg, err := ParseConfig(data)
	if err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(cfg.KindNames(), []string{"rebase", "review", "start-item"}) {
		t.Fatalf("kinds = %v", cfg.KindNames())
	}
	if k := cfg.Kinds["review"]; k.Parallel != 1 || k.Timeout != 2*time.Hour || k.Target != "pr-{subject}" {
		t.Fatalf("review = %+v", k)
	}
	if k := cfg.Kinds["start-item"]; !k.Start || k.Timeout != DefaultTimeout {
		t.Fatalf("start-item = %+v", k)
	}
	want := Pacing{MinSpacing: 5 * time.Minute, MaxInFlight: 3, LaneCap: 2, MinFreeDisk: 10 << 30}
	if cfg.Start != want || cfg.Poll != 10*time.Second {
		t.Fatalf("start = %+v, poll %s", cfg.Start, cfg.Poll)
	}
}

func TestParseConfigRefuses(t *testing.T) {
	for _, tt := range []struct{ name, json, want string }{
		{"a gate", `{"kinds": {"merge": {"command": ["x"], "gate": "auto"}}}`, "unknown field"},
		{"hooks", `{"hooks": {}, "kinds": {"a": {"command": ["x"]}}}`, "unknown field"},
		{"no kinds", `{}`, "at least one kind"},
		{"nothing to run", `{"kinds": {"a": {}}}`, "want a command or a target"},
		{"bad timeout", `{"kinds": {"a": {"command": ["x"], "timeout": "soon"}}}`, "positive duration"},
		{"negative cap", `{"kinds": {"a": {"command": ["x"]}}, "start": {"laneCap": -1}}`, "negative"},
	} {
		if _, err := ParseConfig([]byte(tt.json)); err == nil || !strings.Contains(err.Error(), tt.want) {
			t.Errorf("%s: err = %v, want %q", tt.name, err, tt.want)
		}
	}
}
