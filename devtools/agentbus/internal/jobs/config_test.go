package jobs

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestParseConfig(t *testing.T) {
	cfg, err := ParseConfig([]byte(`{
  "heartbeatTimeout": "90s",
  "kinds": {
    "rebase": {"idempotent": true, "maxAttempts": 4, "retryExits": [75], "backoff": "30s"},
    "comment": {"onBlocked": "park", "reconcile": ["prsync", "reconcile"]}
  }
}`))
	if err != nil {
		t.Fatal(err)
	}
	if cfg.HeartbeatTimeout != 90*time.Second {
		t.Errorf("heartbeat timeout = %s", cfg.HeartbeatTimeout)
	}
	r := cfg.Kind("rebase")
	if !r.Idempotent || r.MaxAttempts != 4 || r.Backoff != 30*time.Second || len(r.RetryExits) != 1 {
		t.Errorf("rebase = %+v", r)
	}
	if c := cfg.Kind("comment"); c.OnBlocked != OnBlockedPark || len(c.Reconcile) != 2 {
		t.Errorf("comment = %+v", c)
	}
	// An undeclared kind is outward, tried once, and blocks its subject.
	if u := cfg.Kind("push"); u.Idempotent || u.MaxAttempts != 1 || u.OnBlocked != OnBlockedBlock || u.Backoff != DefaultBackoff {
		t.Errorf("undeclared kind = %+v", u)
	}
	if (Config{}).Timeout() != DefaultHeartbeatTimeout {
		t.Errorf("default heartbeat timeout = %s", (Config{}).Timeout())
	}
}

func TestParseConfigRefusesBadValues(t *testing.T) {
	for _, text := range []string{
		`{"heartbeatTimeout": "soon"}`,
		`{"kinds": {"x": {"onBlocked": "wait"}}}`,
		`{"kinds": {"x": {"backoff": "-1s"}}}`,
		`{"kinds": {"x": {"maxAttempts": -1}}}`,
		`{"kinds": {"x": {"gate": "auto"}}}`,
		`{"kinds": {"bad kind": {}}}`,
	} {
		if _, err := ParseConfig([]byte(text)); err == nil {
			t.Errorf("ParseConfig(%s) succeeded, want an error", text)
		}
	}
}

func TestLoadConfigMissingFileIsDefaults(t *testing.T) {
	cfg, err := LoadConfig(filepath.Join(t.TempDir(), "jobs.json"))
	if err != nil || len(cfg.Kinds) != 0 {
		t.Fatalf("LoadConfig = %+v, %v", cfg, err)
	}
	path := filepath.Join(t.TempDir(), "jobs.json")
	if err := os.WriteFile(path, []byte(`{"kinds":{"a":{"idempotent":true}}}`), 0o600); err != nil {
		t.Fatal(err)
	}
	if cfg, err := LoadConfig(path); err != nil || !cfg.Kind("a").Idempotent {
		t.Fatalf("LoadConfig = %+v, %v", cfg, err)
	}
}

func TestParseConfigBudget(t *testing.T) {
	cfg, err := ParseConfig([]byte(`{
  "budget": {"limit": 5000, "window": "30m", "floor": 500},
  "kinds": {"rebase": {"cost": 3}, "merge": {"cost": 2, "belowFloor": true}}
}`))
	if err != nil {
		t.Fatal(err)
	}
	if b := cfg.Budget; b.Limit != 5000 || b.Window != 30*time.Minute || b.Floor != 500 {
		t.Errorf("budget = %+v", b)
	}
	if k := cfg.Kind("rebase"); k.Cost != 3 || k.BelowFloor {
		t.Errorf("rebase = %+v", k)
	}
	if k := cfg.Kind("merge"); k.Cost != 2 || !k.BelowFloor {
		t.Errorf("merge = %+v", k)
	}
	cfg, err = ParseConfig([]byte(`{"budget": {"limit": 10}}`))
	if err != nil || cfg.Budget.Window != DefaultBudgetWindow {
		t.Errorf("default window = %+v, %v", cfg.Budget, err)
	}
	for _, text := range []string{
		`{"budget": {"limit": 0}}`,
		`{"budget": {"limit": 10, "floor": 11}}`,
		`{"budget": {"limit": 10, "floor": -1}}`,
		`{"budget": {"limit": 10, "window": "soon"}}`,
		`{"budget": {"limit": 10, "remaining": 3}}`,
		`{"kinds": {"x": {"cost": -1}}}`,
	} {
		if _, err := ParseConfig([]byte(text)); err == nil {
			t.Errorf("ParseConfig(%s) succeeded, want an error", text)
		}
	}
}
