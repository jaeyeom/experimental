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
