package web

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/pkg/toolcli"
)

func findCheck(t *testing.T, checks []toolcli.CheckResult, name string) toolcli.CheckResult {
	t.Helper()
	for _, c := range checks {
		if c.Name == name {
			return c
		}
	}
	t.Fatalf("check %q not found in %+v", name, checks)
	return toolcli.CheckResult{}
}

func TestDoctorWebAbsentWhenUnconfigured(t *testing.T) {
	home := t.TempDir()
	env := DoctorEnv{
		GOOS:       "linux",
		UID:        "1000",
		HomeDir:    home,
		ConfigPath: filepath.Join(home, ".config", "decide", "config.json"),
		Executable: "/usr/local/bin/decide",
		LookPath:   func(string) (string, error) { return "", errors.New("not found") },
	}
	checks := DoctorWebChecks(context.Background(), env, false)
	if len(checks) != 0 {
		t.Fatalf("expected no web checks when unconfigured, got %+v", checks)
	}
}

func setupHealthyDoctorFixture(t *testing.T) (DoctorEnv, decide.WebConfig, *fakeRunner) {
	t.Helper()
	home := t.TempDir()
	cfgPath := filepath.Join(home, ".config", "decide", "config.json")
	tokenPath := filepath.Join(home, ".config", "decide", "web-token")
	exePath := filepath.Join(home, "bin", "decide")
	for _, p := range []string{cfgPath, tokenPath, exePath} {
		if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
			t.Fatal(err)
		}
	}
	if err := os.WriteFile(exePath, []byte("bin"), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(tokenPath, []byte("0123456789abcdef0123456789abcdef\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	unitPath := systemdUnitPath(home)
	if err := os.MkdirAll(filepath.Dir(unitPath), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(unitPath, []byte(systemdUnitContent(exePath, tokenPath)), 0o600); err != nil {
		t.Fatal(err)
	}

	webCfg := decide.WebConfig{
		Addr:      "127.0.0.1:8787",
		PublicURL: "https://box.tail.ts.net",
		Auth:      decide.WebAuthConfig{Mode: decide.WebAuthToken, TokenFile: tokenPath},
		Push:      decide.WebPushConfig{Enabled: true, Subject: "mailto:alice@example.com"},
	}
	raw, err := json.Marshal(map[string]any{"web": webCfg})
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(cfgPath, raw, 0o600); err != nil {
		t.Fatal(err)
	}

	srv := newFakeHealthServer(t, true)
	loopbackAddr := strings.TrimPrefix(srv.URL, "http://")

	runner := &fakeRunner{
		handlers: map[string]func([]string) (string, string, error){
			"systemctl --user is-active decide-web.service":    func([]string) (string, string, error) { return "active\n", "", nil },
			"systemctl --user is-active decide-follow.service": func([]string) (string, string, error) { return "inactive\n", "", errors.New("inactive") },
			"tailscale serve status --json": func([]string) (string, string, error) {
				return `{"Web":{"box.tail.ts.net:443":{"Handlers":{"/":{"Proxy":"http://127.0.0.1:8787"}}}}}`, "", nil
			},
		},
	}

	env := DoctorEnv{
		GOOS:       "linux",
		UID:        "1000",
		HomeDir:    home,
		ConfigPath: cfgPath,
		Executable: exePath,
		LookPath: func(file string) (string, error) {
			if file == "decide" {
				return exePath, nil
			}
			return "/usr/bin/" + file, nil
		},
		RunCmd: runner.run,
		HTTPClient: &http.Client{
			Transport: roundTripFunc(func(req *http.Request) (*http.Response, error) {
				u := *req.URL
				u.Scheme = "http"
				u.Host = loopbackAddr
				r2 := req.Clone(req.Context())
				r2.URL = &u
				return http.DefaultTransport.RoundTrip(r2)
			}),
		},
	}
	return env, webCfg, runner
}

func TestDoctorWebAllPass(t *testing.T) {
	env, _, _ := setupHealthyDoctorFixture(t)
	checks := DoctorWebChecks(context.Background(), env, false)
	wantNames := []string{
		"web-service",
		"web-addr",
		"web-token",
		"web-auth",
		"web-public-url",
		"web-health",
		"web-forwarded-proto",
		"web-push",
		"web-binary",
		"web-follow-conflict",
	}
	if len(checks) != len(wantNames) {
		t.Fatalf("got %d checks, want %d: %+v", len(checks), len(wantNames), checks)
	}
	for _, name := range wantNames {
		c := findCheck(t, checks, name)
		if c.Status != toolcli.StatusOK {
			t.Errorf("check %s = %+v, want ok", name, c)
		}
	}
}

func TestDoctorWebFailCases(t *testing.T) {
	writeWebCfg := func(t *testing.T, path string, w decide.WebConfig) {
		t.Helper()
		raw, err := json.Marshal(map[string]any{"web": w})
		if err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, raw, 0o600); err != nil {
			t.Fatal(err)
		}
	}

	t.Run("web-service not running", func(t *testing.T) {
		env, _, runner := setupHealthyDoctorFixture(t)
		runner.handlers["systemctl --user is-active decide-web.service"] = func([]string) (string, string, error) {
			return "inactive", "", errors.New("inactive")
		}
		c := findCheck(t, DoctorWebChecks(context.Background(), env, false), "web-service")
		if c.Status != toolcli.StatusFail || !strings.Contains(c.Hint, "decide web setup") {
			t.Fatalf("got %+v, want fail with decide web setup hint", c)
		}
	})

	t.Run("web-addr invalid", func(t *testing.T) {
		env, cfg, _ := setupHealthyDoctorFixture(t)
		cfg.Addr = "not-a-host-port"
		writeWebCfg(t, env.ConfigPath, cfg)
		c := findCheck(t, DoctorWebChecks(context.Background(), env, false), "web-addr")
		if c.Status != toolcli.StatusFail || !strings.Contains(c.Hint, "decide web setup") {
			t.Fatalf("got %+v, want fail", c)
		}
	})

	t.Run("web-token bad mode and short token", func(t *testing.T) {
		env, cfg, _ := setupHealthyDoctorFixture(t)
		if err := os.Chmod(cfg.Auth.TokenFile, 0o644); err != nil {
			t.Fatal(err)
		}
		c := findCheck(t, DoctorWebChecks(context.Background(), env, false), "web-token")
		if c.Status != toolcli.StatusFail || !strings.Contains(c.Detail, "0644") {
			t.Fatalf("got %+v, want fail for 0644", c)
		}
		if err := os.Chmod(cfg.Auth.TokenFile, 0o600); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(cfg.Auth.TokenFile, []byte("short\n"), 0o600); err != nil {
			t.Fatal(err)
		}
		c = findCheck(t, DoctorWebChecks(context.Background(), env, false), "web-token")
		if c.Status != toolcli.StatusFail || !strings.Contains(c.Detail, "16") {
			t.Fatalf("got %+v, want fail for short token", c)
		}
	})

	t.Run("web-auth none on non-loopback", func(t *testing.T) {
		env, cfg, _ := setupHealthyDoctorFixture(t)
		cfg.Addr = "0.0.0.0:8787"
		cfg.Auth = decide.WebAuthConfig{Mode: decide.WebAuthNone}
		writeWebCfg(t, env.ConfigPath, cfg)
		c := findCheck(t, DoctorWebChecks(context.Background(), env, false), "web-auth")
		if c.Status != toolcli.StatusFail || !strings.Contains(c.Hint, "decide web setup") {
			t.Fatalf("got %+v, want fail", c)
		}
	})

	t.Run("web-public-url tailscale mismatch", func(t *testing.T) {
		env, cfg, _ := setupHealthyDoctorFixture(t)
		cfg.PublicURL = "https://other.tail.ts.net"
		writeWebCfg(t, env.ConfigPath, cfg)
		c := findCheck(t, DoctorWebChecks(context.Background(), env, false), "web-public-url")
		if c.Status != toolcli.StatusFail || !strings.Contains(c.Detail, "does not match") {
			t.Fatalf("got %+v, want fail for tailscale mismatch", c)
		}
	})

	t.Run("web-health unreachable", func(t *testing.T) {
		env, _, _ := setupHealthyDoctorFixture(t)
		env.HTTPClient = &http.Client{
			Transport: roundTripFunc(func(*http.Request) (*http.Response, error) {
				return nil, errors.New("connection refused")
			}),
		}
		c := findCheck(t, DoctorWebChecks(context.Background(), env, false), "web-health")
		if c.Status != toolcli.StatusFail || !strings.Contains(c.Hint, "decide web setup") {
			t.Fatalf("got %+v, want fail", c)
		}
	})

	t.Run("web-forwarded-proto missing", func(t *testing.T) {
		env, _, _ := setupHealthyDoctorFixture(t)
		insecureSrv := newFakeHealthServer(t, false)
		insecureAddr := strings.TrimPrefix(insecureSrv.URL, "http://")
		env.HTTPClient = &http.Client{
			Transport: roundTripFunc(func(req *http.Request) (*http.Response, error) {
				u := *req.URL
				u.Scheme = "http"
				u.Host = insecureAddr
				r2 := req.Clone(req.Context())
				r2.URL = &u
				return http.DefaultTransport.RoundTrip(r2)
			}),
		}
		c := findCheck(t, DoctorWebChecks(context.Background(), env, false), "web-forwarded-proto")
		if c.Status != toolcli.StatusFail || !strings.Contains(c.Detail, "X-Forwarded-Proto: https") {
			t.Fatalf("got %+v, want fail", c)
		}
	})

	t.Run("web-push invalid subject", func(t *testing.T) {
		env, cfg, _ := setupHealthyDoctorFixture(t)
		cfg.Push = decide.WebPushConfig{Enabled: true, Subject: DefaultVAPIDSubject}
		writeWebCfg(t, env.ConfigPath, cfg)
		c := findCheck(t, DoctorWebChecks(context.Background(), env, false), "web-push")
		if c.Status != toolcli.StatusFail || !strings.Contains(c.Hint, "decide web setup") {
			t.Fatalf("got %+v, want fail", c)
		}
	})

	t.Run("web-binary mismatch", func(t *testing.T) {
		env, cfg, _ := setupHealthyDoctorFixture(t)
		unitPath := systemdUnitPath(env.HomeDir)
		if err := os.WriteFile(unitPath, []byte(systemdUnitContent("/old/bin/decide", cfg.Auth.TokenFile)), 0o600); err != nil {
			t.Fatal(err)
		}
		c := findCheck(t, DoctorWebChecks(context.Background(), env, false), "web-binary")
		if c.Status != toolcli.StatusFail || !strings.Contains(c.Detail, "/old/bin/decide") {
			t.Fatalf("got %+v, want fail", c)
		}
	})

	t.Run("web-follow-conflict both running", func(t *testing.T) {
		env, _, runner := setupHealthyDoctorFixture(t)
		runner.handlers["systemctl --user is-active decide-follow.service"] = func([]string) (string, string, error) {
			return "active\n", "", nil
		}
		c := findCheck(t, DoctorWebChecks(context.Background(), env, false), "web-follow-conflict")
		if c.Status != toolcli.StatusFail || !strings.Contains(c.Detail, "decide-follow") {
			t.Fatalf("got %+v, want fail", c)
		}
	})
}
