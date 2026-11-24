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
		unitPath := systemdUnitPath(env.HomeDir)
		noTokenUnit := "[Unit]\nDescription=decide web inbox\n\n[Service]\nExecStart=" + env.Executable + " web\n\n[Install]\nWantedBy=default.target\n"
		if err := os.WriteFile(unitPath, []byte(noTokenUnit), 0o600); err != nil {
			t.Fatal(err)
		}
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

func TestExpandServicePath(t *testing.T) {
	env := pathExpandEnv{
		HomeDir: "/home/alice",
		User:    "alice",
		UID:     "1000",
		Getenv: func(k string) string {
			switch k {
			case "HOME":
				return "/home/alice"
			case "XDG_CONFIG_HOME":
				return "/srv/config"
			default:
				return ""
			}
		},
	}
	tests := []struct {
		name string
		in   string
		want string
	}{
		{name: "empty", in: "", want: ""},
		{name: "home specifier", in: "%h/go/bin/decide", want: "/home/alice/go/bin/decide"},
		{name: "user specifier", in: "/home/%u/go/bin/decide", want: "/home/alice/go/bin/decide"},
		{name: "uid specifier", in: "/run/user/%U/decide-token", want: "/run/user/1000/decide-token"},
		{name: "escaped percent", in: "/tmp/%%h/decide", want: "/tmp/%h/decide"},
		{name: "tilde prefix", in: "~/.config/decide/web-token", want: "/home/alice/.config/decide/web-token"},
		{name: "tilde only", in: "~", want: "/home/alice"},
		{name: "env var", in: "$HOME/go/bin/decide", want: "/home/alice/go/bin/decide"},
		{name: "braced env var", in: "${XDG_CONFIG_HOME}/decide/web-token", want: "/srv/config/decide/web-token"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := expandServicePath(tc.in, env); got != tc.want {
				t.Errorf("expandServicePath(%q) = %q, want %q", tc.in, got, tc.want)
			}
		})
	}
}

func TestDoctorWebServiceFlagsAndSpecifiers(t *testing.T) {
	t.Run("unit token-file without config web.auth and %h binary", func(t *testing.T) {
		env, cfg, _ := setupHealthyDoctorFixture(t)
		// Remove web.auth from config.json.
		cfg.Auth = decide.WebAuthConfig{}
		raw, err := json.Marshal(map[string]any{"web": cfg})
		if err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(env.ConfigPath, raw, 0o600); err != nil {
			t.Fatal(err)
		}
		// Write unit with %h binary and ~ token-file.
		unit := "[Unit]\nDescription=decide web inbox\n\n[Service]\nExecStart=%h/bin/decide web --token-file ~/.config/decide/web-token\nRestart=on-failure\n\n[Install]\nWantedBy=default.target\n"
		if err := os.WriteFile(systemdUnitPath(env.HomeDir), []byte(unit), 0o600); err != nil {
			t.Fatal(err)
		}

		checks := DoctorWebChecks(context.Background(), env, false)
		tok := findCheck(t, checks, "web-token")
		if tok.Status != toolcli.StatusOK || !strings.Contains(tok.Detail, "service ExecStart") {
			t.Errorf("web-token = %+v, want ok with source service ExecStart", tok)
		}
		auth := findCheck(t, checks, "web-auth")
		if auth.Status != toolcli.StatusOK || !strings.Contains(auth.Detail, "service ExecStart") {
			t.Errorf("web-auth = %+v, want ok with source service ExecStart", auth)
		}
		bin := findCheck(t, checks, "web-binary")
		if bin.Status != toolcli.StatusOK {
			t.Errorf("web-binary = %+v, want ok for %%h expansion", bin)
		}
	})

	t.Run("prefers systemctl show ExecStart on linux", func(t *testing.T) {
		env, cfg, runner := setupHealthyDoctorFixture(t)
		tokenPath := cfg.Auth.TokenFile
		cfg.Auth = decide.WebAuthConfig{}
		raw, err := json.Marshal(map[string]any{"web": cfg})
		if err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(env.ConfigPath, raw, 0o600); err != nil {
			t.Fatal(err)
		}
		// Unit file on disk has no --token-file, but systemctl show reports the effective ExecStart.
		noTokenUnit := "[Unit]\nDescription=decide web\n\n[Service]\nExecStart=" + env.Executable + " web\n"
		if err := os.WriteFile(systemdUnitPath(env.HomeDir), []byte(noTokenUnit), 0o600); err != nil {
			t.Fatal(err)
		}
		runner.handlers["systemctl --user show -p ExecStart decide-web.service"] = func([]string) (string, string, error) {
			return "ExecStart={ path=" + env.Executable + " ; argv[]=" + env.Executable + " web --token-file " + tokenPath + " ; ignore_errors=no ; status=0/0 }\n", "", nil
		}

		checks := DoctorWebChecks(context.Background(), env, false)
		tok := findCheck(t, checks, "web-token")
		if tok.Status != toolcli.StatusOK || !strings.Contains(tok.Detail, "service ExecStart") {
			t.Errorf("web-token = %+v, want ok from systemctl show", tok)
		}
		auth := findCheck(t, checks, "web-auth")
		if auth.Status != toolcli.StatusOK || !strings.Contains(auth.Detail, "service ExecStart") {
			t.Errorf("web-auth = %+v, want ok from systemctl show", auth)
		}
	})

	t.Run("darwin plist ProgramArguments token-file without config web.auth", func(t *testing.T) {
		env, cfg, runner := setupHealthyDoctorFixture(t)
		tokenPath := cfg.Auth.TokenFile
		cfg.Auth = decide.WebAuthConfig{}
		raw, err := json.Marshal(map[string]any{"web": cfg})
		if err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(env.ConfigPath, raw, 0o600); err != nil {
			t.Fatal(err)
		}
		env.GOOS = "darwin"
		plistPath := launchAgentPath(env.HomeDir)
		if err := os.MkdirAll(filepath.Dir(plistPath), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(plistPath, []byte(launchAgentContent(env.Executable, tokenPath, launchAgentLogPath(env.HomeDir))), 0o600); err != nil {
			t.Fatal(err)
		}
		runner.handlers["launchctl print gui/1000/com.github.jaeyeom.decide-web"] = func([]string) (string, string, error) {
			return "state = running", "", nil
		}
		runner.handlers["launchctl print gui/1000/com.github.jaeyeom.decide-follow"] = func([]string) (string, string, error) {
			return "", "not found", errors.New("not loaded")
		}

		checks := DoctorWebChecks(context.Background(), env, false)
		tok := findCheck(t, checks, "web-token")
		if tok.Status != toolcli.StatusOK || !strings.Contains(tok.Detail, "ProgramArguments") {
			t.Errorf("web-token = %+v, want ok from ProgramArguments", tok)
		}
		auth := findCheck(t, checks, "web-auth")
		if auth.Status != toolcli.StatusOK || !strings.Contains(auth.Detail, "ProgramArguments") {
			t.Errorf("web-auth = %+v, want ok from ProgramArguments", auth)
		}
	})
}

func TestDoctorWebOldServerForwardedProto(t *testing.T) {
	env, _, _ := setupHealthyDoctorFixture(t)
	oldSrv := newFakeOldHealthServer(t)
	oldAddr := strings.TrimPrefix(oldSrv.URL, "http://")
	env.HTTPClient = &http.Client{
		Transport: roundTripFunc(func(req *http.Request) (*http.Response, error) {
			u := *req.URL
			u.Scheme = "http"
			u.Host = oldAddr
			r2 := req.Clone(req.Context())
			r2.URL = &u
			return http.DefaultTransport.RoundTrip(r2)
		}),
	}

	checks := DoctorWebChecks(context.Background(), env, false)
	h := findCheck(t, checks, "web-health")
	if h.Status != toolcli.StatusOK {
		t.Errorf("web-health = %+v, want ok", h)
	}
	fp := findCheck(t, checks, "web-forwarded-proto")
	if fp.Status != toolcli.StatusWarn {
		t.Fatalf("web-forwarded-proto = %+v, want warn (not fail)", fp)
	}
	if !strings.Contains(fp.Detail, "server predates this check; restart the web service") {
		t.Errorf("web-forwarded-proto detail = %q, want old-server warning", fp.Detail)
	}
}

func TestDoctorWebAuthProbe401(t *testing.T) {
	env, cfg, _ := setupHealthyDoctorFixture(t)
	// Neither config nor unit specifies token auth, but live server returns 401 on /api/v1/items.
	cfg.Auth = decide.WebAuthConfig{}
	raw, err := json.Marshal(map[string]any{"web": cfg})
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(env.ConfigPath, raw, 0o600); err != nil {
		t.Fatal(err)
	}
	noTokenUnit := "[Unit]\nDescription=decide web\n\n[Service]\nExecStart=" + env.Executable + " web\n"
	if err := os.WriteFile(systemdUnitPath(env.HomeDir), []byte(noTokenUnit), 0o600); err != nil {
		t.Fatal(err)
	}

	probeSrv := newFakeAuthProbeServer(t, true, http.StatusUnauthorized)
	probeAddr := strings.TrimPrefix(probeSrv.URL, "http://")
	env.HTTPClient = &http.Client{
		Transport: roundTripFunc(func(req *http.Request) (*http.Response, error) {
			u := *req.URL
			u.Scheme = "http"
			u.Host = probeAddr
			r2 := req.Clone(req.Context())
			r2.URL = &u
			return http.DefaultTransport.RoundTrip(r2)
		}),
	}

	checks := DoctorWebChecks(context.Background(), env, false)
	auth := findCheck(t, checks, "web-auth")
	if auth.Status != toolcli.StatusOK || !strings.Contains(strings.ToLower(auth.Detail), "token") {
		t.Errorf("web-auth = %+v, want ok reporting token-enforced", auth)
	}
	tok := findCheck(t, checks, "web-token")
	if tok.Status != toolcli.StatusOK {
		t.Errorf("web-token = %+v, want ok when server enforces token auth", tok)
	}
}
