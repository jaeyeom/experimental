package web

import (
	"bytes"
	"context"
	"crypto/rand"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

type fakeRunner struct {
	calls     []string
	sudoCalls []string
	handlers  map[string]func(args []string) (string, string, error)
}

func (f *fakeRunner) run(_ context.Context, name string, args ...string) (string, string, error) {
	key := name + " " + strings.Join(args, " ")
	f.calls = append(f.calls, key)
	if fn, ok := f.handlers[key]; ok {
		return fn(args)
	}
	if fn, ok := f.handlers[name]; ok {
		return fn(args)
	}
	return "", "", errors.New("unexpected command: " + key)
}

func (f *fakeRunner) runSudo(_ context.Context, name string, args ...string) error {
	key := name + " " + strings.Join(args, " ")
	f.sudoCalls = append(f.sudoCalls, key)
	return nil
}

func newFakeHealthServer(t *testing.T, secure bool) *httptest.Server {
	t.Helper()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/api/v1/health" {
			http.NotFound(w, r)
			return
		}
		w.Header().Set("Content-Type", "application/json")
		_ = json.NewEncoder(w).Encode(map[string]any{"ok": true, "secure": secure})
	}))
	t.Cleanup(srv.Close)
	return srv
}

func TestSetupDryRunPrintsPlanAndChangesNothing(t *testing.T) {
	home := t.TempDir()
	cfgPath := filepath.Join(home, ".config", "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(cfgPath), 0o755); err != nil {
		t.Fatal(err)
	}
	origCfg := `{"notifyPane":"p1"}`
	if err := os.WriteFile(cfgPath, []byte(origCfg), 0o600); err != nil {
		t.Fatal(err)
	}

	runner := &fakeRunner{
		handlers: map[string]func([]string) (string, string, error){
			"systemctl --user is-active decide-web.service":      func([]string) (string, string, error) { return "inactive", "", errors.New("inactive") },
			"loginctl show-user alice --property=Linger --value": func([]string) (string, string, error) { return "no\n", "", nil },
			"tailscale status --json":                            func([]string) (string, string, error) { return `{"CertDomains":["box.tail.ts.net"]}`, "", nil },
			"tailscale serve status --json":                      func([]string) (string, string, error) { return `{}`, "", nil },
			"tailscale debug prefs":                              func([]string) (string, string, error) { return `{"OperatorUser":""}`, "", nil },
		},
	}

	var stdout, stderr bytes.Buffer
	env := SetupEnv{
		GOOS:       "linux",
		User:       "alice",
		UID:        "1000",
		HomeDir:    home,
		ConfigPath: cfgPath,
		Executable: "/usr/local/bin/decide",
		LookPath:   func(file string) (string, error) { return "/usr/bin/" + file, nil },
		RunCmd:     runner.run,
		RunSudoCmd: runner.runSudo,
		Stdin:      strings.NewReader(""),
		Stdout:     &stdout,
		Stderr:     &stderr,
	}

	if err := RunSetup(context.Background(), env, SetupOptions{DryRun: true}); err != nil {
		t.Fatalf("RunSetup dry-run: %v", err)
	}

	// Nothing created or modified.
	tokenPath := filepath.Join(filepath.Dir(cfgPath), "web-token")
	if _, err := os.Stat(tokenPath); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("dry-run created token file: err=%v", err)
	}
	if _, err := os.Stat(systemdUnitPath(home)); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("dry-run created systemd unit: err=%v", err)
	}
	if _, err := os.Stat(cfgPath + ".bak"); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("dry-run created backup: err=%v", err)
	}
	gotCfg, err := os.ReadFile(cfgPath)
	if err != nil {
		t.Fatal(err)
	}
	if string(gotCfg) != origCfg {
		t.Fatalf("dry-run modified config.json: got %q, want %q", gotCfg, origCfg)
	}
	if len(runner.sudoCalls) != 0 {
		t.Fatalf("dry-run executed sudo commands: %v", runner.sudoCalls)
	}

	out := stdout.String()
	for _, want := range []string{
		"create token file",
		"decide-web.service",
		"sudo loginctl enable-linger alice",
		"sudo tailscale set --operator=alice",
		"tailscale serve --bg http://127.0.0.1:8787",
		"[dry-run]",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("dry-run output missing %q:\n%s", want, out)
		}
	}
}

func TestSetupFullRunBackupTokenSecretAndIdempotentSecondRun(t *testing.T) {
	home := t.TempDir()
	cfgPath := filepath.Join(home, ".config", "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(cfgPath), 0o755); err != nil {
		t.Fatal(err)
	}
	origCfg := "{\n  \"notifyPane\": \"p1\"\n}\n"
	if err := os.WriteFile(cfgPath, []byte(origCfg), 0o600); err != nil {
		t.Fatal(err)
	}

	healthSrv := newFakeHealthServer(t, true)
	loopbackAddr := strings.TrimPrefix(healthSrv.URL, "http://")

	// Start with config pointing web.addr to our fake health server so step f succeeds.
	if err := decide.SetWebConfigAt(cfgPath, decide.WebConfig{Addr: loopbackAddr}); err != nil {
		t.Fatal(err)
	}
	// Remove backup created by test fixture setup, rewrite fresh config with a new path?
	// Even better: write the initial config directly so backupConfig has never seen this path.
	cfgPath2 := filepath.Join(t.TempDir(), "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(cfgPath2), 0o755); err != nil {
		t.Fatal(err)
	}
	initCfg := `{"notifyPane":"p1","web":{"addr":"` + loopbackAddr + `"}}`
	if err := os.WriteFile(cfgPath2, []byte(initCfg), 0o600); err != nil {
		t.Fatal(err)
	}

	serviceActive := false
	lingerEnabled := false
	operatorSet := false
	serving := false

	runner := &fakeRunner{
		handlers: map[string]func([]string) (string, string, error){
			"systemctl --user is-active decide-web.service": func([]string) (string, string, error) {
				if serviceActive {
					return "active\n", "", nil
				}
				return "inactive\n", "", errors.New("inactive")
			},
			"systemctl --user daemon-reload": func([]string) (string, string, error) { return "", "", nil },
			"systemctl --user enable --now decide-web.service": func([]string) (string, string, error) {
				serviceActive = true
				return "", "", nil
			},
			"loginctl show-user alice --property=Linger --value": func([]string) (string, string, error) {
				if lingerEnabled {
					return "yes\n", "", nil
				}
				return "no\n", "", nil
			},
			"tailscale status --json": func([]string) (string, string, error) {
				return `{"CertDomains":["box.tail.ts.net"]}`, "", nil
			},
			"tailscale serve status --json": func([]string) (string, string, error) {
				if !serving {
					return `{}`, "", nil
				}
				return `{"Web":{"box.tail.ts.net:443":{"Handlers":{"/":{"Proxy":"http://` + loopbackAddr + `"}}}}}`, "", nil
			},
			"tailscale debug prefs": func([]string) (string, string, error) {
				if operatorSet {
					return `{"OperatorUser":"alice"}`, "", nil
				}
				return `{"OperatorUser":""}`, "", nil
			},
			"tailscale serve --bg http://" + loopbackAddr: func([]string) (string, string, error) {
				serving = true
				return "", "", nil
			},
		},
	}

	sudoFn := func(_ context.Context, name string, args ...string) error {
		key := name + " " + strings.Join(args, " ")
		runner.sudoCalls = append(runner.sudoCalls, key)
		if strings.Contains(key, "enable-linger") {
			lingerEnabled = true
		}
		if strings.Contains(key, "--operator=alice") {
			operatorSet = true
		}
		return nil
	}

	// Route HTTPS requests for box.tail.ts.net to our fake health server.
	httpClient := &http.Client{
		Transport: roundTripFunc(func(req *http.Request) (*http.Response, error) {
			u := *req.URL
			u.Scheme = "http"
			u.Host = loopbackAddr
			r2 := req.Clone(req.Context())
			r2.URL = &u
			return http.DefaultTransport.RoundTrip(r2)
		}),
	}

	// Interactive answers:
	// 1. token: y
	// 2. systemd service: y
	// 3. sudo linger: y
	// 4. sudo operator: y
	// 5. tailscale serve: y
	// 6. update config: y
	// 7. enable web push: y
	// 8. push subject (first try default -> refused, second try real mailto -> accepted)
	answers := strings.Join([]string{
		"y",
		"y",
		"y",
		"y",
		"y",
		"y",
		"y",
		DefaultVAPIDSubject,
		"mailto:alice@example.com",
	}, "\n") + "\n"

	var stdout, stderr bytes.Buffer
	env := SetupEnv{
		GOOS:       "linux",
		User:       "alice",
		UID:        "1000",
		HomeDir:    home,
		ConfigPath: cfgPath2,
		Executable: "/usr/local/bin/decide",
		LookPath:   func(file string) (string, error) { return "/usr/bin/" + file, nil },
		RunCmd:     runner.run,
		RunSudoCmd: sudoFn,
		HTTPClient: httpClient,
		Rand:       rand.Reader,
		Stdin:      strings.NewReader(answers),
		Stdout:     &stdout,
		Stderr:     &stderr,
	}

	if err := RunSetup(context.Background(), env, SetupOptions{}); err != nil {
		t.Fatalf("first RunSetup: %v\nstdout:\n%s", err, stdout.String())
	}

	// Verify token file exists, mode 0600, and token is NEVER printed.
	tokenPath := filepath.Join(filepath.Dir(cfgPath2), "web-token")
	info, err := os.Stat(tokenPath)
	if err != nil {
		t.Fatalf("stat token file: %v", err)
	}
	if info.Mode().Perm() != 0o600 {
		t.Fatalf("token mode = %04o, want 0600", info.Mode().Perm())
	}
	tokBytes, err := os.ReadFile(tokenPath)
	if err != nil {
		t.Fatal(err)
	}
	tok := strings.TrimSpace(string(tokBytes))
	if len(tok) < 16 {
		t.Fatalf("token too short: %d", len(tok))
	}
	if strings.Contains(stdout.String(), tok) || strings.Contains(stderr.String(), tok) {
		t.Fatalf("token value was printed in output!")
	}
	if !strings.Contains(stdout.String(), "Refused") {
		t.Errorf("expected default VAPID subject to be refused:\n%s", stdout.String())
	}

	// Verify config.json.bak was written with original contents before the change.
	bak, err := os.ReadFile(cfgPath2 + ".bak")
	if err != nil {
		t.Fatalf("read backup: %v", err)
	}
	if string(bak) != initCfg {
		t.Fatalf("backup = %q, want %q", string(bak), initCfg)
	}

	// Verify updated config.json.
	gotCfg, err := decide.LoadConfigAt(cfgPath2)
	if err != nil {
		t.Fatal(err)
	}
	if gotCfg.Web.PublicURL != "https://box.tail.ts.net" {
		t.Errorf("publicUrl = %q, want https://box.tail.ts.net", gotCfg.Web.PublicURL)
	}
	if !gotCfg.Web.Push.Enabled || gotCfg.Web.Push.Subject != "mailto:alice@example.com" {
		t.Errorf("push = %+v", gotCfg.Web.Push)
	}

	// Second run: empty stdin, every step must already be done and ask NOTHING.
	stdout.Reset()
	stderr.Reset()
	runner.sudoCalls = nil
	env.Stdin = strings.NewReader("")
	env.Stdout = &stdout
	env.Stderr = &stderr
	if err := RunSetup(context.Background(), env, SetupOptions{}); err != nil {
		t.Fatalf("second RunSetup: %v", err)
	}
	out2 := stdout.String()
	if strings.Contains(out2, "[y/N]") {
		t.Fatalf("second run asked a question:\n%s", out2)
	}
	if len(runner.sudoCalls) != 0 {
		t.Fatalf("second run called sudo: %v", runner.sudoCalls)
	}
}

func TestSetupYesStillAsksBeforeSudo(t *testing.T) {
	home := t.TempDir()
	cfgPath := filepath.Join(t.TempDir(), "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(cfgPath), 0o755); err != nil {
		t.Fatal(err)
	}

	runner := &fakeRunner{
		handlers: map[string]func([]string) (string, string, error){
			"systemctl --user is-active decide-web.service":      func([]string) (string, string, error) { return "inactive", "", errors.New("inactive") },
			"systemctl --user daemon-reload":                     func([]string) (string, string, error) { return "", "", nil },
			"systemctl --user enable --now decide-web.service":   func([]string) (string, string, error) { return "", "", nil },
			"loginctl show-user alice --property=Linger --value": func([]string) (string, string, error) { return "no\n", "", nil },
			"tailscale status --json":                            func([]string) (string, string, error) { return `{"CertDomains":["box.tail.ts.net"]}`, "", nil },
			"tailscale serve status --json":                      func([]string) (string, string, error) { return `{}`, "", nil },
			"tailscale debug prefs":                              func([]string) (string, string, error) { return `{"OperatorUser":""}`, "", nil },
			"tailscale serve --bg http://127.0.0.1:8787":         func([]string) (string, string, error) { return "", "", nil },
		},
	}

	// Provide "n" to both sudo prompts while --yes is true.
	var stdout bytes.Buffer
	env := SetupEnv{
		GOOS:       "linux",
		User:       "alice",
		UID:        "1000",
		HomeDir:    home,
		ConfigPath: cfgPath,
		Executable: "/usr/local/bin/decide",
		LookPath:   func(file string) (string, error) { return "/usr/bin/" + file, nil },
		RunCmd:     runner.run,
		RunSudoCmd: runner.runSudo,
		HTTPClient: &http.Client{
			Transport: roundTripFunc(func(*http.Request) (*http.Response, error) {
				return nil, errors.New("offline")
			}),
		},
		Stdin:  strings.NewReader("n\nn\n"),
		Stdout: &stdout,
		Stderr: &bytes.Buffer{},
	}

	if err := RunSetup(context.Background(), env, SetupOptions{Yes: true}); err != nil {
		t.Fatalf("RunSetup --yes: %v", err)
	}

	// Non-sudo steps ran automatically, but sudo commands were declined because --yes still asked.
	if len(runner.sudoCalls) != 0 {
		t.Fatalf("sudo ran despite 'n' answer with --yes: %v", runner.sudoCalls)
	}
	if _, err := os.Stat(systemdUnitPath(home)); err != nil {
		t.Fatalf("non-sudo systemd unit not written with --yes: %v", err)
	}
	out := stdout.String()
	if !strings.Contains(out, "Run `sudo loginctl enable-linger alice`? [y/N]: ") {
		t.Errorf("missing sudo linger prompt:\n%s", out)
	}
	if !strings.Contains(out, "Run `sudo tailscale set --operator=alice`? [y/N]: ") {
		t.Errorf("missing sudo operator prompt:\n%s", out)
	}
}

func TestSetupDarwinLaunchAgent(t *testing.T) {
	home := t.TempDir()
	cfgPath := filepath.Join(t.TempDir(), "decide", "config.json")
	if err := os.MkdirAll(filepath.Dir(cfgPath), 0o755); err != nil {
		t.Fatal(err)
	}

	loaded := false
	runner := &fakeRunner{
		handlers: map[string]func([]string) (string, string, error){
			"launchctl print gui/501/com.github.jaeyeom.decide-web": func([]string) (string, string, error) {
				if loaded {
					return "state = running", "", nil
				}
				return "", "not found", errors.New("not loaded")
			},
			"launchctl": func(args []string) (string, string, error) {
				if len(args) > 0 && args[0] == "bootstrap" {
					loaded = true
					return "", "", nil
				}
				return "", "", nil
			},
		},
	}

	var stdout bytes.Buffer
	env := SetupEnv{
		GOOS:       "darwin",
		User:       "alice",
		UID:        "501",
		HomeDir:    home,
		ConfigPath: cfgPath,
		Executable: "/opt/bin/decide",
		LookPath:   func(string) (string, error) { return "", errors.New("not found") },
		RunCmd:     runner.run,
		RunSudoCmd: runner.runSudo,
		HTTPClient: &http.Client{
			Transport: roundTripFunc(func(*http.Request) (*http.Response, error) {
				return nil, errors.New("offline")
			}),
		},
		Stdin:  strings.NewReader(""),
		Stdout: &stdout,
		Stderr: &bytes.Buffer{},
	}

	if err := RunSetup(context.Background(), env, SetupOptions{Yes: true}); err != nil {
		t.Fatalf("RunSetup darwin: %v", err)
	}
	plist, err := os.ReadFile(launchAgentPath(home))
	if err != nil {
		t.Fatalf("read plist: %v", err)
	}
	if !strings.Contains(string(plist), "<string>/opt/bin/decide</string>") {
		t.Fatalf("plist missing executable path:\n%s", plist)
	}
	if !loaded {
		t.Fatal("expected launchctl bootstrap to be called")
	}
}

type roundTripFunc func(*http.Request) (*http.Response, error)

func (f roundTripFunc) RoundTrip(r *http.Request) (*http.Response, error) {
	return f(r)
}
