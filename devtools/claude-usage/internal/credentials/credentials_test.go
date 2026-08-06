package credentials

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestKeychainServiceNameDefault(t *testing.T) {
	t.Parallel()
	home := "/Users/alice"
	got := KeychainServiceName(filepath.Join(home, ".claude"), home)
	if got != LegacyKeychainService {
		t.Fatalf("got %q, want %q", got, LegacyKeychainService)
	}
}

func TestKeychainServiceNameCustomDirHasHashSuffix(t *testing.T) {
	t.Parallel()
	home := "/Users/alice"
	custom := "/Users/alice/work/claude-config"
	got := KeychainServiceName(custom, home)
	if !strings.HasPrefix(got, LegacyKeychainService+"-") {
		t.Fatalf("got %q, want prefix %q", got, LegacyKeychainService+"-")
	}
	suffix := strings.TrimPrefix(got, LegacyKeychainService+"-")
	if len(suffix) != 8 {
		t.Fatalf("hash suffix length = %d, want 8: %q", len(suffix), suffix)
	}
	// Stable hash for the cleaned absolute path.
	again := KeychainServiceName(custom, home)
	if again != got {
		t.Fatalf("unstable hash: %q vs %q", got, again)
	}
}

func TestKeychainServiceNamesIncludesLegacyFallback(t *testing.T) {
	t.Parallel()
	home := "/Users/alice"
	custom := "/tmp/claude-x"
	names := KeychainServiceNames(custom, home, func(string) string { return "" })
	if len(names) < 2 {
		t.Fatalf("want at least primary+legacy, got %v", names)
	}
	if names[len(names)-1] != LegacyKeychainService {
		t.Fatalf("last should be legacy, got %v", names)
	}
}

func TestKeychainServiceNamesWithEnvCustom(t *testing.T) {
	t.Parallel()
	home := "/Users/alice"
	configDir := filepath.Join(home, ".claude")
	envDir := "/custom/claude"
	names := KeychainServiceNames(configDir, home, func(k string) string {
		if k == "CLAUDE_CONFIG_DIR" {
			return envDir
		}
		return ""
	})
	// primary (legacy for default config), env-hashed, legacy again (deduped).
	if names[0] != LegacyKeychainService {
		t.Fatalf("primary = %q", names[0])
	}
	foundEnvHash := false
	for _, n := range names {
		if strings.HasPrefix(n, LegacyKeychainService+"-") {
			foundEnvHash = true
		}
	}
	if !foundEnvHash {
		t.Fatalf("expected env-hashed service in %v", names)
	}
}

func TestParseCredentialsJSON(t *testing.T) {
	t.Parallel()
	now := time.Date(2026, 8, 1, 0, 0, 0, 0, time.UTC)
	raw := []byte(`{
		"claudeAiOauth": {
			"accessToken": "sk-ant-oat-test",
			"subscriptionType": "team",
			"expiresAt": 1893456000000
		}
	}`)
	creds, err := parseCredentialsJSON(raw, now)
	if err != nil {
		t.Fatal(err)
	}
	if creds.AccessToken != "sk-ant-oat-test" {
		t.Fatalf("token = %q", creds.AccessToken)
	}
	if creds.SubscriptionType != "team" {
		t.Fatalf("sub = %q", creds.SubscriptionType)
	}
}

func TestParseCredentialsJSONExpired(t *testing.T) {
	t.Parallel()
	now := time.Date(2026, 8, 1, 0, 0, 0, 0, time.UTC)
	raw := []byte(`{
		"claudeAiOauth": {
			"accessToken": "sk-ant-oat-test",
			"subscriptionType": "pro",
			"expiresAt": 1000
		}
	}`)
	_, err := parseCredentialsJSON(raw, now)
	if !errors.Is(err, ErrExpired) {
		t.Fatalf("got %v, want ErrExpired", err)
	}
}

func TestParseCredentialsJSONMissingToken(t *testing.T) {
	t.Parallel()
	_, err := parseCredentialsJSON([]byte(`{"claudeAiOauth":{}}`), time.Now())
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("got %v, want ErrNotFound", err)
	}
}

func TestResolveFromFile(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	now := time.Date(2026, 8, 1, 0, 0, 0, 0, time.UTC)
	payload := `{
		"claudeAiOauth": {
			"accessToken": "file-token",
			"subscriptionType": "pro",
			"expiresAt": 1893456000000
		}
	}`
	if err := os.WriteFile(filepath.Join(dir, ".credentials.json"), []byte(payload), 0o600); err != nil {
		t.Fatal(err)
	}

	r := &Resolver{
		ConfigDir: dir,
		HomeDir:   t.TempDir(),
		GOOS:      "linux",
		Now:       func() time.Time { return now },
		LoadKeychain: func(context.Context, string, string) (string, error) {
			t.Fatal("keychain should not be used on linux")
			return "", nil
		},
	}
	creds, err := r.Resolve(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if creds.AccessToken != "file-token" || creds.SubscriptionType != "pro" {
		t.Fatalf("got %+v", creds)
	}
}

func TestResolveKeychainPreferred(t *testing.T) {
	t.Parallel()
	home := t.TempDir()
	configDir := filepath.Join(home, ".claude")
	if err := os.MkdirAll(configDir, 0o755); err != nil {
		t.Fatal(err)
	}
	// File has different token; keychain must win.
	if err := os.WriteFile(filepath.Join(configDir, ".credentials.json"), []byte(`{
		"claudeAiOauth": {"accessToken":"file-token","subscriptionType":"pro","expiresAt":1893456000000}
	}`), 0o600); err != nil {
		t.Fatal(err)
	}

	now := time.Date(2026, 8, 1, 0, 0, 0, 0, time.UTC)
	r := &Resolver{
		ConfigDir: configDir,
		HomeDir:   home,
		GOOS:      "darwin",
		Username:  "tester",
		Now:       func() time.Time { return now },
		LoadKeychain: func(_ context.Context, service, account string) (string, error) {
			if service != LegacyKeychainService {
				return "", fmt.Errorf("could not be found in the keychain")
			}
			if account != "tester" {
				return "", fmt.Errorf("could not be found in the keychain")
			}
			return `{
				"claudeAiOauth": {
					"accessToken": "kc-token",
					"subscriptionType": "team",
					"expiresAt": 1893456000000
				}
			}`, nil
		},
	}
	creds, err := r.Resolve(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if creds.AccessToken != "kc-token" {
		t.Fatalf("token = %q, want keychain token", creds.AccessToken)
	}
	if creds.SubscriptionType != "team" {
		t.Fatalf("sub = %q", creds.SubscriptionType)
	}
}

func TestResolveKeychainSupplementsSubscriptionFromFile(t *testing.T) {
	t.Parallel()
	home := t.TempDir()
	configDir := filepath.Join(home, ".claude")
	if err := os.MkdirAll(configDir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(configDir, ".credentials.json"), []byte(`{
		"claudeAiOauth": {"accessToken":"file-token","subscriptionType":"max","expiresAt":1893456000000}
	}`), 0o600); err != nil {
		t.Fatal(err)
	}
	now := time.Date(2026, 8, 1, 0, 0, 0, 0, time.UTC)
	r := &Resolver{
		ConfigDir: configDir,
		HomeDir:   home,
		GOOS:      "darwin",
		Username:  "tester",
		Now:       func() time.Time { return now },
		LoadKeychain: func(context.Context, string, string) (string, error) {
			return `{"claudeAiOauth":{"accessToken":"kc-token","expiresAt":1893456000000}}`, nil
		},
	}
	creds, err := r.Resolve(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	if creds.AccessToken != "kc-token" {
		t.Fatalf("token = %q", creds.AccessToken)
	}
	if creds.SubscriptionType != "max" {
		t.Fatalf("sub = %q, want max from file", creds.SubscriptionType)
	}
}

func TestResolveNotFound(t *testing.T) {
	t.Parallel()
	r := &Resolver{
		ConfigDir: t.TempDir(),
		HomeDir:   t.TempDir(),
		GOOS:      "darwin",
		Username:  "tester",
		Now:       time.Now,
		LoadKeychain: func(context.Context, string, string) (string, error) {
			return "", fmt.Errorf("could not be found in the keychain")
		},
	}
	_, err := r.Resolve(context.Background())
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("got %v, want ErrNotFound", err)
	}
}

func TestCredsStringRedactsToken(t *testing.T) {
	t.Parallel()
	c := Creds{AccessToken: "sk-secret", SubscriptionType: "team"}
	s := c.String()
	if strings.Contains(s, "sk-secret") {
		t.Fatalf("token leaked in String(): %s", s)
	}
	if !strings.Contains(s, "<redacted>") {
		t.Fatalf("expected redaction marker: %s", s)
	}
	if strings.Contains(fmt.Sprintf("%#v", c), "sk-secret") {
		t.Fatalf("token leaked in GoString")
	}
}

func TestPlanName(t *testing.T) {
	t.Parallel()
	tests := []struct {
		in, want string
	}{
		{"team", "Team"},
		{"pro", "Pro"},
		{"max", "Max"},
		{"api", ""},
		{"", ""},
		{"enterprise", "Enterprise"},
	}
	for _, tt := range tests {
		if got := PlanName(tt.in); got != tt.want {
			t.Errorf("PlanName(%q) = %q, want %q", tt.in, got, tt.want)
		}
	}
}

func TestIsMissingKeychainItem(t *testing.T) {
	t.Parallel()
	// security(1) phrasing (matcher is case-insensitive; trailing period optional).
	if !isMissingKeychainItem(errors.New("could not be found in the keychain")) {
		t.Fatal("expected missing detection")
	}
	if isMissingKeychainItem(fmt.Errorf("User interaction is not allowed")) {
		t.Fatal("auth prompt should not count as missing")
	}
}
