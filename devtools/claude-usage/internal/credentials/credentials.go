// Package credentials resolves Claude Code OAuth credentials without claude-hud.
//
// Primary source on macOS is the Keychain item written by Claude Code.
// Older installs may store credentials in $CLAUDE_CONFIG_DIR/.credentials.json.
//
// Security: access tokens are held only in memory and must never be logged or
// printed. Error messages intentionally omit Keychain/file payload content.
package credentials

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/user"
	"path/filepath"
	"runtime"
	"strings"
	"time"
)

// LegacyKeychainService is the Keychain service name for the default ~/.claude config dir.
const LegacyKeychainService = "Claude Code-credentials"

// Sentinel errors for credential resolution failures.
var (
	// ErrNotFound means no usable OAuth credentials were found.
	ErrNotFound = errors.New("claude code oauth credentials not found")
	// ErrExpired means credentials were found but the access token is expired.
	ErrExpired = errors.New("claude code oauth access token expired")
	// ErrUnsupportedPlatform means Keychain lookup is not available (non-macOS)
	// and no file credentials were found either. Callers usually treat this like
	// ErrNotFound after trying the file fallback.
	ErrUnsupportedPlatform = errors.New("keychain credentials only supported on macOS")
)

// Creds is a short-lived in-memory credential snapshot.
// Do not log or print AccessToken.
type Creds struct {
	AccessToken      string
	SubscriptionType string
}

// String redacts the token so accidental fmt.Print never leaks it.
func (c Creds) String() string {
	if c.AccessToken == "" {
		return "Creds{AccessToken:<empty>, SubscriptionType:" + c.SubscriptionType + "}"
	}
	return "Creds{AccessToken:<redacted>, SubscriptionType:" + c.SubscriptionType + "}"
}

// GoString redacts the token for %#v formatting.
func (c Creds) GoString() string { return c.String() }

// KeychainLoader loads a generic-password item by service (and optional account).
// Implementations must not log the returned secret.
// A missing item should return an error whose text contains
// "could not be found in the keychain" (matching security(1)).
type KeychainLoader func(ctx context.Context, service, account string) (string, error)

// Resolver loads Claude Code OAuth credentials.
type Resolver struct {
	// ConfigDir is CLAUDE_CONFIG_DIR (default: ~/.claude under HomeDir).
	ConfigDir string
	// HomeDir overrides the user home directory (default: os.UserHomeDir).
	HomeDir string
	// Env looks up environment variables (default: os.Getenv).
	Env func(string) string
	// Now returns the current time (default: time.Now). Used for expiry checks.
	Now func() time.Time
	// GOOS overrides runtime.GOOS for tests (default: runtime.GOOS).
	GOOS string
	// Username is the Keychain account name (default: current OS user).
	Username string
	// LoadKeychain reads Keychain items (default: security(1) on macOS).
	LoadKeychain KeychainLoader
	// ReadFile reads credential files (default: os.ReadFile).
	ReadFile func(name string) ([]byte, error)
}

// Resolve returns OAuth credentials from Keychain (macOS) or the legacy file.
func (r *Resolver) Resolve(ctx context.Context) (*Creds, error) {
	r.applyDefaults()

	now := r.Now()
	home, err := r.homeDir()
	if err != nil {
		return nil, err
	}
	configDir, err := r.configDir(home)
	if err != nil {
		return nil, err
	}

	// Keychain is authoritative on macOS (Claude Code 2.x).
	if r.GOOS == "darwin" {
		kc, err := r.resolveKeychain(ctx, configDir, home, now)
		if err != nil && !errors.Is(err, ErrNotFound) && !errors.Is(err, ErrExpired) {
			// Non-missing Keychain failures: still try file fallback below.
			// Prefer file if present; otherwise surface the keychain error.
			if fileCreds, fileErr := r.resolveFile(configDir, now); fileErr == nil {
				return fileCreds, nil
			}
			return nil, err
		}
		if kc != nil {
			if kc.SubscriptionType == "" {
				if sub, ok := r.fileSubscriptionType(configDir); ok {
					kc.SubscriptionType = sub
				}
			}
			return kc, nil
		}
		if errors.Is(err, ErrExpired) {
			// Token expired in keychain — still allow file if it has a fresher token.
			if fileCreds, fileErr := r.resolveFile(configDir, now); fileErr == nil {
				return fileCreds, nil
			}
			return nil, ErrExpired
		}
	}

	fileCreds, err := r.resolveFile(configDir, now)
	if err != nil {
		if r.GOOS != "darwin" {
			return nil, fmt.Errorf("%w: %v (keychain unsupported on %s)", ErrNotFound, err, r.GOOS)
		}
		return nil, err
	}
	return fileCreds, nil
}

func (r *Resolver) applyDefaults() {
	if r.Env == nil {
		r.Env = os.Getenv
	}
	if r.Now == nil {
		r.Now = time.Now
	}
	if r.GOOS == "" {
		r.GOOS = runtime.GOOS
	}
	if r.ReadFile == nil {
		r.ReadFile = os.ReadFile
	}
	if r.LoadKeychain == nil {
		r.LoadKeychain = defaultKeychainLoader
	}
}

func (r *Resolver) homeDir() (string, error) {
	if r.HomeDir != "" {
		return r.HomeDir, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("resolve home dir: %w", err)
	}
	return home, nil
}

func (r *Resolver) configDir(home string) (string, error) {
	if r.ConfigDir != "" {
		return filepath.Clean(r.ConfigDir), nil
	}
	if v := strings.TrimSpace(r.Env("CLAUDE_CONFIG_DIR")); v != "" {
		return expandHome(v, home), nil
	}
	return filepath.Join(home, ".claude"), nil
}

func expandHome(p, home string) string {
	if p == "~" {
		return home
	}
	if strings.HasPrefix(p, "~/") || strings.HasPrefix(p, `~\`) {
		return filepath.Join(home, p[2:])
	}
	if filepath.IsAbs(p) {
		return filepath.Clean(p)
	}
	abs, err := filepath.Abs(p)
	if err != nil {
		return filepath.Clean(p)
	}
	return abs
}

// KeychainServiceName returns the primary Keychain service for configDir.
// Default ~/.claude uses the legacy name; custom dirs use a short path hash suffix.
func KeychainServiceName(configDir, homeDir string) string {
	normalizedConfig := filepath.Clean(configDir)
	if !filepath.IsAbs(normalizedConfig) {
		if abs, err := filepath.Abs(normalizedConfig); err == nil {
			normalizedConfig = abs
		}
	}
	normalizedDefault := filepath.Clean(filepath.Join(homeDir, ".claude"))
	if !filepath.IsAbs(normalizedDefault) {
		if abs, err := filepath.Abs(normalizedDefault); err == nil {
			normalizedDefault = abs
		}
	}
	if normalizedConfig == normalizedDefault {
		return LegacyKeychainService
	}
	sum := sha256.Sum256([]byte(normalizedConfig))
	return LegacyKeychainService + "-" + hex.EncodeToString(sum[:])[:8]
}

// KeychainServiceNames returns service names to try, matching claude-hud order:
// primary for config dir, optional env-derived name, then legacy fallback.
func KeychainServiceNames(configDir, homeDir string, env func(string) string) []string {
	if env == nil {
		env = os.Getenv
	}
	names := []string{KeychainServiceName(configDir, homeDir)}
	if envConfig := strings.TrimSpace(env("CLAUDE_CONFIG_DIR")); envConfig != "" {
		normalizedDefault := filepath.Clean(filepath.Join(homeDir, ".claude"))
		normalizedEnv := expandHome(envConfig, homeDir)
		if normalizedEnv == normalizedDefault {
			names = append(names, LegacyKeychainService)
		} else {
			// claude-hud hashes the raw env string (not necessarily resolved path).
			sum := sha256.Sum256([]byte(envConfig))
			names = append(names, LegacyKeychainService+"-"+hex.EncodeToString(sum[:])[:8])
		}
	}
	names = append(names, LegacyKeychainService)
	return uniquePreserve(names)
}

func uniquePreserve(in []string) []string {
	seen := make(map[string]struct{}, len(in))
	out := make([]string, 0, len(in))
	for _, s := range in {
		if _, ok := seen[s]; ok {
			continue
		}
		seen[s] = struct{}{}
		out = append(out, s)
	}
	return out
}

func (r *Resolver) accountName() string {
	if r.Username != "" {
		return r.Username
	}
	u, err := user.Current()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(u.Username)
}

func (r *Resolver) resolveKeychain(ctx context.Context, configDir, home string, now time.Time) (*Creds, error) {
	services := KeychainServiceNames(configDir, home, r.Env)
	account := r.accountName()

	// Prefer account-scoped lookup when we know the username (Claude Code stores acct).
	if account != "" {
		creds, err := r.tryKeychainServices(ctx, services, account, now)
		if creds != nil || (err != nil && !errors.Is(err, ErrNotFound)) {
			return creds, err
		}
	}
	// Generic fallback without -a.
	return r.tryKeychainServices(ctx, services, "", now)
}

func (r *Resolver) tryKeychainServices(ctx context.Context, services []string, account string, now time.Time) (*Creds, error) {
	var sawExpired bool
	var lastErr error
	for _, service := range services {
		raw, err := r.LoadKeychain(ctx, service, account)
		if err != nil {
			if isMissingKeychainItem(err) {
				continue
			}
			// Do not include err text if it might embed secrets from stdout.
			lastErr = fmt.Errorf("keychain lookup for service %q failed", service)
			continue
		}
		raw = strings.TrimSpace(raw)
		if raw == "" {
			continue
		}
		creds, err := parseCredentialsJSON([]byte(raw), now)
		if errors.Is(err, ErrExpired) {
			sawExpired = true
			continue
		}
		if err != nil {
			// Invalid payload — do not include raw content.
			lastErr = fmt.Errorf("invalid keychain credentials payload for service %q", service)
			continue
		}
		return creds, nil
	}
	if sawExpired {
		return nil, ErrExpired
	}
	if lastErr != nil {
		return nil, lastErr
	}
	return nil, ErrNotFound
}

func isMissingKeychainItem(err error) bool {
	if err == nil {
		return false
	}
	msg := strings.ToLower(err.Error())
	return strings.Contains(msg, "could not be found in the keychain") ||
		strings.Contains(msg, "sec_item_not_found") ||
		strings.Contains(msg, "item not found")
}

func (r *Resolver) resolveFile(configDir string, now time.Time) (*Creds, error) {
	path := filepath.Join(configDir, ".credentials.json")
	raw, err := r.ReadFile(path)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return nil, ErrNotFound
		}
		return nil, fmt.Errorf("read credentials file: %w", err)
	}
	creds, err := parseCredentialsJSON(raw, now)
	if err != nil {
		return nil, err
	}
	return creds, nil
}

func (r *Resolver) fileSubscriptionType(configDir string) (string, bool) {
	path := filepath.Join(configDir, ".credentials.json")
	raw, err := r.ReadFile(path)
	if err != nil {
		return "", false
	}
	var data credentialsFile
	if err := json.Unmarshal(raw, &data); err != nil {
		return "", false
	}
	sub := strings.TrimSpace(data.ClaudeAiOauth.SubscriptionType)
	if sub == "" {
		return "", false
	}
	return sub, true
}

type credentialsFile struct {
	ClaudeAiOauth struct {
		AccessToken      string `json:"accessToken"`
		SubscriptionType string `json:"subscriptionType"`
		ExpiresAt        *int64 `json:"expiresAt"`
	} `json:"claudeAiOauth"`
}

func parseCredentialsJSON(raw []byte, now time.Time) (*Creds, error) {
	var data credentialsFile
	if err := json.Unmarshal(raw, &data); err != nil {
		return nil, fmt.Errorf("%w: invalid credentials json", ErrNotFound)
	}
	token := strings.TrimSpace(data.ClaudeAiOauth.AccessToken)
	if token == "" {
		return nil, ErrNotFound
	}
	if data.ClaudeAiOauth.ExpiresAt != nil && *data.ClaudeAiOauth.ExpiresAt <= now.UnixMilli() {
		return nil, ErrExpired
	}
	return &Creds{
		AccessToken:      token,
		SubscriptionType: strings.TrimSpace(data.ClaudeAiOauth.SubscriptionType),
	}, nil
}

// PlanName maps a Claude Code subscriptionType to a short display plan name.
// Returns empty string for API-key style subscriptions that have no usage windows.
func PlanName(subscriptionType string) string {
	lower := strings.ToLower(strings.TrimSpace(subscriptionType))
	switch {
	case lower == "":
		return ""
	case strings.Contains(lower, "max"):
		return "Max"
	case strings.Contains(lower, "pro"):
		return "Pro"
	case strings.Contains(lower, "team"):
		return "Team"
	case strings.Contains(lower, "api"):
		return ""
	default:
		// Capitalize first letter for unknown types.
		return strings.ToUpper(subscriptionType[:1]) + subscriptionType[1:]
	}
}
