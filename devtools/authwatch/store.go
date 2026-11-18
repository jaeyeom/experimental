package authwatch

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

// ErrStoreNotFound indicates no credentials exist for the profile.
var ErrStoreNotFound = errors.New("credentials not found")

// Expiries holds only the parsed token expiration timestamps.
// Token values are never stored.
type Expiries struct {
	AccessExpiresAt  time.Time
	RefreshExpiresAt time.Time
}

// FormatError reports a missing or mistyped field in the credentials store
// without ever including any field value.
type FormatError struct {
	Field    string
	Expected string
}

func (e *FormatError) Error() string {
	return fmt.Sprintf("unknown format: field %q missing or wrong type (expected %s)", e.Field, e.Expected)
}

// ParseCredentialsMetadata extracts only access and refresh expiry timestamps
// from raw credentials JSON, dropping all other fields (including token values).
func ParseCredentialsMetadata(data []byte) (Expiries, error) {
	dec := json.NewDecoder(bytes.NewReader(data))
	dec.UseNumber()
	var rootAny any
	if err := dec.Decode(&rootAny); err != nil {
		return Expiries{}, &FormatError{Field: "<root>", Expected: "valid JSON object"}
	}
	root, ok := rootAny.(map[string]any)
	if !ok {
		return Expiries{}, &FormatError{Field: "<root>", Expected: "object"}
	}
	oauthAny, exists := root["claudeAiOauth"]
	if !exists {
		return Expiries{}, &FormatError{Field: "claudeAiOauth", Expected: "object"}
	}
	oauth, ok := oauthAny.(map[string]any)
	if !ok {
		return Expiries{}, &FormatError{Field: "claudeAiOauth", Expected: "object"}
	}
	accessMs, err := parseEpochMsField(oauth, "claudeAiOauth.expiresAt")
	if err != nil {
		return Expiries{}, err
	}
	refreshMs, err := parseEpochMsField(oauth, "claudeAiOauth.refreshTokenExpiresAt")
	if err != nil {
		return Expiries{}, err
	}
	return Expiries{
		AccessExpiresAt:  time.UnixMilli(accessMs).UTC(),
		RefreshExpiresAt: time.UnixMilli(refreshMs).UTC(),
	}, nil
}

func parseEpochMsField(obj map[string]any, fullName string) (int64, error) {
	key := fullName
	if idx := strings.LastIndex(fullName, "."); idx >= 0 {
		key = fullName[idx+1:]
	}
	raw, exists := obj[key]
	if !exists {
		return 0, &FormatError{Field: fullName, Expected: "number (epoch ms)"}
	}
	num, ok := raw.(json.Number)
	if !ok {
		return 0, &FormatError{Field: fullName, Expected: "number (epoch ms)"}
	}
	if i, err := num.Int64(); err == nil && i > 0 {
		return i, nil
	}
	f, err := num.Float64()
	if err != nil || math.IsNaN(f) || math.IsInf(f, 0) || f <= 0 {
		return 0, &FormatError{Field: fullName, Expected: "positive number (epoch ms)"}
	}
	return int64(f), nil
}

// CmdOutputRunner executes a command and returns its stdout, exit code, and any error.
type CmdOutputRunner func(ctx context.Context, name string, args []string, env []string) ([]byte, int, error)

// ExecOutput is the default CmdOutputRunner using os/exec.
func ExecOutput(ctx context.Context, name string, args, env []string) ([]byte, int, error) {
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, name, args...)
	if len(env) > 0 {
		cmd.Env = env
	}
	out, err := cmd.Output()
	if err == nil {
		return out, 0, nil
	}
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		return out, exitErr.ExitCode(), fmt.Errorf("run %s: %w", name, err)
	}
	return out, -1, fmt.Errorf("run %s: %w", name, err)
}

// StoreReader reads raw credentials bytes for a profile.
type StoreReader func(ctx context.Context, profile ProfileConfig, env []string) ([]byte, error)

// DefaultStoreReader reads from the macOS keychain on darwin or the Linux
// credentials file otherwise.
func DefaultStoreReader(goos string, runCmd CmdOutputRunner) StoreReader {
	if runCmd == nil {
		runCmd = ExecOutput
	}
	return func(ctx context.Context, profile ProfileConfig, env []string) ([]byte, error) {
		if goos == "darwin" {
			return readKeychainStore(ctx, profile, env, runCmd)
		}
		return readFileStore(profile, env)
	}
}

func readFileStore(profile ProfileConfig, env []string) ([]byte, error) {
	path := profile.CredentialsFile
	if path == "" {
		dir, err := ResolveConfigDir(profile.ConfigDir, env)
		if err != nil {
			return nil, err
		}
		path = filepath.Join(dir, DefaultCredentialsFileName)
	}
	data, err := os.ReadFile(path) //nolint:gosec // G304: profile credentials file path
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return nil, ErrStoreNotFound
		}
		return nil, fmt.Errorf("read credentials file: %w", err)
	}
	return data, nil
}

func readKeychainStore(ctx context.Context, profile ProfileConfig, env []string, runCmd CmdOutputRunner) ([]byte, error) {
	service := profile.KeychainService
	if service == "" {
		m := envValues(env)
		home := m["HOME"]
		if home == "" {
			return nil, fmt.Errorf("HOME is not set")
		}
		dir, err := ResolveConfigDir(profile.ConfigDir, env)
		if err != nil {
			return nil, err
		}
		service = KeychainServiceName(dir, home)
	}
	out, code, err := runCmd(ctx, "security", []string{"find-generic-password", "-w", "-s", service}, env)
	if err != nil {
		// Exit code 44 is errSecItemNotFound in macOS security(1).
		if code == 44 {
			return nil, ErrStoreNotFound
		}
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) && strings.Contains(string(exitErr.Stderr), "could not be found") {
			return nil, ErrStoreNotFound
		}
		return nil, fmt.Errorf("read keychain item %q: %w", service, err)
	}
	return bytes.TrimSpace(out), nil
}
