package ghask

import (
	"crypto/rand"
	"encoding/hex"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

// TokenBytes is the size of the random secret before hex encoding.
const TokenBytes = 32

// EnsureToken returns the token in path, creating it with mode 0600 when
// the file does not exist.
func EnsureToken(path string) (string, error) {
	token, err := ReadToken(path)
	if err == nil {
		return token, nil
	}
	if !errors.Is(err, fs.ErrNotExist) {
		return "", err
	}
	raw := make([]byte, TokenBytes)
	if _, err := rand.Read(raw); err != nil {
		return "", fmt.Errorf("generate token: %w", err)
	}
	token = hex.EncodeToString(raw)
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return "", fmt.Errorf("create token dir: %w", err)
	}
	// O_EXCL keeps a concurrent caller from overwriting a token that was
	// just created.
	f, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_EXCL, 0o600) //nolint:gosec // G304: the user's own token path
	if err != nil {
		return "", fmt.Errorf("create token: %w", err)
	}
	if _, err := f.WriteString(token + "\n"); err != nil {
		_ = f.Close()
		return "", fmt.Errorf("write token: %w", err)
	}
	if err := f.Close(); err != nil {
		return "", fmt.Errorf("write token: %w", err)
	}
	return token, nil
}

// ReadToken reads the token in path. It refuses an empty file and a file
// that group or others can access.
func ReadToken(path string) (string, error) {
	info, err := os.Stat(path)
	if err != nil {
		return "", fmt.Errorf("token: %w", err)
	}
	if perm := info.Mode().Perm(); perm&0o077 != 0 {
		return "", fmt.Errorf("token %s has mode %o; run chmod 600 on it", path, perm)
	}
	data, err := os.ReadFile(path) //nolint:gosec // G304: the user's own token path
	if err != nil {
		return "", fmt.Errorf("token: %w", err)
	}
	token := strings.TrimSpace(string(data))
	if token == "" {
		return "", fmt.Errorf("token %s is empty", path)
	}
	return token, nil
}
