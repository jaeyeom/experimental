package ghask

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
)

// threadEntry is the thread stored for one PR.
type threadEntry struct {
	ID string `json:"id"`
	// Closed is the last closed value status_command reported for ID.
	Closed bool `json:"closed"`
}

func stateKey(repo string, pr int) string {
	return repo + "#" + strconv.Itoa(pr)
}

// loadState reads the state file. A missing file is an empty state.
func loadState(path string) (map[string]threadEntry, error) {
	data, err := os.ReadFile(path) //nolint:gosec // G304: the server's own state path
	if errors.Is(err, fs.ErrNotExist) {
		return map[string]threadEntry{}, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read state: %w", err)
	}
	threads := map[string]threadEntry{}
	if err := json.Unmarshal(data, &threads); err != nil {
		return nil, fmt.Errorf("state %s: %w", path, err)
	}
	return threads, nil
}

// saveState writes threads to a mode-0600 temp file next to path and
// renames it over path, so a crash never leaves a half-written file.
func saveState(path string, threads map[string]threadEntry) error {
	data, err := json.MarshalIndent(threads, "", "  ")
	if err != nil {
		return fmt.Errorf("encode state: %w", err)
	}
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return fmt.Errorf("create state dir: %w", err)
	}
	f, err := os.CreateTemp(dir, ".threads-*.json")
	if err != nil {
		return fmt.Errorf("create state: %w", err)
	}
	tmp := f.Name()
	fail := func(err error) error {
		_ = f.Close()
		_ = os.Remove(tmp)
		return fmt.Errorf("write state: %w", err)
	}
	if err := f.Chmod(0o600); err != nil {
		return fail(err)
	}
	if _, err := f.Write(append(data, '\n')); err != nil {
		return fail(err)
	}
	if err := f.Close(); err != nil {
		_ = os.Remove(tmp)
		return fmt.Errorf("write state: %w", err)
	}
	if err := os.Rename(tmp, path); err != nil {
		_ = os.Remove(tmp)
		return fmt.Errorf("write state: %w", err)
	}
	return nil
}

// DefaultStatePath is $XDG_STATE_HOME/gh-ask/threads.json, or
// $HOME/.local/state/gh-ask/threads.json.
func DefaultStatePath(env []string) (string, error) {
	root := lookupEnv(env, "XDG_STATE_HOME")
	if root == "" {
		home := lookupEnv(env, "HOME")
		if home == "" {
			return "", errors.New("HOME is not set")
		}
		root = filepath.Join(home, ".local", "state")
	}
	return filepath.Join(root, "gh-ask", "threads.json"), nil
}
