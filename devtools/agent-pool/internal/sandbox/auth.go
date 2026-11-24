package sandbox

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

type credentialsFile struct {
	ClaudeAiOauth struct {
		AccessToken string `json:"accessToken"`
	} `json:"claudeAiOauth"`
}

// ReadIsolatedOAuthToken reads claudeAiOauth.accessToken from
// <claudeConfigDir>/.credentials.json. It returns ("", nil) when the file does
// not exist so callers can handle missing auth cleanly.
func ReadIsolatedOAuthToken(claudeConfigDir string) (string, error) {
	path := filepath.Join(claudeConfigDir, ".credentials.json")
	raw, err := os.ReadFile(path)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return "", nil
		}
		return "", fmt.Errorf("read isolated credentials file: %w", err)
	}

	var data credentialsFile
	if err := json.Unmarshal(raw, &data); err != nil {
		return "", errors.New("parse isolated credentials file: invalid json")
	}
	return strings.TrimSpace(data.ClaudeAiOauth.AccessToken), nil
}
