package authwatch

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestReadmeCoversContract(t *testing.T) {
	doc := readReadme(t)
	required := []string{
		"metadata only",
		"Linux",
		"macOS",
		"security find-generic-password -w",
		"config.toml",
		"state.json",
		"unknown-format",
		"report-failure",
		"use_cli_status",
		"AUTHWATCH_PROFILE",
		"logged-out",
	}
	for _, want := range required {
		if !strings.Contains(doc, want) {
			t.Errorf("README.org missing %q", want)
		}
	}
}

func readReadme(t *testing.T) string {
	t.Helper()
	var candidates []string
	if _, file, _, ok := runtime.Caller(0); ok {
		candidates = append(candidates, filepath.Join(filepath.Dir(file), "README.org"))
	}
	if src := os.Getenv("TEST_SRCDIR"); src != "" {
		ws := os.Getenv("TEST_WORKSPACE")
		if ws == "" {
			ws = "_main"
		}
		candidates = append(candidates, filepath.Join(src, ws, "devtools", "authwatch", "README.org"))
	}
	for _, p := range candidates {
		body, err := os.ReadFile(p)
		if err == nil {
			return string(body)
		}
	}
	t.Fatal("README.org not found")
	return ""
}
