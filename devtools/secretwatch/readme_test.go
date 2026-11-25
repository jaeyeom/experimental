package secretwatch

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
		"github-token",
		"aws-access-key",
		"gcp-api-key",
		"gcp-service-account-key",
		"slack-token",
		"pem-private-key",
		"auth-header",
		"generic-secret-assignment",
		"config.pkl",
		"state.json",
		"secretwatch check",
		"secretwatch scan",
		"--follow",
		"--since",
		"--until",
		"--unit",
		"--json",
		"SECRETWATCH_PREFIX",
		"contrib/secretwatch.timer",
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
		candidates = append(candidates, filepath.Join(src, ws, "devtools", "secretwatch", "README.org"))
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
