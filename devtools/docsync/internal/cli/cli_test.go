package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/docsync/internal/version"
)

func TestVersionJSON(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"version"}, &stdout, &stderr, nil)
	if code != ExitOK {
		t.Fatalf("exit = %d, stderr=%q", code, stderr.String())
	}
	if stderr.Len() != 0 {
		t.Fatalf("stderr = %q", stderr.String())
	}
	var got map[string]string
	if err := json.Unmarshal(stdout.Bytes(), &got); err != nil {
		t.Fatalf("json: %v\n%s", err, stdout.String())
	}
	if got["version"] != version.Version {
		t.Fatalf("version = %q, want %q", got["version"], version.Version)
	}
}

func TestUnknownCommand(t *testing.T) {
	t.Parallel()

	var stdout, stderr bytes.Buffer
	code := Execute(context.Background(), []string{"not-a-command"}, &stdout, &stderr, nil)
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
	}
	if !strings.Contains(stderr.String(), "unknown command") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestReportExitError(t *testing.T) {
	t.Parallel()

	var stderr bytes.Buffer
	code := report(&stderr, &ExitError{Code: ExitPrecondition, Err: errors.New("git missing")})
	if code != ExitPrecondition {
		t.Fatalf("exit = %d, want %d", code, ExitPrecondition)
	}
	if got := stderr.String(); got != "docsync: git missing\n" {
		t.Fatalf("stderr = %q", got)
	}
}

func TestReportNil(t *testing.T) {
	t.Parallel()
	if code := report(&bytes.Buffer{}, nil); code != ExitOK {
		t.Fatalf("exit = %d, want 0", code)
	}
}
