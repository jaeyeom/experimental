package cli

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/version"
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

func TestNoCommandsBeyondVersion(t *testing.T) {
	t.Parallel()

	for _, name := range []string{"scan", "dispatch", "gate"} {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			var stdout, stderr bytes.Buffer
			code := Execute(context.Background(), []string{name}, &stdout, &stderr, nil)
			if code != ExitUsage {
				t.Fatalf("exit = %d, want %d, stderr=%q", code, ExitUsage, stderr.String())
			}
			if !strings.Contains(stderr.String(), "unknown command") {
				t.Fatalf("stderr = %q", stderr.String())
			}
		})
	}
}

func TestReportKeyError(t *testing.T) {
	t.Parallel()

	var stderr bytes.Buffer
	err := &config.KeyError{Key: "title_id_pattern", Reason: "error parsing regexp: missing closing ]: `[A-Z+`"}
	code := report(&stderr, err)
	if code != ExitUsage {
		t.Fatalf("exit = %d, want %d", code, ExitUsage)
	}
	want := "prsync: config error: key title_id_pattern: error parsing regexp: missing closing ]: `[A-Z+`\n"
	if got := stderr.String(); got != want {
		t.Fatalf("stderr = %q, want %q", got, want)
	}
}

func TestReportExitError(t *testing.T) {
	t.Parallel()

	var stderr bytes.Buffer
	code := report(&stderr, &ExitError{Code: ExitPrecondition, Err: errors.New("gh missing")})
	if code != ExitPrecondition {
		t.Fatalf("exit = %d, want %d", code, ExitPrecondition)
	}
	if !strings.Contains(stderr.String(), "gh missing") {
		t.Fatalf("stderr = %q", stderr.String())
	}
}

func TestReportNil(t *testing.T) {
	t.Parallel()
	if code := report(&bytes.Buffer{}, nil); code != ExitOK {
		t.Fatalf("exit = %d, want 0", code)
	}
}
