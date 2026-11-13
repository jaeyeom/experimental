package main

import (
	"bytes"
	"context"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/diskwatch"
)

func TestCLIDryRunDoesNotRunAction(t *testing.T) {
	home := t.TempDir()
	configPath := writeConfig(t, home)
	var calls int
	rt := runtime{
		stat: func(string) (diskwatch.Usage, error) {
			return diskwatch.Usage{Free: 100, Total: 1000}, nil
		},
		run: func(context.Context, []string, []string) error {
			calls++
			return nil
		},
		now:      func() time.Time { return time.Date(2026, 11, 13, 0, 0, 0, 0, time.UTC) },
		hostname: "host",
	}
	var stdout, stderr bytes.Buffer
	code := run([]string{"check", "--dry-run", "--config", configPath}, &stdout, &stderr, []string{"HOME=" + home}, rt)
	if code != 0 || calls != 0 || !strings.Contains(stdout.String(), "dry-run / low") {
		t.Fatalf("code %d calls %d stdout %q stderr %q", code, calls, stdout.String(), stderr.String())
	}
	paths, err := diskwatch.LocatePaths([]string{"HOME=" + home})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(paths.StateFile); err == nil {
		t.Fatal("dry-run wrote state")
	}
}

func TestCLIHookRateLimit(t *testing.T) {
	home := t.TempDir()
	configPath := writeConfig(t, home)
	now := time.Date(2026, 11, 13, 0, 0, 0, 0, time.UTC)
	var calls int
	rt := runtime{
		stat: func(string) (diskwatch.Usage, error) {
			return diskwatch.Usage{Free: 800, Total: 1000}, nil
		},
		run:      func(context.Context, []string, []string) error { calls++; return nil },
		now:      func() time.Time { return now },
		hostname: "host",
	}
	env := []string{"HOME=" + home}
	var stdout, stderr bytes.Buffer
	if code := run([]string{"hook", "--config", configPath}, &stdout, &stderr, env, rt); code != 0 {
		t.Fatal(stderr.String())
	}
	now = now.Add(30 * time.Second)
	rt.stat = func(string) (diskwatch.Usage, error) {
		return diskwatch.Usage{Free: 100, Total: 1000}, nil
	}
	stdout.Reset()
	if code := run([]string{"hook", "--config", configPath}, &stdout, &stderr, env, rt); code != 0 || !strings.Contains(stdout.String(), "hook skipped") || calls != 0 {
		t.Fatalf("code %d calls %d stdout %q stderr %q", code, calls, stdout.String(), stderr.String())
	}
}

func TestCLIInvalidConfig(t *testing.T) {
	home := t.TempDir()
	path := filepath.Join(home, "bad.toml")
	body := "on_low = []\n[[mounts]]\npath = \"/\"\nlow = \"40%\"\nhigh = \"10%\"\n"
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	var stderr bytes.Buffer
	code := run([]string{"check", "--config", path}, io.Discard, &stderr, []string{"HOME=" + home}, runtime{now: time.Now})
	if code != 2 || !strings.Contains(stderr.String(), "on_low is empty") {
		t.Fatalf("code %d stderr %q", code, stderr.String())
	}
}

func TestCLIStatusJSON(t *testing.T) {
	home := t.TempDir()
	configPath := writeConfig(t, home)
	rt := runtime{
		stat: func(string) (diskwatch.Usage, error) {
			return diskwatch.Usage{Free: 100, Total: 1000}, nil
		},
		now:      func() time.Time { return time.Date(2026, 11, 13, 0, 0, 0, 0, time.UTC) },
		hostname: "host",
	}
	var stdout, stderr bytes.Buffer
	code := run([]string{"status", "--json", "--config", configPath}, &stdout, &stderr, []string{"HOME=" + home}, rt)
	if code != 0 || !strings.Contains(stdout.String(), `"freeBytes": 100`) || !strings.Contains(stdout.String(), `"armed": true`) {
		t.Fatalf("code %d stdout %q stderr %q", code, stdout.String(), stderr.String())
	}
}

func writeConfig(t *testing.T, home string) string {
	t.Helper()
	path := filepath.Join(home, "config.toml")
	body := "on_low = [\"/bin/true\"]\n[[mounts]]\npath = \"/\"\nlow = \"15%\"\nhigh = \"25%\"\nurgent = \"5%\"\n"
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return path
}
