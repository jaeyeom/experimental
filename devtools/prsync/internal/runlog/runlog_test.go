package runlog

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io"
	"io/fs"
	"log/slog"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestDirUsesXDGStateHome(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", "/tmp/xdg-state-home")

	got, err := Dir()
	if err != nil {
		t.Fatalf("Dir() error = %v", err)
	}
	want := filepath.Join("/tmp/xdg-state-home", "prsync")
	if got != want {
		t.Fatalf("Dir() = %q, want %q", got, want)
	}
}

func TestDirFallsBackToLocalState(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", "")
	t.Setenv("HOME", "/home/tester")

	got, err := Dir()
	if err != nil {
		t.Fatalf("Dir() error = %v", err)
	}
	want := filepath.Join("/home/tester", ".local", "state", "prsync")
	if got != want {
		t.Fatalf("Dir() = %q, want %q", got, want)
	}
}

func TestFilePathIsJSONLUnderDir(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", "/tmp/xdg-state-home")

	got, err := FilePath()
	if err != nil {
		t.Fatalf("FilePath() error = %v", err)
	}
	want := filepath.Join("/tmp/xdg-state-home", "prsync", "prsync.jsonl")
	if got != want {
		t.Fatalf("FilePath() = %q, want %q", got, want)
	}
}

func TestOpenAppendsJSONLinesAcrossRuns(t *testing.T) {
	path := filepath.Join(t.TempDir(), "prsync.jsonl")

	f, err := Open(Options{Path: path, Level: slog.LevelInfo})
	if err != nil {
		t.Fatalf("Open() error = %v", err)
	}
	f.Logger.Info("first", "n", 1)
	if err := f.Close(); err != nil {
		t.Fatalf("Close() error = %v", err)
	}

	f, err = Open(Options{Path: path, Level: slog.LevelInfo})
	if err != nil {
		t.Fatalf("second Open() error = %v", err)
	}
	f.Logger.Info("second", "n", 2)
	if err := f.Close(); err != nil {
		t.Fatalf("second Close() error = %v", err)
	}

	recs := readRecords(t, path)
	if len(recs) != 2 {
		t.Fatalf("records = %d, want 2: %v", len(recs), recs)
	}
	if recs[0]["msg"] != "first" || recs[1]["msg"] != "second" {
		t.Fatalf("msgs = %v, want first then second", recs)
	}
}

func TestOpenHonorsLevel(t *testing.T) {
	path := filepath.Join(t.TempDir(), "prsync.jsonl")
	f, err := Open(Options{Path: path, Level: slog.LevelInfo})
	if err != nil {
		t.Fatalf("Open() error = %v", err)
	}
	f.Logger.Debug("hidden")
	f.Logger.Info("visible")
	if err := f.Close(); err != nil {
		t.Fatalf("Close() error = %v", err)
	}
	recs := readRecords(t, path)
	if len(recs) != 1 || recs[0]["msg"] != "visible" {
		t.Fatalf("records = %v, want only visible info line", recs)
	}
}

func TestOpenRotatesWhenTooLarge(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "prsync.jsonl")
	if err := os.WriteFile(path, bytes.Repeat([]byte("x"), 64), 0o600); err != nil {
		t.Fatal(err)
	}

	f, err := Open(Options{Path: path, Level: slog.LevelInfo, MaxSize: 32, MaxBackups: 2})
	if err != nil {
		t.Fatalf("Open() error = %v", err)
	}
	f.Logger.Info("after-rotate")
	if err := f.Close(); err != nil {
		t.Fatalf("Close() error = %v", err)
	}

	if _, err := os.Stat(path + ".1"); err != nil {
		t.Fatalf("rotated backup missing: %v", err)
	}
	recs := readRecords(t, path)
	if len(recs) != 1 || recs[0]["msg"] != "after-rotate" {
		t.Fatalf("active log = %v, want only the post-rotate line", recs)
	}
}

func TestOpenRotatesWhenTooOld(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "prsync.jsonl")
	if err := os.WriteFile(path, []byte("{}\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	old := time.Now().Add(-48 * time.Hour)
	if err := os.Chtimes(path, old, old); err != nil {
		t.Fatal(err)
	}

	f, err := Open(Options{
		Path:       path,
		Level:      slog.LevelInfo,
		MaxSize:    10 << 20,
		MaxAge:     24 * time.Hour,
		MaxBackups: 2,
		Now:        time.Now,
	})
	if err != nil {
		t.Fatalf("Open() error = %v", err)
	}
	if err := f.Close(); err != nil {
		t.Fatalf("Close() error = %v", err)
	}
	if _, err := os.Stat(path + ".1"); err != nil {
		t.Fatalf("age-rotated backup missing: %v", err)
	}
}

func TestOpenDropsBackupsBeyondMax(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "prsync.jsonl")
	mustWrite(t, path, bytes.Repeat([]byte("a"), 64))
	mustWrite(t, path+".1", []byte("one\n"))
	mustWrite(t, path+".2", []byte("two\n"))

	f, err := Open(Options{Path: path, Level: slog.LevelInfo, MaxSize: 32, MaxBackups: 2})
	if err != nil {
		t.Fatalf("Open() error = %v", err)
	}
	if err := f.Close(); err != nil {
		t.Fatalf("Close() error = %v", err)
	}
	if _, err := os.Stat(path + ".3"); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("backup .3 exists, want dropped beyond MaxBackups=2: %v", err)
	}
	if _, err := os.Stat(path + ".2"); err != nil {
		t.Fatalf("backup .2 missing: %v", err)
	}
}

func TestParseLevel(t *testing.T) {
	t.Parallel()

	tests := []struct {
		in      string
		want    slog.Level
		wantErr bool
	}{
		{in: "debug", want: slog.LevelDebug},
		{in: "INFO", want: slog.LevelInfo},
		{in: "warn", want: slog.LevelWarn},
		{in: "warning", want: slog.LevelWarn},
		{in: "error", want: slog.LevelError},
		{in: "", want: slog.LevelInfo},
		{in: "loud", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.in, func(t *testing.T) {
			t.Parallel()
			got, err := ParseLevel(tc.in)
			if tc.wantErr {
				if err == nil {
					t.Fatal("error = nil, want invalid level")
				}
				return
			}
			if err != nil {
				t.Fatalf("ParseLevel(%q) error = %v", tc.in, err)
			}
			if got != tc.want {
				t.Fatalf("ParseLevel(%q) = %v, want %v", tc.in, got, tc.want)
			}
		})
	}
}

func TestFromContextDiscardWhenMissing(t *testing.T) {
	t.Parallel()

	log := FromContext(context.Background())
	if log == nil {
		t.Fatal("FromContext() = nil, want discard logger")
	}
	log.Info("must not panic")
}

func TestWithLoggerRoundTrip(t *testing.T) {
	t.Parallel()

	var buf bytes.Buffer
	want := slog.New(slog.NewJSONHandler(&buf, nil))
	ctx := WithLogger(context.Background(), want)
	got := FromContext(ctx)
	got.Info("hello")
	if !strings.Contains(buf.String(), `"msg":"hello"`) {
		t.Fatalf("log output = %q, want hello record", buf.String())
	}
}

func TestOpenCreatesParentDir(t *testing.T) {
	path := filepath.Join(t.TempDir(), "nested", "state", "prsync.jsonl")
	f, err := Open(Options{Path: path, Level: slog.LevelInfo})
	if err != nil {
		t.Fatalf("Open() error = %v", err)
	}
	if err := f.Close(); err != nil {
		t.Fatalf("Close() error = %v", err)
	}
	if _, err := os.Stat(filepath.Dir(path)); err != nil {
		t.Fatalf("parent dir missing: %v", err)
	}
}

func TestDiscardLoggerWritesNothing(t *testing.T) {
	t.Parallel()

	f := Discard()
	f.Logger.Info("nope")
	if err := f.Close(); err != nil {
		t.Fatalf("Close() error = %v", err)
	}
}

func readRecords(t *testing.T, path string) []map[string]any {
	t.Helper()
	data, err := os.ReadFile(path) //nolint:gosec // testdata path
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	var recs []map[string]any
	dec := json.NewDecoder(bytes.NewReader(data))
	for {
		var rec map[string]any
		if err := dec.Decode(&rec); err != nil {
			if err == io.EOF {
				break
			}
			t.Fatalf("decode %s: %v\n%s", path, err, data)
		}
		recs = append(recs, rec)
	}
	return recs
}

func mustWrite(t *testing.T, path string, data []byte) {
	t.Helper()
	if err := os.WriteFile(path, data, 0o600); err != nil {
		t.Fatal(err)
	}
}
