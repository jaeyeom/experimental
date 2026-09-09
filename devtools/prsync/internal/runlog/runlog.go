// Package runlog writes append-only JSON run logs under $XDG_STATE_HOME/prsync.
package runlog

import (
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"log/slog"
	"os"
	"path/filepath"
	"strings"
	"time"
)

const (
	defaultMaxSize    int64 = 10 << 20
	defaultMaxAge           = 14 * 24 * time.Hour
	defaultMaxBackups       = 5
	logFileName             = "prsync.jsonl"
)

type ctxKey struct{}

// Options configure Open.
type Options struct {
	Path       string
	Level      slog.Level
	MaxSize    int64
	MaxAge     time.Duration
	MaxBackups int
	Now        func() time.Time
}

// File is an open JSONL log and its slog logger.
type File struct {
	Logger *slog.Logger
	Path   string
	closer io.Closer
}

// Close closes the underlying file. A nil File or a discard File is a no-op.
func (f *File) Close() error {
	if f == nil || f.closer == nil {
		return nil
	}
	err := f.closer.Close()
	f.closer = nil
	if err != nil {
		return fmt.Errorf("close run log: %w", err)
	}
	return nil
}

// Dir returns $XDG_STATE_HOME/prsync, or ~/.local/state/prsync when unset.
func Dir() (string, error) {
	if v := os.Getenv("XDG_STATE_HOME"); v != "" {
		return filepath.Join(v, "prsync"), nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("home dir: %w", err)
	}
	return filepath.Join(home, ".local", "state", "prsync"), nil
}

// FilePath is Dir()/prsync.jsonl.
func FilePath() (string, error) {
	dir, err := Dir()
	if err != nil {
		return "", err
	}
	return filepath.Join(dir, logFileName), nil
}

// ParseLevel maps debug/info/warn/error (case-insensitive). Empty is info.
func ParseLevel(s string) (slog.Level, error) {
	switch strings.ToLower(strings.TrimSpace(s)) {
	case "", "info":
		return slog.LevelInfo, nil
	case "debug":
		return slog.LevelDebug, nil
	case "warn", "warning":
		return slog.LevelWarn, nil
	case "error":
		return slog.LevelError, nil
	default:
		return 0, fmt.Errorf("invalid log level %q", s)
	}
}

// WithLogger stores l on ctx for FromContext.
func WithLogger(ctx context.Context, l *slog.Logger) context.Context {
	return context.WithValue(ctx, ctxKey{}, l)
}

// FromContext returns the logger stored by WithLogger, or a discard logger.
func FromContext(ctx context.Context) *slog.Logger {
	if ctx != nil {
		if l, ok := ctx.Value(ctxKey{}).(*slog.Logger); ok && l != nil {
			return l
		}
	}
	return slog.New(slog.DiscardHandler)
}

// Discard returns a File that drops all records.
func Discard() *File {
	return &File{Logger: slog.New(slog.DiscardHandler)}
}

// OpenDefault opens FilePath at the given level, creating the state dir.
func OpenDefault(level slog.Level) (*File, error) {
	path, err := FilePath()
	if err != nil {
		return nil, err
	}
	return Open(Options{Path: path, Level: level})
}

// Open creates or appends a JSONL log at opts.Path, rotating by size or age.
func Open(opts Options) (*File, error) {
	if opts.Path == "" {
		return nil, fmt.Errorf("run log path is empty")
	}
	if opts.MaxSize <= 0 {
		opts.MaxSize = defaultMaxSize
	}
	if opts.MaxAge <= 0 {
		opts.MaxAge = defaultMaxAge
	}
	if opts.MaxBackups <= 0 {
		opts.MaxBackups = defaultMaxBackups
	}
	if opts.Now == nil {
		opts.Now = time.Now
	}
	if err := os.MkdirAll(filepath.Dir(opts.Path), 0o755); err != nil {
		return nil, fmt.Errorf("create run log dir: %w", err)
	}
	if err := rotate(opts); err != nil {
		return nil, err
	}
	f, err := os.OpenFile(opts.Path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o600) //nolint:gosec // operator-local state file
	if err != nil {
		return nil, fmt.Errorf("open run log: %w", err)
	}
	h := slog.NewJSONHandler(f, &slog.HandlerOptions{Level: opts.Level})
	return &File{Logger: slog.New(h), Path: opts.Path, closer: f}, nil
}

func rotate(opts Options) error {
	fi, err := os.Stat(opts.Path)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return nil
		}
		return fmt.Errorf("stat run log: %w", err)
	}
	tooBig := fi.Size() >= opts.MaxSize
	tooOld := opts.Now().Sub(fi.ModTime()) >= opts.MaxAge
	if !tooBig && !tooOld {
		return pruneBackups(opts)
	}
	for i := opts.MaxBackups; i >= 1; i-- {
		src := backupName(opts.Path, i)
		if i == opts.MaxBackups {
			_ = os.Remove(src)
			continue
		}
		if _, err := os.Stat(src); err != nil {
			continue
		}
		if err := os.Rename(src, backupName(opts.Path, i+1)); err != nil {
			return fmt.Errorf("rotate run log: %w", err)
		}
	}
	dst := backupName(opts.Path, 1)
	if err := os.Rename(opts.Path, dst); err != nil {
		return fmt.Errorf("rotate run log: %w", err)
	}
	now := opts.Now()
	if err := os.Chtimes(dst, now, now); err != nil {
		return fmt.Errorf("touch rotated run log: %w", err)
	}
	return pruneBackups(opts)
}

func pruneBackups(opts Options) error {
	for i := opts.MaxBackups + 1; i <= opts.MaxBackups+8; i++ {
		_ = os.Remove(backupName(opts.Path, i))
	}
	if opts.MaxAge <= 0 {
		return nil
	}
	cutoff := opts.Now().Add(-opts.MaxAge)
	for i := 1; i <= opts.MaxBackups; i++ {
		p := backupName(opts.Path, i)
		fi, err := os.Stat(p)
		if err != nil {
			continue
		}
		if fi.ModTime().Before(cutoff) {
			_ = os.Remove(p)
		}
	}
	return nil
}

func backupName(path string, n int) string {
	return fmt.Sprintf("%s.%d", path, n)
}
