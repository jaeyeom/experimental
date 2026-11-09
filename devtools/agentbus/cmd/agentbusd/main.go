// Command agentbusd is the local agentbus daemon.
package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/signal"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/herdr"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/daemon"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
	"golang.org/x/sys/unix"
)

func main() {
	os.Exit(run(os.Environ(), os.Stderr))
}

func run(env []string, stderr io.Writer) int {
	dirs, err := paths.FromEnv(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbusd: %s\n", err)
		return 1
	}
	days, err := retentionDays(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbusd: %s\n", err)
		return 1
	}
	inline, err := inlineBytes(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbusd: %s\n", err)
		return 1
	}
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()
	if err := daemon.Serve(ctx, dirs, daemon.Config{
		RetentionDays: days,
		Adapters:      []adapter.Adapter{&herdr.Adapter{}},
		InlineBytes:   inline,
	}); err != nil && !errors.Is(err, context.Canceled) {
		fmt.Fprintf(stderr, "agentbusd: %s\n", err)
		return 1
	}
	return 0
}

// retentionDays reads AGENTBUS_LOG_RETENTION_DAYS. Unset means the default.
func retentionDays(env []string) (int, error) {
	days := 0
	for _, entry := range env {
		key, val, ok := strings.Cut(entry, "=")
		if !ok || key != "AGENTBUS_LOG_RETENTION_DAYS" {
			continue
		}
		if val == "" {
			days = 0
			continue
		}
		n, err := strconv.Atoi(val)
		if err != nil || n <= 0 {
			return 0, errors.New("AGENTBUS_LOG_RETENTION_DAYS must be a positive number of days")
		}
		days = n
	}
	return days, nil
}

// inlineBytes reads AGENTBUS_INLINE_BYTES. Unset means the default.
func inlineBytes(env []string) (int, error) {
	for _, entry := range env {
		key, val, ok := strings.Cut(entry, "=")
		if !ok || key != "AGENTBUS_INLINE_BYTES" {
			continue
		}
		if val == "" {
			return 0, nil
		}
		n, err := strconv.Atoi(val)
		if err != nil || n <= 0 {
			return 0, errors.New("AGENTBUS_INLINE_BYTES must be a positive number of bytes")
		}
		return n, nil
	}
	return 0, nil
}
