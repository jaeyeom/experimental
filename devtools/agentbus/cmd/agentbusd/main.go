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

const usage = `Usage: agentbusd

agentbusd is the local agentbus daemon. agentbus starts it on demand, and
'agentbus install' runs it under launchd or a systemd user unit.

Flags:
  -h, --help  Print this help.

Environment:
  XDG_RUNTIME_DIR              required; the socket is $XDG_RUNTIME_DIR/agentbus/daemon.sock
  XDG_STATE_HOME               state directory parent (default: $HOME/.local/state)
  HOME                         required when XDG_STATE_HOME is unset
  AGENTBUS_LOG_RETENTION_DAYS  daily log files to keep (default: 30)
  AGENTBUS_INLINE_BYTES        largest body pasted in full (default: 4096)

Example:
  agentbusd
`

func main() {
	os.Exit(run(os.Args[1:], os.Environ(), os.Stdout, os.Stderr))
}

func run(args, env []string, stdout, stderr io.Writer) int {
	if len(args) > 0 {
		if args[0] == "-h" || args[0] == "--help" {
			fmt.Fprint(stdout, usage)
			return 0
		}
		fmt.Fprintf(stderr, "agentbusd: unexpected argument %q\nrun 'agentbusd --help' for usage\n", args[0])
		return 2
	}
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
