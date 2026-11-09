// Command agentbusd is the local agentbus daemon.
package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/signal"

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
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()
	if err := daemon.Serve(ctx, dirs); err != nil && !errors.Is(err, context.Canceled) {
		fmt.Fprintf(stderr, "agentbusd: %s\n", err)
		return 1
	}
	return 0
}
