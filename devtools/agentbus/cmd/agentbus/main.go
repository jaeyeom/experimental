// Command agentbus is the client for the local agentbus daemon.
package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/client"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/paths"
)

func main() {
	os.Exit(run(os.Args[1:], os.Environ(), os.Stderr))
}

func run(args, env []string, stderr io.Writer) int {
	if len(args) > 0 {
		fmt.Fprintf(stderr, "agentbus: commands are not available yet\n")
		return 2
	}
	dirs, err := paths.FromEnv(env)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
		return 1
	}
	exe, err := os.Executable()
	if err != nil {
		fmt.Fprintf(stderr, "agentbus: cannot find this executable: %s\n", err)
		return 1
	}
	bin, err := client.DaemonBinBeside(exe)
	if err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
		return 1
	}
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	if err := client.Ensure(ctx, dirs, bin, env); err != nil {
		fmt.Fprintf(stderr, "agentbus: %s\n", err)
		return 1
	}
	fmt.Fprintf(stderr, "agentbusd ready at %s\n", dirs.SocketPath)
	return 0
}
