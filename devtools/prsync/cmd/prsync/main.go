// Command prsync surveys open GitHub PRs and matches them to herdr agent tabs.
package main

import (
	"context"
	"io"
	"os"
	"os/signal"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/cli"
	executor "github.com/jaeyeom/go-cmdexec"
	"golang.org/x/sys/unix"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()
	return cli.Execute(ctx, args, stdout, stderr, executor.NewBasicExecutor())
}
