// Command factory is the agentic-factory command line.
package main

import (
	"context"
	"os"

	"github.com/jaeyeom/experimental/devtools/factory/internal/cli"
)

func main() {
	os.Exit(cli.Execute(context.Background(), os.Args[1:], os.Stdout, os.Stderr))
}
