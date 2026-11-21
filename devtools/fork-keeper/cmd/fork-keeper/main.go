// Command fork-keeper rebases configured personal forks on upstream and opens
// review pull requests.
package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"strings"

	forkkeeper "github.com/jaeyeom/experimental/devtools/fork-keeper"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	ctx := context.Background()
	if code, handled := newForkKeeperTool().Handle(ctx, args, stdout, stderr); handled {
		return code
	}
	for _, arg := range args {
		if arg == "-h" || arg == "--help" || arg == "help" {
			usage(stdout)
			return 0
		}
		if strings.HasPrefix(arg, "-") {
			_, _ = fmt.Fprintf(stderr, "unknown flag: %s\n", arg)
			usage(stderr)
			return 2
		}
	}
	if err := forkkeeper.Run(ctx, forkkeeper.Options{
		ForkNames: args,
		Stdout:    stdout,
		Stderr:    stderr,
	}); err != nil {
		return 1
	}
	return 0
}

func usage(w io.Writer) {
	_, _ = fmt.Fprintln(w, `Usage:
  fork-keeper [<fork-name>...]
  fork-keeper version [--json]
  fork-keeper update [--check] [--ref <ref>] [--json]
  fork-keeper doctor [--offline] [--json]
  fork-keeper skill [install|uninstall|status ...]`)
}
