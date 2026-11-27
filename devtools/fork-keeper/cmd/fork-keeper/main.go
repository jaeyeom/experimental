// Command fork-keeper rebases configured personal forks on upstream and opens
// review pull requests.
package main

import (
	"context"
	"errors"
	"flag"
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

	var opts forkkeeper.Options
	var reviewerCmdRaw string
	fs := flag.NewFlagSet("fork-keeper", flag.ContinueOnError)
	fs.SetOutput(stderr)
	fs.Usage = func() { usage(stderr) }
	fs.StringVar(&opts.ConfigPath, "config", "", "path to forks.json (default FORK_KEEPER_CONFIG or ~/.config/fork-keeper/forks.json)")
	fs.StringVar(&opts.CacheDir, "cache-dir", "", "cache directory (default FORK_KEEPER_CACHE or ~/.cache/fork-keeper)")
	fs.StringVar(&opts.GHBin, "gh-bin", "", "GitHub CLI executable or wrapper (default gh_bin config, GH_BIN, or gh)")
	fs.StringVar(&reviewerCmdRaw, "reviewer-cmd", "", "external reviewer command (parsed without a shell)")
	fs.StringVar(&opts.Sandbox, "sandbox", "", "agent confinement mode: auto, seatbelt, bwrap, or none")
	fs.StringVar(&opts.Today, "date", "", "UTC date YYYY-MM-DD for sync branch and pre-sync tags")

	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			usage(stdout)
			return 0
		}
		return 2
	}

	explicit := make(map[string]bool)
	fs.Visit(func(f *flag.Flag) {
		explicit[f.Name] = true
	})

	if explicit["gh-bin"] && strings.TrimSpace(opts.GHBin) == "" {
		_, _ = fmt.Fprintln(stderr, "error: --gh-bin must not be empty")
		return 2
	}
	if explicit["reviewer-cmd"] {
		argv, err := forkkeeper.SplitCommand(reviewerCmdRaw)
		if err != nil {
			_, _ = fmt.Fprintf(stderr, "error: invalid --reviewer-cmd: %v\n", err)
			return 2
		}
		opts.ReviewerCmd = argv
	}

	positional := fs.Args()
	if len(positional) == 1 && positional[0] == "help" {
		usage(stdout)
		return 0
	}

	opts.ForkNames = positional
	opts.Stdout = stdout
	opts.Stderr = stderr
	if err := forkkeeper.Run(ctx, opts); err != nil {
		return 1
	}
	return 0
}

func usage(w io.Writer) {
	_, _ = fmt.Fprintln(w, `Usage:
  fork-keeper [flags] [<fork-name>...]
  fork-keeper version [--json]
  fork-keeper update [--check] [--ref <ref>] [--json]
  fork-keeper doctor [--offline] [--json] [--config <path>]
  fork-keeper skill [install|uninstall|status ...]

Flags:
  --config <path>        path to forks.json
  --cache-dir <path>     cache directory for scratch clones and logs
  --gh-bin <bin>         GitHub CLI executable or wrapper (e.g. gh-throttle)
  --reviewer-cmd <cmd>   external reviewer command (parsed without a shell)
  --sandbox <mode>       agent confinement mode: auto, seatbelt, bwrap, or none
  --date <YYYY-MM-DD>    date used in sync branch and pre-sync tag names`)
}
