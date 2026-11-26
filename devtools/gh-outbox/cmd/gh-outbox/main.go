// Command gh-outbox holds pull request comments and reviews that coding
// agents draft until the user approves the exact text in decide.
//
//	gh-outbox submit --repo owner/name --pr N --owner AGENT [--kind K] [--body-file F] [--line path:line=file]... [--in-reply-to ID] [--allow-resolved] [--ref R] [--edit ID] [--lint-only]
//	gh-outbox ls
//	gh-outbox show [--json] ID
//	gh-outbox poll [--watch]
package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"

	ghoutbox "github.com/jaeyeom/experimental/devtools/gh-outbox"
	"golang.org/x/sys/unix"
)

const usage = `usage:
  gh-outbox submit --repo owner/name --pr N --owner AGENT [--kind thread|reply|comment|request-changes|approve]
                   [--body-file FILE] [--line path:line=FILE]... [--in-reply-to ID] [--allow-resolved]
                   [--ref REF] [--edit ID] [--lint-only]
  gh-outbox ls
  gh-outbox show [--json] ID
  gh-outbox poll [--watch]

Every command takes --config PATH. The default config is
$XDG_CONFIG_HOME/gh-outbox/config.toml, or $HOME/.config/gh-outbox/config.toml.
`

// lines is a repeatable --line flag.
type lines []string

func (l *lines) String() string { return strings.Join(*l, ",") }

func (l *lines) Set(v string) error {
	*l = append(*l, v)
	return nil
}

type app struct {
	stdout, stderr io.Writer
	env            []string
	// newOutbox builds the outbox for cfg. The tests replace it.
	newOutbox func(cfg ghoutbox.Config) *ghoutbox.Outbox
}

func main() {
	a := app{stdout: os.Stdout, stderr: os.Stderr, env: os.Environ(), newOutbox: realOutbox}
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, unix.SIGTERM)
	code := a.run(ctx, os.Args[1:])
	stop()
	os.Exit(code)
}

func realOutbox(cfg ghoutbox.Config) *ghoutbox.Outbox {
	return &ghoutbox.Outbox{
		Store:  ghoutbox.NewStore(cfg.DataDir),
		Decide: ghoutbox.DecideCLI{Run: ghoutbox.Exec(cfg.DecideCommand)},
		GH:     ghoutbox.GHCLI{Run: ghoutbox.Exec(cfg.GHCommand)},
		Bus:    ghoutbox.AgentbusCLI{Run: ghoutbox.Exec(cfg.AgentbusCommand)},
		Config: cfg,
	}
}

func (a app) run(ctx context.Context, args []string) int {
	if code, ok := newGHOutboxTool().Handle(ctx, args, a.stdout, a.stderr); ok {
		return code
	}
	if len(args) == 0 {
		fmt.Fprint(a.stderr, usage)
		return 2
	}
	switch args[0] {
	case "submit":
		return a.submit(ctx, args[1:])
	case "ls":
		return a.ls(args[1:])
	case "show":
		return a.show(args[1:])
	case "poll":
		return a.poll(ctx, args[1:])
	case "help", "-h", "--help":
		fmt.Fprint(a.stdout, usage)
		return 0
	default:
		fmt.Fprintf(a.stderr, "gh-outbox: unknown command %q\n%s", args[0], usage)
		return 2
	}
}

func (a app) flags(name string) (*flag.FlagSet, *string) {
	fs := flag.NewFlagSet("gh-outbox "+name, flag.ContinueOnError)
	fs.SetOutput(a.stderr)
	return fs, fs.String("config", "", "config file")
}

func (a app) outbox(configPath string) (*ghoutbox.Outbox, error) {
	if configPath == "" {
		p, err := ghoutbox.DefaultConfigPath(a.env)
		if err != nil {
			return nil, fmt.Errorf("config path: %w", err)
		}
		configPath = p
	}
	cfg, err := ghoutbox.LoadConfig(configPath, a.env)
	if err != nil {
		return nil, fmt.Errorf("load config: %w", err)
	}
	return a.newOutbox(cfg), nil
}

func (a app) fail(err error) int {
	fmt.Fprintf(a.stderr, "gh-outbox: %v\n", err)
	var ue *ghoutbox.UsageError
	if errors.As(err, &ue) {
		return 2
	}
	return 1
}

func (a app) submit(ctx context.Context, args []string) int {
	fs, configPath := a.flags("submit")
	var (
		d        ghoutbox.Draft
		lineSpec lines
	)
	fs.StringVar(&d.Repo, "repo", "", "owner/name")
	fs.IntVar(&d.PR, "pr", 0, "pull request number")
	fs.StringVar(&d.Kind, "kind", "", "thread (default), reply, comment, request-changes or approve")
	bodyFile := fs.String("body-file", "", "review or reply body file")
	fs.Var(&lineSpec, "line", "path:line=text-file (repeatable)")
	fs.StringVar(&d.InReplyTo, "in-reply-to", "", "id of a pull request review comment to reply to")
	fs.BoolVar(&d.AllowResolved, "allow-resolved", false, "allow replying to a resolved review thread")
	fs.StringVar(&d.Owner, "owner", "", "the submitting agent's agentbus name")
	fs.StringVar(&d.Ref, "ref", "", "the caller's own reference, echoed in notices")
	fs.StringVar(&d.Edit, "edit", "", "id of an existing review comment or review to replace")
	lintOnly := fs.Bool("lint-only", false, "check the draft and do not submit it")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() > 0 {
		fmt.Fprintf(a.stderr, "gh-outbox submit: unexpected argument %q\n", fs.Arg(0))
		return 2
	}
	if err := readTexts(&d, *bodyFile, lineSpec); err != nil {
		return a.fail(err)
	}
	ob, err := a.outbox(*configPath)
	if err != nil {
		return a.fail(err)
	}
	if *lintOnly {
		return a.lintOnly(ctx, ob, d)
	}
	if err := d.Validate(); err != nil {
		return a.fail(err)
	}
	rec, err := ob.Submit(ctx, d)
	if err != nil {
		return a.fail(err)
	}
	fmt.Fprintln(a.stdout, rec.ID)
	return 0
}

func (a app) lintOnly(ctx context.Context, ob *ghoutbox.Outbox, d ghoutbox.Draft) int {
	if err := d.ValidateText(); err != nil {
		return a.fail(err)
	}
	hits, err := ob.Lint(ctx, d)
	if err != nil {
		return a.fail(err)
	}
	if len(hits) > 0 {
		return a.fail(&ghoutbox.LintError{Hits: hits})
	}
	fmt.Fprintln(a.stdout, "clean")
	return 0
}

// readTexts reads the body and line files byte for byte.
func readTexts(d *ghoutbox.Draft, bodyFile string, specs []string) error {
	if bodyFile != "" {
		data, err := os.ReadFile(bodyFile) //nolint:gosec // G304: the caller's own draft file
		if err != nil {
			return fmt.Errorf("read --body-file: %w", err)
		}
		d.Body = string(data)
	}
	for _, spec := range specs {
		path, line, file, err := ghoutbox.ParseLineSpec(spec)
		if err != nil {
			return fmt.Errorf("parse --line: %w", err)
		}
		data, err := os.ReadFile(file) //nolint:gosec // G304: the caller's own draft file
		if err != nil {
			return fmt.Errorf("read --line %s: %w", spec, err)
		}
		d.Lines = append(d.Lines, ghoutbox.LineComment{Path: path, Line: line, Text: string(data)})
	}
	return nil
}

func (a app) ls(args []string) int {
	fs, configPath := a.flags("ls")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	ob, err := a.outbox(*configPath)
	if err != nil {
		return a.fail(err)
	}
	recs, err := ob.Store.List()
	if err != nil {
		return a.fail(err)
	}
	w := tabwriter.NewWriter(a.stdout, 0, 4, 2, ' ', 0)
	fmt.Fprintln(w, "ID\tSTATE\tTARGET\tKIND\tOWNER\tREF\tUPDATED")
	for _, r := range recs {
		fmt.Fprintf(w, "%d\t%s\t%s\t%s\t%s\t%s\t%s\n", r.ID, r.State, r.Target(), r.Kind, r.Owner, r.Ref, r.UpdatedAt.Format(time.RFC3339))
	}
	if err := w.Flush(); err != nil {
		return a.fail(err)
	}
	return 0
}

func (a app) show(args []string) int {
	fs, configPath := a.flags("show")
	asJSON := fs.Bool("json", false, "print the stored record as JSON")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 1 {
		fmt.Fprintln(a.stderr, "gh-outbox show: want one draft id")
		return 2
	}
	id, err := strconv.ParseInt(fs.Arg(0), 10, 64)
	if err != nil {
		fmt.Fprintf(a.stderr, "gh-outbox show: bad id %q\n", fs.Arg(0))
		return 2
	}
	ob, err := a.outbox(*configPath)
	if err != nil {
		return a.fail(err)
	}
	r, err := ob.Store.Get(id)
	if err != nil {
		return a.fail(err)
	}
	if *asJSON {
		enc := json.NewEncoder(a.stdout)
		enc.SetIndent("", "  ")
		if err := enc.Encode(r); err != nil {
			return a.fail(err)
		}
		return 0
	}
	fmt.Fprintf(a.stdout, "State: %s\nOwner: %s\n", r.State, r.Owner)
	if r.Outcome != "" {
		fmt.Fprintf(a.stdout, "Outcome: %s\n", r.Outcome)
	}
	for _, l := range r.Posted {
		fmt.Fprintf(a.stdout, "Posted: %s %s\n", l.Where, l.URL)
	}
	fmt.Fprintf(a.stdout, "\n%s\n", ghoutbox.Question(r))
	return 0
}

func (a app) poll(ctx context.Context, args []string) int {
	fs, configPath := a.flags("poll")
	watch := fs.Bool("watch", false, "keep polling every poll_interval until stopped")
	if err := fs.Parse(args); err != nil {
		return 2
	}
	ob, err := a.outbox(*configPath)
	if err != nil {
		return a.fail(err)
	}
	if !*watch {
		if err := ob.Poll(ctx); err != nil {
			return a.fail(err)
		}
		return 0
	}
	t := time.NewTicker(ob.Config.PollInterval)
	defer t.Stop()
	for {
		if err := ob.Poll(ctx); err != nil {
			fmt.Fprintf(a.stderr, "gh-outbox: %v\n", err)
		}
		select {
		case <-ctx.Done():
			return 0
		case <-t.C:
		}
	}
}
