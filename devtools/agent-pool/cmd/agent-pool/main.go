// Command agent-pool provides the Producer CLI, Consumer worker daemon CLI,
// catalog validator, supervisor installer, and standard toolcli protocol for
// agent-pool.
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"runtime"
	"strings"
)

type cliDeps struct {
	goos       string
	homeDir    func() (string, error)
	executable func() (string, error)
	lookPath   func(file string) (string, error)
	runCmd     func(ctx context.Context, name string, args []string, env []string) ([]byte, error)
	httpClient *http.Client
}

func defaultDeps() cliDeps {
	return cliDeps{
		goos:       runtime.GOOS,
		homeDir:    os.UserHomeDir,
		executable: os.Executable,
		lookPath:   exec.LookPath,
		runCmd:     defaultRunCmd,
	}
}

func (d cliDeps) resolved() cliDeps {
	def := defaultDeps()
	if d.goos == "" {
		d.goos = def.goos
	}
	if d.homeDir == nil {
		d.homeDir = def.homeDir
	}
	if d.executable == nil {
		d.executable = def.executable
	}
	if d.lookPath == nil {
		d.lookPath = def.lookPath
	}
	if d.runCmd == nil {
		d.runCmd = def.runCmd
	}
	return d
}

func defaultRunCmd(ctx context.Context, name string, args []string, env []string) ([]byte, error) {
	// Direct execve of a known diagnostic/supervisor binary without a shell.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, name, args...)
	if len(env) > 0 {
		cmd.Env = env
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		return out, fmt.Errorf("run %s %s: %w", name, strings.Join(args, " "), err)
	}
	return out, nil
}

func main() {
	os.Exit(runCLI(context.Background(), os.Args[1:], os.Stdout, os.Stderr, defaultDeps()))
}

func runCLI(ctx context.Context, args []string, stdout, stderr io.Writer, deps cliDeps) int {
	deps = deps.resolved()
	if len(args) == 0 {
		printUsage(stderr)
		return 2
	}

	tool := newAgentPoolTool(deps)
	if code, handled := tool.Handle(ctx, args, stdout, stderr); handled {
		return code
	}

	switch args[0] {
	case "-h", "--help", "help":
		printUsage(stdout)
		return 0
	case "status":
		return cmdStatus(ctx, args[1:], stdout, stderr, deps)
	case "dispatch":
		return cmdDispatch(ctx, args[1:], stdout, stderr, deps)
	case "result":
		return cmdResult(ctx, args[1:], stdout, stderr, deps)
	case "cancel":
		return cmdCancel(ctx, args[1:], stdout, stderr, deps)
	case "catalog":
		return cmdCatalog(args[1:], stdout, stderr)
	case "init-auth":
		return cmdInitAuth(ctx, args[1:], stdout, stderr)
	case "worker":
		return cmdWorker(ctx, args[1:], stdout, stderr, deps)
	default:
		fmt.Fprintf(stderr, "agent-pool: unknown subcommand %q\n", args[0])
		printUsage(stderr)
		return 2
	}
}

func printUsage(w io.Writer) {
	fmt.Fprintln(w, "Usage: agent-pool <command> [flags]")
	fmt.Fprintln(w, "")
	fmt.Fprintln(w, "Producer commands:")
	fmt.Fprintln(w, "  status     [--json]")
	fmt.Fprintln(w, "  dispatch   <repo> <skill> --args-json '{...}' [--idempotency-key <k>] [--wait] [--timeout 5m] [--json]")
	fmt.Fprintln(w, "  result     <job_id> [--wait] [--timeout 5m] [--json]")
	fmt.Fprintln(w, "  cancel     <job_id> [--json]")
	fmt.Fprintln(w, "")
	fmt.Fprintln(w, "Consumer commands:")
	fmt.Fprintln(w, "  init-auth  [--config worker.toml]")
	fmt.Fprintln(w, "  worker     run|pause|resume|stats|install [flags]")
	fmt.Fprintln(w, "")
	fmt.Fprintln(w, "Catalog commands:")
	fmt.Fprintln(w, "  catalog    validate [path-to-repo-or-catalog.json]")
	fmt.Fprintln(w, "")
	fmt.Fprintln(w, "Standard CLI protocol commands:")
	fmt.Fprintln(w, "  version    [--json]")
	fmt.Fprintln(w, "  update     [--check] [--ref <ref>] [--json]")
	fmt.Fprintln(w, "  doctor     [--json] [--offline]")
	fmt.Fprintln(w, "  skill      [install|uninstall|status ...]")
}

func writeJSONOut(w io.Writer, v any) error {
	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	if err := enc.Encode(v); err != nil {
		return fmt.Errorf("encode JSON: %w", err)
	}
	return nil
}

// reorderFlags moves flags before positional arguments so commands like
// `dispatch <repo> <skill> --json` parse cleanly with Go's standard flag.FlagSet.
func reorderFlags(args []string, valueFlags map[string]bool) []string {
	var flags []string
	var positional []string
	for i := 0; i < len(args); i++ {
		arg := args[i]
		if arg == "--" {
			positional = append(positional, args[i:]...)
			break
		}
		if !strings.HasPrefix(arg, "-") || arg == "-" {
			positional = append(positional, arg)
			continue
		}
		flags = append(flags, arg)
		name := strings.TrimLeft(arg, "-")
		if strings.Contains(name, "=") {
			continue
		}
		if valueFlags[name] && i+1 < len(args) {
			i++
			flags = append(flags, args[i])
		}
	}
	return append(flags, positional...)
}
