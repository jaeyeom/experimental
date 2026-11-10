package main

import (
	"context"
	"fmt"
	"io"
	"strings"
)

type runFunc func(ctx context.Context, args, env []string, stdin io.Reader, stdout, stderr io.Writer) int

// command is one subcommand. The dispatcher and the help both read commands().
type command struct {
	name    string
	summary string
	usage   string
	flags   string
	example string
	run     runFunc
}

func noStdin(f func(ctx context.Context, args, env []string, stdout, stderr io.Writer) int) runFunc {
	return func(ctx context.Context, args, env []string, _ io.Reader, stdout, stderr io.Writer) int {
		return f(ctx, args, env, stdout, stderr)
	}
}

func commands() []command {
	return []command{
		{
			name:    "register",
			summary: "Claim a name. --when is required and says what you are doing.",
			usage:   "agentbus register <name> --when <text>",
			flags:   "--when <text>  what this agent is actually doing (required; no default)",
			example: `agentbus register ops/inbox --when "reviewing the queue"`,
			run:     noStdin(cmdRegister),
		},
		{
			name:    "describe",
			summary: "Replace --when on the names this caller holds.",
			usage:   "agentbus describe --when <text>",
			flags:   "--when <text>  what this agent is actually doing (required; no default)",
			example: `agentbus describe --when "waiting on a reply from ops/inbox"`,
			run:     noStdin(cmdDescribe),
		},
		{
			name:    "whoami",
			summary: "Print the names this caller holds, or its raw handle when none are registered.",
			usage:   "agentbus whoami",
			example: "agentbus whoami",
			run:     noStdin(cmdWhoami),
		},
		{
			name:    "release",
			summary: "Drop a name, or hand it to a claimer with --to. Send context first.",
			usage:   "agentbus release <name> [--to <claimer>]",
			flags:   "--to <claimer>  hand the name to this claimer (default: none, drop the name)",
			example: "agentbus release ops/inbox --to herdr:local/default/w2:p3",
			run:     noStdin(cmdRelease),
		},
		{
			name:    "list",
			summary: "Registered names, --when, liveness, and transport.",
			usage:   "agentbus list",
			example: "agentbus list",
			run:     noStdin(cmdList),
		},
		{
			name:    "search",
			summary: "Every reachable agent, registered or not.",
			usage:   "agentbus search [query]",
			example: "agentbus search inbox",
			run:     noStdin(cmdSearch),
		},
		{
			name:    "send",
			summary: "Send. The body comes from the argument, --file, or stdin.",
			usage:   "agentbus send <name|handle> [<body>] [--file <path>] [--ttl <duration>] [--wait]",
			flags: "--file <path>       read the body from this file (default: none)\n" +
				"--ttl <duration>    deadline for this message, such as 30m (default: none, no deadline)\n" +
				"--wait              block until delivered or the deadline passes (default: false)",
			example: `agentbus send ops/inbox "are you reviewing the queue?"`,
			run:     cmdSend,
		},
		{
			name:    "reply",
			summary: "Reply. to is the original sender and re is id.",
			usage:   "agentbus reply <id> <body>",
			example: `agentbus reply m-7f3a "yes, the queue is in progress"`,
			run:     noStdin(cmdReply),
		},
		{
			name:    "show",
			summary: "Print one stored message.",
			usage:   "agentbus show <id>",
			example: "agentbus show m-7f3a",
			run:     noStdin(cmdShow),
		},
		{
			name:    "thread",
			summary: "Print the chain.",
			usage:   "agentbus thread <id>",
			example: "agentbus thread m-7f3a",
			run:     noStdin(cmdThread),
		},
		{
			name:    "status",
			summary: "Print id, state, and how long it has waited.",
			usage:   "agentbus status <id>",
			example: "agentbus status m-7f3a",
			run:     noStdin(cmdStatus),
		},
		{
			name:    "log",
			summary: "Read the daemon event log.",
			usage:   "agentbus log [flags]",
			flags: "--since <time>   include events at or after this RFC3339 time (default: all)\n" +
				"--name <name>    include events for this name (default: all)\n" +
				"--msg <id>       include events for this message id (default: all)\n" +
				"--event <type>   include events of this type (default: all)\n" +
				"--follow         wait for new events until interrupted (default: false)\n" +
				"--json           print each log line as JSON (default: false)",
			example: "agentbus log --name ops/inbox",
			run:     noStdin(cmdLog),
		},
		{
			name:    "install",
			summary: "Install the supervisor and load it.",
			usage:   "agentbus install",
			example: "agentbus install",
			run:     noStdin(cmdInstall),
		},
		{
			name:    "install-hooks",
			summary: "Install the observe-only SendMessage hook and the merged-draft prompt guard.",
			usage:   "agentbus install-hooks",
			example: "agentbus install-hooks",
			run:     noStdin(cmdInstallHooks),
		},
		{
			name:    "hook",
			summary: "Record one Claude Code SendMessage call. The install-hooks script runs it.",
			usage:   "agentbus hook < hook-input.json",
			example: "agentbus hook < hook-input.json",
			run:     cmdHook,
		},
		{
			name:    "prompt-hook",
			summary: "Block a Claude Code prompt where a bus message was pasted after a user draft, and re-queue it. The install-hooks script runs it.",
			usage:   "agentbus prompt-hook < hook-input.json",
			example: "agentbus prompt-hook < hook-input.json",
			run:     cmdPromptHook,
		},
	}
}

func findCommand(name string) (command, bool) {
	for _, cmd := range commands() {
		if cmd.name == name {
			return cmd, true
		}
	}
	return command{}, false
}

func isHelpFlag(arg string) bool {
	return arg == "-h" || arg == "--help"
}

func unknownCommand(name string, stderr io.Writer) int {
	fmt.Fprintf(stderr, "agentbus: unknown command %q\nrun 'agentbus --help' for usage\n", name)
	return 2
}

// cmdHelp handles `agentbus help [command]`.
func cmdHelp(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		printUsage(stdout)
		return 0
	}
	cmd, ok := findCommand(args[0])
	if !ok {
		return unknownCommand(args[0], stderr)
	}
	printCommandHelp(stdout, cmd)
	return 0
}

func printUsage(w io.Writer) {
	cmds := commands()
	width := len("help")
	for _, cmd := range cmds {
		width = max(width, len(cmd.name))
	}
	fmt.Fprint(w, `Usage: agentbus [command] [arguments]

agentbus is a local message bus for coding agents on one machine.
With no command, it prints this help and starts agentbusd if it is down.

Commands:
`)
	for _, cmd := range cmds {
		fmt.Fprintf(w, "  %-*s  %s\n", width, cmd.name, cmd.summary)
	}
	fmt.Fprintf(w, "  %-*s  %s\n", width, "help", "Print help for a command.")
	fmt.Fprint(w, `
Global flags:
  -h, --help  Print this help. After a command, print that command's help.

Run 'agentbus help <command>' for its flags and an example.
`)
}

func printCommandHelp(w io.Writer, cmd command) {
	fmt.Fprintf(w, "Usage: %s\n\n%s\n", cmd.usage, cmd.summary)
	if cmd.flags != "" {
		fmt.Fprintf(w, "\nFlags:\n%s\n", indent(cmd.flags))
	}
	fmt.Fprintf(w, "\nExample:\n%s\n", indent(cmd.example))
}

func indent(text string) string {
	return "  " + strings.ReplaceAll(text, "\n", "\n  ")
}
