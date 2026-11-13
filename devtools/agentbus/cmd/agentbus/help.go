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
			usage:   "agentbus list [--json] [--fresh]",
			flags: "--json   print a JSON array, one object per name, with the fields\n" +
				"         name, when, liveness (alive, dead, hibernated, or unknown),\n" +
				"         liveness_age (seconds since liveness was observed, or null),\n" +
				"         transport, and handle; [] when none are registered\n" +
				"         (default: false, text)\n" +
				"--fresh  probe every name's liveness now instead of answering from\n" +
				"         the daemon's cache, which a list refreshes in the background\n" +
				"         once it is older than list_liveness_max_age (default: false)",
			example: "agentbus list --json",
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
			usage:   "agentbus send <name|handle> [<body>] [--file <path>] [--ttl <duration>] [--wait] [--client-id <id>] [--json]",
			flags: "--file <path>       read the body from this file (default: none)\n" +
				"--ttl <duration>    deadline for this message, such as 30m (default: none, no deadline)\n" +
				"--wait              block until delivered or the deadline passes (default: false).\n" +
				"                    Without it, a message that passes the recipient, state, and\n" +
				"                    composer checks prints \"queued for delivery\": stored and\n" +
				"                    being pasted. agentbus status <id> then shows delivered\n" +
				"--client-id <id>    retry key, <machine>:<ulid>, up to 64 [A-Za-z0-9._:-] with a colon (default: a generated <machine>:<ulid>)\n" +
				"--json              print id, state, and duplicate as JSON (default: false)",
			example: `agentbus send ops/inbox "are you reviewing the queue?"`,
			run:     cmdSend,
		},
		{
			name:    "reply",
			summary: "Reply. to is the original sender and re is id.",
			usage:   "agentbus reply <id> <body> [--client-id <id>] [--json]",
			flags: "--client-id <id>    retry key, <machine>:<ulid>, up to 64 [A-Za-z0-9._:-] with a colon (default: a generated <machine>:<ulid>)\n" +
				"--json              print id, state, and duplicate as JSON (default: false)",
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
			summary: "Print id, state, client id when set, and how long it has waited. id may be a client id.",
			usage:   "agentbus status <id>",
			example: "agentbus status m-7f3a",
			run:     noStdin(cmdStatus),
		},
		{
			name:    "log",
			summary: "Read the daemon event log.",
			usage:   "agentbus log [flags]",
			flags: "--since <time>   include events at or after this time: a duration ago\n" +
				"                 (30m, 6h, 2d, 1d12h), an RFC3339 time, or a YYYY-MM-DD\n" +
				"                 date at local midnight (default: all)\n" +
				"--name <name>    include events for this name (default: all)\n" +
				"--msg <id>       include events for this message id (default: all)\n" +
				"--event <type>   include events of this type (default: all)\n" +
				"--follow         wait for new events until interrupted (default: false)\n" +
				"--json           print each log line as JSON (default: false)",
			example: "agentbus log --name ops/inbox",
			run:     noStdin(cmdLog),
		},
		{
			name:    "outbox",
			summary: "List or flush hub-mode messages waiting on this machine.",
			usage:   "agentbus outbox <ls|flush> [--watch]",
			flags:   "--watch  with flush, retry until interrupted, backing off from about 5s to 5m (default: false, flush once)",
			example: "agentbus outbox ls",
			run:     noStdin(cmdOutbox),
		},
		{
			name:    "lease",
			summary: "Claim, extend, leave, check, release, or adopt the lease on a guarded checkout; pause, unshare, or resume it as the user; status shows who holds it; hook and install-hook enforce it in Claude Code.",
			usage:   "agentbus lease <claim|extend|leave|status|check|release|adopt|pause|unshare|resume|hook|install-hook> [flags]",
			flags:   leaseFlagsHelp,
			example: `agentbus lease claim --for 40m --why "issue 623"`,
			run:     runLease,
		},
		{
			name:    "mem",
			summary: "Memory per local agent pane: process-tree RSS, long-lived jobs, names, and leases.",
			usage:   "agentbus mem [--json]",
			flags: "--json  print one object: agents (pane, tab, handle, names, state,\n" +
				"        idle_since, rss_bytes, jobs, leases, session), total_rss_bytes,\n" +
				"        available_bytes, machine, states, and notes (default: false, text)",
			example: "agentbus mem",
			run:     noStdin(cmdMem),
		},
		{
			name: "hibernate",
			summary: "Stop idle agents on this machine but keep their panes and names. " +
				"Refuses, naming each failed check, unless the agent is idle or done, shows no dialog, " +
				"runs no background job and no build tool of its own in its cwd (any build there while it holds or waits for a lease), " +
				"has an empty composer (Claude Code only), " +
				"holds and waits for no lease, and is not in hibernate_exempt.",
			usage: "agentbus hibernate <name|handle>... | --idle-over <duration> [--dry-run] [--json] | --policy",
			flags: "--idle-over <d>  every agent the daemon has seen idle or done for at least d\n" +
				"--dry-run       print \"would hibernate <name> (<rss>)\" and change nothing\n" +
				"--json          print a JSON array: name, handle, pane, outcome (hibernated,\n" +
				"                refused, would-hibernate, failed), reasons, rss_bytes, warning\n" +
				"--policy        in hub mode, run hibernate_policy for this machine until stopped;\n" +
				"                agentbus install runs it when hibernate_policy is on or dry-run",
			example: "agentbus hibernate --idle-over 2h --dry-run",
			run:     noStdin(cmdHibernate),
		},
		{
			name: "wake",
			summary: "Relaunch a hibernated agent in its pane now, with no message to deliver. " +
				"A message to a hibernated agent wakes it too. It returns once herdr reports " +
				"the agent idle or done, within wake_timeout. Exit 0: awake, or not hibernated " +
				"with --if-hibernated; 1: the wake failed; 2: unknown name or handle.",
			usage: wakeUsage,
			flags: "--if-hibernated  exit 0 and print nothing when the target is not hibernated,\n" +
				"                 so a dispatch tool can call it before every prompt\n" +
				"--wait           return once the agent is ready (always the case)\n" +
				"--machine <m>    the target is a herdr pane id on machine m (local for this one)",
			example: "agentbus wake --if-hibernated --machine local w1:p1",
			run:     noStdin(cmdWake),
		},
		{
			name:    "install",
			summary: "Install the supervisor and load it; in hub mode, the tunnel to the hub.",
			usage:   "agentbus install",
			example: "agentbus install",
			run:     noStdin(cmdInstall),
		},
		{
			name:    "paths",
			summary: "Print this machine's socket and state paths as JSON.",
			usage:   "agentbus paths --json",
			flags:   "--json  print socket, hub_socket, tunnel_socket, and state_dir",
			example: "agentbus paths --json",
			run:     noStdin(cmdPaths),
		},
		{
			name: "doctor",
			summary: "Report other agentbusd processes for this user. In hub mode, report the tunnel, the ssh port, " +
				"the outbox flusher, and pending outbox entries instead of the local daemon socket. With --sandbox, " +
				"print the Claude Code sandbox allowlist this machine needs and test each connection.",
			usage:   "agentbus doctor [--sandbox]",
			flags:   "--sandbox  also print the sandbox settings entries and report each connection ok or blocked",
			example: "agentbus doctor --sandbox",
			run:     noStdin(cmdDoctor),
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
			summary: "Record one Claude Code SendMessage call, and hint the bus name when the recipient holds one. The install-hooks script runs it.",
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

agentbus is a local message bus for coding agents. The daemon runs on one
machine and also delivers to agents on saved herdr machines.
With no command, it prints this help and starts agentbusd if it is down.

Commands:
`)
	for _, cmd := range cmds {
		fmt.Fprintf(w, "  %-*s  %s\n", width, cmd.name, cmd.summary)
	}
	fmt.Fprintf(w, "  %-*s  %s\n", width, "help", "Print help for a command.")
	fmt.Fprint(w, `
Handles:
  herdr:<machine>/<session>/<pane>  <machine> is local, or a herdr saved-machine
                                    label or id, reached with herdr --machine.

Config ($XDG_CONFIG_HOME/agentbus/config.toml):
  machines = ["linux2"]   saved machines whose agents search lists
  remote_timeout = "5s"   bound on each call to another machine, and the
                          ssh connect timeout in hub mode
  hub = "me@hub"          hub mode: run every command on this ssh target
                          (AGENTBUS_HUB overrides it); install refuses
  ssh_identity_agent = "~/.ssh/agent.sock"
                          ssh IdentityAgent for the hub; "~/" expands
                          against HOME. Empty leaves ssh's own agent
  machine = "linux2"      this machine's herdr label on the hub, so a
                          hub-mode caller is herdr:linux2/<session>/<pane>
  lease_checkouts = ["/home/u/src/repo"]
                          this machine's checkouts that need the lease;
                          lease commands are local-only for now
  lease_check_interval = "2m", lease_checkin_repeat = "15m",
  lease_idle_grace = "5m" soft-deadline timing, read by the daemon: how
                          often an overrun holder is looked at, least time
                          between check-ins, and idle-and-silent time
                          before takeover
  lease_takeover = "log"  read by the daemon: "on" carries out takeovers;
                          "log" (the default, and any invalid value) only
                          logs lease_would_take_over once per holder
  lease_ignore_procs = ["zsh", "claude"]
                          programs whose working directory in the checkout
                          does not block lease adopt; replaces the default
                          sh, bash, zsh, fish, claude, codex, gemini,
                          emacs, vim, nvim, less, agentbus
  hibernate_exempt = ["ops/*"]
                          names, handles, or globs hibernate never stops
  wake_timeout = "60s"    read by the daemon: how long a hibernated agent
                          it relaunches has to become idle or done

In hub mode a hub that cannot be reached fails the command with
"agentbus: hub <target> unreachable: <cause>", except send and reply,
which stay in a local outbox and exit 0 with
"queued (hub unreachable): <client-id>". It never falls back to a
local daemon. When $XDG_RUNTIME_DIR/agentbus/tunnel.sock is a forward of
the hub's $XDG_RUNTIME_DIR/agentbus/hub.sock, a call from a herdr pane
goes over it instead of starting ssh; log still runs over ssh.

A message to a machine that cannot be reached is held, not failed, with
"held: machine <m> unreachable: host unreachable" or
"held: machine <m> unreachable: authentication locked (unlock your SSH/gpg agent)".

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
