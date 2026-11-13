package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/inboxrpc"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
	"github.com/jaeyeom/experimental/devtools/decide/tui/avatar"
	"github.com/muesli/termenv"
	"golang.org/x/sys/unix"
)

const (
	avatarCols = 10
	avatarRows = 6
	// avatarLabelRow is the figure row its id is printed beside.
	avatarLabelRow = 2
	// avatarGap separates figures in one row of the preview.
	avatarGap = "  "
	// defaultWidth is the preview width when the terminal's is unknown.
	defaultWidth = 80
)

func cmdAvatars(args []string, stdout, stderr io.Writer) int {
	if len(args) > 0 && args[0] == "set" {
		return cmdAvatarsSet(args[1:], stdout, stderr)
	}
	if len(args) == 0 || args[0] != "list" {
		return fail(stderr, fmt.Errorf("decide avatars requires list or set"), 2)
	}
	fs := flag.NewFlagSet("avatars list", flag.ContinueOnError)
	fs.SetOutput(stderr)
	name := fs.String("name", "", "also show the built-in this name gets")
	idsOnly := fs.Bool("ids", false, "print only the ids, one per line")
	if code, stop := stopped(parseFlags(fs, args[1:])); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide avatars list does not take arguments"), 2)
	}
	if *idsOnly {
		for _, id := range avatar.BuiltinIDs() {
			fmt.Fprintln(stdout, id)
		}
		return 0
	}
	profile := termenv.NewOutput(stdout).EnvColorProfile()
	if *name != "" {
		id := avatar.PickBuiltin(*name)
		fmt.Fprintf(stdout, "%s: %s\n", *name, id)
		writeAvatarRow(stdout, []string{id}, profile, len(id))
		fmt.Fprintln(stdout)
	}
	ids := avatar.BuiltinIDs()
	idWidth := 0
	for _, id := range ids {
		idWidth = max(idWidth, len(id))
	}
	cell := avatarCols + 1 + idWidth + len(avatarGap)
	perRow := max(1, (terminalWidth(stdout)+len(avatarGap))/cell)
	for start := 0; start < len(ids); start += perRow {
		if start > 0 {
			fmt.Fprintln(stdout)
		}
		writeAvatarRow(stdout, ids[start:min(start+perRow, len(ids))], profile, idWidth)
	}
	return 0
}

// cmdAvatarsSet saves name's avatar. Hub mode writes it on the hub.
// --local writes this machine's config instead.
func cmdAvatarsSet(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("avatars set", flag.ContinueOnError)
	fs.SetOutput(stderr)
	local := fs.Bool("local", false, "write this machine's config even in hub mode")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 2 {
		return fail(stderr, fmt.Errorf("decide avatars set requires <name> <id|default|none>"), 2)
	}
	name, choice := fs.Arg(0), fs.Arg(1)
	if !*local {
		if code, done := setAvatarOnHub(name, choice, stdout, stderr); done {
			return code
		}
	}
	if _, err := tui.SaveAvatar(name, choice); err != nil {
		return fail(stderr, err, 1)
	}
	fmt.Fprintf(stdout, "avatar for %s: %s (saved to config)\n", name, choice)
	return 0
}

// setAvatarOnHub writes the rule through the hub's set-avatar op.
// done is false when hub mode is off. A failure does not write locally.
func setAvatarOnHub(name, choice string, stdout, stderr io.Writer) (int, bool) {
	settings, err := hubConfig("")
	if err != nil {
		return fail(stderr, err, 1), true
	}
	if settings.hub.Target == "" {
		return 0, false
	}
	client := &inboxrpc.Client{Dial: inboxrpc.HubDialer(settings.hub), Label: settings.hub.Target}
	if err := client.Connect(); err != nil {
		return fail(stderr, err, 1), true
	}
	defer client.Close()
	if _, err := client.SetAvatar(name, choice); err != nil {
		return fail(stderr, err, 1), true
	}
	note := fmt.Sprintf("avatar for %s: %s (saved on hub)", name, choice)
	local, err := decide.LoadConfigFile()
	if err != nil {
		return fail(stderr, err, 1), true
	}
	if decide.LocalAvatarHides(local, name) {
		note = fmt.Sprintf("avatar for %s: %s (saved on hub; a local rule for %s overrides it here)", name, choice, name)
	}
	fmt.Fprintln(stdout, note)
	return 0, true
}

// writeAvatarRow prints figures side by side, each with its id beside it.
func writeAvatarRow(w io.Writer, ids []string, profile termenv.Profile, idWidth int) {
	figures := make([][]string, len(ids))
	for i, id := range ids {
		lines, err := avatar.Builtin(id, avatarCols, avatarRows, profile)
		if err != nil {
			panic(err) // ids come from avatar.BuiltinIDs
		}
		figures[i] = lines
	}
	for row := range avatarRows {
		var b strings.Builder
		for i, id := range ids {
			if i > 0 {
				b.WriteString(avatarGap)
			}
			label := ""
			if row == avatarLabelRow {
				label = id
			}
			fmt.Fprintf(&b, "%s %-*s", figures[i][row], idWidth, label)
		}
		fmt.Fprintln(w, strings.TrimRight(b.String(), " "))
	}
}

// terminalWidth is $COLUMNS, else the width of w when it is a terminal,
// else defaultWidth.
func terminalWidth(w io.Writer) int {
	if n, err := strconv.Atoi(os.Getenv("COLUMNS")); err == nil && n > 0 {
		return n
	}
	if f, ok := w.(*os.File); ok {
		if ws, err := unix.IoctlGetWinsize(int(f.Fd()), unix.TIOCGWINSZ); err == nil && ws.Col > 0 {
			return int(ws.Col)
		}
	}
	return defaultWidth
}
