package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

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
	if len(args) == 0 || args[0] != "list" {
		return fail(stderr, fmt.Errorf("decide avatars requires list"), 2)
	}
	fs := flag.NewFlagSet("avatars list", flag.ContinueOnError)
	fs.SetOutput(stderr)
	name := fs.String("name", "", "also show the built-in this name gets")
	if code, stop := stopped(parseFlags(fs, args[1:])); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide avatars list does not take arguments"), 2)
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
