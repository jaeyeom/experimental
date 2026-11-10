package tui

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
)

// keyDebugEnv names the file that logs every key message the inbox gets.
// 1 means decide-keys.log in the temp directory. Empty or 0 turns it off.
const keyDebugEnv = "DECIDE_KEYDEBUG"

func keyDebugPath(v string) string {
	switch v = strings.TrimSpace(v); v {
	case "", "0":
		return ""
	case "1":
		return filepath.Join(os.TempDir(), "decide-keys.log")
	default:
		return v
	}
}

// openKeyLog opens the DECIDE_KEYDEBUG file for appending, or returns nil
// when it is off.
func openKeyLog() (*os.File, error) {
	path := keyDebugPath(os.Getenv(keyDebugEnv))
	if path == "" {
		return nil, nil
	}
	f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o600) //nolint:gosec // G304: operator-chosen log path
	if err != nil {
		return nil, fmt.Errorf("open %s file: %w", keyDebugEnv, err)
	}
	return f, nil
}

// logInput writes one line for a key message, or for input bubbletea did
// not recognize, with its raw bytes. Other messages are not logged.
func logInput(w io.Writer, msg tea.Msg) {
	if k, ok := msg.(tea.KeyMsg); ok {
		fmt.Fprintf(w, "key type=%d (%s) alt=%v runes=%q str=%q\n",
			int(k.Type), k.Type, k.Alt, string(k.Runes), k.String())
		return
	}
	// Bubbletea v1 reports unparsed input with unexported types such as
	// unknownCSISequenceMsg ([]byte) and unknownInputByteMsg (byte).
	name := fmt.Sprintf("%T", msg)
	if !strings.HasPrefix(name, "tea.unknown") {
		return
	}
	if v := reflect.ValueOf(msg); v.Kind() == reflect.Slice && v.Type().Elem().Kind() == reflect.Uint8 {
		fmt.Fprintf(w, "unknown %s bytes=%q\n", name, v.Bytes())
		return
	}
	fmt.Fprintf(w, "unknown %s value=%v\n", name, msg)
}
