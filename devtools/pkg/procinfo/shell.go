package procinfo

import (
	"slices"
	"strings"
)

// Shells are the interactive shells IsShell knows by name.
var Shells = []string{"sh", "bash", "zsh", "fish", "dash", "ksh", "mksh", "tcsh", "csh", "nu", "elvish", "xonsh"}

// IsShell reports whether p is an interactive shell: its name or
// argv[0] base, without a login shell's leading "-", is in Shells.
func IsShell(p Process) bool {
	for _, name := range []string{p.Name, Argv0Base(p)} {
		if name = strings.TrimPrefix(name, "-"); name != "" && slices.Contains(Shells, name) {
			return true
		}
	}
	return false
}
