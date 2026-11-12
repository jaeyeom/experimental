package main

import (
	"path/filepath"
	"slices"
	"strings"
)

// gitChanging are the git subcommands that change a checkout. stash,
// tag, branch and worktree also have read-only forms; gitChanges tells
// them apart.
var gitChanging = map[string]bool{
	"checkout": true, "switch": true, "commit": true, "merge": true,
	"rebase": true, "reset": true, "stash": true, "pull": true,
	"cherry-pick": true, "revert": true, "am": true, "apply": true,
	"clean": true, "restore": true, "rm": true, "mv": true, "add": true,
	"tag": true, "branch": true, "worktree": true,
}

// gitChangeDirs returns the directories where command, a Bash tool
// command run in cwd, runs a git subcommand that changes a checkout. It
// is a conservative reading, not a shell: it splits at ; & | && || ( )
// and newlines, skips variable assignments and the env, command, exec,
// nohup and time prefixes, reads sh/bash/zsh -c scripts, and follows cd,
// pushd and git -C, --work-tree and --git-dir. Since a cd inside a pipe
// or subshell does not last, a git command after a cd counts in both the
// cd target and the directory the command started in. A cd it cannot
// resolve (a variable, cd -) leaves the directory as it was.
func gitChangeDirs(command, cwd, home string) []string {
	var out []string
	for _, c := range gitChangeList(command, cwd, home) {
		if !slices.Contains(out, c.dir) {
			out = append(out, c.dir)
		}
	}
	return out
}

// gitChange is a state-changing git command: the directory it runs in
// and its subcommand, such as switch.
type gitChange struct {
	dir, sub string
}

// gitChangeList is gitChangeDirs with each command's subcommand, in the
// order read, without repeats.
func gitChangeList(command, cwd, home string) []gitChange {
	var out []gitChange
	add := func(c gitChange) {
		if !slices.Contains(out, c) {
			out = append(out, c)
		}
	}
	scanCommands(command, cwd, home, add)
	return out
}

// scanCommands reads each simple command of script in turn, starting in
// cwd, and reports the state-changing git commands.
func scanCommands(script, cwd, home string, add func(gitChange)) {
	dir, moved := cwd, false
	for _, words := range splitCommands(script) {
		words = stripPrefixes(words)
		if len(words) == 0 {
			continue
		}
		switch name := filepath.Base(words[0]); name {
		case "cd", "pushd":
			if target, ok := cdTarget(words[1:], dir, home); ok {
				dir, moved = target, true
			}
		case "sh", "bash", "zsh":
			if i := slices.Index(words, "-c"); i > 0 && i+1 < len(words) {
				scanCommands(words[i+1], dir, home, add)
				if moved {
					scanCommands(words[i+1], cwd, home, add)
				}
			}
		case "eval":
			scanCommands(strings.Join(words[1:], " "), dir, home, add)
		case "git":
			if target, sub, ok := gitTarget(words[1:], dir); ok {
				add(gitChange{target, sub})
				if moved {
					if again, _, _ := gitTarget(words[1:], cwd); again != "" {
						add(gitChange{again, sub})
					}
				}
			}
		}
	}
}

// stripPrefixes drops leading NAME=value words and the env, command,
// exec, nohup, time and builtin prefixes.
func stripPrefixes(words []string) []string {
	for len(words) > 0 {
		switch w := words[0]; {
		case isAssignment(w), plainPrefixes[w]:
			words = words[1:]
		case w == "env":
			words = skipEnvOptions(words[1:])
		default:
			return words
		}
	}
	return words
}

// plainPrefixes run the rest of the command line as a command.
var plainPrefixes = map[string]bool{"command": true, "exec": true, "nohup": true, "time": true, "builtin": true}

// skipEnvOptions drops env's options; -u, -S and -C take a value.
func skipEnvOptions(words []string) []string {
	for len(words) > 0 && strings.HasPrefix(words[0], "-") {
		if (words[0] == "-u" || words[0] == "-S" || words[0] == "-C") && len(words) > 1 {
			words = words[1:]
		}
		words = words[1:]
	}
	return words
}

func isAssignment(w string) bool {
	name, _, ok := strings.Cut(w, "=")
	if !ok || name == "" {
		return false
	}
	for i, r := range name {
		if r == '_' || r >= 'a' && r <= 'z' || r >= 'A' && r <= 'Z' || i > 0 && r >= '0' && r <= '9' {
			continue
		}
		return false
	}
	return true
}

// cdTarget is where cd args moves from dir, when it can be told.
func cdTarget(args []string, dir, home string) (string, bool) {
	for len(args) > 0 && strings.HasPrefix(args[0], "-") && args[0] != "-" {
		args = args[1:]
	}
	if len(args) == 0 {
		return home, home != ""
	}
	target := args[0]
	switch {
	case target == "-" || strings.ContainsAny(target, "$`"):
		return "", false
	case target == "~" || strings.HasPrefix(target, "~/"):
		if home == "" {
			return "", false
		}
		target = home + target[1:]
	}
	return joinDir(dir, target), true
}

// gitTarget is the directory a git command with args runs in from dir,
// and its subcommand, when it changes the checkout.
func gitTarget(args []string, dir string) (string, string, bool) {
	for len(args) > 0 && strings.HasPrefix(args[0], "-") {
		opt, value, hasValue := strings.Cut(args[0], "=")
		switch opt {
		case "-C", "--work-tree", "--git-dir":
			if !hasValue {
				if len(args) < 2 {
					return "", "", false
				}
				value, args = args[1], args[1:]
			}
			dir = joinDir(dir, value)
		case "-c", "--namespace", "--config-env", "--exec-path", "--super-prefix":
			if !hasValue && opt != "--exec-path" {
				args = args[1:]
			}
		}
		args = args[1:]
	}
	if len(args) == 0 || !gitChanges(args[0], args[1:]) {
		return "", "", false
	}
	return dir, args[0], true
}

// gitChanges reports whether git sub with args changes the checkout.
func gitChanges(sub string, args []string) bool {
	if !gitChanging[sub] {
		return false
	}
	switch sub {
	case "stash":
		return len(args) == 0 || (args[0] != "list" && args[0] != "show")
	case "worktree":
		return len(args) == 0 || args[0] != "list"
	case "tag":
		return len(args) > 0 && args[0] != "-l" && args[0] != "--list"
	case "branch":
		return slices.ContainsFunc(args, branchChanges)
	}
	return true
}

// branchChanges reports whether a git branch argument deletes, moves,
// copies or force-sets a branch.
func branchChanges(arg string) bool {
	switch arg {
	case "--delete", "--move", "--copy", "--force":
		return true
	}
	if strings.HasPrefix(arg, "--") || !strings.HasPrefix(arg, "-") {
		return false
	}
	return strings.ContainsAny(arg[1:], "dDmMcCf")
}

func joinDir(dir, target string) string {
	if filepath.IsAbs(target) {
		return filepath.Clean(target)
	}
	return filepath.Join(dir, target)
}

// splitCommands splits script into simple commands of words. Quotes
// group words; ; & | ( ) ` and newlines end a command, so the commands
// inside $(...), (...) and backticks are read too. A # starting a word
// comments out the rest of the line.
func splitCommands(script string) [][]string {
	sp := splitter{rs: []rune(script)}
	for sp.i = 0; sp.i < len(sp.rs); sp.i++ {
		sp.step()
	}
	sp.endCmd()
	return sp.cmds
}

// splitter is splitCommands' state: the script, the position, and the
// commands, words and word read so far.
type splitter struct {
	rs     []rune
	i      int
	cmds   [][]string
	words  []string
	word   strings.Builder
	inWord bool
}

// step reads the rune at i, and for a quote, escape or comment the runes
// it covers, leaving i on the last one read.
func (sp *splitter) step() {
	switch r := sp.rs[sp.i]; {
	case r == '\'':
		sp.quoted('\'', "")
	case r == '"':
		sp.quoted('"', "\"\\$`")
	case r == '\\':
		if sp.i+1 < len(sp.rs) {
			sp.i++
			if sp.rs[sp.i] != '\n' {
				sp.add(sp.rs[sp.i])
			}
		}
	case r == '#' && !sp.inWord:
		for sp.i < len(sp.rs) && sp.rs[sp.i] != '\n' {
			sp.i++
		}
		sp.endCmd()
	case strings.ContainsRune(";&|()`\n", r):
		sp.endCmd()
	case r == ' ' || r == '\t' || r == '\r':
		sp.endWord()
	default:
		sp.add(r)
	}
}

// quoted reads up to the closing quote; a backslash escapes the runes
// in escapes.
func (sp *splitter) quoted(quote rune, escapes string) {
	sp.inWord = true
	for sp.i++; sp.i < len(sp.rs) && sp.rs[sp.i] != quote; sp.i++ {
		if sp.rs[sp.i] == '\\' && sp.i+1 < len(sp.rs) && strings.ContainsRune(escapes, sp.rs[sp.i+1]) {
			sp.i++
		}
		sp.word.WriteRune(sp.rs[sp.i])
	}
}

func (sp *splitter) add(r rune) {
	sp.inWord = true
	sp.word.WriteRune(r)
}

func (sp *splitter) endWord() {
	if sp.inWord {
		sp.words = append(sp.words, sp.word.String())
		sp.word.Reset()
		sp.inWord = false
	}
}

func (sp *splitter) endCmd() {
	sp.endWord()
	if len(sp.words) > 0 {
		sp.cmds = append(sp.cmds, sp.words)
		sp.words = nil
	}
}
