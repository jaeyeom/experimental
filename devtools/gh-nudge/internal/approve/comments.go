package approve

import (
	"fmt"
	"path/filepath"
	"strconv"
	"strings"
	"unicode"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
)

// Block-comment scan states. A block opened from a context line is
// poison, so an added line inside it is code.
const (
	blockNone = iota
	blockAdded
	blockContext
)

// syntax is how added lines comment in one language.
type syntax struct {
	line  string
	line2 string
	open  string
	close string
}

func syntaxFor(path string) (syntax, bool) {
	base := filepath.Base(path)
	switch base {
	case "BUILD", "WORKSPACE", "Makefile", "Dockerfile":
		return syntax{line: "#"}, true
	}
	switch strings.ToLower(filepath.Ext(base)) {
	case ".py", ".sh", ".bash", ".zsh", ".bzl", ".bazel", ".yaml", ".yml", ".toml", ".rb", ".pl", ".mk", ".r":
		return syntax{line: "#"}, true
	case ".cfg", ".ini":
		return syntax{line: "#", line2: ";"}, true
	case ".go", ".c", ".cc", ".cpp", ".cxx", ".h", ".hh", ".hpp", ".rs", ".ts", ".tsx", ".js", ".jsx", ".java", ".kt", ".swift", ".proto", ".scala", ".cs":
		return syntax{line: "//", open: "/*", close: "*/"}, true
	case ".sql", ".lua", ".hs":
		return syntax{line: "--"}, true
	case ".md", ".html", ".htm", ".xml", ".svg":
		return syntax{open: "<!--", close: "-->"}, true
	case ".el", ".lisp", ".clj":
		return syntax{line: ";"}, true
	default:
		return syntax{}, false
	}
}

// fileCommentAdditions reports how many added lines are comments or
// blank. The reason is set when the file does not pass.
func fileCommentAdditions(file github.PullFile) (int, string, bool) {
	if !file.PatchSet {
		if file.Status == "renamed" && file.Additions == 0 && file.Deletions == 0 {
			return 0, "", true
		}
		return 0, "patch unavailable for " + file.Path, false
	}
	lang, known := syntaxFor(file.Path)
	n, line, text, ok := scanPatch(file.Patch, lang, known)
	if !ok {
		return n, codeReason(file.Path, line, text), false
	}
	if file.Additions > n {
		return n, "patch unavailable for " + file.Path, false
	}
	return n, "", true
}

// commentOnlyChange reports whether every added and deleted line of file
// is a comment or blank. The reason is set when it is not.
func commentOnlyChange(file github.PullFile) (string, bool) {
	switch {
	case !file.PatchSet:
		return "patch unavailable for " + file.Path, false
	case file.Status == "renamed":
		return file.Path + " is renamed from " + file.Previous, false
	}
	lang, known := syntaxFor(file.Path)
	lines := parsePatch(file.Patch)
	adds, line, text, ok := scanLines(lines, lang, known)
	if !ok {
		return fmt.Sprintf("%s:+%d is code: %s", file.Path, line, quoteExcerpt(text)), false
	}
	dels, line, text, ok := scanLines(oldSide(lines), lang, known)
	if !ok {
		return fmt.Sprintf("%s:-%d is code: %s", file.Path, line, quoteExcerpt(text)), false
	}
	if file.Additions > adds || file.Deletions > dels {
		return "patch unavailable for " + file.Path, false
	}
	return "", true
}

// oldSide turns the deleted lines of a patch into added lines numbered in
// the old file, so scanLines classifies them. Added lines are dropped.
func oldSide(lines []patchLine) []patchLine {
	out := make([]patchLine, 0, len(lines))
	for _, ln := range lines {
		switch {
		case ln.added:
		case ln.deleted:
			out = append(out, patchLine{added: true, text: ln.text, newLine: ln.oldLine})
		default:
			out = append(out, patchLine{text: ln.text, newLine: ln.oldLine})
		}
	}
	return out
}

func codeReason(path string, line int, text string) string {
	return fmt.Sprintf("deletions_and_comments_only: %s:+%d is code: %s", path, line, quoteExcerpt(text))
}

func quoteExcerpt(s string) string {
	const limit = 80
	r := []rune(s)
	if len(r) > limit {
		s = string(r[:limit]) + "…"
	}
	return strconv.Quote(s)
}

func missingPatchFile(pr github.ApprovalPullRequest) string {
	have := make(map[string]bool, len(pr.Patches))
	for _, file := range pr.Patches {
		have[file.Path] = true
	}
	for _, file := range pr.Files {
		if !have[file.Path] {
			return file.Path
		}
	}
	return "(truncated)"
}

type patchLine struct {
	added   bool
	deleted bool
	text    string
	newLine int
	oldLine int
}

func scanPatch(patch string, lang syntax, known bool) (adds, line int, text string, ok bool) {
	return scanLines(parsePatch(patch), lang, known)
}

func scanLines(lines []patchLine, lang syntax, known bool) (adds, line int, text string, ok bool) {
	state := blockNone
	for _, ln := range lines {
		if ln.deleted {
			continue
		}
		switch state {
		case blockContext:
			if ln.added {
				return adds, ln.newLine, ln.text, false
			}
			state = afterOpenLine(ln.text, lang, blockContext)
		case blockAdded:
			if !ln.added {
				state = afterOpenLine(ln.text, lang, blockAdded)
				continue
			}
			adds++
			still, bad := continueBlock(ln.text, lang)
			if bad {
				return adds, ln.newLine, ln.text, false
			}
			if !still {
				state = blockNone
			}
		default:
			if !ln.added {
				if contextLeavesBlock(ln.text, lang) {
					state = blockContext
				}
				continue
			}
			adds++
			if !known {
				return adds, ln.newLine, ln.text, false
			}
			next, bad := classifyAdded(ln.text, lang)
			if bad {
				return adds, ln.newLine, ln.text, false
			}
			state = next
		}
	}
	return adds, 0, "", true
}

// afterOpenLine returns the block state after a non-added line while a
// block is open. A closer on that line ends the block.
func afterOpenLine(text string, lang syntax, current int) int {
	closed, rest := closeRest(text, lang.close)
	if !closed {
		return current
	}
	if contextLeavesBlock(rest, lang) {
		return blockContext
	}
	return blockNone
}

func classifyAdded(text string, lang syntax) (int, bool) {
	if strings.TrimSpace(text) == "" {
		return blockNone, false
	}
	trimmed := trimLeftSpace(text)
	if startsComment(trimmed, lang) {
		return blockNone, false
	}
	if lang.open != "" && strings.HasPrefix(trimmed, lang.open) {
		still, bad := continueBlock(trimmed[len(lang.open):], lang)
		if bad {
			return blockNone, true
		}
		if still {
			return blockAdded, false
		}
		return blockNone, false
	}
	return blockNone, true
}

func continueBlock(text string, lang syntax) (bool, bool) {
	i := strings.Index(text, lang.close)
	if i < 0 {
		return true, false
	}
	rest := text[i+len(lang.close):]
	if strings.TrimSpace(rest) == "" {
		return false, false
	}
	next, bad := classifyAdded(rest, lang)
	if bad {
		return false, true
	}
	return next == blockAdded, false
}

func startsComment(trimmed string, lang syntax) bool {
	if lang.line != "" && strings.HasPrefix(trimmed, lang.line) {
		return true
	}
	return lang.line2 != "" && strings.HasPrefix(trimmed, lang.line2)
}

func closeRest(text, end string) (bool, string) {
	if end == "" {
		return false, text
	}
	i := strings.Index(text, end)
	if i < 0 {
		return false, text
	}
	return true, text[i+len(end):]
}

// contextLeavesBlock reports whether a context line opens a block that
// does not close on that same line. The opener may sit after code.
func contextLeavesBlock(text string, lang syntax) bool {
	if lang.open == "" {
		return false
	}
	rest := text
	inBlock := false
	for rest != "" {
		if !inBlock {
			openAt := strings.Index(rest, lang.open)
			lineAt := -1
			if lang.line != "" {
				lineAt = strings.Index(rest, lang.line)
			}
			if openAt < 0 {
				return false
			}
			if lineAt >= 0 && lineAt < openAt {
				return false
			}
			rest = rest[openAt+len(lang.open):]
			inBlock = true
			continue
		}
		closeAt := strings.Index(rest, lang.close)
		if closeAt < 0 {
			return true
		}
		rest = rest[closeAt+len(lang.close):]
		inBlock = false
	}
	return inBlock
}

func trimLeftSpace(s string) string {
	return strings.TrimLeftFunc(s, unicode.IsSpace)
}

func parsePatch(patch string) []patchLine {
	var out []patchLine
	newLine, oldLine := 0, 0
	inHunk := false
	for _, raw := range strings.Split(patch, "\n") {
		if strings.HasPrefix(raw, "@@") {
			n, ok := hunkNewStart(raw)
			if ok {
				newLine = n
				oldLine, _ = hunkStart(raw, " -")
				inHunk = true
			}
			continue
		}
		if !inHunk || strings.HasPrefix(raw, "diff ") || strings.HasPrefix(raw, "index ") ||
			strings.HasPrefix(raw, "---") || strings.HasPrefix(raw, "+++") || strings.HasPrefix(raw, "\\") {
			continue
		}
		switch {
		case strings.HasPrefix(raw, "+"):
			out = append(out, patchLine{added: true, text: raw[1:], newLine: newLine})
			newLine++
		case strings.HasPrefix(raw, "-"):
			out = append(out, patchLine{deleted: true, text: raw[1:], oldLine: oldLine})
			oldLine++
		default:
			out = append(out, patchLine{text: strings.TrimPrefix(raw, " "), newLine: newLine, oldLine: oldLine})
			newLine++
			oldLine++
		}
	}
	return out
}

func hunkNewStart(header string) (int, bool) {
	return hunkStart(header, " +")
}

// hunkStart returns the start line after marker, " -" or " +", in a hunk header.
func hunkStart(header, marker string) (int, bool) {
	at := strings.Index(header, marker)
	if at < 0 {
		return 0, false
	}
	rest := header[at+len(marker):]
	end := 0
	for end < len(rest) && rest[end] >= '0' && rest[end] <= '9' {
		end++
	}
	if end == 0 {
		return 0, false
	}
	n, err := strconv.Atoi(rest[:end])
	if err != nil {
		return 0, false
	}
	return n, true
}
