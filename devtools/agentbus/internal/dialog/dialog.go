// Package dialog detects pending dialogs and input prompts on agent pane screens.
package dialog

import (
	"fmt"
	"regexp"
	"strings"
	"unicode"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/config"
)

// PromptMarker starts Claude Code's input line.
const PromptMarker = "❯"

// DefaultComposerPlaceholders are the known post-error and idle
// placeholder suggestions treated as empty when style info is absent.
var DefaultComposerPlaceholders = []string{
	"Once fixed, resend your message",
}

// tailLines is how many of the last screen lines a dialog is looked for in.
const tailLines = 20

// maxMatchRunes limits the single-line matched text returned by Detect.
const maxMatchRunes = 120

// Rule is a compiled regex rule for detecting a dialog or input prompt.
type Rule struct {
	Name    string
	Pattern *regexp.Regexp
}

// Match describes a matched dialog or input prompt on a screen.
type Match struct {
	Rule string
	Text string
}

// CompileRules compiles config.DialogRule entries into executable Rules.
func CompileRules(cfg []config.DialogRule) ([]Rule, error) {
	if len(cfg) == 0 {
		return nil, nil
	}
	out := make([]Rule, 0, len(cfg))
	for _, r := range cfg {
		if err := config.ValidateDialogRule(r.Name, r.Pattern); err != nil {
			return nil, fmt.Errorf("%w", err)
		}
		re, err := regexp.Compile(r.Pattern)
		if err != nil {
			return nil, fmt.Errorf("dialog_rules[%q]: invalid regex %q: %w", r.Name, r.Pattern, err)
		}
		out = append(out, Rule{Name: r.Name, Pattern: re})
	}
	return out, nil
}

// claudeMenuOption is the selected option of a Claude Code menu.
var claudeMenuOption = regexp.MustCompile(`^\s*` + PromptMarker + `\s*\d+\.\s`)

// Detect checks screen for a built-in dialog (by agent kind) or any matching
// extra regex rule in the trailing lines. For a Claude Code screen whose normal
// input box parses, Detect reports no dialog so text in prior replies never
// causes a false positive.
func Detect(kind, screen string, extra []Rule) (Match, bool) {
	if kind == "claude" {
		if _, ok := ClaudeComposerEmpty(screen, DefaultComposerPlaceholders); ok {
			return Match{}, false
		}
	}
	plain := StripANSI(strings.TrimRight(screen, "\n"))
	lines := strings.Split(plain, "\n")
	lines = lines[max(0, len(lines)-tailLines):]
	if kind == "claude" {
		if m, ok := detectClaudeBuiltins(lines); ok {
			return m, true
		}
	}
	for _, rule := range extra {
		for _, line := range lines {
			if rule.Pattern.MatchString(line) {
				return Match{Rule: rule.Name, Text: truncateLine(line, maxMatchRunes)}, true
			}
		}
	}
	return Match{}, false
}

func detectClaudeBuiltins(lines []string) (Match, bool) {
	for _, line := range lines {
		if strings.Contains(line, "Esc to cancel") {
			return Match{Rule: "question prompt", Text: truncateLine(line, maxMatchRunes)}, true
		}
	}
	for _, line := range lines {
		if strings.Contains(line, "Do you want to") {
			return Match{Rule: "permission prompt", Text: truncateLine(line, maxMatchRunes)}, true
		}
	}
	for _, line := range lines {
		if claudeMenuOption.MatchString(line) {
			return Match{Rule: "permission prompt", Text: truncateLine(line, maxMatchRunes)}, true
		}
	}
	return Match{}, false
}

func truncateLine(s string, maxRunes int) string {
	s = strings.TrimSpace(strings.ReplaceAll(strings.ReplaceAll(s, "\r", " "), "\t", " "))
	if maxRunes <= 0 {
		return ""
	}
	count := 0
	for i := range s {
		if count == maxRunes {
			return s[:i] + "…"
		}
		count++
	}
	return s
}

// ClaudeComposerEmpty finds Claude Code's input box: the lines between the
// last two horizontal rules, the first starting with the prompt marker.
// The shell-mode marker "!" always counts as a draft.
func ClaudeComposerEmpty(screen string, placeholders []string) (empty, ok bool) {
	text, ok := ClaudeComposerDraftText(screen, placeholders)
	if !ok {
		return false, false
	}
	if strings.HasPrefix(text, "!") {
		return false, true
	}
	for _, line := range strings.Split(text, "\n") {
		if !blank(line) {
			return false, true
		}
	}
	return true, true
}

// ClaudeComposerText is the input box with the prompt marker removed.
// Shell mode keeps the "!" line. ok is false when the screen does not parse.
func ClaudeComposerText(screen string) (string, bool) {
	lines := strings.Split(StripANSI(screen), "\n")
	bottom := lastRule(lines, len(lines))
	if bottom < 0 {
		return "", false
	}
	top := lastRule(lines, bottom)
	if top < 0 || top+1 >= bottom {
		return "", false
	}
	box := append([]string(nil), lines[top+1:bottom]...)
	switch {
	case strings.HasPrefix(box[0], "!"):
		return strings.Join(box, "\n"), true
	case !strings.HasPrefix(box[0], PromptMarker):
		return "", false
	}
	box[0] = strings.TrimPrefix(box[0], PromptMarker)
	return strings.Join(box, "\n"), true
}

// ClaudeComposerDraftText extracts Claude Code's input box, removes ANSI
// styling, and treats a known placeholder as empty. Dim styling alone does
// not make composer text empty: Claude also dims collapsed pasted text.
func ClaudeComposerDraftText(screen string, placeholders []string) (string, bool) {
	rawLines := strings.Split(screen, "\n")
	plainLines := make([]string, len(rawLines))
	for i, l := range rawLines {
		plainLines[i] = StripANSI(l)
	}
	bottom := lastRule(plainLines, len(plainLines))
	if bottom < 0 {
		return "", false
	}
	top := lastRule(plainLines, bottom)
	if top < 0 || top+1 >= bottom {
		return "", false
	}
	firstPlain := plainLines[top+1]
	switch {
	case strings.HasPrefix(firstPlain, "!"):
		return strings.Join(plainLines[top+1:bottom], "\n"), true
	case !strings.HasPrefix(firstPlain, PromptMarker):
		return "", false
	}
	out := make([]string, 0, bottom-top-1)
	for i := top + 1; i < bottom; i++ {
		plain := StripANSI(rawLines[i])
		if i == top+1 {
			plain = strings.TrimPrefix(plain, PromptMarker)
		}
		out = append(out, plain)
	}
	joined := strings.Join(out, "\n")
	if isPlaceholderOnly(out, placeholders) {
		return "", true
	}
	return joined, true
}

func isPlaceholderOnly(lines, placeholders []string) bool {
	var nonBlank []string
	for _, l := range lines {
		if !blank(l) {
			nonBlank = append(nonBlank, strings.TrimFunc(l, unicode.IsSpace))
		}
	}
	if len(nonBlank) != 1 {
		return false
	}
	for _, p := range placeholders {
		if p != "" && nonBlank[0] == p {
			return true
		}
	}
	return false
}

// StripANSI removes CSI and OSC escape sequences from s.
func StripANSI(s string) string {
	var b strings.Builder
	b.Grow(len(s))
	for i := 0; i < len(s); {
		if s[i] == '\x1b' {
			i = skipEscape(s, i)
			continue
		}
		b.WriteByte(s[i])
		i++
	}
	return b.String()
}

func skipEscape(s string, i int) int {
	if i+1 >= len(s) {
		return i + 1
	}
	switch s[i+1] {
	case '[':
		return skipCSI(s, i)
	case ']':
		for j := i + 2; j < len(s); j++ {
			if s[j] == '\x07' {
				return j + 1
			}
			if s[j] == '\x1b' && j+1 < len(s) && s[j+1] == '\\' {
				return j + 2
			}
		}
		return len(s)
	default:
		return i + 2
	}
}

func skipCSI(s string, i int) int {
	for j := i + 2; j < len(s); j++ {
		if s[j] >= 0x40 && s[j] <= 0x7e {
			return j + 1
		}
	}
	return len(s)
}

func lastRule(lines []string, before int) int {
	for i := before - 1; i >= 0; i-- {
		line := strings.TrimSpace(lines[i])
		if line != "" && strings.Trim(line, "─") == "" {
			return i
		}
	}
	return -1
}

func blank(s string) bool {
	return strings.TrimFunc(s, unicode.IsSpace) == ""
}
