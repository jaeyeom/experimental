package tui

import (
	"context"
	"fmt"
	"os/exec"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
)

// OpenRunner runs an opener argv; the URL is the last argument.
type OpenRunner func(ctx context.Context, argv []string) error

// openTimeout bounds one opener run. It runs off the update loop, so the
// inbox never waits for it.
const openTimeout = 5 * time.Second

// statusURLWidth is the widest URL the status line shows before it
// shortens the middle.
const statusURLWidth = 60

// directPickMax is the most links for which one digit opens a link
// without Enter.
const directPickMax = 9

const linkPickHint = "j/k move  enter opens  number then enter  esc cancels"

// openedMsg is the result of one opener run.
type openedMsg struct {
	url string
	err error
}

// defaultOpenCommand is the opener for goos when config sets none.
func defaultOpenCommand(goos string) []string {
	if goos == "darwin" {
		return []string{"open"}
	}
	return []string{"xdg-open"}
}

// openSelectedLink opens the only link. Several links open the picker.
func (m *Model) openSelectedLink() error {
	links := itemLinks(m.detail)
	switch len(links) {
	case 0:
		m.status = "no link"
	case 1:
		m.openURL(links[0])
	default:
		m.linkPick = true
		m.linkCursor = 0
		m.linkDigits = ""
	}
	return nil
}

// linkPickKey drives the picker. j and k move, Enter opens the row or
// the typed number, and Esc cancels. With at most nine links a digit
// opens its link at once; with more, digits build a number for Enter.
func (m *Model) linkPickKey(r rune) (bool, error) {
	links := itemLinks(m.detail)
	switch {
	case r == '\x1b':
		m.closeLinkPick()
		m.status = ""
	case r == 'j':
		m.moveLinkPick(1)
	case r == 'k':
		m.moveLinkPick(-1)
	case r == '\r':
		i := m.linkCursor
		if m.linkDigits != "" {
			n, _ := strconv.Atoi(m.linkDigits)
			i = n - 1
		}
		m.pickLink(links, i)
	case r == '\b' || r == '\x7f':
		m.linkDigits = dropLastRune(m.linkDigits)
	case r >= '0' && r <= '9':
		m.typeLinkDigit(links, r)
	}
	return false, nil
}

func (m *Model) typeLinkDigit(links []string, r rune) {
	digits := m.linkDigits + string(r)
	n, _ := strconv.Atoi(digits)
	if n == 0 {
		return
	}
	if len(links) <= directPickMax {
		m.pickLink(links, n-1)
		return
	}
	m.linkDigits = digits
	if n <= len(links) {
		m.linkCursor = n - 1
	}
	m.status = ""
}

func (m *Model) moveLinkPick(delta int) {
	n := len(itemLinks(m.detail))
	m.linkCursor = max(min(m.linkCursor+delta, n-1), 0)
	m.linkDigits = ""
}

// pickLink opens links[i] and closes the picker. A number out of range
// keeps the picker open.
func (m *Model) pickLink(links []string, i int) {
	if i < 0 || i >= len(links) {
		m.linkDigits = ""
		m.status = "no such link"
		return
	}
	m.closeLinkPick()
	m.openURL(links[i])
}

func (m *Model) closeLinkPick() {
	m.linkPick = false
	m.linkCursor = 0
	m.linkDigits = ""
}

// openURL keeps url as the current link and asks Update to open it.
func (m *Model) openURL(url string) {
	if url == "" {
		m.status = "no link"
		return
	}
	m.linkURL = url
	m.openWant = url
	m.status = "opening " + shortURL(url, statusURLWidth)
}

// openArgv is the configured opener, or the platform default, with url
// as the last argument.
func (m *Model) openArgv(url string) []string {
	argv := slices.Clone(m.OpenCommand)
	if len(argv) == 0 {
		argv = defaultOpenCommand(runtime.GOOS)
	}
	return append(argv, url)
}

// openCmd runs the open openURL asked for off the update loop.
func (m *Model) openCmd() tea.Cmd {
	if m.openWant == "" {
		return nil
	}
	url := m.openWant
	m.openWant = ""
	argv := m.openArgv(url)
	run := m.runOpen
	if run == nil {
		run = execOpen
	}
	return func() tea.Msg {
		ctx, cancel := context.WithTimeout(context.Background(), openTimeout)
		defer cancel()
		return openedMsg{url: url, err: run(ctx, argv)}
	}
}

func (m *Model) finishOpen(msg openedMsg) {
	if msg.err != nil {
		m.status = "open failed: " + msg.err.Error()
		return
	}
	m.status = "opened " + shortURL(msg.url, statusURLWidth)
}

// execOpen runs argv without a shell. Its output is discarded so a
// browser it starts cannot draw over the inbox or hold the run open.
func execOpen(ctx context.Context, argv []string) error {
	if len(argv) == 0 {
		return fmt.Errorf("open: empty command")
	}
	// The program comes from the user's own config or the platform
	// default; the URL is one argument and no shell parses it.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...) //nolint:gosec // G204: see above
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("%s: %w", argv[0], err)
	}
	return nil
}

// shortURL fits url in width runes. A long URL drops its scheme, then
// its middle, keeping the host and the tail of the path.
func shortURL(url string, width int) string {
	if len([]rune(url)) <= width {
		return url
	}
	if _, rest, ok := strings.Cut(url, "://"); ok {
		url = rest
	}
	r := []rune(url)
	if len(r) <= width {
		return url
	}
	if width < 3 {
		return string(r[:max(width, 0)])
	}
	host, _, _ := strings.Cut(url, "/")
	head := min(len([]rune(host)), width/2)
	tail := width - head - 1
	return string(r[:head]) + "…" + string(r[len(r)-tail:])
}

// linkPickLines is the picker: a title, then one numbered row per link.
func (m *Model) linkPickLines() []screenLine {
	links := itemLinks(m.detail)
	title := fmt.Sprintf("open link (1-%d)", len(links))
	if m.linkDigits != "" {
		title += ": " + withCursor(m.linkDigits, len([]rune(m.linkDigits)))
	}
	lines := []screenLine{{text: plainText(title)}}
	numW := len(strconv.Itoa(len(links)))
	width := m.contentWidth()
	if width <= 0 {
		width = 80
	}
	urlW := max(width-2-2-numW-2, 10)
	for i, link := range links {
		marker := "  "
		if i == m.linkCursor {
			marker = "> "
		}
		text := fmt.Sprintf("%s%*d  %s", marker, numW, i+1, shortURL(link, urlW))
		lines = append(lines, screenLine{text: plainText(text)})
	}
	return boxLines(lines)
}
