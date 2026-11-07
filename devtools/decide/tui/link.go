package tui

import (
	"encoding/base64"
	"fmt"
	"os/exec"
	"runtime"
	"strings"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// itemLinks lists the URLs an item shows, in the order the view draws them.
func itemLinks(it *decide.Item) []string {
	if it == nil {
		return nil
	}
	var out []string
	seen := map[string]bool{}
	add := func(s string) {
		if s == "" || seen[s] {
			return
		}
		seen[s] = true
		out = append(out, s)
	}
	texts := []string{it.Question}
	if len(it.Messages) > 0 {
		texts = texts[:0]
		for _, msg := range it.Messages {
			texts = append(texts, msg.Text)
		}
	}
	for _, text := range texts {
		for _, match := range urlPattern.FindAllString(text, -1) {
			add(match)
		}
	}
	for _, link := range it.Links {
		add(link)
	}
	return out
}

// openSelectedLink opens the only link. Several links ask for a number.
func (m *Model) openSelectedLink() error {
	links := itemLinks(m.detail)
	switch len(links) {
	case 0:
		m.status = "no link"
		return nil
	case 1:
		return m.openURL(links[0])
	default:
		m.linkPick = true
		m.status = fmt.Sprintf("link number (1-%d)", len(links))
		return nil
	}
}

func (m *Model) linkPickKey(r rune) (bool, error) {
	m.linkPick = false
	if r == '\x1b' {
		m.status = ""
		return false, nil
	}
	if r >= '1' && r <= '9' {
		return false, m.openLinkIndex(int(r - '1'))
	}
	m.status = "no such link"
	return false, nil
}

func (m *Model) openLinkIndex(i int) error {
	links := itemLinks(m.detail)
	if i < 0 || i >= len(links) {
		m.status = "no such link"
		return nil
	}
	return m.openURL(links[i])
}

// openURL opens a link with the platform opener and keeps the URL as current.
func (m *Model) openURL(url string) error {
	if url == "" {
		m.status = "no link"
		return nil
	}
	m.linkURL = url
	m.status = url
	open := m.runOpen
	if open == nil {
		open = openLink
	}
	if err := open(url); err != nil {
		m.status = err.Error()
	}
	return nil
}

func openCommand(goos string) string {
	if goos == "darwin" {
		return "open"
	}
	return "xdg-open"
}

func openLink(url string) error {
	var cmd *exec.Cmd
	switch openCommand(runtime.GOOS) {
	case "open":
		cmd = exec.Command("open", url) //nolint:gosec // G204: the URL is one argument, not a shell string
	default:
		cmd = exec.Command("xdg-open", url) //nolint:gosec // G204: the URL is one argument, not a shell string
	}
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("open link: %w", err)
	}
	return nil
}

// copyCurrentMessage copies the latest message, or the question when the
// thread has no messages.
func (m *Model) copyCurrentMessage() {
	if m.detail == nil {
		m.status = "nothing to copy"
		return
	}
	text := m.detail.Question
	if n := len(m.detail.Messages); n > 0 {
		text = m.detail.Messages[n-1].Text
	}
	if text == "" {
		m.status = "nothing to copy"
		return
	}
	m.copyText(text)
}

// copyCurrentLink copies the full URL of the current link.
func (m *Model) copyCurrentLink() {
	url := m.linkURL
	if url == "" {
		links := itemLinks(m.detail)
		if len(links) > 0 {
			url = links[0]
		}
	}
	if url == "" {
		m.status = "no link"
		return
	}
	m.copyText(url)
}

func (m *Model) copyText(text string) {
	wrote := m.writeOSC52(text)
	clip := m.copyClip
	if clip == nil {
		clip = copyClipboard
	}
	if err := clip(text); err != nil && !wrote {
		m.status = err.Error()
		return
	}
	m.status = "copied"
}

func (m *Model) writeOSC52(text string) bool {
	if m.out == nil {
		return false
	}
	payload := base64.StdEncoding.EncodeToString([]byte(text))
	_, err := fmt.Fprintf(m.out, "\x1b]52;c;%s\x07", payload)
	return err == nil
}

func copyClipboard(text string) error {
	return copyWith(text, execClip)
}

func copyWith(text string, run func(name string, args []string, text string) error) error {
	programs := []struct {
		name string
		args []string
	}{
		{name: "xclip", args: []string{"-selection", "clipboard"}},
		{name: "wl-copy"},
		{name: "pbcopy"},
	}
	var last error
	for _, program := range programs {
		err := run(program.name, program.args, text)
		if err == nil {
			return nil
		}
		last = err
	}
	if last == nil {
		last = fmt.Errorf("clipboard command not found")
	}
	return fmt.Errorf("copy text: %w", last)
}

func execClip(name string, _ []string, text string) error {
	var cmd *exec.Cmd
	switch name {
	case "xclip":
		cmd = exec.Command("xclip", "-selection", "clipboard")
	case "wl-copy":
		cmd = exec.Command("wl-copy")
	case "pbcopy":
		cmd = exec.Command("pbcopy")
	default:
		return fmt.Errorf("look up %s: unsupported clipboard command", name)
	}
	cmd.Stdin = strings.NewReader(text)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("run %s: %w", name, err)
	}
	return nil
}

func (m *Model) toggleMouse() {
	m.mouseOn = !m.mouseOn
	m.mouseSync = true
}

func (m *Model) mouseLabel() string {
	if m.mouseOn {
		return "mouse on  "
	}
	return "mouse off "
}
