package tui

import (
	"errors"
	"fmt"
	"slices"
	"strings"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// UnreadCount returns the number of unread open items in the inbox, excluding
// notes.
func (m *Model) UnreadCount() int {
	return m.unreadCount
}

// SetClassFilter restricts the list to items matching classes (empty shows all).
func (m *Model) SetClassFilter(classes []string) {
	m.classFilter = append([]string(nil), classes...)
	m.refilter(m.cursorID())
}

func (m *Model) isInboxFolder() bool {
	return m.folder >= 0 && m.folder < len(Folders) && Folders[m.folder].State == decide.StateOpen
}

func (m *Model) selectedApproval() (decide.Item, bool) {
	if m.mode != modeList || !m.isInboxFolder() || m.cursor < 0 || m.cursor >= len(m.entries) {
		return decide.Item{}, false
	}
	e := m.entries[m.cursor]
	if e.ApprovalWaiting && len(e.Approvals) > 0 {
		return e.Approvals[0], true
	}
	if e.Item.EffectiveClass() == decide.ClassApproval && e.Item.State == decide.StateOpen {
		return e.Item, true
	}
	return decide.Item{}, false
}

func (m *Model) answerApprovalFromList(r rune) (bool, error) {
	optKey, ok := decide.ApprovalKeyOption(r)
	if !ok {
		return false, nil
	}
	card, hasApp := m.selectedApproval()
	if !hasApp {
		return false, nil
	}
	if optKey == "edit" {
		if err := m.openRow(); err != nil {
			return true, err
		}
		m.beginApprovalEdit(&card)
		return true, nil
	}
	updated, err := m.store().Answer(card.ID, optKey)
	var notifyErr *NotifyError
	switch {
	case errors.As(err, &notifyErr):
		m.status = err.Error()
	case err != nil:
		m.status = err.Error()
		return true, nil
	default:
		m.status = "answered " + updated.ID
	}
	return true, m.Reload()
}

func (m *Model) beginApprovalEdit(card *decide.Item) {
	if card == nil {
		return
	}
	prefill := card.Verbatim
	if prefill == "" {
		prefill = "edit"
	}
	m.pendingAnswerID = card.ID
	m.pendingAnswerKey = "edit"
	m.pendingAnswerPrefill = prefill
	m.reply = prefill
	syncField(&m.replyField, m.reply)
	m.replyField.CursorEnd()
	m.focusReply()
	m.replyField.Focus()
	m.status = "editing approval " + card.ID
}

func (m *Model) approvalCardLines(card *decide.Item, width int) []screenLine {
	var lines []screenLine
	lines = append(lines, screenLine{})
	header := fmt.Sprintf("approval waiting: %s", card.ID)
	if card.Question != "" {
		header += "  " + card.Question
	}
	lines = append(lines, screenLine{text: plainText(header)})
	if card.Verbatim != "" {
		for _, line := range strings.Split(card.Verbatim, "\n") {
			for _, ch := range wrapIndexed(line, width) {
				lines = append(lines, screenLine{text: plainText(ch.text)})
			}
		}
	}
	for i, opt := range card.Options {
		if i >= 9 {
			break
		}
		mark := ""
		if opt.Recommended {
			mark = " (recommended)"
		}
		label := fmt.Sprintf("%d  %s%s", i+1, opt.Label, mark)
		useStyles()
		for _, ch := range wrapIndexed(label, width) {
			lines = append(lines, screenLine{
				text: optionButton.Render(ch.text),
				hits: []hit{{kind: hitOption, n: i}},
			})
		}
	}
	return lines
}

var classFilterKeys = []struct {
	key   rune
	class string
	label string
}{
	{'d', decide.ClassDecision, "[d]ecision"},
	{'a', decide.ClassApproval, "[a]pproval"},
	{'r', decide.ClassRequest, "[r]equest"},
	{'q', decide.ClassFact, "[q]fact"},
	{'i', decide.ClassInbound, "[i]nbound"},
	{'t', decide.ClassTask, "[t]ask"},
	{'n', decide.ClassNote, "[n]ote"},
}

func (m *Model) toggleClassFilter(class string) {
	if idx := slices.Index(m.classFilter, class); idx >= 0 {
		m.classFilter = slices.Delete(m.classFilter, idx, idx+1)
	} else {
		m.classFilter = append(m.classFilter, class)
	}
	m.refilter(m.cursorID())
}

func (m *Model) classFilterKey(r rune) (bool, error) {
	switch r {
	case '\r', '\n', 'f':
		m.classFiltering = false
		return false, nil
	case '\x1b':
		m.classFiltering = false
		m.SetClassFilter(nil)
		return false, nil
	case '0', 'c':
		m.SetClassFilter(nil)
		return false, nil
	}
	for _, spec := range classFilterKeys {
		if r == spec.key {
			m.toggleClassFilter(spec.class)
			return false, nil
		}
	}
	return false, nil
}

func (m *Model) classFilterPrompt() screenLine {
	var parts []string
	for _, spec := range classFilterKeys {
		label := spec.label
		if slices.Contains(m.classFilter, spec.class) {
			label = "*" + label
		}
		parts = append(parts, label)
	}
	return screenLine{text: "filter class: " + strings.Join(parts, " ") + "  0/c clear  enter/esc"}
}

func (m *Model) applyFolderCounts(all []decide.Item) error {
	for _, it := range all {
		for i, folder := range Folders {
			if i == 0 {
				continue
			}
			if folder.Snoozed == it.Snoozed() && (folder.Snoozed || decide.InState(&it, folder.State)) {
				m.counts[i]++
				m.folderTexts[i] = append(m.folderTexts[i], searchText(&it))
				m.folderRows[i] = append(m.folderRows[i], it)
			}
		}
	}
	return m.applyInboxFolder(all)
}

func (m *Model) applyInboxFolder(all []decide.Item) error {
	openItems, err := decide.Select(all, decide.StateOpen)
	if err != nil {
		return fmt.Errorf("select decide inbox: %w", err)
	}
	m.unreadCount = decide.UnreadCount(openItems)
	inboxEntries := decide.InboxEntries(openItems, nil)
	m.counts[0] = len(inboxEntries)
	for _, e := range inboxEntries {
		m.folderTexts[0] = append(m.folderTexts[0], searchText(&e.Item))
		m.folderRows[0] = append(m.folderRows[0], e.Item)
	}
	return nil
}

func (m *Model) promptStatusLine() (screenLine, bool) {
	if m.searching {
		return m.searchPrompt(), true
	}
	if m.classFiltering {
		return m.classFilterPrompt(), true
	}
	return screenLine{}, false
}

func (m *Model) classFilterHeader() (screenLine, bool) {
	if len(m.classFilter) == 0 || m.composing || m.mode != modeList {
		return screenLine{}, false
	}
	return screenLine{text: "class: " + strings.Join(m.classFilter, ", ")}, true
}
