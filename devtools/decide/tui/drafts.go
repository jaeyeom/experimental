package tui

import (
	"errors"
	"fmt"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
)

const draftDebounce = 1500 * time.Millisecond

type draftTickMsg struct {
	kind composerKind
	seq  int
}

type draftState struct {
	key      string
	baseRev  int64
	dirty    bool
	lastEdit time.Time
	armed    bool
	seq      int
}

type conflictAction int

const (
	conflictKeepCurrent conflictAction = iota
	conflictUseOther
	conflictMergeBoth
)

func (m *Model) draftBackend() (DraftBackend, bool) {
	if m.backend == nil && m.Dir == "" {
		return nil, false
	}
	db, ok := m.store().(DraftBackend)
	return db, ok
}

func (m *Model) loadDrafts() {
	db, ok := m.draftBackend()
	if !ok {
		return
	}
	list, err := db.ListDrafts()
	if err != nil {
		return
	}
	m.applyDrafts(list)
}

func (m *Model) applyDrafts(list []decide.Draft) {
	next := make(map[string]decide.Draft, len(list))
	for _, d := range list {
		next[d.Key] = d
	}
	m.drafts = next
	if m.detail != nil && m.outboxEditing == "" {
		m.syncOpenDetailDrafts()
	}
	if m.composing && m.outboxEditing == "" && !m.composeDraft.dirty {
		if key := m.draftKey(composerNote); key != "" {
			if d, ok := m.drafts[key]; ok {
				m.composeDraft.key = d.Key
				m.composeDraft.baseRev = d.Rev
			}
		}
	}
}

func (m *Model) syncOpenDetailDrafts() {
	if m.detail == nil {
		return
	}
	rKey := decide.ReplyDraftKey(m.detail.ID)
	if d, ok := m.drafts[rKey]; ok && !m.replyDraft.dirty {
		if m.replyDraft.key != rKey {
			m.replyDraft = draftState{key: rKey, baseRev: d.Rev}
			m.reply = d.Text
			m.replyField.SetValue(d.Text)
		} else {
			m.replyDraft.baseRev = d.Rev
			if !m.replyFocus && !m.replySend && len(d.Conflicts) == 0 && d.Text != m.reply {
				m.reply = d.Text
				m.replyField.SetValue(d.Text)
			}
		}
	}
	aKey := decide.AnswerDraftKey(m.detail.ID)
	if d, ok := m.drafts[aKey]; ok && !m.answerDraft.dirty {
		if m.answerDraft.key != aKey {
			m.answerDraft = draftState{key: aKey, baseRev: d.Rev}
			m.draft = d.Text
			m.free.SetValue(d.Text)
		} else {
			m.answerDraft.baseRev = d.Rev
			if !m.answerFocus && len(d.Conflicts) == 0 && d.Text != m.draft {
				m.draft = d.Text
				m.free.SetValue(d.Text)
			}
		}
	}
}

func (m *Model) restoreOpenDetailDrafts(id string) {
	if m.outboxEditing != "" {
		return
	}
	if m.drafts == nil {
		m.loadDrafts()
	}
	rKey := decide.ReplyDraftKey(id)
	m.replyDraft = draftState{key: rKey}
	if d, ok := m.drafts[rKey]; ok {
		m.reply = d.Text
		m.replyField.SetValue(d.Text)
		m.replyDraft.baseRev = d.Rev
	}
	aKey := decide.AnswerDraftKey(id)
	m.answerDraft = draftState{key: aKey}
	if d, ok := m.drafts[aKey]; ok {
		m.draft = d.Text
		m.free.SetValue(d.Text)
		m.answerDraft.baseRev = d.Rev
	}
}

func (m *Model) draftStateFor(kind composerKind) *draftState {
	switch kind {
	case composerReply:
		return &m.replyDraft
	case composerAnswer:
		return &m.answerDraft
	case composerNote:
		return &m.composeDraft
	default:
		return nil
	}
}

func (m *Model) draftKey(kind composerKind) string {
	if m.outboxEditing != "" {
		return ""
	}
	switch kind {
	case composerReply:
		if m.detail == nil {
			return ""
		}
		return decide.ReplyDraftKey(m.detail.ID)
	case composerAnswer:
		if m.detail == nil {
			return ""
		}
		return decide.AnswerDraftKey(m.detail.ID)
	case composerNote:
		if m.forward != "" {
			return decide.ForwardDraftKey(m.forward)
		}
		return decide.DraftKeyCompose
	default:
		return ""
	}
}

func (m *Model) draftText(kind composerKind) string {
	switch kind {
	case composerReply:
		return m.reply
	case composerAnswer:
		return m.draft
	case composerNote:
		return m.note
	default:
		return ""
	}
}

func (m *Model) buildDraft(kind composerKind, key, text string) decide.Draft {
	d := decide.Draft{
		Key:  key,
		Text: text,
	}
	switch kind {
	case composerReply:
		d.Kind = decide.DraftKindReply
		if m.detail != nil {
			d.ItemID = m.detail.ID
		}
	case composerAnswer:
		d.Kind = decide.DraftKindAnswer
		if m.detail != nil {
			d.ItemID = m.detail.ID
		}
	case composerNote:
		if m.forward != "" {
			d.Kind = decide.DraftKindForward
			d.ItemID = m.forward
		} else {
			d.Kind = decide.DraftKindCompose
		}
		d.Forward = m.forward
		if tg, ok := m.selectedTarget(); ok {
			d.To = tg.Name
			d.Bus = tg.onBus()
		}
	}
	return d
}

func (m *Model) markDraftDirty(kind composerKind) {
	key := m.draftKey(kind)
	st := m.draftStateFor(kind)
	if key == "" || st == nil {
		return
	}
	if st.key != key {
		st.key = key
		if d, ok := m.drafts[key]; ok {
			st.baseRev = d.Rev
		} else {
			st.baseRev = 0
		}
	}
	st.dirty = true
	st.lastEdit = m.now()
	st.seq++
	st.armed = true
}

func (m *Model) cancelDraftDebounce(kind composerKind) {
	st := m.draftStateFor(kind)
	if st == nil {
		return
	}
	st.dirty = false
	st.armed = false
	st.seq++
}

func (m *Model) draftTick() tea.Cmd {
	var cmds []tea.Cmd
	for _, kind := range []composerKind{composerReply, composerAnswer, composerNote} {
		st := m.draftStateFor(kind)
		if st == nil || !st.armed {
			continue
		}
		st.armed = false
		seq := st.seq
		k := kind
		cmds = append(cmds, tea.Tick(draftDebounce, func(time.Time) tea.Msg {
			return draftTickMsg{kind: k, seq: seq}
		}))
	}
	if len(cmds) == 0 {
		return nil
	}
	if len(cmds) == 1 {
		return cmds[0]
	}
	return tea.Batch(cmds...)
}

func (m *Model) finishDraftDebounce(kind composerKind, seq int) {
	st := m.draftStateFor(kind)
	if st == nil || !st.dirty || st.seq != seq {
		return
	}
	if m.now().Sub(st.lastEdit) < draftDebounce {
		return
	}
	m.flushDraft(kind)
}

func (m *Model) flushAllDrafts() {
	m.flushDraft(composerReply)
	m.flushDraft(composerAnswer)
	m.flushDraft(composerNote)
}

func (m *Model) flushDraft(kind composerKind) {
	key := m.draftKey(kind)
	st := m.draftStateFor(kind)
	if key == "" || st == nil || !st.dirty {
		return
	}
	st.dirty = false
	st.armed = false
	db, ok := m.draftBackend()
	if !ok {
		return
	}
	text := m.draftText(kind)
	if strings.TrimSpace(text) == "" {
		if _, existed := m.drafts[key]; existed || st.baseRev > 0 {
			if err := db.DeleteDraft(key, ""); err != nil {
				m.status = err.Error()
				return
			}
			delete(m.drafts, key)
			st.baseRev = 0
		}
		return
	}
	d := m.buildDraft(kind, key, text)
	saved, conflict, err := db.PutDraft(d, st.baseRev)
	if err != nil {
		if errors.Is(err, decide.ErrDraftTooLarge) {
			m.status = "draft exceeds 64 KiB limit; kept in memory"
			return
		}
		m.status = err.Error()
		return
	}
	if m.drafts == nil {
		m.drafts = make(map[string]decide.Draft)
	}
	m.drafts[key] = saved
	st.baseRev = saved.Rev
	if conflict {
		m.status = "draft conflict detected"
	}
}

func (m *Model) deleteDraft(kind composerKind) {
	m.cancelDraftDebounce(kind)
	key := m.draftKey(kind)
	st := m.draftStateFor(kind)
	if key == "" {
		return
	}
	if st != nil {
		st.baseRev = 0
	}
	db, ok := m.draftBackend()
	if !ok {
		delete(m.drafts, key)
		return
	}
	if _, existed := m.drafts[key]; existed {
		if err := db.DeleteDraft(key, ""); err != nil {
			m.status = err.Error()
			return
		}
		delete(m.drafts, key)
	}
}

func (m *Model) itemDraftMark(itemID string) string {
	if len(m.drafts) == 0 {
		return ""
	}
	var hasDraft bool
	for _, key := range []string{decide.ReplyDraftKey(itemID), decide.AnswerDraftKey(itemID)} {
		d, ok := m.drafts[key]
		if !ok {
			continue
		}
		if len(d.Conflicts) > 0 {
			return "draft!"
		}
		hasDraft = true
	}
	if hasDraft {
		return "draft"
	}
	return ""
}

func (m *Model) draftFor(kind composerKind) (decide.Draft, bool) {
	key := m.draftKey(kind)
	if key == "" || len(m.drafts) == 0 {
		return decide.Draft{}, false
	}
	d, ok := m.drafts[key]
	return d, ok
}

func (m *Model) hasDraftConflict(kind composerKind) bool {
	d, ok := m.draftFor(kind)
	return ok && len(d.Conflicts) > 0
}

func (m *Model) checkComposeRestore() {
	if m.note != "" || m.forward != "" || m.outboxEditing != "" {
		m.composeRestorePrompt = false
		return
	}
	if m.drafts == nil {
		m.loadDrafts()
	}
	if d, ok := m.drafts[decide.DraftKeyCompose]; ok && strings.TrimSpace(d.Text) != "" {
		m.composeRestorePrompt = true
	} else {
		m.composeRestorePrompt = false
	}
}

func (m *Model) restoreComposeDraft() {
	m.composeRestorePrompt = false
	d, ok := m.drafts[decide.DraftKeyCompose]
	if !ok {
		return
	}
	m.note = d.Text
	m.noteField.SetValue(d.Text)
	m.forward = d.Forward
	m.composeDraft = draftState{key: decide.DraftKeyCompose, baseRev: d.Rev}
	m.applyDraftTarget(d.To, d.Bus)
	m.status = "restored compose draft"
}

func (m *Model) applyDraftTarget(to string, bus bool) {
	if to == "" {
		return
	}
	if !m.selectName(to) && bus {
		m.composeDead = append(m.composeDead, Target{Name: to, Bus: true, Dead: true})
		m.selectName(to)
	}
}

func (m *Model) discardComposeDraft() {
	m.composeRestorePrompt = false
	m.deleteDraft(composerNote)
	m.clearNote()
	m.status = "discarded compose draft"
}

func (m *Model) composeRestoreLines(width int) []screenLine {
	if !m.composeRestorePrompt {
		return nil
	}
	d, ok := m.drafts[decide.DraftKeyCompose]
	if !ok {
		return nil
	}
	when := ""
	if !d.UpdatedAt.IsZero() {
		when = " (" + stamp(d.UpdatedAt, m.now(), m.zone()) + ")"
	}
	prefix := fmt.Sprintf("Saved draft%s: ", when)
	useStyles()
	restoreBtn := optionButton.Render("[r] Restore")
	discardBtn := optionButton.Render("[d] Discard")
	pW := lipgloss.Width(prefix)
	rW := lipgloss.Width(restoreBtn)
	dW := lipgloss.Width(discardBtn)
	hits := []hit{
		{kind: hitComposeRestore, x0: pW + 1, x1: pW + rW},
		{kind: hitComposeDiscard, x0: pW + rW + 3, x1: pW + rW + 2 + dW},
	}
	line := screenLine{
		text: prefix + restoreBtn + "  " + discardBtn,
		hits: hits,
	}
	if width > 0 {
		line = fitLine(line, width)
	}
	return []screenLine{line}
}

func (m *Model) conflictBannerLines(kind composerKind, width int) []screenLine {
	d, ok := m.draftFor(kind)
	if !ok || len(d.Conflicts) == 0 {
		return nil
	}
	c := d.Conflicts[0]
	who := c.UpdatedBy
	if who == "" {
		who = "other"
	}
	when := stamp(c.UpdatedAt, m.now(), m.zone())
	header := fmt.Sprintf("Conflict with %s (%s):", who, when)
	useStyles()
	kBtn := optionButton.Render("[k] Keep current")
	uBtn := optionButton.Render("[u] Use other")
	mBtn := optionButton.Render("[m] Merge both")
	kW := lipgloss.Width(kBtn)
	uW := lipgloss.Width(uBtn)
	mW := lipgloss.Width(mBtn)
	actions := kBtn + "  " + uBtn + "  " + mBtn
	hits := []hit{
		{kind: hitConflictKeep, n: int(kind), x0: 1, x1: kW},
		{kind: hitConflictUse, n: int(kind), x0: kW + 3, x1: kW + 2 + uW},
		{kind: hitConflictMerge, n: int(kind), x0: kW + uW + 5, x1: kW + uW + 4 + mW},
	}
	lines := []screenLine{
		{text: plainText(header)},
		{text: actions, hits: hits},
	}
	if width > 0 {
		for i := range lines {
			lines[i] = fitLine(lines[i], width)
		}
	}
	return lines
}

func (m *Model) resolveConflict(kind composerKind, action conflictAction) {
	d, ok := m.draftFor(kind)
	if !ok || len(d.Conflicts) == 0 {
		return
	}
	db, hasDB := m.draftBackend()
	if !hasDB {
		return
	}
	c := d.Conflicts[0]
	st := m.draftStateFor(kind)
	m.cancelDraftDebounce(kind)

	switch action {
	case conflictKeepCurrent:
		m.resolveKeepCurrent(db, kind, d, c, st)
	case conflictUseOther:
		m.resolveUseOther(db, kind, d, c, st)
	case conflictMergeBoth:
		m.resolveMergeBoth(db, kind, d, c, st)
	}
}

func (m *Model) resolveKeepCurrent(db DraftBackend, kind composerKind, d decide.Draft, c decide.DraftConflict, st *draftState) {
	curText := m.draftText(kind)
	if err := db.DeleteDraft(d.Key, c.ID); err != nil {
		m.status = err.Error()
		return
	}
	m.loadDrafts()
	if curText != "" && curText != d.Text {
		m.saveConflictResolution(db, kind, d.Key, curText, st)
	} else if updated, ok := m.drafts[d.Key]; ok && st != nil {
		st.baseRev = updated.Rev
	}
	m.status = "kept current draft"
}

func (m *Model) resolveUseOther(db DraftBackend, kind composerKind, d decide.Draft, c decide.DraftConflict, st *draftState) {
	if kind == composerNote {
		m.applyComposeConflictTarget(c)
	}
	if err := db.DeleteDraft(d.Key, c.ID); err != nil {
		m.status = err.Error()
		return
	}
	m.loadDrafts()
	m.setComposerText(kind, c.Text)
	if m.saveConflictResolution(db, kind, d.Key, c.Text, st) {
		m.status = "using other draft"
	}
}

func (m *Model) resolveMergeBoth(db DraftBackend, kind composerKind, d decide.Draft, c decide.DraftConflict, st *draftState) {
	curText := m.draftText(kind)
	if curText == "" {
		curText = d.Text
	}
	who := c.UpdatedBy
	if who == "" {
		who = "other"
	}
	when := c.UpdatedAt.UTC().Format(time.RFC3339)
	merged := formatMergeConflict(curText, c.Text, who, when)
	if err := db.DeleteDraft(d.Key, c.ID); err != nil {
		m.status = err.Error()
		return
	}
	m.loadDrafts()
	m.setComposerText(kind, merged)
	if m.saveConflictResolution(db, kind, d.Key, merged, st) {
		m.status = "merged conflict; edit and save"
	}
}

func (m *Model) applyComposeConflictTarget(c decide.DraftConflict) {
	m.forward = c.Forward
	m.applyDraftTarget(c.To, c.Bus)
}

func (m *Model) saveConflictResolution(db DraftBackend, kind composerKind, key, text string, st *draftState) bool {
	var baseRev int64
	if updated, ok := m.drafts[key]; ok {
		baseRev = updated.Rev
	}
	saved, _, err := db.PutDraft(m.buildDraft(kind, key, text), baseRev)
	if err != nil {
		if errors.Is(err, decide.ErrDraftTooLarge) {
			m.status = "merged draft exceeds 64 KiB limit; kept in memory"
			return false
		}
		m.status = err.Error()
		return false
	}
	m.drafts[key] = saved
	if st != nil {
		st.baseRev = saved.Rev
	}
	return true
}

func formatMergeConflict(current, other, updatedBy, timestamp string) string {
	var b strings.Builder
	b.WriteString("<<<<<<< current\n")
	b.WriteString(current)
	if current != "" && !strings.HasSuffix(current, "\n") {
		b.WriteByte('\n')
	}
	b.WriteString("=======\n")
	b.WriteString(other)
	if other != "" && !strings.HasSuffix(other, "\n") {
		b.WriteByte('\n')
	}
	fmt.Fprintf(&b, ">>>>>>> %s %s", updatedBy, timestamp)
	return b.String()
}
