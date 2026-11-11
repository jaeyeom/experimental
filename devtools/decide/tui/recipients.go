package tui

import (
	"context"
	"encoding/json"
	"fmt"
	"log/slog"
	"slices"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

const (
	// busListTimeout bounds `agentbus list --json` when the picker opens.
	busListTimeout = 500 * time.Millisecond
	// busCacheTTL is how long one bus listing serves the picker.
	busCacheTTL = 30 * time.Second
	// busSendTimeout bounds `agentbus send`, which only queues the note.
	busSendTimeout = 10 * time.Second
	// busPanePrefix marks a thread's notify pane as a bus name.
	busPanePrefix = "agentbus:"
	// pickerMaxRows caps the recipient dropdown.
	pickerMaxRows = 8
	// whenLimit truncates a bus name's --when text in the dropdown.
	whenLimit = 40
)

// pickerHint is the footer while the recipient list is focused.
const pickerHint = "type to filter  ↑/↓ C-n/C-p move  Enter pick  Esc clear/cancel  Tab text"

// BusRunner runs `agentbus` with args and returns its stdout.
type BusRunner = decide.AgentbusRunner

// busAgent is one object of `agentbus list --json`.
type busAgent struct {
	Name     string `json:"name"`
	When     string `json:"when"`
	Liveness string `json:"liveness"`
	Handle   string `json:"handle"`
}

func (m *Model) busRunner() BusRunner {
	if m.busRun != nil {
		return m.busRun
	}
	return decide.ExecAgentbus
}

// refreshBus reloads the bus names when the cache is older than
// busCacheTTL. Any failure leaves only the configured targets.
func (m *Model) refreshBus() {
	if !m.busOn {
		return
	}
	now := m.Now()
	if !m.busAt.IsZero() && now.Sub(m.busAt) < busCacheTTL {
		return
	}
	m.busAt = now
	m.busNames = nil
	ctx, cancel := context.WithTimeout(context.Background(), busListTimeout)
	defer cancel()
	out, err := m.busRunner()(ctx, []string{"list", "--json"})
	if err != nil {
		slog.Debug("decide: agentbus list", "err", err)
		return
	}
	var agents []busAgent
	if err := json.Unmarshal(out, &agents); err != nil {
		slog.Debug("decide: agentbus list", "err", err)
		return
	}
	for _, a := range agents {
		if strings.TrimSpace(a.Name) == "" {
			continue
		}
		m.busNames = append(m.busNames, Target{
			Name: a.Name, Pane: a.Handle, Bus: true, When: a.When, Dead: a.Liveness == "dead",
		})
	}
	slices.SortStableFunc(m.busNames, func(a, b Target) int {
		if a.Dead != b.Dead {
			if a.Dead {
				return 1
			}
			return -1
		}
		return strings.Compare(a.Name, b.Name)
	})
}

// candidates are the configured targets, then live bus names, then dead
// ones. A configured target whose pane a bus name holds is left out.
func (m *Model) candidates() []Target {
	if len(m.busNames) == 0 {
		return m.Targets
	}
	out := make([]Target, 0, len(m.Targets)+len(m.busNames))
	for _, tg := range m.Targets {
		if !slices.ContainsFunc(m.busNames, func(b Target) bool { return samePane(b.Pane, tg.Pane) }) {
			out = append(out, tg)
		}
	}
	return append(out, m.busNames...)
}

// samePane reports whether bus handle holds the configured pane. A herdr
// handle ends in "/<pane>".
func samePane(handle, pane string) bool {
	if handle == "" || pane == "" {
		return false
	}
	return handle == pane || strings.HasSuffix(handle, "/"+pane)
}

// visibleTargets are the candidate indices that match the picker filter
// in the name or the --when text, ignoring case.
func (m *Model) visibleTargets() []int {
	cands := m.candidates()
	q := strings.ToLower(m.pickFilter)
	out := make([]int, 0, len(cands))
	for i, tg := range cands {
		if q == "" || strings.Contains(strings.ToLower(tg.Name), q) || strings.Contains(strings.ToLower(tg.When), q) {
			out = append(out, i)
		}
	}
	return out
}

// selectedTarget is the picked candidate when the filter still shows it.
func (m *Model) selectedTarget() (Target, bool) {
	vis := m.visibleTargets()
	if !slices.Contains(vis, m.target) {
		if len(vis) == 0 {
			return Target{}, false
		}
		m.target = vis[0]
	}
	return m.candidates()[m.target], true
}

func (m *Model) moveTarget(delta int) {
	vis := m.visibleTargets()
	if len(vis) == 0 {
		return
	}
	pos := slices.Index(vis, m.target)
	next := pos + delta
	if pos < 0 {
		next = 0
	}
	if next < 0 || next >= len(vis) {
		return
	}
	m.selectTarget(vis[next])
}

// selectTarget picks candidate i. A dead bus name stays selectable and
// the status line says so.
func (m *Model) selectTarget(i int) {
	cands := m.candidates()
	if i < 0 || i >= len(cands) {
		return
	}
	m.target = i
	if cands[i].Dead {
		m.status = cands[i].Name + " is dead: the bus holds the note until the name is claimed"
	}
}

// selectName picks the candidate called name and reports whether one is.
func (m *Model) selectName(name string) bool {
	i := slices.IndexFunc(m.candidates(), func(tg Target) bool { return tg.Name == name })
	if i >= 0 {
		m.target = i
	}
	return i >= 0
}

// pickerKey edits the filter while the recipient list is focused. Esc
// clears a filter first and leaves compose second.
func (m *Model) pickerKey(r rune) bool {
	switch {
	case r == '\x1b' && m.pickFilter != "":
		m.pickFilter = ""
	case r == '\x7f' || r == '\b':
		if f := []rune(m.pickFilter); len(f) > 0 {
			m.pickFilter = string(f[:len(f)-1])
		}
	case r == '\r' || r == '\n':
		if _, ok := m.selectedTarget(); ok {
			m.advanceCompose()
		}
	case r >= ' ':
		m.pickFilter += string(r)
		if vis := m.visibleTargets(); len(vis) > 0 && !slices.Contains(vis, m.target) {
			m.selectTarget(vis[0])
		}
	default:
		return false
	}
	return true
}

// pickerRowLimit is the dropdown height: at most pickerMaxRows and a
// third of the screen.
func (m *Model) pickerRowLimit() int {
	if m.height <= 0 {
		return pickerMaxRows
	}
	return max(min(pickerMaxRows, m.height/3), 1)
}

// pickerLines is the recipient dropdown and the index of the selected
// row in it. A count line shows when a filter is typed or the list
// scrolls. Name-prefix group headers show when the list scrolls.
func (m *Model) pickerLines() ([]screenLine, int) {
	cands := m.candidates()
	if len(cands) == 0 {
		return []screenLine{{text: "(no recipients)"}}, 0
	}
	if m.target < 0 || m.target >= len(cands) {
		m.target = 0
	}
	limit := m.pickerRowLimit()
	vis := m.visibleTargets()
	scrolls := len(cands) > limit
	head := m.pickerCount(len(vis), len(cands), scrolls)
	if len(vis) == 0 {
		return append(head, screenLine{text: "(no match)"}), 0
	}
	if !slices.Contains(vis, m.target) {
		m.target = vis[0]
	}
	rows := make([]screenLine, 0, len(vis))
	sel := 0
	group := "\x00"
	for _, i := range vis {
		tg := cands[i]
		if g := targetGroup(tg); scrolls && g != group {
			group = g
			rows = append(rows, screenLine{text: mutedPicker(g)})
		}
		if i == m.target {
			sel = len(rows)
		}
		rows = append(rows, screenLine{
			text: m.targetLabel(tg, i == m.target),
			hits: []hit{{kind: hitTarget, n: i}},
		})
	}
	rows = windowLines(rows, sel, max(limit-len(head), 1))
	for j, row := range rows {
		if len(row.hits) > 0 && row.hits[0].n == m.target {
			sel = j
		}
	}
	return append(head, rows...), sel + len(head)
}

// pickerCount is the filter and match-count line, shown when a filter is
// typed or the list scrolls.
func (m *Model) pickerCount(matches, total int, scrolls bool) []screenLine {
	if m.pickFilter == "" && !scrolls {
		return nil
	}
	label := fmt.Sprintf("%d/%d", matches, total)
	if m.pickFilter != "" {
		label = "filter: " + m.pickFilter + "  " + label
	}
	return []screenLine{{text: mutedPicker(label)}}
}

// targetGroup is a candidate's dropdown group: configured targets, the
// name's prefix up to its first slash, or dead names.
func targetGroup(tg Target) string {
	switch {
	case !tg.Bus:
		return "targets"
	case tg.Dead:
		return "dead"
	}
	if i := strings.Index(tg.Name, "/"); i >= 0 {
		return tg.Name[:i+1]
	}
	return "agentbus"
}

func (m *Model) targetLabel(tg Target, selected bool) string {
	label := tg.Name
	if selected {
		label = "[" + label + "]"
		if m.composeFocus == focusRecipient {
			label = "> " + label
		}
	}
	if !tg.Bus {
		return label
	}
	mark := "●"
	if tg.Dead {
		mark = "✗ dead"
	}
	label += " " + mark
	if when := strings.TrimSpace(tg.When); when != "" {
		if clipped := clipRunes(when, whenLimit); clipped != when {
			when = clipped + "…"
		}
		label += "  " + mutedPicker(when)
	}
	return label
}

func mutedPicker(s string) string {
	useStyles()
	return paletteStyle(loadColors().Time, paletteTime).Render(s)
}

// sendBus delivers through `agentbus send <name> <note>`. The bus owns
// name resolution, liveness, and retries. A failed send keeps the draft
// and drops the thread it opened.
func (m *Model) sendBus(tg Target, text string) error {
	led, err := decide.Open(m.Dir)
	if err != nil {
		m.status = fmt.Sprintf("open decide ledger: %v", err)
		return nil
	}
	pane := busPanePrefix + tg.Name
	item, err := led.ForwardThread(m.forward, tg.Name, pane, text)
	if err != nil {
		m.status = fmt.Sprintf("store decide thread: %v", err)
		return nil
	}
	note := decide.DeliveryNote(item.ID, "human", text)
	if m.forward != "" {
		note = decide.ForwardNote(item.ID, m.forward, "human", text)
	}
	msgID, err := m.busSend(tg.Name, note)
	if err != nil {
		m.status = err.Error()
		if derr := led.Delete(item.ID); derr != nil {
			m.status += "; " + derr.Error()
		}
		return nil
	}
	if _, err := led.SetBusMessage(item.ID, msgID); err != nil {
		m.status = err.Error()
		return nil
	}
	if err := decide.AppendSend(m.Dir, tg.Name, pane, text); err != nil {
		m.status = err.Error()
		return nil
	}
	m.clearNote()
	m.status = "sent to " + tg.Name
	return nil
}

// busSend runs `agentbus send name note` and returns the message id,
// the first word agentbus prints.
func (m *Model) busSend(name, note string) (string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), busSendTimeout)
	defer cancel()
	out, err := m.busRunner()(ctx, []string{"send", name, note})
	if err != nil {
		return "", err
	}
	id, _, _ := strings.Cut(strings.TrimSpace(string(out)), " ")
	return id, nil
}
