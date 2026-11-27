package tui

import (
	"bytes"
	"encoding/json"
	"fmt"
	"math"
	"os"
	"slices"
	"strings"
	"time"

	"github.com/charmbracelet/lipgloss"
	"github.com/jaeyeom/experimental/devtools/decide"
)

const (
	panelUnknownMaxLines = 6
	panelUnknownMaxRunes = 240
	panelGaugeBarWidth   = 20
	panelChartMaxPoints  = 24
)

type statusPanel struct {
	id         string
	title      string
	kind       string
	enabled    bool
	stale      bool
	unreadable bool
	file       string
	updatedAt  *time.Time
	data       json.RawMessage
	builtIn    bool
}

func (m *Model) loadPanelsOnce() {
	if m.panelsLoaded {
		return
	}
	pb, ok := m.store().(PanelsBackend)
	if !ok {
		return
	}
	panels, err := pb.Panels()
	if err != nil {
		return
	}
	m.panels = panels
	m.panelsLoaded = true
}

func (m *Model) loadDashboardConfigOnce() {
	if m.dashboardLoaded {
		return
	}
	m.dashboardLoaded = true
	cfg, err := decide.LoadConfig()
	if err != nil {
		return
	}
	m.dashboardPanels = cfg.UI.Dashboard.Panels
}

func (m *Model) nowTime() time.Time {
	if m.Now != nil {
		return m.Now()
	}
	return time.Now()
}

func (m *Model) statusPanels(now time.Time) []statusPanel {
	m.loadPanelsOnce()
	m.loadDashboardConfigOnce()

	disabled := map[string]bool{}
	for _, cfg := range m.dashboardPanels {
		if !cfg.IsEnabled() {
			disabled[cfg.ID] = true
		}
	}

	byID := map[string]statusPanel{}
	availableIDs := make([]string, 0, 4+len(m.panels))
	add := func(sp statusPanel) {
		if m.filter != "" && !statusPanelMatchesFilter(&sp, m.filter) {
			return
		}
		byID[sp.id] = sp
		availableIDs = append(availableIDs, sp.id)
	}

	m.addBuiltinStatusPanels(now, disabled, add)
	m.addExternalStatusPanels(now, byID, add)

	if len(availableIDs) == 0 {
		return nil
	}
	resolved := decide.ResolveDashboardPanels(m.dashboardPanels, availableIDs)
	out := make([]statusPanel, 0, len(availableIDs))
	for _, st := range resolved {
		sp, ok := byID[st.ID]
		if !ok {
			continue
		}
		sp.enabled = st.Enabled
		out = append(out, sp)
	}
	return out
}

func (m *Model) addBuiltinStatusPanels(now time.Time, disabled map[string]bool, add func(statusPanel)) {
	if len(m.agents) > 0 || disabled[decide.BuiltinPanelAgents] {
		add(statusPanel{
			id:      decide.BuiltinPanelAgents,
			title:   decide.BuiltinPanelTitle(decide.BuiltinPanelAgents),
			kind:    decide.BuiltinPanelAgents,
			builtIn: true,
		})
	}
	if len(m.resources) > 0 || disabled[decide.BuiltinPanelResources] {
		add(statusPanel{
			id:      decide.BuiltinPanelResources,
			title:   decide.BuiltinPanelTitle(decide.BuiltinPanelResources),
			kind:    decide.BuiltinPanelResources,
			builtIn: true,
		})
	}
	if m.hasBusHealthPanelData() || disabled[decide.BuiltinPanelBusHealth] {
		add(statusPanel{
			id:      decide.BuiltinPanelBusHealth,
			title:   decide.BuiltinPanelTitle(decide.BuiltinPanelBusHealth),
			kind:    decide.PanelKindList,
			data:    m.busHealthPanelData(),
			builtIn: true,
		})
	}
	if ob, ok := m.outboxBackend(); ok && (len(ob.Outbox()) > 0 || disabled[decide.BuiltinPanelOutbox]) {
		add(statusPanel{
			id:      decide.BuiltinPanelOutbox,
			title:   decide.BuiltinPanelTitle(decide.BuiltinPanelOutbox),
			kind:    decide.PanelKindList,
			data:    m.outboxPanelData(ob.Outbox(), now),
			builtIn: true,
		})
	}
	if m.hasAsks() {
		add(statusPanel{
			id:      decide.BuiltinPanelAsks,
			title:   decide.BuiltinPanelTitle(decide.BuiltinPanelAsks),
			kind:    decide.PanelKindList,
			data:    m.asksPanelData(),
			builtIn: true,
		})
	}
}

func (m *Model) hasBusHealthPanelData() bool {
	return m.busHealth.Daemon != "" || m.busHealth.Transport != "" || len(m.busHealth.Problems) > 0
}

func (m *Model) addExternalStatusPanels(now time.Time, byID map[string]statusPanel, add func(statusPanel)) {
	panels, _ := decide.RecomputePanelsStale(m.panels, now)
	for _, p := range panels {
		if _, dup := byID[p.ID]; dup {
			continue
		}
		sp := statusPanel{
			id:         p.ID,
			title:      p.Title,
			kind:       p.Kind,
			stale:      p.Stale,
			unreadable: p.Unreadable,
			file:       p.File,
			updatedAt:  p.UpdatedAt,
			data:       p.Data,
		}
		if sp.title == "" {
			sp.title = sp.id
		}
		add(sp)
	}
}

func (m *Model) allDashboardPanelIDs() []string {
	m.loadPanelsOnce()
	ids := append([]string(nil), decide.BuiltinPanelIDs()...)
	if m.hasAsks() {
		ids = append(ids, decide.BuiltinPanelAsks)
	}
	for _, p := range m.panels {
		if p.ID != "" && !slices.Contains(ids, p.ID) {
			ids = append(ids, p.ID)
		}
	}
	return ids
}

func (m *Model) busHealthPanelData() json.RawMessage {
	var rows []decide.PanelListRow
	if m.busHealth.Daemon != "" {
		rows = append(rows, decide.PanelListRow{
			Label: "daemon",
			Value: m.busHealth.Daemon,
			State: busFieldState(m.busHealth.Daemon),
		})
	}
	if m.busHealth.Transport != "" {
		rows = append(rows, decide.PanelListRow{
			Label: "transport",
			Value: m.busHealth.Transport,
			State: busFieldState(m.busHealth.Transport),
		})
	}
	for _, prob := range m.busHealth.Problems {
		if strings.TrimSpace(prob) != "" {
			rows = append(rows, decide.PanelListRow{
				Label: "problem",
				Value: prob,
				State: decide.PanelStateFail,
			})
		}
	}
	if len(rows) == 0 {
		rows = []decide.PanelListRow{{Label: "status", Value: "unknown"}}
	}
	raw, _ := json.Marshal(decide.PanelListData{Rows: rows})
	return raw
}

func busFieldState(v string) string {
	switch strings.ToLower(strings.TrimSpace(v)) {
	case "up", "ok", "running":
		return decide.PanelStateOK
	case "down", "error", "failed":
		return decide.PanelStateFail
	default:
		return decide.PanelStateWarn
	}
}

func (m *Model) outboxPanelData(entries []OutboxEntry, now time.Time) json.RawMessage {
	rows := make([]decide.PanelListRow, 0, len(entries))
	thresh := m.outboxStaleThreshold()
	for _, e := range entries {
		label := strings.TrimSpace(strings.Join([]string{e.Op, e.ItemID, e.To}, " "))
		if label == "" {
			label = e.ID
		}
		state := decide.PanelStateOK
		switch e.Status {
		case OutboxFailed:
			state = decide.PanelStateFail
		case OutboxHeld:
			state = decide.PanelStateWarn
		case OutboxPending:
			if !e.WrittenAt.IsZero() && now.Sub(e.WrittenAt) > thresh {
				state = decide.PanelStateWarn
			}
		}
		val := outboxEntryStateText(e)
		if e.Error != "" {
			val += ": " + e.Error
		}
		rows = append(rows, decide.PanelListRow{
			Label: label,
			Value: val,
			State: state,
		})
	}
	raw, _ := json.Marshal(decide.PanelListData{Rows: rows})
	return raw
}

func (m *Model) renderStatusPanels(width int, now time.Time) (lines []screenLine, panelOffsets []int, agentLineOffset int) {
	panels := m.statusPanels(now)
	if len(panels) == 0 {
		return nil, nil, -1
	}
	m.clampStatusCursorWith(panels)
	useStyles()
	panelOffsets = make([]int, len(panels))
	agentLineOffset = -1
	for i, p := range panels {
		if i > 0 {
			lines = append(lines, screenLine{})
		}
		panelOffsets[i] = len(lines)
		selected := i == m.panelCursor
		block, aOff := m.renderOneStatusPanel(p, selected, width, now)
		if aOff >= 0 {
			agentLineOffset = len(lines) + aOff
		}
		lines = append(lines, block...)
	}
	return lines, panelOffsets, agentLineOffset
}

func (m *Model) renderOneStatusPanel(p statusPanel, selected bool, width int, now time.Time) ([]screenLine, int) {
	if p.unreadable {
		name := p.file
		if name == "" {
			name = p.id
		}
		prefix := "  "
		if selected {
			prefix = "> "
		}
		return []screenLine{{text: staleLine.Render(prefix + fmt.Sprintf("panel unreadable: %s", name))}}, -1
	}
	prefix := "  "
	if selected {
		prefix = "> "
	}
	if !p.enabled {
		return []screenLine{{text: staleLine.Render(prefix + p.title + "  (off)")}}, -1
	}

	header := m.renderPanelHeader(p, prefix, selected, now)
	lines := []screenLine{header}
	agentOffset := -1
	switch p.id {
	case decide.BuiltinPanelAgents:
		agentOffset = 1 + 1 + m.agentCursor
		lines = append(lines, m.agentTable(width, now, selected)...)
	case decide.BuiltinPanelResources:
		lines = append(lines, m.resourceTable(width, now)...)
	default:
		lines = append(lines, renderPanelBody(p, width, now, m.zone())...)
	}
	return lines, agentOffset
}

func (m *Model) renderPanelHeader(p statusPanel, prefix string, selected bool, now time.Time) screenLine {
	var meta string
	if p.stale {
		if p.updatedAt != nil {
			meta = "stale · " + decide.FormatPanelUpdatedAgo(*p.updatedAt, now)
		} else {
			meta = "stale"
		}
		return screenLine{text: staleLine.Render(prefix + p.title + "  " + meta)}
	}
	if p.updatedAt != nil {
		meta = decide.FormatPanelUpdatedAgo(*p.updatedAt, now)
	}
	titleText := prefix + p.title
	if selected && os.Getenv("NO_COLOR") == "" {
		titleText = focusMark.Render(titleText)
	}
	if meta == "" {
		return screenLine{text: titleText}
	}
	if os.Getenv("NO_COLOR") == "" {
		meta = paletteRenderer().NewStyle().Foreground(paletteTime).Render(meta)
	}
	return screenLine{text: titleText + "  " + meta}
}

func renderPanelBody(p statusPanel, width int, now time.Time, loc *time.Location) []screenLine {
	switch p.kind {
	case decide.PanelKindText:
		var d decide.PanelTextData
		if err := json.Unmarshal(p.data, &d); err == nil {
			return renderTextPanelLines(d.Text, width, p.stale)
		}
	case decide.PanelKindList:
		var d decide.PanelListData
		if err := json.Unmarshal(p.data, &d); err == nil {
			return renderListPanelLines(d, width, p.stale)
		}
	case decide.PanelKindGauge:
		var d decide.PanelGaugeData
		if err := json.Unmarshal(p.data, &d); err == nil && d.Max > 0 {
			return renderGaugePanelLines(d, width, p.stale)
		}
	case decide.PanelKindChart:
		var d decide.PanelChartData
		if err := json.Unmarshal(p.data, &d); err == nil {
			return renderChartPanelLines(d, width, p.stale, now, loc)
		}
	case decide.PanelKindTable:
		var d decide.PanelTableData
		if err := json.Unmarshal(p.data, &d); err == nil {
			return renderTablePanelLines(d, width, p.stale)
		}
	}
	return renderUnknownPanelLines(p.data, width, p.stale)
}

func renderTextPanelLines(text string, width int, stale bool) []screenLine {
	if text == "" {
		return nil
	}
	wrapW := 0
	if width > 2 {
		wrapW = width - 2
	}
	var out []screenLine
	for _, rawLine := range strings.Split(text, "\n") {
		if wrapW <= 0 {
			out = append(out, styledPanelLine("  "+rawLine, stale))
			continue
		}
		chunks := wrapIndexed(rawLine, wrapW)
		if len(chunks) == 0 {
			out = append(out, styledPanelLine("  ", stale))
			continue
		}
		for _, ch := range chunks {
			out = append(out, styledPanelLine("  "+ch.text, stale))
		}
	}
	return out
}

func renderUnknownPanelLines(data json.RawMessage, width int, stale bool) []screenLine {
	var buf bytes.Buffer
	trimmed := bytes.TrimSpace(data)
	if err := json.Indent(&buf, trimmed, "", "  "); err != nil {
		buf.Reset()
		buf.Write(trimmed)
	}
	raw := buf.String()
	lines := strings.Split(raw, "\n")
	truncated := false
	if len(lines) > panelUnknownMaxLines {
		lines = lines[:panelUnknownMaxLines]
		truncated = true
	}
	joined := strings.Join(lines, "\n")
	runes := []rune(joined)
	if len(runes) > panelUnknownMaxRunes {
		joined = string(runes[:panelUnknownMaxRunes-1]) + "…"
		truncated = false
	}
	if truncated {
		joined += "\n…"
	}
	return renderTextPanelLines(joined, width, stale)
}

func renderListPanelLines(d decide.PanelListData, width int, stale bool) []screenLine {
	if len(d.Rows) == 0 {
		return []screenLine{styledPanelLine("  (empty)", stale)}
	}
	labelW := 0
	for _, r := range d.Rows {
		if w := lipgloss.Width(r.Label); w > labelW {
			labelW = w
		}
	}
	if width > 0 && labelW > width/2 {
		labelW = max(width/2, 4)
	}
	out := make([]screenLine, 0, len(d.Rows))
	for _, r := range d.Rows {
		lbl := r.Label
		if lipgloss.Width(lbl) > labelW {
			lbl = truncateVisible(lbl, labelW)
		}
		pad := strings.Repeat(" ", max(0, labelW-lipgloss.Width(lbl)))
		line := "  " + lbl + pad + "  " + r.Value
		if r.State != "" {
			line += "  [" + r.State + "]"
		}
		if stale {
			out = append(out, screenLine{text: staleLine.Render(line)})
			continue
		}
		if r.State != "" && os.Getenv("NO_COLOR") == "" {
			st := panelStateStyle(r.State)
			badge := st.Render("[" + r.State + "]")
			out = append(out, screenLine{text: "  " + lbl + pad + "  " + r.Value + "  " + badge})
			continue
		}
		out = append(out, screenLine{text: plainLine.Render(line)})
	}
	return out
}

func renderGaugePanelLines(d decide.PanelGaugeData, width int, stale bool) []screenLine {
	ratio := d.Value / d.Max
	if ratio < 0 {
		ratio = 0
	}
	if ratio > 1 {
		ratio = 1
	}
	barW := panelGaugeBarWidth
	if width > 0 && width < 40 {
		barW = max(8, width-20)
	}
	filled := int(math.Round(ratio * float64(barW)))
	if filled > barW {
		filled = barW
	}
	filledStr := strings.Repeat("█", filled)
	emptyStr := strings.Repeat("░", barW-filled)
	pct := int(math.Round(ratio * 100))
	label := fmt.Sprintf("%s/%s", decide.FormatPanelNumber(d.Value), decide.FormatPanelNumber(d.Max))
	if d.Unit != "" {
		label += d.Unit
	}
	summary := fmt.Sprintf(" %s (%d%%)", label, pct)
	state := decide.GaugeState(d)
	if stale {
		return []screenLine{{text: staleLine.Render("  [" + filledStr + emptyStr + "]" + summary)}}
	}
	if os.Getenv("NO_COLOR") == "" {
		colored := panelStateStyle(state).Render(filledStr)
		return []screenLine{{text: "  [" + colored + emptyStr + "]" + summary}}
	}
	return []screenLine{{text: plainLine.Render("  [" + filledStr + emptyStr + "]" + summary)}}
}

var sparkBlocks = []rune("▁▂▃▄▅▆▇█")

func renderChartPanelLines(d decide.PanelChartData, _ int, stale bool, now time.Time, loc *time.Location) []screenLine {
	if len(d.Series) == 0 {
		return []screenLine{styledPanelLine("  (no data)", stale)}
	}
	pts := d.Series
	if len(pts) > panelChartMaxPoints {
		pts = pts[len(pts)-panelChartMaxPoints:]
	}
	minV, maxV := chartValueRange(pts, d.Limit)
	spark := chartSparkline(pts, minV, maxV)
	latest := pts[len(pts)-1].V
	unit := d.Unit
	summary := fmt.Sprintf("%s  latest %s%s (min %s, max %s)",
		spark,
		decide.FormatPanelNumber(latest), unit,
		decide.FormatPanelNumber(minV),
		decide.FormatPanelNumber(maxV),
	)
	out := []screenLine{styledPanelLine("  "+summary, stale)}
	if meta := chartMetaLine(d, now, loc); meta != "" {
		out = append(out, styledPanelLine("  "+meta, stale))
	}
	return out
}

func chartValueRange(pts []decide.PanelChartPoint, limit *float64) (minV, maxV float64) {
	minV, maxV = pts[0].V, pts[0].V
	for _, pt := range pts[1:] {
		minV = min(minV, pt.V)
		maxV = max(maxV, pt.V)
	}
	if limit != nil {
		minV = min(minV, *limit)
		maxV = max(maxV, *limit)
	}
	return minV, maxV
}

func chartSparkline(pts []decide.PanelChartPoint, minV, maxV float64) string {
	var spark strings.Builder
	span := maxV - minV
	for _, pt := range pts {
		idx := len(sparkBlocks) / 2
		if span > 0 {
			norm := (pt.V - minV) / span
			idx = clampInt(int(math.Round(norm*float64(len(sparkBlocks)-1))), 0, len(sparkBlocks)-1)
		}
		spark.WriteRune(sparkBlocks[idx])
	}
	return spark.String()
}

func chartMetaLine(d decide.PanelChartData, now time.Time, loc *time.Location) string {
	var metaParts []string
	if d.Limit != nil {
		metaParts = append(metaParts, "limit "+decide.FormatPanelNumber(*d.Limit)+d.Unit)
	}
	if d.Projection != nil {
		atText := d.Projection.At
		if parsed, err := time.Parse(time.RFC3339Nano, d.Projection.At); err == nil {
			atText = stamp(parsed, now, loc)
		}
		metaParts = append(metaParts, fmt.Sprintf("projected %s%s at %s", decide.FormatPanelNumber(d.Projection.To), d.Unit, atText))
	}
	return strings.Join(metaParts, "  ·  ")
}

func clampInt(v, lo, hi int) int {
	return max(lo, min(hi, v))
}

func renderTablePanelLines(d decide.PanelTableData, width int, stale bool) []screenLine {
	if len(d.Columns) == 0 {
		return []screenLine{styledPanelLine("  (empty table)", stale)}
	}
	maxRows := decide.MaxPanelTableRows
	displayCount := len(d.Rows)
	more := 0
	if displayCount > maxRows {
		more = displayCount - maxRows
		displayCount = maxRows
	}
	rows := make([][]string, displayCount)
	for i := 0; i < displayCount; i++ {
		src := d.Rows[i]
		row := make([]string, len(d.Columns))
		for j := 0; j < len(d.Columns) && j < len(src); j++ {
			row[j] = decide.FormatTableCell(src[j])
		}
		rows[i] = row
	}
	tableW := 0
	if width > 2 {
		tableW = width - 2
	}
	tableLines := styledStatusRows(d.Columns, rows, tableW, func(int) lipgloss.Style {
		if stale {
			return staleLine
		}
		return plainLine
	})
	out := make([]screenLine, 0, len(tableLines)+1)
	for i, ln := range tableLines {
		if stale && i == 0 {
			out = append(out, screenLine{text: staleLine.Render("  " + ln.text)})
		} else {
			out = append(out, screenLine{text: "  " + ln.text})
		}
	}
	if more > 0 {
		out = append(out, styledPanelLine(fmt.Sprintf("  +%d more", more), stale))
	}
	return out
}

func styledPanelLine(text string, stale bool) screenLine {
	if stale {
		return screenLine{text: staleLine.Render(text)}
	}
	return screenLine{text: plainLine.Render(text)}
}

func panelStateStyle(state string) lipgloss.Style {
	r := paletteRenderer()
	switch state {
	case decide.PanelStateOK:
		return r.NewStyle().Foreground(lipgloss.ANSIColor(2))
	case decide.PanelStateWarn:
		return r.NewStyle().Foreground(lipgloss.ANSIColor(3))
	case decide.PanelStateFail:
		return r.NewStyle().Foreground(lipgloss.ANSIColor(1))
	default:
		return r.NewStyle()
	}
}

func statusPanelMatchesFilter(sp *statusPanel, filter string) bool {
	if sp.id == decide.BuiltinPanelAgents || sp.id == decide.BuiltinPanelResources {
		return true
	}
	q := strings.ToLower(filter)
	if strings.Contains(strings.ToLower(sp.id), q) ||
		strings.Contains(strings.ToLower(sp.file), q) ||
		strings.Contains(strings.ToLower(sp.title), q) ||
		strings.Contains(strings.ToLower(sp.kind), q) {
		return true
	}
	if len(sp.data) > 0 && strings.Contains(strings.ToLower(string(sp.data)), q) {
		return true
	}
	return false
}

func (m *Model) clampStatusCursor() {
	panels := m.statusPanels(m.nowTime())
	m.clampStatusCursorWith(panels)
}

func (m *Model) clampStatusCursorWith(panels []statusPanel) {
	if len(panels) == 0 {
		m.panelCursor = 0
		m.agentCursor = 0
		return
	}
	if m.panelCursor < 0 {
		m.panelCursor = 0
	}
	if m.panelCursor >= len(panels) {
		m.panelCursor = len(panels) - 1
	}
	if panels[m.panelCursor].id == decide.BuiltinPanelAgents && panels[m.panelCursor].enabled {
		m.clampAgentCursor()
	}
}

func (m *Model) moveStatusCursor(delta int) {
	panels := m.statusPanels(m.nowTime())
	if len(panels) == 0 {
		m.panelCursor = 0
		m.agentCursor = 0
		return
	}
	m.clampStatusCursorWith(panels)
	forward := delta > 0
	count := delta
	if count < 0 {
		count = -count
	}
	for range count {
		m.stepStatusCursor(panels, forward)
	}
	m.revealStatusCursor()
}

func (m *Model) stepStatusCursor(panels []statusPanel, forward bool) {
	cur := panels[m.panelCursor]
	onAgents := cur.id == decide.BuiltinPanelAgents && cur.enabled
	if forward {
		if onAgents && m.agentCursor < len(m.agents)-1 {
			m.agentCursor++
			return
		}
		if m.panelCursor < len(panels)-1 {
			m.panelCursor++
			if panels[m.panelCursor].id == decide.BuiltinPanelAgents && panels[m.panelCursor].enabled {
				m.agentCursor = 0
			}
		}
		return
	}
	if onAgents && m.agentCursor > 0 {
		m.agentCursor--
		return
	}
	if m.panelCursor > 0 {
		m.panelCursor--
		if panels[m.panelCursor].id == decide.BuiltinPanelAgents && panels[m.panelCursor].enabled && len(m.agents) > 0 {
			m.agentCursor = len(m.agents) - 1
		}
	}
}

func (m *Model) jumpStatusCursor(index int) {
	panels := m.statusPanels(m.nowTime())
	if len(panels) == 0 {
		return
	}
	if index <= 0 {
		m.panelCursor = 0
		m.agentCursor = 0
	} else {
		m.panelCursor = len(panels) - 1
		if panels[m.panelCursor].id == decide.BuiltinPanelAgents && panels[m.panelCursor].enabled && len(m.agents) > 0 {
			m.agentCursor = len(m.agents) - 1
		}
	}
	m.revealStatusCursor()
}

func (m *Model) statusListLen() int {
	panels := m.statusPanels(m.nowTime())
	total := 0
	for _, p := range panels {
		if p.id == decide.BuiltinPanelAgents && p.enabled && len(m.agents) > 0 {
			total += len(m.agents)
		} else {
			total++
		}
	}
	return total
}

func (m *Model) revealStatusCursor() {
	now := m.nowTime()
	width := m.contentWidth()
	_, offsets, agentOffset := m.renderStatusPanels(width, now)
	if len(offsets) == 0 {
		m.scroll = 0
		return
	}
	target := offsets[m.panelCursor]
	if agentOffset >= 0 {
		target = agentOffset
	}
	_, limit := m.layoutBudget()
	m.scroll = revealRow(m.scroll, target, limit)
}

func (m *Model) selectedStatusPanel() (statusPanel, bool) {
	panels := m.statusPanels(m.nowTime())
	if len(panels) == 0 {
		return statusPanel{}, false
	}
	m.clampStatusCursorWith(panels)
	return panels[m.panelCursor], true
}

func (m *Model) toggleSelectedPanel() {
	sp, ok := m.selectedStatusPanel()
	if !ok {
		return
	}
	next := decide.ToggleDashboardPanel(m.dashboardPanels, m.allDashboardPanelIDs(), sp.id)
	cfg, err := decide.SetConfigValue("ui.dashboard.panels", next)
	if err != nil {
		m.status = err.Error()
		return
	}
	m.dashboardPanels = cfg.UI.Dashboard.Panels
	m.dashboardLoaded = true
	state := "on"
	if sp.enabled {
		state = "off"
	}
	m.status = fmt.Sprintf("panel %s %s", sp.id, state)
	m.revealStatusCursor()
}

func (m *Model) moveSelectedPanel(delta int) {
	panels := m.statusPanels(m.nowTime())
	if len(panels) == 0 {
		return
	}
	m.clampStatusCursorWith(panels)
	sp := panels[m.panelCursor]
	visibleIDs := make([]string, len(panels))
	for i, p := range panels {
		visibleIDs[i] = p.id
	}
	next, ok := decide.MoveDashboardPanel(m.dashboardPanels, m.allDashboardPanelIDs(), visibleIDs, sp.id, delta)
	if !ok {
		return
	}
	cfg, err := decide.SetConfigValue("ui.dashboard.panels", next)
	if err != nil {
		m.status = err.Error()
		return
	}
	m.dashboardPanels = cfg.UI.Dashboard.Panels
	m.dashboardLoaded = true
	m.panelCursor += delta
	dir := "down"
	if delta < 0 {
		dir = "up"
	}
	m.status = fmt.Sprintf("moved %s %s", sp.id, dir)
	m.revealStatusCursor()
}
