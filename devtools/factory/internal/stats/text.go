package stats

import (
	"fmt"
	"io"
	"slices"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"
	"unicode/utf8"

	"github.com/jaeyeom/experimental/devtools/factory/internal/stats/chart"
)

// ParseSince reads --since: Nd, Nw, or a Go duration such as 36h.
func ParseSince(s string) (time.Duration, error) {
	unit := time.Duration(0)
	switch {
	case strings.HasSuffix(s, "d"):
		unit = 24 * time.Hour
	case strings.HasSuffix(s, "w"):
		unit = 7 * 24 * time.Hour
	}
	if unit != 0 {
		n, err := strconv.Atoi(s[:len(s)-1])
		if err != nil || n < 1 {
			return 0, fmt.Errorf("stats --since: expected Nd, Nw, or a duration such as 36h, found %q", s)
		}
		return time.Duration(n) * unit, nil
	}
	d, err := time.ParseDuration(s)
	if err != nil || d <= 0 {
		return 0, fmt.Errorf("stats --since: expected Nd, Nw, or a duration such as 36h, found %q", s)
	}
	return d, nil
}

// MinChartWidth is the narrowest terminal that gets charts. Below it the
// report prints only its tables.
const MinChartWidth = 40

// Burndown chart styles.
const (
	BurndownBraille = "braille"
	BurndownBars    = "bars"
)

// TextOptions shape the human report. The zero value prints tables only,
// uncolored, as before charts existed.
type TextOptions struct {
	Width    int    // Terminal columns. Below MinChartWidth, tables only.
	Color    bool   // ANSI colors in charts and status cells.
	ASCII    bool   // Charts use #, =, . and * instead of blocks and braille.
	Burndown string // BurndownBraille (the default) or BurndownBars.
	// BurndownPace adds the dotted pace line, straight from the first
	// bucket's remaining count to zero at the last bucket, to the braille
	// burndown. It is a reference only, not a target or a forecast.
	BurndownPace bool
	// BarWidth is the cell width of the bar column in the Throughput,
	// Backlog, and Epics tables. Zero leaves the column out.
	BarWidth int
}

// DefaultBarWidth is the bar column width the CLI uses by default.
const DefaultBarWidth = 12

// WriteText writes the human report as tables only.
func (r Report) WriteText(w io.Writer) error {
	return r.WriteTextWith(w, TextOptions{})
}

// WriteTextWith writes the human report. Every number in the tables stays;
// charts are drawn under them when o.Width allows.
func (r Report) WriteTextWith(w io.Writer, o TextOptions) error {
	tw := tabwriter.NewWriter(w, 0, 0, 2, ' ', 0)
	p := &printer{w: tw, o: o, style: chart.Style{Color: o.Color, ASCII: o.ASCII}, loc: r.loc}
	if p.loc == nil {
		p.loc = time.UTC
	}
	r.header(p)
	r.writeThroughput(p)
	r.writeBurndown(p)
	r.writeBacklog(p)
	r.writeEpics(p)
	r.writeFlow(p)
	r.writeSplit(p)
	r.writeTree(p)
	if p.err != nil {
		return fmt.Errorf("write stats: %w", p.err)
	}
	if err := tw.Flush(); err != nil {
		return fmt.Errorf("write stats: %w", err)
	}
	return nil
}

// printer keeps the first write error so each section stays a list of lines.
type printer struct {
	w     io.Writer
	err   error
	o     TextOptions
	style chart.Style
	loc   *time.Location
}

func (p *printer) line(cells ...string) {
	if p.err != nil {
		return
	}
	_, p.err = fmt.Fprintln(p.w, strings.Join(cells, "\t"))
}

// charts reports whether the terminal is wide enough for charts.
func (p *printer) charts() bool {
	return p.o.Width >= MinChartWidth
}

// bars reports whether tables get a bar column.
func (p *printer) bars() bool {
	return p.charts() && p.o.BarWidth > 0
}

// bar is a bar column cell: parts sized against scale, padded to BarWidth.
// With color every part is wrapped in its code even when empty, so each
// cell of the column carries escapes of the same length and tabwriter
// still aligns the columns after it.
func (p *printer) bar(parts []chart.Part, scale float64) string {
	var b strings.Builder
	used := 0
	for i, seg := range chart.Stack(parts, scale, p.o.BarWidth, p.style) {
		used += utf8.RuneCountInString(seg)
		b.WriteString(p.raw(parts[i].Color, seg))
	}
	b.WriteString(strings.Repeat(" ", p.o.BarWidth-used))
	return b.String()
}

// barHead is a bar column heading with the escapes of an n-part bar cell.
func (p *printer) barHead(text string, n int) string {
	out := p.raw("39", text)
	for range n - 1 {
		out += p.raw("39", "")
	}
	return out
}

// raw wraps text in code when coloring, even when text is empty.
func (p *printer) raw(code, text string) string {
	if !p.style.Color {
		return text
	}
	return "\x1b[" + code + "m" + text + "\x1b[0m"
}

// marks is one mark per merged pull request, or mark×N when N is wider than
// the bar column.
func (p *printer) marks(n int) string {
	mark, times := "◆", "×"
	if p.style.ASCII {
		mark, times = "*", "x"
	}
	if n <= 0 {
		return ""
	}
	if n > p.o.BarWidth {
		return p.style.Paint(chart.Magenta, mark+times+strconv.Itoa(n))
	}
	return p.style.Paint(chart.Magenta, strings.Repeat(mark, n))
}

// cell colors a table cell so tabwriter still aligns its column: every cell
// of a colored column carries escapes of the same length, using the
// default color (39) when the cell has none.
func (p *printer) cell(code, text string) string {
	if !p.style.Color {
		return text
	}
	if code == "" {
		code = "39"
	}
	return p.style.Paint(code, text)
}

// stamp is t in the report's zone, to the minute, with the zone name.
func (p *printer) stamp(t *time.Time) string {
	if t == nil {
		return "never"
	}
	return t.In(p.loc).Format("2006-01-02 15:04 MST")
}

func (r Report) header(p *printer) {
	start, end := r.Window.Start, r.Window.End
	title := fmt.Sprintf("stats %s to %s, by %s", p.stamp(&start), p.stamp(&end), r.Window.By)
	if r.Window.Epic != "" {
		title += ", epic " + r.Window.Epic
	}
	p.line(title)
	syncs := make([]string, len(r.Freshness.LastSync))
	for i, s := range r.Freshness.LastSync {
		syncs[i] = s.Source + " " + p.stamp(s.At)
	}
	p.line("last sync: " + strings.Join(syncs, ", ") + "; PR state last seen: " + p.stamp(r.Freshness.PRLastSeen))
}

func (r Report) writeThroughput(p *printer) {
	p.line("")
	p.line("Throughput")
	if p.charts() {
		r.throughputSparks(p)
	}
	p.line(strings.ToUpper(r.Throughput.By), "FINISHED", "STARTED", "PRS OPENED", "PRS MERGED")
	most := 0
	for _, b := range r.Throughput.Buckets {
		most = max(most, b.Finished+b.Started)
	}
	finished := chart.Part{Color: chart.Green, Fill: '#', Shade: '█'}
	started := chart.Part{Color: chart.Blue, Fill: '=', Shade: '▒'}
	for _, b := range r.Throughput.Buckets {
		cells := append([]string{b.Start.In(p.loc).Format(time.DateOnly)}, countCells(b.Counts)...)
		if p.bars() && b.Finished+b.Started+b.PRsMerged > 0 {
			finished.Value, started.Value = float64(b.Finished), float64(b.Started)
			cells = append(cells, strings.TrimRight(p.bar([]chart.Part{finished, started}, float64(most))+" "+p.marks(b.PRsMerged), " "))
		}
		p.line(cells...)
	}
	p.line(append([]string{"total"}, countCells(r.Throughput.Total)...)...)
	if p.bars() {
		mark := "◆"
		if p.style.ASCII {
			mark = "*"
		}
		p.line(p.swatch(finished) + " finished  " + p.swatch(started) + " started  " + p.style.Paint(chart.Magenta, mark) + " PR merged")
	}
}

// throughputSparks prints one sparkline per series for the trend.
func (r Report) throughputSparks(p *printer) {
	series := []struct {
		name  string
		color string
		value func(Counts) int
	}{
		{"finished", chart.Green, func(c Counts) int { return c.Finished }},
		{"started", chart.Blue, func(c Counts) int { return c.Started }},
		{"merged", chart.Magenta, func(c Counts) int { return c.PRsMerged }},
	}
	for _, s := range series {
		values := make([]float64, len(r.Throughput.Buckets))
		for i, b := range r.Throughput.Buckets {
			values[i] = float64(s.value(b.Counts))
		}
		p.line(fmt.Sprintf("%-9s%s", s.name, p.style.Paint(s.color, chart.Sparkline(values, p.style))))
	}
}

func countCells(c Counts) []string {
	return []string{strconv.Itoa(c.Finished), strconv.Itoa(c.Started), strconv.Itoa(c.PRsOpened), strconv.Itoa(c.PRsMerged)}
}

// writeBurndown draws remaining work per bucket, with a legend, the
// projection from the last 7 days' finishes spelled out, and what items with
// no create time do to the line. o.BurndownPace adds a dotted pace line.
func (r Report) writeBurndown(p *printer) {
	b := r.Burndown
	if b == nil || len(b.Points) == 0 || !p.charts() {
		return
	}
	p.line("")
	p.line("Burndown")
	values := make([]float64, len(b.Points))
	most := 0
	for i, pt := range b.Points {
		values[i] = float64(pt.Remaining)
		most = max(most, pt.Remaining)
	}
	axis := len(strconv.Itoa(most))
	width := max(8, min(60, p.o.Width-axis-4))
	const height = 8
	bars := p.o.Burndown == BurndownBars
	pace := p.o.BurndownPace && !bars
	var rows []string
	switch {
	case bars:
		rows = chart.VBars(values, height, p.style)
	case pace:
		ideal := make([]float64, len(values))
		for i := range ideal {
			if len(ideal) > 1 {
				ideal[i] = values[0] * float64(len(ideal)-1-i) / float64(len(ideal)-1)
			}
		}
		rows = chart.Braille([][]float64{values, ideal}, []bool{false, true}, width, height, p.style)
	default:
		rows = chart.Braille([][]float64{values}, []bool{false}, width, height, p.style)
	}
	for i, row := range rows {
		label := ""
		switch i {
		case 0:
			label = strconv.Itoa(most)
		case len(rows) - 1:
			label = "0"
		}
		p.line(fmt.Sprintf("%*s │%s", axis, label, p.style.Paint(chart.Yellow, row)))
	}
	chartWidth := len([]rune(rows[0]))
	rule := "─"
	if p.style.ASCII {
		rule = "-"
	}
	p.line(fmt.Sprintf("%*s └%s", axis, "", strings.Repeat(rule, chartWidth)))
	first := b.Points[0].At.In(p.loc).Format("01-02")
	last := b.Points[len(b.Points)-1].At.In(p.loc).Format("01-02")
	gap := max(1, chartWidth-len(first)-len(last))
	p.line(fmt.Sprintf("%*s  %s%s%s", axis, "", first, strings.Repeat(" ", gap), last))
	r.burndownLegend(p, bars, pace, values[0])
	r.burndownNotes(p)
}

// burndownLegend says what each line or bar of the burndown is.
func (r Report) burndownLegend(p *printer, bars, pace bool, start float64) {
	b := r.Burndown
	mark := "─"
	switch {
	case bars && p.style.ASCII:
		mark = "#"
	case bars:
		mark = "█"
	case p.style.ASCII:
		mark = "*"
	}
	p.line(p.style.Paint(chart.Yellow, mark) + " remaining items (end of each " + r.Window.By + ")")
	if !pace {
		return
	}
	// ASCII draws both lines with *, so name the pace line by its shape.
	dots, what := "┄", "pace"
	if p.style.ASCII {
		dots, what = "*", "pace (the straight line)"
	}
	end := b.Points[len(b.Points)-1].At.In(p.loc).Format(time.DateOnly)
	p.line(fmt.Sprintf("%s %s to reach 0 by %s from %d (reference only)", p.style.Paint(chart.Yellow, dots), what, end, int(start)))
}

// burndownNotes explains items with no create time and the projection.
func (r Report) burndownNotes(p *printer) {
	b := r.Burndown
	if n := b.UnknownCreated; n > 0 {
		from := b.Start.In(p.loc).Format(time.DateOnly)
		if n == 1 {
			p.line(fmt.Sprintf("1 item has no create time. It counts as remaining from %s, so the start of the line may be up to 1 too high.", from))
		} else {
			p.line(fmt.Sprintf("%d items have no create time. They count as remaining from %s, so the start of the line may be up to %d too high.", n, from, n))
		}
	}
	switch {
	case b.Rate <= 0:
		p.line("no projection: nothing finished in the last 7 days")
	case b.Projected == nil:
		p.line("no projection: nothing remains")
	default:
		div, approx, arrow := "÷", "≈", "→"
		if p.style.ASCII {
			div, approx, arrow = "/", "~", "->"
		}
		days := float64(b.RemainingNow) / b.Rate
		p.line(fmt.Sprintf("projection: %d remaining %s %.1f finished/day (last 7 days) %s %.0f days %s done around %s",
			b.RemainingNow, div, b.Rate, approx, days, arrow, b.Projected.In(p.loc).Format(time.DateOnly)))
		p.line("assumes the same finish rate and no new items; not drawn on the chart")
	}
}

func (r Report) writeBacklog(p *printer) {
	p.line("")
	p.line("Backlog")
	now := r.Backlog.Now
	parts := []chart.Part{
		{Value: float64(now.Ready), Color: chart.Green, Fill: '#', Shade: '█'},
		{Value: float64(now.Blocked), Color: chart.Red, Fill: 'x', Shade: '▓'},
		{Value: float64(now.InFlight), Color: chart.Blue, Fill: '=', Shade: '▒'},
		{Value: float64(now.NeedsHuman), Color: chart.Magenta, Fill: '?', Shade: '░'},
	}
	p.line("", p.cell("", "READY"), p.cell("", "BLOCKED"), p.cell("", "IN FLIGHT"), p.cell("", "NEEDS-HUMAN"))
	nowCells := []string{"now", p.cell("", strconv.Itoa(now.Ready)), p.cell("", strconv.Itoa(now.Blocked)), p.cell("", strconv.Itoa(now.InFlight)), p.cell("", strconv.Itoa(now.NeedsHuman))}
	if total := now.Ready + now.Blocked + now.InFlight + now.NeedsHuman; p.bars() && total > 0 {
		nowCells = append(nowCells, strings.TrimRight(p.bar(parts, float64(total)), " "))
	}
	p.line(nowCells...)
	ch := r.Backlog.Change
	if ch == nil {
		p.line("change", p.cell("", "n/a"), p.cell("", "n/a"), p.cell("", "n/a"), p.cell("", "n/a"))
	} else {
		// More ready is good. Fewer blocked and fewer needs-human is good.
		p.line("change", p.change(ch.Ready, true), p.change(ch.Blocked, false), p.cell("", signed(ch.InFlight)), p.change(ch.NeedsHuman, false))
	}
	for _, status := range statusNames(now, ch) {
		change := "n/a"
		if ch != nil {
			change = signed(ch.ByStatus[status])
		}
		p.line("status "+status, p.cell("", strconv.Itoa(now.ByStatus[status])), p.cell("", change))
	}
	if p.bars() {
		names := []string{"ready", "blocked", "in flight", "needs-human"}
		keys := make([]string, len(parts))
		for i, part := range parts {
			keys[i] = p.swatch(part) + " " + names[i]
		}
		p.line(strings.Join(keys, "  "))
	}
}

// swatch is the one-cell legend mark for part: its color, else its shade,
// else its ASCII fill.
func (p *printer) swatch(part chart.Part) string {
	switch {
	case p.style.ASCII:
		return string(part.Fill)
	case p.style.Color:
		return p.style.Paint(part.Color, "█")
	case part.Shade != 0:
		return string(part.Shade)
	default:
		return "█"
	}
}

// change colors a signed change green when it moves the good way.
func (p *printer) change(n int, upIsGood bool) string {
	code := ""
	switch {
	case n > 0 && upIsGood, n < 0 && !upIsGood:
		code = chart.Green
	case n != 0:
		code = chart.Red
	}
	return p.cell(code, signed(n))
}

func statusNames(now BacklogCounts, ch *BacklogCounts) []string {
	var names []string
	for s := range now.ByStatus {
		names = append(names, s)
	}
	if ch != nil {
		for s := range ch.ByStatus {
			if _, ok := now.ByStatus[s]; !ok {
				names = append(names, s)
			}
		}
	}
	slices.Sort(names)
	return names
}

func signed(n int) string {
	if n > 0 {
		return "+" + strconv.Itoa(n)
	}
	return strconv.Itoa(n)
}

// epicParts are an epic's progress bar parts: done, in progress, remaining.
func epicParts(row EpicRow) []chart.Part {
	return []chart.Part{
		{Value: float64(row.Done), Color: chart.Green, Fill: '#', Shade: '█'},
		{Value: float64(row.InProgress), Color: chart.Yellow, Fill: '=', Shade: '▓'},
		{Value: float64(row.Remaining), Color: chart.Gray, Fill: '.', Shade: '░'},
	}
}

func (r Report) writeEpics(p *printer) {
	p.line("")
	p.line("Epics (" + r.Epics.Note + "; * is parent_priority)")
	writeEpicRows(p, "PARENT", r.Epics.Parents)
	p.line("")
	writeEpicRows(p, "TOP-LEVEL", r.Epics.TopLevel)
	if p.bars() && len(r.Epics.Parents)+len(r.Epics.TopLevel) > 0 {
		parts := epicParts(EpicRow{})
		p.line(p.swatch(parts[0]) + " done  " + p.swatch(parts[1]) + " in progress  " + p.swatch(parts[2]) + " remaining")
	}
}

// writeEpicRows prints one row per epic. The progress bar column comes
// before TITLE so a long title does not push it out of line; each bar is
// sized against the largest epic in the table.
func writeEpicRows(p *printer, head string, rows []EpicRow) {
	bars := p.bars()
	most := 0
	for _, row := range rows {
		most = max(most, row.Done+row.InProgress+row.Remaining)
	}
	cells := []string{head, "DONE", "IN PROGRESS", "REMAINING", "%", "ADDED"}
	if bars {
		cells = append(cells, p.barHead("PROGRESS", 3))
	}
	p.line(append(cells, "TITLE")...)
	for _, row := range rows {
		cells := []string{
			epicID(row), strconv.Itoa(row.Done), strconv.Itoa(row.InProgress), strconv.Itoa(row.Remaining),
			strconv.Itoa(row.Percent), strconv.Itoa(row.Added),
		}
		if bars {
			cells = append(cells, p.bar(epicParts(row), float64(most)))
		}
		p.line(append(cells, row.Title)...)
	}
}

func epicID(row EpicRow) string {
	if row.Priority {
		return "*" + row.ID
	}
	return row.ID
}

func (r Report) writeFlow(p *printer) {
	p.line("")
	p.line("Flow (pull requests merged in the window)")
	p.line("", "N", "MEDIAN", "P90")
	p.line(append([]string{"claim to first PR"}, distCells(r.Flow.ClaimToPR)...)...)
	p.line(append([]string{"PR open to merge"}, distCells(r.Flow.PRToMerge)...)...)
	p.line("")
	p.line("OPEN DRAFT", "ITEM", "OPENED", "AGE")
	for _, d := range r.Flow.Drafts {
		opened := d.OpenedAt
		p.line(d.Repo+"#"+strconv.Itoa(d.Number), d.ID, p.stamp(&opened), p.cell(ageColor(d.AgeSeconds), seconds(&d.AgeSeconds)))
	}
}

// ageColor is yellow for a draft open over a day, red over three days.
func ageColor(s int64) string {
	switch age := time.Duration(s) * time.Second; {
	case age > 3*24*time.Hour:
		return chart.Red
	case age > 24*time.Hour:
		return chart.Yellow
	default:
		return ""
	}
}

func distCells(d Dist) []string {
	return []string{strconv.Itoa(d.Count), seconds(d.MedianSeconds), seconds(d.P90Seconds)}
}

func seconds(s *int64) string {
	if s == nil {
		return "n/a"
	}
	return Compact(time.Duration(*s) * time.Second)
}

// Compact formats d in its two largest units: 3d 2h, 1d 12h, 4m 49s, 1h.
// Seconds show only under an hour. A zero lower unit is left out.
func Compact(d time.Duration) string {
	if d < 0 {
		return "-" + Compact(-d)
	}
	d = d.Round(time.Second)
	day := 24 * time.Hour
	pair := func(hi int64, hiUnit string, lo int64, loUnit string) string {
		if lo == 0 {
			return strconv.FormatInt(hi, 10) + hiUnit
		}
		return strconv.FormatInt(hi, 10) + hiUnit + " " + strconv.FormatInt(lo, 10) + loUnit
	}
	switch {
	case d >= day:
		return pair(int64(d/day), "d", int64(d%day/time.Hour), "h")
	case d >= time.Hour:
		return pair(int64(d/time.Hour), "h", int64(d%time.Hour/time.Minute), "m")
	case d >= time.Minute:
		return pair(int64(d/time.Minute), "m", int64(d%time.Minute/time.Second), "s")
	default:
		return strconv.FormatInt(int64(d/time.Second), 10) + "s"
	}
}

func (r Report) writeSplit(p *printer) {
	if r.Split == nil {
		return
	}
	p.line("")
	p.line("Split by " + r.Split.By)
	p.line(strings.ToUpper(r.Split.By), "FINISHED", "STARTED", "PRS OPENED", "PRS MERGED", "CLAIM TO PR MEDIAN", "PR TO MERGE MEDIAN")
	for _, row := range r.Split.Rows {
		cells := append([]string{row.Group}, countCells(row.Counts)...)
		p.line(append(cells, seconds(row.ClaimToPR.MedianSeconds), seconds(row.PRToMerge.MedianSeconds))...)
	}
}

func (r Report) writeTree(p *printer) {
	if len(r.Tree) == 0 {
		return
	}
	p.line("")
	p.line("Tree")
	for _, n := range r.Tree {
		p.line(strings.Repeat("  ", n.Depth)+n.ID, p.cell(stateColor(n.State), n.State), n.Title)
	}
}

// stateColor is green for done, yellow for in progress, gray otherwise.
func stateColor(state string) string {
	switch state {
	case StateDone:
		return chart.Green
	case StateInProgress:
		return chart.Yellow
	default:
		return chart.Gray
	}
}
