package quotapace

import (
	"fmt"
	"math"
	"strings"
	"time"
)

const (
	dashboardChartRows = 11
	dashboardChartCols = 48
)

// DashboardOptions configures how Snapshot.Dashboard formats the dashboard.
type DashboardOptions struct {
	Stale   time.Duration
	Profile string
}

// Dashboard renders a plain-text per-profile, per-window dashboard of quota
// usage, plan projections, reset countdowns, and reading freshness.
func (s Snapshot) Dashboard(o DashboardOptions) string {
	if len(s.Windows()) == 0 {
		return emptyDashboardHint(o.Profile)
	}
	var b strings.Builder
	fmt.Fprintf(&b, "quotapace dashboard (%s)\n", s.Now.UTC().Format("2006-01-02 15:04 UTC"))
	for _, ps := range s.Profiles {
		for _, ws := range ps.Windows {
			b.WriteByte('\n')
			writeDashboardWindow(&b, ps, ws, s.Now, o.Stale)
		}
	}
	return b.String()
}

func emptyDashboardHint(profile string) string {
	p := profile
	if p == "" {
		p = "work"
	}
	var b strings.Builder
	if profile != "" {
		fmt.Fprintf(&b, "No quota data for profile %q.\n\n", profile)
	} else {
		b.WriteString("No quota data yet.\n\n")
	}
	b.WriteString("Record a reading and start a pacing plan:\n")
	fmt.Fprintf(&b, "  quotapace record --profile %s --window 7d --percent 25 --resets-in 5d\n", p)
	fmt.Fprintf(&b, "  quotapace plan start --profile %s --window 7d\n", p)
	return b.String()
}

func writeDashboardWindow(b *strings.Builder, ps ProfileStatus, ws WindowStatus, now time.Time, stale time.Duration) {
	hasPlan := ws.Plan != nil && !ws.Plan.Implicit
	isStale, age := readingStaleness(ws.Latest, now, stale)
	fmt.Fprintf(b, "%s  %s  [%s]\n", ps.Profile, ws.Window, dashboardBadge(ws, hasPlan, isStale))

	chartStart, chartEnd, loc := dashboardBounds(ws)
	if chartEnd.After(chartStart) {
		writeDashboardChart(b, ps.Offset, ws, hasPlan, chartStart, chartEnd, now, loc)
	}
	if hasPlan {
		b.WriteString("  Legend: * sample  . plan  : projection\n")
	} else {
		b.WriteString("  Legend: * sample\n")
	}
	writeDashboardPlanLine(b, ps.Offset, ws, hasPlan, isStale)
	writeDashboardResetLine(b, chartEnd, now, loc)
	writeDashboardFreshnessLine(b, ws.Latest, isStale, age, stale)
}

func readingStaleness(latest *Latest, now time.Time, stale time.Duration) (bool, time.Duration) {
	if latest == nil {
		return true, 0
	}
	age := now.Sub(latest.At)
	return stale > 0 && age > stale, age
}

func dashboardBadge(ws WindowStatus, hasPlan, isStale bool) string {
	if !hasPlan {
		switch {
		case ws.Latest == nil:
			return "no plan, missing"
		case isStale:
			return "no plan, stale"
		default:
			return "no plan"
		}
	}
	if isStale && ws.Latest != nil {
		return ws.State + ", stale"
	}
	return ws.State
}

func dashboardBounds(ws WindowStatus) (start, end time.Time, loc *time.Location) {
	loc = time.UTC
	if ws.Plan != nil {
		if l, err := ws.plan.Location(); err == nil && l != nil {
			loc = l
		}
		return ws.Plan.BaselineAt, ws.Plan.ResetsAt, loc
	}
	if ws.Latest != nil {
		if length, err := WindowLength(ws.Window); err == nil {
			return ws.Latest.ResetsAt.Add(-length), ws.Latest.ResetsAt, loc
		}
		return ws.Latest.At, ws.Latest.ResetsAt, loc
	}
	return time.Time{}, time.Time{}, loc
}

type chartGrid [dashboardChartRows][dashboardChartCols]byte

func writeDashboardChart(b *strings.Builder, offset float64, ws WindowStatus, hasPlan bool, start, end, now time.Time, loc *time.Location) {
	grid := newChartGrid()
	span := end.Sub(start)

	if hasPlan {
		plotPlanLine(&grid, ws, start, span)
		plotProjectionLine(&grid, ws, start, end, span)
	}
	plotSamples(&grid, offset, ws, start, span)

	renderChartRows(b, &grid)
	renderChartAxis(b, start, end, now, span, loc)
}

func newChartGrid() chartGrid {
	var grid chartGrid
	for r := range grid {
		for c := range grid[r] {
			grid[r][c] = ' '
		}
	}
	return grid
}

func plotPlanLine(grid *chartGrid, ws WindowStatus, start time.Time, span time.Duration) {
	for c := 0; c < dashboardChartCols; c++ {
		t := colTime(start, span, c)
		grid[pctRow(ws.plan.OnTrack(t))][c] = '.'
	}
}

func plotProjectionLine(grid *chartGrid, ws WindowStatus, start, end time.Time, span time.Duration) {
	if ws.Projection == nil || !end.After(ws.Projection.At) {
		return
	}
	projSpan := end.Sub(ws.Projection.At)
	for c := 0; c < dashboardChartCols; c++ {
		t := colTime(start, span, c)
		if t.Before(ws.Projection.At) || t.After(end) {
			continue
		}
		frac := float64(t.Sub(ws.Projection.At)) / float64(projSpan)
		y := ws.Projection.Percent + (ws.Projection.Projected-ws.Projection.Percent)*frac
		grid[pctRow(y)][c] = ':'
	}
}

func plotSamples(grid *chartGrid, offset float64, ws WindowStatus, start time.Time, span time.Duration) {
	for _, r := range ws.History.Readings {
		if c, ok := timeCol(r.At, start, span); ok {
			grid[pctRow(r.Percent+offset)][c] = '*'
		}
	}
	if ws.Latest != nil {
		if c, ok := timeCol(ws.Latest.At, start, span); ok {
			grid[pctRow(ws.Latest.Percent+offset)][c] = '*'
		}
	}
}

func renderChartRows(b *strings.Builder, grid *chartGrid) {
	for r := dashboardChartRows - 1; r >= 0; r-- {
		if r%2 == 0 {
			fmt.Fprintf(b, "  %3d%% | ", r*10)
		} else {
			b.WriteString("       | ")
		}
		b.WriteString(strings.TrimRight(string(grid[r][:]), " "))
		b.WriteByte('\n')
	}
}

func renderChartAxis(b *strings.Builder, start, end, now time.Time, span time.Duration, loc *time.Location) {
	axis := make([]byte, dashboardChartCols)
	for i := range axis {
		axis[i] = '-'
	}
	axis[0] = '+'
	axis[dashboardChartCols-1] = '+'
	nowCol, hasNow := timeCol(now, start, span)
	if hasNow && nowCol > 0 && nowCol < dashboardChartCols-1 {
		axis[nowCol] = '+'
	}
	b.WriteString("         ")
	b.Write(axis)
	b.WriteByte('\n')

	writeChartLabels(b, start, end, loc, nowCol, hasNow)
}

func writeChartLabels(b *strings.Builder, start, end time.Time, loc *time.Location, nowCol int, hasNow bool) {
	startLabel := start.In(loc).Format("Jan 02 15:04")
	endLabel := end.In(loc).Format("Jan 02 15:04")
	buf := make([]byte, dashboardChartCols)
	for i := range buf {
		buf[i] = ' '
	}
	copy(buf, startLabel)
	endPos := dashboardChartCols - len(endLabel)
	if endPos > len(startLabel) {
		copy(buf[endPos:], endLabel)
	}
	if hasNow {
		nowPos := nowCol - 1
		if nowPos > len(startLabel)+1 && nowPos+3 < endPos-1 {
			copy(buf[nowPos:], "now")
		}
	}
	b.WriteString("         ")
	b.WriteString(strings.TrimRight(string(buf), " "))
	b.WriteByte('\n')
}

func colTime(start time.Time, span time.Duration, col int) time.Time {
	return start.Add(time.Duration(float64(span) * float64(col) / float64(dashboardChartCols-1)))
}

func timeCol(t, start time.Time, span time.Duration) (int, bool) {
	end := start.Add(span)
	if span <= 0 || t.Before(start) || t.After(end) {
		return 0, false
	}
	col := int(math.Round(float64(t.Sub(start)) / float64(span) * float64(dashboardChartCols-1)))
	return max(0, min(dashboardChartCols-1, col)), true
}

func pctRow(pct float64) int {
	clamped := max(0, min(Limit, pct))
	return int(math.Round(clamped / Limit * float64(dashboardChartRows-1)))
}

func writeDashboardPlanLine(b *strings.Builder, offset float64, ws WindowStatus, hasPlan, isStale bool) {
	if !hasPlan {
		if ws.Latest != nil {
			fmt.Fprintf(b, "  Plan: none (current %.0f%%)\n", ws.Latest.Percent+offset)
			return
		}
		b.WriteString("  Plan: none (no reading)\n")
		return
	}
	if ws.Projection == nil {
		fmt.Fprintf(b, "  Plan: no reading vs %.0f%% on-track | %s | target %.0f%%\n",
			ws.Plan.OnTrack, ws.State, ws.Plan.Target)
		return
	}
	gap := ws.Plan.OnTrack - ws.Projection.Percent
	state := ws.State
	if isStale {
		state = "stale"
	}
	fmt.Fprintf(b, "  Plan: %.0f%% vs %.0f%% on-track (gap %+.1f pts) %s | target %.0f%%, projected %.0f%%\n",
		ws.Projection.Percent, ws.Plan.OnTrack, gap, state, ws.Plan.Target, ws.Projection.Projected)
}

func writeDashboardResetLine(b *strings.Builder, resetsAt, now time.Time, loc *time.Location) {
	if resetsAt.IsZero() {
		b.WriteString("  Reset: unknown\n")
		return
	}
	stamp := resetsAt.In(loc).Format("2006-01-02 15:04 MST")
	if resetsAt.After(now) {
		fmt.Fprintf(b, "  Reset: in %s (%s)\n", formatSpan(resetsAt.Sub(now)), stamp)
		return
	}
	fmt.Fprintf(b, "  Reset: passed %s ago (%s)\n", formatSpan(now.Sub(resetsAt)), stamp)
}

func writeDashboardFreshnessLine(b *strings.Builder, latest *Latest, isStale bool, age, stale time.Duration) {
	if latest == nil {
		b.WriteString("  Freshness: no reading (stale)\n")
		return
	}
	src := ""
	if latest.Source != "" {
		src = " (" + latest.Source + ")"
	}
	if isStale {
		fmt.Fprintf(b, "  Freshness: %s ago (stale, > %s)%s\n", formatSpan(age), formatSpan(stale), src)
		return
	}
	fmt.Fprintf(b, "  Freshness: %s ago%s\n", formatSpan(age), src)
}

func formatSpan(d time.Duration) string {
	if d < 0 {
		d = 0
	}
	if d >= 24*time.Hour {
		days := int(d / (24 * time.Hour))
		remHours := int((d % (24 * time.Hour)) / time.Hour)
		if remHours == 0 {
			return fmt.Sprintf("%dd", days)
		}
		return fmt.Sprintf("%dd%dh", days, remHours)
	}
	if d >= time.Hour {
		h := int(d / time.Hour)
		m := int((d % time.Hour) / time.Minute)
		if m == 0 {
			return fmt.Sprintf("%dh", h)
		}
		return fmt.Sprintf("%dh%dm", h, m)
	}
	if d >= time.Minute {
		return fmt.Sprintf("%dm", int(d/time.Minute))
	}
	return fmt.Sprintf("%ds", int(d/time.Second))
}
