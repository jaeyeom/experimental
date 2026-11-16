package tui

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"log/slog"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/jaeyeom/experimental/devtools/decide"
)

// busDownBanner is the top line while the bus cannot deliver.
const busDownBanner = "Bus down: messages to agents are waiting."

// defaultHealthTimeout bounds one `agentbus health --json`, which gives
// up on the daemon after 2s itself. The rest covers a hub round trip.
const defaultHealthTimeout = 5 * time.Second

// busHealthReport is the `agentbus health --json` object.
type busHealthReport struct {
	Daemon    string   `json:"daemon"`
	Transport string   `json:"transport"`
	Problems  []string `json:"problems"`
}

// down reports whether messages to agents are waiting.
func (r busHealthReport) down() bool {
	return r.Daemon == "down" || r.Transport == "down"
}

// healthTickMsg starts the next health check.
type healthTickMsg struct{}

// healthMsg is the result of one background health check.
type healthMsg struct {
	report busHealthReport
	err    error
}

// healthCheck runs `agentbus health --json` off the update loop. In hub
// mode the bus runner sends it to the hub.
func (m *Model) healthCheck() tea.Cmd {
	run, timeout := m.busRunner(), m.healthTimeout
	if timeout <= 0 {
		timeout = defaultHealthTimeout
	}
	return func() tea.Msg {
		ctx, cancel := context.WithTimeout(context.Background(), timeout)
		defer cancel()
		report, err := checkHealth(ctx, run)
		return healthMsg{report: report, err: err}
	}
}

// healthTick waits healthEvery before the next check, so checks never
// overlap.
func (m *Model) healthTick() tea.Cmd {
	every := m.healthEvery
	if every <= 0 {
		every = decide.DefaultBusHealthInterval
	}
	return tea.Tick(every, func(time.Time) tea.Msg { return healthTickMsg{} })
}

// updateHealth starts a check on its tick and schedules the next tick
// when one finishes. It reports whether msg was a health message.
func (m *Model) updateHealth(msg tea.Msg) (tea.Cmd, bool) {
	switch msg := msg.(type) {
	case healthTickMsg:
		return m.healthCheck(), true
	case healthMsg:
		m.finishHealth(msg)
		return m.healthTick(), true
	}
	return nil, false
}

// checkHealth runs health and parses its report. Health exits nonzero
// when the bus is down but still prints the report, so a report is
// used whatever the exit. No report is an error.
func checkHealth(ctx context.Context, run BusRunner) (busHealthReport, error) {
	out, err := run(ctx, []string{"health", "--json"})
	var report busHealthReport
	if jerr := json.Unmarshal(bytes.TrimSpace(out), &report); jerr == nil && report.Daemon != "" {
		return report, nil
	}
	if err == nil {
		err = fmt.Errorf("unexpected output %q", strings.TrimSpace(string(out)))
	}
	return busHealthReport{}, err
}

// finishHealth keeps a report. A check that could not report clears the
// banner, since a missing or old agentbus says nothing about the bus,
// and is logged once.
func (m *Model) finishHealth(msg healthMsg) {
	if msg.err != nil {
		m.busHealth = busHealthReport{}
		if !m.healthLogged {
			m.healthLogged = true
			slog.Debug("decide: agentbus health cannot report; no bus banner", "err", msg.err)
		}
		return
	}
	m.busHealth = msg.report
}

// bannerRows is the rows the bus banner takes.
func (m *Model) bannerRows() int {
	if m.busHealth.down() {
		return 1
	}
	return 0
}

// busBanner is the bus-down line with the first problem dimmed after
// it, cut to the content width.
func (m *Model) busBanner() (screenLine, bool) {
	if !m.busHealth.down() {
		return screenLine{}, false
	}
	text := busDownBanner
	if len(m.busHealth.Problems) > 0 {
		if problem := strings.Join(strings.Fields(m.busHealth.Problems[0]), " "); problem != "" {
			useStyles()
			text += "  " + dimNav.Render(problem)
		}
	}
	line := screenLine{text: text}
	if w := m.contentWidth(); w > 0 {
		line = fitLine(line, w)
	}
	return line, true
}
