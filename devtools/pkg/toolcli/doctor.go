package toolcli

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

// DoctorOutput is the JSON payload for `<tool> doctor --json`.
type DoctorOutput struct {
	Tool   string        `json:"tool"`
	Status CheckStatus   `json:"status"`
	Checks []CheckResult `json:"checks"`
}

func (t *Tool) cmdDoctor(ctx context.Context, args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("doctor", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "output JSON")
	offline := fs.Bool("offline", false, "skip network-dependent checks")
	var customHandler func(ctx context.Context, stdout, stderr io.Writer) (int, bool)
	if t.RegisterDoctorFlags != nil {
		customHandler = t.RegisterDoctorFlags(fs)
	}
	if err := fs.Parse(args); err != nil {
		return 2
	}
	if fs.NArg() != 0 {
		if t.DoctorUsage != "" {
			return failMsg(stderr, "usage: "+t.DoctorUsage, 2)
		}
		return failMsg(stderr, "doctor does not take positional arguments", 2)
	}
	if customHandler != nil {
		if code, handled := customHandler(ctx, stdout, stderr); handled {
			return code
		}
	}

	checks := t.runDoctorChecks(ctx, *offline)
	overall := aggregateStatus(checks)
	out := DoctorOutput{
		Tool:   t.Name,
		Status: overall,
		Checks: checks,
	}

	if *jsonOut {
		if err := writeJSON(stdout, out); err != nil {
			return failErr(stderr, err, 2)
		}
		return statusExitCode(overall)
	}

	printDoctorHuman(stdout, checks)
	return statusExitCode(overall)
}

func (t *Tool) runDoctorChecks(ctx context.Context, offline bool) []CheckResult {
	checks := []CheckResult{
		t.checkBinaryOnPath(),
		t.checkRevision(ctx, offline),
		t.checkConfig(),
		t.checkExternalCommands(),
		t.checkSkills(),
	}
	for _, fn := range t.ExtraDoctorChecks {
		checks = append(checks, fn(ctx, offline))
	}
	return checks
}

func (t *Tool) checkBinaryOnPath() CheckResult {
	onPath, err := t.lookPath(t.Name)
	if err != nil {
		return CheckResult{
			Name:   "binary-on-path",
			Status: StatusWarn,
			Detail: fmt.Sprintf("%s not found on PATH", t.Name),
			Hint:   fmt.Sprintf("add the directory containing %s to PATH", t.Name),
		}
	}
	resolvedPath, err := filepath.EvalSymlinks(onPath)
	if err != nil {
		resolvedPath = onPath
	}
	exe, err := t.executable()
	if err != nil {
		return CheckResult{
			Name:   "binary-on-path",
			Status: StatusWarn,
			Detail: fmt.Sprintf("found %s on PATH; cannot resolve running binary: %v", onPath, err),
		}
	}
	if resolvedExe, err := filepath.EvalSymlinks(exe); err == nil {
		exe = resolvedExe
	}
	if resolvedPath != exe {
		return CheckResult{
			Name:   "binary-on-path",
			Status: StatusWarn,
			Detail: fmt.Sprintf("PATH resolves %s, running %s", resolvedPath, exe),
			Hint:   fmt.Sprintf("ensure %s is ahead on PATH", filepath.Dir(exe)),
		}
	}
	return CheckResult{
		Name:   "binary-on-path",
		Status: StatusOK,
		Detail: resolvedPath,
	}
}

func (t *Tool) checkRevision(ctx context.Context, offline bool) CheckResult {
	if offline {
		return CheckResult{
			Name:   "revision",
			Status: StatusWarn,
			Detail: "skipped (--offline)",
			Hint:   "run without --offline to check for updates",
		}
	}
	if t.MainPackage == "" {
		return CheckResult{
			Name:   "revision",
			Status: StatusWarn,
			Detail: "main package not configured",
		}
	}
	current := currentRevLabel(t.buildInfo())
	latest, err := t.installer().LatestRevision(ctx, t.MainPackage, "latest")
	if err != nil {
		return CheckResult{
			Name:   "revision",
			Status: StatusWarn,
			Detail: fmt.Sprintf("cannot check latest revision: %v", err),
			Hint:   "check network connectivity or pass --offline",
		}
	}
	newRev := latest.Revision
	if newRev == "" {
		newRev = latest.Version
	}
	if current == "unknown" || !revisionsMatch(current, newRev) {
		return CheckResult{
			Name:   "revision",
			Status: StatusWarn,
			Detail: fmt.Sprintf("installed %s, latest %s", current, newRev),
			Hint:   fmt.Sprintf("run `%s update`", t.Name),
		}
	}
	return CheckResult{
		Name:   "revision",
		Status: StatusOK,
		Detail: current,
	}
}

func (t *Tool) checkConfig() CheckResult {
	if t.CheckConfig != nil {
		if err := t.CheckConfig(); err != nil {
			return CheckResult{
				Name:   "config",
				Status: StatusFail,
				Detail: err.Error(),
				Hint:   "fix the configuration file syntax or values",
			}
		}
	}
	if t.ConfigPath != nil {
		path, ok := t.ConfigPath()
		if !ok {
			return CheckResult{
				Name:   "config",
				Status: StatusWarn,
				Detail: "config path unavailable",
			}
		}
		if _, err := os.Stat(path); errors.Is(err, fs.ErrNotExist) {
			return CheckResult{
				Name:   "config",
				Status: StatusOK,
				Detail: fmt.Sprintf("%s (not present; using defaults)", path),
			}
		} else if err != nil {
			return CheckResult{
				Name:   "config",
				Status: StatusFail,
				Detail: err.Error(),
			}
		}
		return CheckResult{
			Name:   "config",
			Status: StatusOK,
			Detail: path,
		}
	}
	return CheckResult{
		Name:   "config",
		Status: StatusOK,
		Detail: "no config file required",
	}
}

func (t *Tool) checkExternalCommands() CheckResult {
	if len(t.RequiredCommands) == 0 {
		return CheckResult{
			Name:   "external-commands",
			Status: StatusOK,
			Detail: "none required",
		}
	}
	var missing []string
	for _, cmd := range t.RequiredCommands {
		if _, err := t.lookPath(cmd); err != nil {
			missing = append(missing, cmd)
		}
	}
	if len(missing) > 0 {
		return CheckResult{
			Name:   "external-commands",
			Status: StatusFail,
			Detail: "missing: " + strings.Join(missing, ", "),
			Hint:   "install missing commands and ensure they are on PATH",
		}
	}
	return CheckResult{
		Name:   "external-commands",
		Status: StatusOK,
		Detail: strings.Join(t.RequiredCommands, ", "),
	}
}

func (t *Tool) checkSkills() CheckResult {
	if t.SkillText == "" {
		return CheckResult{
			Name:   "skills",
			Status: StatusOK,
			Detail: "no embedded skill",
		}
	}
	entries := t.collectSkillStatus()
	var installed, stale []string
	for _, e := range entries {
		switch e.Status {
		case "current":
			installed = append(installed, fmt.Sprintf("%s (%s)", e.Agent, e.Scope))
		case "stale":
			stale = append(stale, fmt.Sprintf("%s (%s)", e.Agent, e.Scope))
		}
	}
	if len(stale) > 0 {
		return CheckResult{
			Name:   "skills",
			Status: StatusWarn,
			Detail: "stale: " + strings.Join(stale, ", "),
			Hint:   fmt.Sprintf("run `%s skill install --agent all`", t.Name),
		}
	}
	if len(installed) == 0 {
		return CheckResult{
			Name:   "skills",
			Status: StatusOK,
			Detail: "no agent skills installed",
		}
	}
	return CheckResult{
		Name:   "skills",
		Status: StatusOK,
		Detail: "current: " + strings.Join(installed, ", "),
	}
}

func aggregateStatus(checks []CheckResult) CheckStatus {
	status := StatusOK
	for _, c := range checks {
		if c.Status == StatusFail {
			return StatusFail
		}
		if c.Status == StatusWarn {
			status = StatusWarn
		}
	}
	return status
}

func statusExitCode(status CheckStatus) int {
	switch status {
	case StatusWarn:
		return 1
	case StatusFail:
		return 2
	default:
		return 0
	}
}

func printDoctorHuman(w io.Writer, checks []CheckResult) {
	printDoctorHumanColored(w, checks, useColor(w))
}

func printDoctorHumanColored(w io.Writer, checks []CheckResult, color bool) {
	const minNameWidth = 18
	nameWidth := minNameWidth
	for _, c := range checks {
		if len(c.Name) > nameWidth {
			nameWidth = len(c.Name)
		}
	}
	// Icon (2 display cols) + space + status (4) + 2 spaces + nameWidth + 2 spaces.
	hintIndent := strings.Repeat(" ", 11+nameWidth)

	for _, c := range checks {
		icon, ansi := statusStyle(c.Status)
		badge := paint(fmt.Sprintf("%s %-4s", icon, c.Status), ansi, color)
		_, _ = fmt.Fprintf(w, "%s  %-*s  %s\n", badge, nameWidth, c.Name, c.Detail)
		if c.Status != StatusOK && c.Hint != "" {
			hint := paint("hint: "+c.Hint, ansiDim, color)
			_, _ = fmt.Fprintf(w, "%s%s\n", hintIndent, hint)
		}
	}
}

const (
	ansiReset  = "\x1b[0m"
	ansiDim    = "\x1b[2m"
	ansiRed    = "\x1b[31m"
	ansiGreen  = "\x1b[32m"
	ansiYellow = "\x1b[33m"
)

func statusStyle(status CheckStatus) (icon, ansi string) {
	switch status {
	case StatusOK:
		return "✅", ansiGreen
	case StatusWarn:
		return "⚠️", ansiYellow
	case StatusFail:
		return "❌", ansiRed
	default:
		return " ", ""
	}
}

func paint(text, ansi string, color bool) string {
	if !color || ansi == "" {
		return text
	}
	return ansi + text + ansiReset
}

func useColor(w io.Writer) bool {
	if os.Getenv("NO_COLOR") != "" || os.Getenv("TERM") == "dumb" {
		return false
	}
	return isTerminal(w)
}

func isTerminal(w io.Writer) bool {
	file, ok := w.(*os.File)
	if !ok {
		return false
	}
	info, err := file.Stat()
	return err == nil && info.Mode()&os.ModeCharDevice != 0
}
