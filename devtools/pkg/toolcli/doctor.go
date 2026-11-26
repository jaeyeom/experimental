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

// DoctorOptions configures RunDoctor.
type DoctorOptions struct {
	Offline bool
	JSON    bool
	Extra   []CheckResult
}

// RunDoctor executes standard and extra doctor checks and writes the output.
// It returns the protocol exit code (0 ok, 1 warnings, 2 failures).
func (t *Tool) RunDoctor(ctx context.Context, opts DoctorOptions, stdout io.Writer) (int, error) {
	checks := t.runDoctorChecks(ctx, opts.Offline)
	checks = append(checks, opts.Extra...)
	overall := aggregateStatus(checks)
	out := DoctorOutput{
		Tool:   t.Name,
		Status: overall,
		Checks: checks,
	}

	if opts.JSON {
		if err := writeJSON(stdout, out); err != nil {
			return 2, err
		}
		return statusExitCode(overall), nil
	}

	if err := printDoctorHumanColored(stdout, checks, t.useColor(stdout)); err != nil {
		return 2, err
	}
	return statusExitCode(overall), nil
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

	code, err := t.RunDoctor(ctx, DoctorOptions{
		Offline: *offline,
		JSON:    *jsonOut,
	}, stdout)
	if err != nil {
		return failErr(stderr, err, code)
	}
	return code
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
	for _, fn := range t.ExtraDoctorCheckGroups {
		checks = append(checks, fn(ctx, offline)...)
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
	bi := t.buildInfo()
	current := currentRevLabel(bi)
	latest, err := t.installer().LatestRevision(ctx, t.modulePath(bi), defaultRef)
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
	if isAhead(bi, latest) {
		return CheckResult{
			Name:   "revision",
			Status: StatusOK,
			Detail: fmt.Sprintf("%s (ahead of %s)", current, defaultRef),
		}
	}
	if current == "unknown" || (newRev != "" && !buildMatchesLatest(bi, latest)) {
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
	if t.SkillText == "" && len(t.ExtraSkills) == 0 {
		return CheckResult{
			Name:   "skills",
			Status: StatusOK,
			Detail: "no embedded skill",
		}
	}
	if err := validateAllSkillFiles(t.skills()); err != nil {
		return CheckResult{
			Name:   "skills",
			Status: StatusFail,
			Detail: err.Error(),
		}
	}
	entries := t.collectSkillStatus()
	var installed, stale []string
	for _, e := range entries {
		label := fmt.Sprintf("%s (%s)", e.Agent, e.Scope)
		if e.Skill != "" && e.Skill != t.Name {
			label = fmt.Sprintf("%s/%s (%s)", e.Agent, e.Skill, e.Scope)
		}
		switch e.Status {
		case "current":
			installed = append(installed, label)
		case "stale":
			stale = append(stale, label)
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

func printDoctorHumanColored(w io.Writer, checks []CheckResult, color bool) error {
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
		if _, err := fmt.Fprintf(w, "%s  %-*s  %s\n", badge, nameWidth, c.Name, c.Detail); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
		if c.Status != StatusOK && c.Hint != "" {
			hint := paint("hint: "+c.Hint, ansiDim, color)
			if _, err := fmt.Fprintf(w, "%s%s\n", hintIndent, hint); err != nil {
				return fmt.Errorf("write output: %w", err)
			}
		}
	}
	return nil
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

func (t *Tool) useColor(w io.Writer) bool {
	if t.NoColor {
		return false
	}
	return useColor(w)
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
