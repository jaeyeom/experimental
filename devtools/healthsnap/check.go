package healthsnap

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"
)

// CmdRequest describes one external command invocation for a check.
type CmdRequest struct {
	Argv    []string
	Dir     string
	Env     []string
	Timeout time.Duration
}

// CmdResult holds the completed process output and exit code.
type CmdResult struct {
	ExitCode int
	Stdout   string
	Stderr   string
}

// CmdRunner executes a check command without a shell.
type CmdRunner func(ctx context.Context, req CmdRequest) (CmdResult, error)

// StatFunc stats a file path for file-age checks.
type StatFunc func(path string) (fs.FileInfo, error)

// CheckDeps bundles injectable OS/clock dependencies for check execution.
type CheckDeps struct {
	RunCmd   CmdRunner
	StatFile StatFunc
	Host     HostReader
	Now      func() time.Time
}

func (d CheckDeps) withDefaults() CheckDeps {
	if d.RunCmd == nil {
		d.RunCmd = ExecCmd
	}
	if d.StatFile == nil {
		d.StatFile = os.Stat
	}
	if d.Host == nil {
		d.Host = SystemHostReader{}
	}
	if d.Now == nil {
		d.Now = time.Now
	}
	return d
}

// ExecCmd executes req.Argv directly without a shell and returns non-zero exits
// in CmdResult unless execution failed or timed out.
func ExecCmd(ctx context.Context, req CmdRequest) (CmdResult, error) {
	if len(req.Argv) == 0 {
		return CmdResult{}, errors.New("empty command argv")
	}
	runCtx := ctx
	if req.Timeout > 0 {
		var cancel context.CancelFunc
		runCtx, cancel = context.WithTimeout(ctx, req.Timeout)
		defer cancel()
	}
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(runCtx, req.Argv[0], req.Argv[1:]...) //nolint:gosec // G204: user-configured argv without shell
	if req.Dir != "" {
		cmd.Dir = req.Dir
	}
	if len(req.Env) > 0 {
		cmd.Env = req.Env
	}
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err := cmd.Run()
	if err == nil {
		return CmdResult{ExitCode: 0, Stdout: stdout.String(), Stderr: stderr.String()}, nil
	}
	if errors.Is(runCtx.Err(), context.DeadlineExceeded) {
		return CmdResult{}, fmt.Errorf("command timed out after %s: %w", req.Timeout, context.DeadlineExceeded)
	}
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		return CmdResult{ExitCode: exitErr.ExitCode(), Stdout: stdout.String(), Stderr: stderr.String()}, nil
	}
	return CmdResult{}, fmt.Errorf("run %s: %w", req.Argv[0], err)
}

type checkEvalResult struct {
	value      any
	display    string
	vars       map[string]any
	durationMs int64
	execErr    string
}

func executeCheck(ctx context.Context, cfg CheckConfig, baseEnv []string, deps CheckDeps) checkEvalResult {
	start := deps.Now()
	var res checkEvalResult
	switch cfg.Kind {
	case KindCommand:
		res = executeCommandCheck(ctx, cfg, baseEnv, deps)
	case KindFileAge:
		res = executeFileAgeCheck(cfg, baseEnv, deps)
	case KindHost:
		res = executeHostCheck(ctx, cfg, baseEnv, deps)
	default:
		res = checkEvalResult{execErr: fmt.Sprintf("unsupported check kind %q", cfg.Kind)}
	}
	elapsed := deps.Now().Sub(start)
	if elapsed < 0 {
		elapsed = 0
	}
	res.durationMs = elapsed.Milliseconds()
	if res.vars != nil {
		res.vars["duration_ms"] = res.durationMs
		res.vars["durationMs"] = res.durationMs
	}
	return res
}

func executeCommandCheck(ctx context.Context, cfg CheckConfig, baseEnv []string, deps CheckDeps) checkEvalResult {
	req := CmdRequest{
		Argv:    cfg.Argv,
		Dir:     ExpandHome(cfg.Dir, baseEnv),
		Env:     mergeEnv(baseEnv, cfg.Env),
		Timeout: cfg.TimeoutDuration(),
	}
	cmdRes, err := deps.RunCmd(ctx, req)
	if err != nil {
		return checkEvalResult{execErr: err.Error()}
	}
	vars := map[string]any{
		"exit":   cmdRes.ExitCode,
		"stdout": strings.TrimSpace(cmdRes.Stdout),
		"stderr": strings.TrimSpace(cmdRes.Stderr),
	}
	val, display, err := parseCommandResult(cfg.Parse, cmdRes, vars)
	if err != nil {
		msg := err.Error()
		if s := strings.TrimSpace(cmdRes.Stderr); s != "" {
			msg = fmt.Sprintf("%s (exit=%d stderr=%s)", msg, cmdRes.ExitCode, truncateText(s, 160))
		}
		return checkEvalResult{execErr: msg}
	}
	vars["value"] = val
	return checkEvalResult{
		value:   val,
		display: display,
		vars:    vars,
	}
}

func parseCommandResult(p ParseConfig, cmdRes CmdResult, vars map[string]any) (any, string, error) {
	switch p.Mode {
	case ParseExit:
		return cmdRes.ExitCode, fmt.Sprintf("exit %d", cmdRes.ExitCode), nil
	case ParseJSONPath:
		val, err := p.compiledJP.EvalBytes([]byte(cmdRes.Stdout))
		if err != nil {
			return nil, "", err
		}
		return val, formatScalar(val), nil
	case ParseRegex:
		return evalRegexParser(p, cmdRes.Stdout, vars)
	default:
		return nil, "", fmt.Errorf("unsupported parse mode %q", p.Mode)
	}
}

func evalRegexParser(p ParseConfig, stdout string, vars map[string]any) (any, string, error) {
	lines := splitNonEmptyLines(stdout)
	hasGroup := p.compiledRE.NumSubexp() > 0
	var matches int
	var firstMatch string
	var firstCapture string

	for _, line := range lines {
		m := p.compiledRE.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		if matches == 0 {
			firstMatch = m[0]
			if hasGroup && len(m) > 1 {
				firstCapture = m[1]
			}
		}
		matches++
	}

	vars["matches"] = matches
	vars["matched"] = matches > 0
	vars["match"] = firstMatch

	if !hasGroup {
		cnt := float64(matches)
		return cnt, fmt.Sprintf("%d match(es)", matches), nil
	}
	if matches == 0 {
		return nil, "no match", nil
	}
	if num, err := strconv.ParseFloat(firstCapture, 64); err == nil {
		return num, formatScalar(num), nil
	}
	return firstCapture, firstCapture, nil
}

func executeFileAgeCheck(cfg CheckConfig, baseEnv []string, deps CheckDeps) checkEvalResult {
	target := ExpandHome(cfg.Path, baseEnv)
	info, err := deps.StatFile(target)
	if err != nil {
		return checkEvalResult{execErr: err.Error()}
	}
	age := deps.Now().Sub(info.ModTime())
	if age < 0 {
		age = 0
	}
	secs := age.Seconds()
	vars := map[string]any{
		"value":       secs,
		"age":         secs,
		"age_seconds": secs,
		"ageSeconds":  secs,
		"size":        info.Size(),
		"mtime":       info.ModTime().UTC().Format(time.RFC3339),
	}
	return checkEvalResult{
		value:   secs,
		display: age.Round(time.Second).String(),
		vars:    vars,
	}
}

func executeHostCheck(ctx context.Context, cfg CheckConfig, baseEnv []string, deps CheckDeps) checkEvalResult {
	switch cfg.Metric {
	case "load", "load1", "load5", "load15", "load-per-cpu":
		return evalHostLoad(ctx, cfg.Metric, deps.Host)
	case "memory", "memory-pressure":
		return evalHostMemory(ctx, deps.Host)
	case "disk", "free-disk", "free-disk-gib", "disk-used":
		return evalHostDisk(ctx, cfg.Metric, ExpandHome(cfg.Path, baseEnv), deps.Host)
	default:
		return checkEvalResult{execErr: fmt.Sprintf("unsupported host metric %q", cfg.Metric)}
	}
}

func evalHostLoad(ctx context.Context, metric string, reader HostReader) checkEvalResult {
	st, err := reader.Load(ctx)
	if err != nil {
		return checkEvalResult{execErr: err.Error()}
	}
	val := st.Load1
	switch metric {
	case "load5":
		val = st.Load5
	case "load15":
		val = st.Load15
	case "load-per-cpu":
		val = st.LoadPerCPU
	}
	vars := map[string]any{
		"value":        val,
		"load1":        st.Load1,
		"load5":        st.Load5,
		"load15":       st.Load15,
		"cpus":         st.CPUs,
		"load_per_cpu": st.LoadPerCPU,
		"loadPerCpu":   st.LoadPerCPU,
	}
	return checkEvalResult{
		value:   val,
		display: fmt.Sprintf("%.2f (1m=%.2f 5m=%.2f 15m=%.2f cpus=%d)", val, st.Load1, st.Load5, st.Load15, st.CPUs),
		vars:    vars,
	}
}

func evalHostMemory(ctx context.Context, reader HostReader) checkEvalResult {
	st, err := reader.Memory(ctx)
	if err != nil {
		return checkEvalResult{execErr: err.Error()}
	}
	availMiB := float64(st.AvailableBytes) / float64(1<<20)
	vars := map[string]any{
		"value":           st.UsedPercent,
		"used_percent":    st.UsedPercent,
		"usedPercent":     st.UsedPercent,
		"available_bytes": float64(st.AvailableBytes),
		"availableBytes":  float64(st.AvailableBytes),
		"available_mib":   availMiB,
		"availableMib":    availMiB,
		"total_bytes":     float64(st.TotalBytes),
		"totalBytes":      float64(st.TotalBytes),
		"pressure_level":  st.PressureLevel,
		"pressureLevel":   st.PressureLevel,
		"psi_some_avg10":  st.PSISomeAvg10,
	}
	return checkEvalResult{
		value:   st.UsedPercent,
		display: fmt.Sprintf("%.1f%% used (%.0f MiB free, %s)", st.UsedPercent, availMiB, st.PressureLevel),
		vars:    vars,
	}
}

func evalHostDisk(ctx context.Context, metric, path string, reader HostReader) checkEvalResult {
	st, err := reader.Disk(ctx, path)
	if err != nil {
		return checkEvalResult{execErr: err.Error()}
	}
	val := st.FreePercent
	switch metric {
	case "free-disk-gib":
		val = st.FreeGiB
	case "disk-used":
		val = st.UsedPercent
	}
	vars := map[string]any{
		"value":        val,
		"free_percent": st.FreePercent,
		"freePercent":  st.FreePercent,
		"used_percent": st.UsedPercent,
		"usedPercent":  st.UsedPercent,
		"free_bytes":   float64(st.FreeBytes),
		"freeBytes":    float64(st.FreeBytes),
		"free_gib":     st.FreeGiB,
		"freeGiB":      st.FreeGiB,
		"total_bytes":  float64(st.TotalBytes),
		"totalBytes":   float64(st.TotalBytes),
	}
	return checkEvalResult{
		value:   val,
		display: fmt.Sprintf("%.1f%% free (%.1f GiB)", st.FreePercent, st.FreeGiB),
		vars:    vars,
	}
}

func mergeEnv(baseEnv []string, extra map[string]string) []string {
	if len(extra) == 0 {
		return baseEnv
	}
	merged := make([]string, 0, len(baseEnv)+len(extra))
	merged = append(merged, baseEnv...)
	for k, v := range extra {
		merged = append(merged, k+"="+v)
	}
	return merged
}

func splitNonEmptyLines(s string) []string {
	if s == "" {
		return nil
	}
	raw := strings.Split(s, "\n")
	if raw[len(raw)-1] == "" {
		raw = raw[:len(raw)-1]
	}
	return raw
}

func formatScalar(v any) string {
	switch x := v.(type) {
	case nil:
		return "null"
	case float64:
		return strconv.FormatFloat(x, 'f', -1, 64)
	case string:
		return x
	default:
		return fmt.Sprint(x)
	}
}

func truncateText(s string, maxLen int) string {
	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen] + "..."
}
