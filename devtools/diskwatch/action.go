package diskwatch

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
)

// Exec runs argv without a shell. env replaces the process environment.
func Exec(ctx context.Context, argv []string, env []string) error {
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...) //nolint:gosec // G204: configured argv, not a shell
	cmd.Env = env
	out, err := cmd.CombinedOutput()
	if err != nil {
		msg := strings.TrimSpace(string(out))
		if msg == "" {
			return fmt.Errorf("on_low: %w", err)
		}
		return fmt.Errorf("on_low: %w: %s", err, msg)
	}
	return nil
}

func actionEnv(base []string, mount string, free, total uint64, host, summary, summaryFile string) []string {
	env := append([]string{}, base...)
	env = append(env,
		"DISKWATCH_MOUNT="+mount,
		"DISKWATCH_FREE_BYTES="+strconv.FormatUint(free, 10),
		"DISKWATCH_FREE_PCT="+strconv.FormatFloat(FreePct(free, total), 'f', 2, 64),
		"DISKWATCH_TOTAL_BYTES="+strconv.FormatUint(total, 10),
		"DISKWATCH_HOST="+host,
		"DISKWATCH_SUMMARY="+summary,
		"DISKWATCH_SUMMARY_FILE="+summaryFile,
	)
	return env
}

func writeSummaryFile(dir, mount string, summary Summary) (string, error) {
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", fmt.Errorf("create summary dir: %w", err)
	}
	safe := strings.NewReplacer("/", "_", "\\", "_", ":", "_").Replace(mount)
	path := filepath.Join(dir, "summary-"+safe+".json")
	body, err := json.Marshal(struct {
		Mount   string  `json:"mount"`
		Entries []Entry `json:"entries"`
		Partial bool    `json:"partial"`
	}{Mount: mount, Entries: summary.Entries, Partial: summary.Partial})
	if err != nil {
		return "", fmt.Errorf("encode summary: %w", err)
	}
	body = append(body, '\n')
	if err := os.WriteFile(path, body, 0o600); err != nil {
		return "", fmt.Errorf("write summary: %w", err)
	}
	return path, nil
}
