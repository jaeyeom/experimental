package slackwatch

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

// agentbusListTimeout bounds one agentbus list call.
const agentbusListTimeout = 5 * time.Second

// AgentbusNames runs `agentbus list --json` and returns the agent names.
// The error does not include command output beyond a short stderr summary.
func AgentbusNames(ctx context.Context) ([]string, error) {
	path, err := exec.LookPath("agentbus")
	if err != nil {
		return nil, errors.New("agentbus not on PATH")
	}
	ctx, cancel := context.WithTimeout(ctx, agentbusListTimeout)
	defer cancel()
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, path, "list", "--json") //nolint:gosec // G204: fixed argv, path from LookPath.
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg == "" {
			msg = err.Error()
		}
		return nil, fmt.Errorf("agentbus list: %s", msg)
	}
	var rows []struct {
		Name string `json:"name"`
	}
	if err := json.Unmarshal(stdout.Bytes(), &rows); err != nil {
		return nil, fmt.Errorf("agentbus list: parse output: %w", err)
	}
	names := make([]string, 0, len(rows))
	for _, row := range rows {
		if row.Name != "" {
			names = append(names, row.Name)
		}
	}
	return names, nil
}
