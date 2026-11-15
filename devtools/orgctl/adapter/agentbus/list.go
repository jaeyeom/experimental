// Package agentbus reads registered names from the agentbus CLI.
// It runs only list --json.
package agentbus

import (
	"context"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
)

// Runner runs agentbus with args and returns stdout. args does not include
// the agentbus binary.
type Runner func(ctx context.Context, args []string) ([]byte, error)

// Name is one registered name. A herdr handle is split into Server,
// SessionID and Pane. Any other handle, and a herdr handle that does not
// split, stays raw.
type Name struct {
	Name      string `json:"name"`
	When      string `json:"when"`
	Liveness  string `json:"liveness"`
	Transport string `json:"transport"`
	Handle    string `json:"handle"`
	Server    string `json:"server,omitempty"`
	SessionID string `json:"sessionId,omitempty"`
	Pane      string `json:"pane,omitempty"`
	Raw       bool   `json:"raw,omitempty"`
}

// NotReadError means agentbus list could not be read.
// Plan prints the error and continues.
type NotReadError struct {
	Err error
}

func (e *NotReadError) Error() string {
	return fmt.Sprintf("agentbus: not read (%v)", e.Err)
}

func (e *NotReadError) Unwrap() error { return e.Err }

// List runs agentbus list --json. A nil run uses Exec. An unreachable
// agentbus is a NotReadError.
func List(ctx context.Context, run Runner) ([]Name, []string, error) {
	if run == nil {
		run = Exec
	}
	out, err := run(ctx, []string{"list", "--json"})
	if err != nil {
		return nil, nil, &NotReadError{Err: err}
	}
	var rows []Name
	if err := json.Unmarshal(out, &rows); err != nil {
		return nil, nil, &NotReadError{Err: err}
	}
	if rows == nil {
		rows = []Name{}
	}
	var warnings []string
	for i := range rows {
		server, session, pane, raw, warning := parseHandle(rows[i].Handle)
		rows[i].Server = server
		rows[i].SessionID = session
		rows[i].Pane = pane
		rows[i].Raw = raw
		if warning != "" {
			warnings = append(warnings, warning)
		}
	}
	return rows, warnings, nil
}

// parseHandle splits herdr:<server>/<session>/<pane>. The pane may contain
// colons. Other transports are raw and quiet. A herdr handle that does not
// split is raw and warned.
func parseHandle(handle string) (server, session, pane string, raw bool, warning string) {
	rest, ok := strings.CutPrefix(handle, "herdr:")
	if !ok {
		return "", "", "", true, ""
	}
	server, rest, ok = strings.Cut(rest, "/")
	session, pane, ok2 := strings.Cut(rest, "/")
	if !ok || !ok2 || server == "" || session == "" || pane == "" {
		return "", "", "", true, "malformed herdr handle: " + handle
	}
	return server, session, pane, false, ""
}

// Exec runs the installed agentbus binary. args must be list --json.
func Exec(ctx context.Context, args []string) ([]byte, error) {
	if strings.Join(args, " ") != "list --json" {
		return nil, fmt.Errorf("agentbus: command not allowed: %s", strings.Join(args, " "))
	}
	cmd := exec.CommandContext(ctx, "agentbus", args...) //nolint:gosec // G204: argv is the allowlisted list
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("agentbus %s: %w", strings.Join(args, " "), err)
	}
	return out, nil
}
