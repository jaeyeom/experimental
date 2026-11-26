package herdr

import (
	"bytes"
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
)

func decodeEnvelope(raw []byte, wantType string) (map[string]json.RawMessage, error) {
	var env struct {
		ID     json.RawMessage `json:"id"`
		Result json.RawMessage `json:"result"`
	}
	if err := json.Unmarshal(raw, &env); err != nil {
		return nil, fmt.Errorf("decode herdr response: expected JSON object with id and result, found %s: %w", quoteSnippet(raw), err)
	}
	if err := requireJSONString(env.ID, "id"); err != nil {
		return nil, fmt.Errorf("%w: %w", ErrDegraded, err)
	}
	if isNull(env.Result) {
		return nil, fmt.Errorf("%w: expected result object, found missing", ErrDegraded)
	}
	var result map[string]json.RawMessage
	if err := json.Unmarshal(env.Result, &result); err != nil {
		return nil, fmt.Errorf("%w: expected result object, found %s: %w", ErrDegraded, quoteSnippet(env.Result), err)
	}
	rawType, err := objectField(result, "type")
	if err != nil {
		return nil, fmt.Errorf("%w: expected result.type %q, found missing", ErrDegraded, wantType)
	}
	var got string
	if err := json.Unmarshal(rawType, &got); err != nil {
		return nil, fmt.Errorf("%w: expected result.type %q, found %s", ErrDegraded, wantType, quoteSnippet(rawType))
	}
	if got != wantType {
		return nil, fmt.Errorf("%w: expected result.type %q, found %q", ErrDegraded, wantType, got)
	}
	return result, nil
}

func objectField(obj map[string]json.RawMessage, key string) (json.RawMessage, error) {
	raw, ok := obj[key]
	if !ok || isNull(raw) {
		return nil, fmt.Errorf("%w: expected %s, found missing", ErrDegraded, key)
	}
	return raw, nil
}

func decodeList[T any](result map[string]json.RawMessage, key string, decode func(json.RawMessage) (T, error)) ([]T, error) {
	raw, err := objectField(result, key)
	if err != nil {
		return nil, err
	}
	var items []json.RawMessage
	if err := json.Unmarshal(raw, &items); err != nil {
		return nil, fmt.Errorf("%w: expected %s array, found %s: %w", ErrDegraded, key, quoteSnippet(raw), err)
	}
	out := make([]T, 0, len(items))
	for i, item := range items {
		value, err := decode(item)
		if err != nil {
			return nil, fmt.Errorf("%s item %d: %w", key, i, err)
		}
		out = append(out, value)
	}
	return out, nil
}

func decodeWorkspace(raw json.RawMessage) (Workspace, error) {
	var ws Workspace
	if err := decodeJSON(raw, &ws, "workspace", "workspace_id", "label", "agent_status"); err != nil {
		return Workspace{}, err
	}
	if err := checkAgentStatus(ws.AgentStatus); err != nil {
		return Workspace{}, fmt.Errorf("%w: workspace %q: %w", ErrDegraded, ws.ID, err)
	}
	return ws, nil
}

func decodeTab(raw json.RawMessage) (Tab, error) {
	var tab Tab
	if err := decodeJSON(raw, &tab, "tab", "tab_id", "workspace_id", "label", "agent_status", "pane_count"); err != nil {
		return Tab{}, err
	}
	if err := checkAgentStatus(tab.AgentStatus); err != nil {
		return Tab{}, fmt.Errorf("%w: tab %q: %w", ErrDegraded, tab.ID, err)
	}
	if tab.PaneCount < 0 {
		return Tab{}, fmt.Errorf("%w: tab %q: expected pane_count >= 0, found %d", ErrDegraded, tab.ID, tab.PaneCount)
	}
	return tab, nil
}

func decodePane(raw json.RawMessage) (Pane, error) {
	var pane Pane
	if err := decodeJSON(raw, &pane, "pane", "pane_id", "tab_id", "workspace_id"); err != nil {
		return Pane{}, err
	}
	return pane, nil
}

func decodeListedPane(raw json.RawMessage) (Pane, error) {
	var pane Pane
	if err := decodeJSON(raw, &pane, "pane", "pane_id", "tab_id", "workspace_id", "agent_status"); err != nil {
		return Pane{}, err
	}
	if err := checkAgentStatus(pane.AgentStatus); err != nil {
		return Pane{}, fmt.Errorf("%w: pane %q: %w", ErrDegraded, pane.ID, err)
	}
	return pane, nil
}

func decodeJSON(raw json.RawMessage, dest any, what string, keys ...string) error {
	if err := requireKeys(raw, keys...); err != nil {
		return fmt.Errorf("%w: %s: %w", ErrDegraded, what, err)
	}
	if err := json.Unmarshal(raw, dest); err != nil {
		return fmt.Errorf("%w: %s: expected %s, found %s: %w", ErrDegraded, what, strings.Join(keys, ", "), quoteSnippet(raw), err)
	}
	return nil
}

func requireKeys(raw json.RawMessage, keys ...string) error {
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(raw, &obj); err != nil {
		return fmt.Errorf("expected object with %s, found %s", strings.Join(keys, ", "), quoteSnippet(raw))
	}
	var missing []string
	for _, key := range keys {
		value, ok := obj[key]
		if !ok || isNull(value) {
			missing = append(missing, key)
		}
	}
	if len(missing) == 0 {
		return nil
	}
	return fmt.Errorf("expected %s, found missing", strings.Join(missing, ", "))
}

func requireJSONString(raw json.RawMessage, key string) error {
	if isNull(raw) {
		return fmt.Errorf("expected string %s, found missing", key)
	}
	var value string
	if err := json.Unmarshal(raw, &value); err != nil {
		return fmt.Errorf("expected string %s, found %s", key, quoteSnippet(raw))
	}
	return nil
}

func checkAgentStatus(status string) error {
	switch status {
	case AgentStatusIdle, AgentStatusWorking, AgentStatusBlocked, AgentStatusDone, AgentStatusUnknown:
		return nil
	default:
		return fmt.Errorf("expected agent_status one of idle, working, blocked, done, unknown, found %q", status)
	}
}

func requireAgent(result map[string]json.RawMessage) error {
	raw, err := objectField(result, "agent")
	if err != nil {
		return err
	}
	var agent struct {
		PaneID      *string `json:"pane_id"`      //nolint:tagliatelle // herdr JSON wire format uses snake_case
		AgentStatus *string `json:"agent_status"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
	}
	if err := json.Unmarshal(raw, &agent); err != nil {
		return fmt.Errorf("%w: expected agent object, found %s: %w", ErrDegraded, quoteSnippet(raw), err)
	}
	if agent.PaneID == nil || agent.AgentStatus == nil {
		return fmt.Errorf("%w: agent: expected pane_id and agent_status, found %s", ErrDegraded, quoteSnippet(raw))
	}
	if err := checkAgentStatus(*agent.AgentStatus); err != nil {
		return fmt.Errorf("%w: agent %q: %w", ErrDegraded, *agent.PaneID, err)
	}
	return nil
}

func requireStringArray(result map[string]json.RawMessage, key string) error {
	raw, err := objectField(result, key)
	if err != nil {
		return err
	}
	var items []string
	if err := json.Unmarshal(raw, &items); err != nil {
		return fmt.Errorf("%w: expected %s array of strings, found %s: %w", ErrDegraded, key, quoteSnippet(raw), err)
	}
	return nil
}

// decodeCloseResult reads a successful herdr tab close.
// tab_closed must name wantID. ok and any other result.type are success too:
// the installed herdr uses ok after the tab has closed, and a zero exit with
// a result object is success rather than only tab_closed.
func decodeCloseResult(raw []byte, wantID string) (string, error) {
	var env struct {
		ID     json.RawMessage `json:"id"`
		Result json.RawMessage `json:"result"`
	}
	if err := json.Unmarshal(raw, &env); err != nil {
		return "", fmt.Errorf("decode herdr response: expected JSON object with id and result, found %s: %w", quoteSnippet(raw), err)
	}
	if err := requireJSONString(env.ID, "id"); err != nil {
		return "", fmt.Errorf("%w: %w", ErrDegraded, err)
	}
	if isNull(env.Result) {
		return "", fmt.Errorf("%w: expected result object, found missing", ErrDegraded)
	}
	var result map[string]json.RawMessage
	if err := json.Unmarshal(env.Result, &result); err != nil {
		return "", fmt.Errorf("%w: expected result object, found %s: %w", ErrDegraded, quoteSnippet(env.Result), err)
	}
	rawType, err := objectField(result, "type")
	if err != nil {
		return "", fmt.Errorf("%w: expected result.type, found missing", ErrDegraded)
	}
	var got string
	if err := json.Unmarshal(rawType, &got); err != nil {
		return "", fmt.Errorf("%w: expected result.type string, found %s", ErrDegraded, quoteSnippet(rawType))
	}
	if got == "" {
		return "", fmt.Errorf("%w: expected a result.type, found empty", ErrDegraded)
	}
	if got == "tab_closed" {
		gotID, err := jsonString(result, "tab_id")
		if err != nil {
			return "", err
		}
		if _, err := jsonString(result, "workspace_id"); err != nil {
			return "", err
		}
		if gotID != wantID {
			return "", fmt.Errorf("herdr tab close: expected tab %s, found %s", wantID, gotID)
		}
	}
	return got, nil
}

func jsonString(obj map[string]json.RawMessage, key string) (string, error) {
	raw, err := objectField(obj, key)
	if err != nil {
		return "", err
	}
	var value string
	if err := json.Unmarshal(raw, &value); err != nil {
		return "", fmt.Errorf("%w: expected string %s, found %s", ErrDegraded, key, quoteSnippet(raw))
	}
	if value == "" {
		return "", fmt.Errorf("%w: expected non-empty %s, found empty", ErrDegraded, key)
	}
	return value, nil
}

func commandErrorCode(stdout, stderr string) string {
	for _, blob := range []string{stdout, stderr} {
		code, ok := errorCode(blob)
		if ok {
			return code
		}
	}
	return ""
}

func errorCode(blob string) (string, bool) {
	blob = strings.TrimSpace(blob)
	if blob == "" || blob[0] != '{' {
		return "", false
	}
	var env struct {
		Error struct {
			Code string `json:"code"`
		} `json:"error"`
	}
	if err := json.Unmarshal([]byte(blob), &env); err != nil || env.Error.Code == "" {
		return "", false
	}
	return env.Error.Code, true
}

// optionalString accepts a JSON string or null. Null is empty.
type optionalString string

func (s *optionalString) UnmarshalJSON(raw []byte) error {
	if isNull(raw) {
		*s = ""
		return nil
	}
	var value string
	if err := json.Unmarshal(raw, &value); err != nil {
		return fmt.Errorf("expected a string or null, found %s", quoteSnippet(raw))
	}
	*s = optionalString(value)
	return nil
}

// UnmarshalJSON reads a herdr pane.
// cwd and foreground_cwd may be a string or null. Null is empty.
func (p *Pane) UnmarshalJSON(raw []byte) error {
	var body struct {
		ID            string         `json:"pane_id"`      //nolint:tagliatelle // herdr JSON wire format uses snake_case
		TabID         string         `json:"tab_id"`       //nolint:tagliatelle // herdr JSON wire format uses snake_case
		WorkspaceID   string         `json:"workspace_id"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
		AgentStatus   string         `json:"agent_status"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
		Cwd           optionalString `json:"cwd"`
		ForegroundCwd optionalString `json:"foreground_cwd"` //nolint:tagliatelle // herdr JSON wire format uses snake_case
	}
	if err := json.Unmarshal(raw, &body); err != nil {
		return fmt.Errorf("decode herdr pane: %w", err)
	}
	*p = Pane{
		ID:            body.ID,
		TabID:         body.TabID,
		WorkspaceID:   body.WorkspaceID,
		AgentStatus:   body.AgentStatus,
		Cwd:           string(body.Cwd),
		ForegroundCwd: string(body.ForegroundCwd),
	}
	return nil
}

func isNull(raw json.RawMessage) bool {
	trimmed := bytes.TrimSpace(raw)
	return len(trimmed) == 0 || bytes.Equal(trimmed, []byte("null"))
}

func quoteSnippet(raw []byte) string {
	const maxSnippet = 180
	text := strings.TrimSpace(string(raw))
	if len(text) > maxSnippet {
		text = text[:maxSnippet] + "..."
	}
	return strconv.Quote(text)
}
