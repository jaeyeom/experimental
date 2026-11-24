package decide

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os/exec"
	"slices"
	"strings"
	"time"
)

const (
	// PluginProtocolVersion is the JSON-over-stdio transport plugin protocol version.
	PluginProtocolVersion = 1

	// DefaultPluginTimeout bounds each transport plugin invocation.
	DefaultPluginTimeout = 10 * time.Second

	pluginOpDeliver = "deliver"
	pluginOpStatus  = "status"
)

// PluginTarget is the target object sent in a v1 transport plugin request.
type PluginTarget struct {
	Scheme  string `json:"scheme"`
	Address string `json:"address"`
	Machine string `json:"machine,omitempty"`
}

// PluginRequest is the JSON request written to a transport plugin's stdin.
type PluginRequest struct {
	Version     int          `json:"version"`
	Op          string       `json:"op"`
	Target      PluginTarget `json:"target"`
	DeliveryKey string       `json:"deliveryKey,omitempty"`
	Item        *Item        `json:"item,omitempty"`
	Message     string       `json:"message,omitempty"`
}

// PluginDeliverResponse is the JSON response a transport plugin writes to
// stdout for op="deliver".
type PluginDeliverResponse struct {
	Outcome    string `json:"outcome"`
	Reason     string `json:"reason,omitempty"`
	Error      string `json:"error,omitempty"`
	ExternalID string `json:"externalId,omitempty"`
}

// PluginStatusResponse is the JSON response a transport plugin writes to
// stdout for op="status".
type PluginStatusResponse struct {
	State string `json:"state"`
	Label string `json:"label,omitempty"`
}

func checkTransports(transports map[string][]string) error {
	if len(transports) == 0 {
		return nil
	}
	schemes := make([]string, 0, len(transports))
	for scheme := range transports {
		schemes = append(schemes, scheme)
	}
	slices.Sort(schemes)
	for _, scheme := range schemes {
		argv := transports[scheme]
		if !isValidTransportScheme(scheme) {
			return fmt.Errorf("decide config transports[%q]: invalid scheme name (must match [a-z0-9][a-z0-9-]*)", scheme)
		}
		if isReservedTransportScheme(scheme) {
			return fmt.Errorf("decide config transports[%q]: cannot shadow built-in scheme", scheme)
		}
		if len(argv) == 0 || strings.TrimSpace(argv[0]) == "" {
			return fmt.Errorf("decide config transports[%q]: command argv must be non-empty", scheme)
		}
	}
	return nil
}

type pluginTransport struct {
	timeout time.Duration
}

func (p pluginTransport) effectiveTimeout() time.Duration {
	if p.timeout > 0 {
		return p.timeout
	}
	return DefaultPluginTimeout
}

func (p pluginTransport) DeliverAnswer(item *Item, target Target, cfg Config, _ io.Writer) (Delivery, error) {
	id := ""
	msg := ""
	if item != nil {
		id = item.ID
		answer, answeredBy := answerFields(item)
		msg = DeliveryNote(item.ID, answeredBy, answer)
	}
	req := PluginRequest{
		Version:     PluginProtocolVersion,
		Op:          pluginOpDeliver,
		Target:      PluginTarget(target),
		DeliveryKey: DeliveryKey(item),
		Item:        item,
		Message:     msg,
	}
	return deliverPlugin(context.Background(), "notify", id, target, cfg.Transports, req, p.effectiveTimeout())
}

func (p pluginTransport) DeliverNote(item *Item, id string, target Target, note string, cfg Config) (Delivery, error) {
	req := PluginRequest{
		Version: PluginProtocolVersion,
		Op:      pluginOpDeliver,
		Target:  PluginTarget(target),
		Item:    item,
		Message: note,
	}
	return deliverPlugin(context.Background(), "decide send", id, target, cfg.Transports, req, p.effectiveTimeout())
}

func deliverPlugin(ctx context.Context, op, id string, target Target, transports map[string][]string, req PluginRequest, timeout time.Duration) (Delivery, error) {
	wire := target.String()
	argv, ok := transports[target.Scheme]
	if !ok || len(argv) == 0 || strings.TrimSpace(argv[0]) == "" {
		return SentDelivery(id, wire, fmt.Errorf("%s: unknown transport %q", op, target.Scheme))
	}
	stdout, err := runPlugin(ctx, argv, req, timeout)
	if err != nil {
		return SentDelivery(id, wire, fmt.Errorf("%s: %w", op, err))
	}
	var resp PluginDeliverResponse
	if err := json.Unmarshal(stdout, &resp); err != nil {
		return SentDelivery(id, wire, fmt.Errorf("%s: invalid plugin response JSON: %w", op, err))
	}
	switch resp.Outcome {
	case OutcomeDelivered:
		return Delivery{
			Target:  wire,
			Outcome: OutcomeDelivered,
			BusID:   resp.ExternalID,
		}, nil
	case OutcomeWaiting:
		return Delivery{
			Target:  wire,
			Outcome: OutcomeWaiting,
			Reason:  resp.Reason,
			BusID:   resp.ExternalID,
		}, nil
	case OutcomeFailed:
		msg := strings.TrimSpace(resp.Error)
		if msg == "" {
			msg = strings.TrimSpace(resp.Reason)
		}
		if msg == "" {
			msg = "plugin reported delivery failure"
		}
		return SentDelivery(id, wire, fmt.Errorf("%s: %s", op, msg))
	default:
		return SentDelivery(id, wire, fmt.Errorf("%s: invalid plugin outcome %q", op, resp.Outcome))
	}
}

func pluginStatus(ctx context.Context, argv []string, target Target) (PluginStatusResponse, error) {
	req := PluginRequest{
		Version: PluginProtocolVersion,
		Op:      pluginOpStatus,
		Target:  PluginTarget(target),
	}
	stdout, err := runPlugin(ctx, argv, req, DefaultPluginTimeout)
	if err != nil {
		return PluginStatusResponse{}, err
	}
	var resp PluginStatusResponse
	if err := json.Unmarshal(stdout, &resp); err != nil {
		return PluginStatusResponse{}, fmt.Errorf("invalid plugin status JSON: %w", err)
	}
	switch resp.State {
	case LocationLive, LocationClosed, LocationUnknown, LocationUnreachable:
		return resp, nil
	default:
		return PluginStatusResponse{State: LocationUnknown, Label: resp.Label}, nil
	}
}

func runPlugin(ctx context.Context, argv []string, req PluginRequest, timeout time.Duration) ([]byte, error) {
	if len(argv) == 0 || strings.TrimSpace(argv[0]) == "" {
		return nil, fmt.Errorf("transport plugin command is empty")
	}
	if ctx == nil {
		ctx = context.Background()
	}
	if timeout <= 0 {
		timeout = DefaultPluginTimeout
	}
	callCtx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()

	var stdinBuf bytes.Buffer
	enc := json.NewEncoder(&stdinBuf)
	enc.SetEscapeHTML(false)
	if err := enc.Encode(req); err != nil {
		return nil, fmt.Errorf("encode plugin request: %w", err)
	}

	// The plugin argv comes from operator config; never invoke a shell.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(callCtx, argv[0], argv[1:]...) //nolint:gosec // G204: transport plugin argv is operator config, executed directly without a shell
	cmd.WaitDelay = 200 * time.Millisecond
	cmd.Stdin = &stdinBuf
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		if errors.Is(callCtx.Err(), context.DeadlineExceeded) {
			return nil, fmt.Errorf("transport plugin %q timed out after %s%s", argv[0], timeout, stderrSuffix(stderr.String()))
		}
		return nil, fmt.Errorf("transport plugin %q: %w%s", argv[0], err, stderrSuffix(stderr.String()))
	}
	return stdout.Bytes(), nil
}
