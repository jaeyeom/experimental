package decide

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/pkg/remotecall"
)

const (
	// DefaultSendTemplate is the compose command when config sets none.
	// {pane} is one argument. The note is appended as its own argument
	// and is not interpolated into a shell command.
	DefaultSendTemplate = "herdr agent prompt {pane}"

	// SendsName is the send history file inside the state directory.
	SendsName = "sends.jsonl"

	// MachineTimeout bounds a send to a pane on another machine, as
	// agentbus bounds each herdr call to another machine.
	MachineTimeout = 5 * time.Second

	// BusPanePrefix marks a notify pane as an agentbus name, which is
	// not on any one machine.
	BusPanePrefix = "agentbus:"
)

// SendRecord is one line of the send history.
type SendRecord struct {
	Target string `json:"target"`
	Pane   string `json:"pane"`
	Text   string `json:"text"`
}

// FindTarget returns the recipient with name.
func FindTarget(targets []ConfigTarget, name string) (ConfigTarget, bool) {
	for _, tg := range targets {
		if tg.Name == name {
			return tg, true
		}
	}
	return ConfigTarget{}, false
}

// SendArgv builds the compose command. An empty template is
// DefaultSendTemplate. {pane} is replaced in the template arguments.
// The note is the last argument and is not passed through a shell.
func SendArgv(template, pane, text string) []string {
	if strings.TrimSpace(template) == "" {
		template = DefaultSendTemplate
	}
	parts := strings.Fields(template)
	for i, part := range parts {
		parts[i] = strings.ReplaceAll(part, "{pane}", pane)
	}
	return append(parts, text)
}

// MachineArgv puts `--machine machine` after the program in argv, so a
// herdr command reaches a pane on that machine. An empty machine keeps
// argv as it is.
func MachineArgv(argv []string, machine string) []string {
	if machine == "" || len(argv) == 0 {
		return argv
	}
	return append([]string{argv[0], "--machine", machine}, argv[1:]...)
}

// RunSend executes argv. The note is already its own argument.
// An argv from MachineArgv runs under MachineTimeout, and a machine that
// cannot be reached fails with a *remotecall.UnreachableError in the
// error chain, as agentbus reports it.
func RunSend(argv []string) error {
	if len(argv) == 0 {
		return fmt.Errorf("decide send: empty command")
	}
	if len(argv) >= 3 && argv[1] == "--machine" {
		return runSendOn(argv[2], argv[0], argv[3:])
	}
	// The note is already its own argument. Operator config chooses the program.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command(argv[0], argv[1:]...) //nolint:gosec // G204: the note is its own argument, not a shell string
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = io.Discard
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("decide send: %w", err)
	}
	if err := stdoutDeliveryError(stdout.String()); err != nil {
		return logDelivery("", fmt.Errorf("decide send: %w", err))
	}
	return nil
}

// runSendOn runs program with `--machine machine args...` under
// MachineTimeout. The probe asks herdr whether the machine answers.
func runSendOn(machine, program string, args []string) error {
	stdout, stderr, code, err := runOnMachine(context.Background(), machine, program, args)
	if err != nil {
		return fmt.Errorf("decide send: %w", err)
	}
	if code != 0 {
		return fmt.Errorf("decide send: %s on machine %s: exit %d: %s", program, machine, code, strings.TrimSpace(stderr))
	}
	if err := stdoutDeliveryError(stdout); err != nil {
		return logDelivery("", fmt.Errorf("decide send: %w", err))
	}
	return nil
}

// stdoutDeliveryError reports a delivery whose command exited 0 but
// printed JSON with a top-level error object. Other stdout is success.
func stdoutDeliveryError(stdout string) error {
	raw := strings.TrimSpace(stdout)
	if raw == "" {
		return nil
	}
	var body struct {
		Error json.RawMessage `json:"error"`
	}
	if err := json.Unmarshal([]byte(raw), &body); err != nil {
		return nil
	}
	if len(body.Error) == 0 || string(body.Error) == "null" || body.Error[0] != '{' {
		return nil
	}
	return fmt.Errorf("command reported %s", raw)
}

// undeliveredBus reports an agentbus send that was held or queued.
// Those states exit 0 and are not delivery.
func undeliveredBus(stdout string) error {
	if !busStateHeldOrQueued(busSendState(stdout)) {
		return nil
	}
	return fmt.Errorf("agentbus send %s", strings.TrimSpace(stdout))
}

func busSendState(stdout string) string {
	raw := strings.TrimSpace(stdout)
	if raw == "" {
		return ""
	}
	if raw[0] == '{' {
		var body struct {
			State string `json:"state"`
		}
		if err := json.Unmarshal([]byte(raw), &body); err != nil {
			return ""
		}
		return body.State
	}
	_, state, ok := strings.Cut(raw, " ")
	if !ok {
		return ""
	}
	return state
}

func busStateHeldOrQueued(state string) bool {
	state = strings.ToLower(strings.TrimSpace(state))
	return state == "held" || state == "queued" ||
		strings.HasPrefix(state, "held:") || strings.HasPrefix(state, "held ") ||
		strings.HasPrefix(state, "queued:") || strings.HasPrefix(state, "queued ")
}

// runOnMachine runs program with `--machine machine args...` under
// MachineTimeout. The probe asks herdr whether the machine answers, so a
// machine that cannot be reached fails with a *remotecall.UnreachableError.
func runOnMachine(ctx context.Context, machine, program string, args []string) (string, string, int, error) {
	run := func(ctx context.Context, argv []string) (string, string, int, error) {
		res, err := remotecall.Run(ctx, program, argv...)
		return res.Stdout, res.Stderr, res.Code, err //nolint:wrapcheck // OnHerdrMachine reports it
	}
	herdr := func(ctx context.Context, argv []string) (string, string, int, error) {
		res, err := remotecall.Run(ctx, "herdr", argv...)
		return res.Stdout, res.Stderr, res.Code, err //nolint:wrapcheck // the probe only reads success
	}
	probe := remotecall.HerdrProbe(herdr, machine)
	return remotecall.OnHerdrMachine(ctx, run, probe, machine, MachineTimeout, args) //nolint:wrapcheck // callers add context
}

// NotifyOwner runs the compose command so pane on machine sees the
// thread header, the original text, and the reply hint. The author is
// human. An empty machine is this machine. An empty template uses
// DefaultSendTemplate. The text stored on the thread is not changed here.
func NotifyOwner(pane, machine, id, text string) error {
	if strings.TrimSpace(pane) == "" {
		return fmt.Errorf("decide compose pane is empty")
	}
	if strings.TrimSpace(id) == "" {
		return fmt.Errorf("decide compose id is empty")
	}
	if name, ok := strings.CutPrefix(pane, BusPanePrefix); ok {
		return deliverBus(id, strings.TrimSpace(name), DeliveryNote(id, "human", text))
	}
	cfg, err := LoadConfig()
	if err != nil {
		return err
	}
	return RunSend(MachineArgv(SendArgv(cfg.SendTemplate, pane, DeliveryNote(id, "human", text)), machine))
}

// AppendSend records one successful send under dir.
func AppendSend(dir, name, pane, text string) error {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("decide send history: %w", err)
	}
	//nolint:gosec // G304: history stays under the ledger directory
	f, err := os.OpenFile(filepath.Join(dir, SendsName), os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o600)
	if err != nil {
		return fmt.Errorf("decide send history: %w", err)
	}
	enc := json.NewEncoder(f)
	enc.SetEscapeHTML(false)
	werr := enc.Encode(SendRecord{Target: name, Pane: pane, Text: text})
	cerr := f.Close()
	if werr != nil {
		return fmt.Errorf("decide send history: %w", werr)
	}
	if cerr != nil {
		return fmt.Errorf("decide send history: %w", cerr)
	}
	return nil
}
