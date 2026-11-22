package decide

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
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

	// BusSendTimeout bounds one `agentbus send`, which only queues the note.
	BusSendTimeout = 10 * time.Second
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

// BusRecipient is one name from `agentbus list --json`. Handle is its
// herdr handle. Delivery does not type into that pane.
type BusRecipient struct {
	Name   string
	Handle string
}

// SendRecipient is who `decide send --to` delivers to. Bus means the
// note goes through agentbus, and Pane is agentbus:<name>.
type SendRecipient struct {
	Name string
	Pane string
	Bus  bool
}

// SamePane reports whether a bus handle holds pane. A herdr handle ends
// in "/<pane>". An empty handle or pane does not match.
func SamePane(handle, pane string) bool {
	if handle == "" || pane == "" {
		return false
	}
	return handle == pane || strings.HasSuffix(handle, "/"+pane)
}

// ResolveSend picks name from targets and, when agentbus is set, bus.
// A configured target whose pane a bus handle holds is left out. A
// configured pane that starts with BusPanePrefix is that agentbus name,
// not a herdr pane, even when the agentbus list is off. The first exact
// name wins, and configured targets come before bus names. When
// agentbus is false, bus is ignored.
func ResolveSend(targets []ConfigTarget, bus []BusRecipient, agentbus bool, name string) (SendRecipient, bool) {
	if !agentbus {
		tg, ok := FindTarget(targets, name)
		if !ok {
			return SendRecipient{}, false
		}
		return recipientForTarget(tg), true
	}
	kept := make([]ConfigTarget, 0, len(targets))
	for _, tg := range targets {
		if busHoldsPane(bus, tg.Pane) {
			continue
		}
		kept = append(kept, tg)
	}
	if tg, ok := FindTarget(kept, name); ok {
		return recipientForTarget(tg), true
	}
	for _, b := range bus {
		if b.Name == name {
			return SendRecipient{Name: b.Name, Pane: BusPanePrefix + b.Name, Bus: true}, true
		}
	}
	return SendRecipient{}, false
}

// recipientForTarget is the send recipient for one configured target.
// An agentbus: pane is delivered with OpenBusThread under the name
// after the prefix, which is the target's display name when the pane
// is agentbus:<that name>.
func recipientForTarget(tg ConfigTarget) SendRecipient {
	if rec, ok := AgentbusPaneRecipient(tg.Pane); ok {
		return rec
	}
	return SendRecipient{Name: tg.Name, Pane: tg.Pane}
}

// AgentbusPaneRecipient reports a pane that names an agentbus recipient.
// The name is the text after BusPanePrefix. An empty name is not a bus pane.
func AgentbusPaneRecipient(pane string) (SendRecipient, bool) {
	name, ok := strings.CutPrefix(strings.TrimSpace(pane), BusPanePrefix)
	name = strings.TrimSpace(name)
	if !ok || name == "" {
		return SendRecipient{}, false
	}
	return SendRecipient{Name: name, Pane: BusPanePrefix + name, Bus: true}, true
}

func busHoldsPane(bus []BusRecipient, pane string) bool {
	for _, b := range bus {
		if SamePane(b.Handle, pane) {
			return true
		}
	}
	return false
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
	return runSend(argv, nil)
}

// runSend executes argv. A non-nil env replaces the process environment.
func runSend(argv, env []string) error {
	if len(argv) == 0 {
		return fmt.Errorf("decide send: empty command")
	}
	if len(argv) >= 3 && argv[1] == "--machine" {
		return runSendOn(argv[2], argv[0], argv[3:], env)
	}
	// The note is already its own argument. Operator config chooses the program.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command(argv[0], argv[1:]...) //nolint:gosec // G204: the note is its own argument, not a shell string
	if len(env) > 0 {
		cmd.Env = env
	}
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("decide send: %w%s", err, stderrSuffix(stderr.String()))
	}
	if err := stdoutDeliveryError(stdout.String()); err != nil {
		return logDelivery("", fmt.Errorf("decide send: %w", err))
	}
	return nil
}

// stderrSuffix is the command's stderr for an error, or empty when the
// command wrote nothing there.
func stderrSuffix(stderr string) string {
	msg := strings.TrimSpace(stderr)
	if msg == "" {
		return ""
	}
	return ": " + msg
}

// runSendOn runs program with `--machine machine args...` under
// MachineTimeout. The probe asks herdr whether the machine answers.
func runSendOn(machine, program string, args, env []string) error {
	stdout, stderr, code, err := runOnMachine(context.Background(), machine, program, args, env)
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
// printed JSON with a top-level error object, or said the pane or agent
// was not found. Other stdout is success.
func stdoutDeliveryError(stdout string) error {
	raw := strings.TrimSpace(stdout)
	if raw == "" {
		return nil
	}
	var body struct {
		Error json.RawMessage `json:"error"`
	}
	if err := json.Unmarshal([]byte(raw), &body); err == nil {
		if len(body.Error) > 0 && string(body.Error) != "null" && body.Error[0] == '{' {
			return fmt.Errorf("command reported %s", raw)
		}
	}
	if phrase := missedTarget(raw); phrase != "" {
		return fmt.Errorf("%s", phrase)
	}
	return nil
}

// missedTarget reports a pane or agent the command said it could not find.
// Empty means the text is not that kind of failure.
func missedTarget(text string) string {
	low := strings.ToLower(text)
	switch {
	case strings.Contains(low, "pane not found"):
		return "pane not found"
	case strings.Contains(low, "agent not found"):
		return "agent not found"
	default:
		return ""
	}
}

// undeliveredBus reports an agentbus send that was held or queued.
// The message was accepted. classifyBusSend records that as queued.
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

// classifyBusSend decides whether agentbus send accepted the message.
// Held or queued stdout is accepted, including the held exit 3.
// queued is true when the bus held or queued the message instead of
// delivering it now. A non-zero exit other than that, or JSON with a
// top-level error object, is an error.
func classifyBusSend(stdout string, runErr error) (queued bool, err error) {
	if err := stdoutDeliveryError(stdout); err != nil {
		return false, err
	}
	if undeliveredBus(stdout) != nil {
		return true, nil
	}
	if runErr != nil {
		return false, fmt.Errorf("agentbus send: %w", runErr)
	}
	return false, nil
}

// runOnMachine runs program with `--machine machine args...` under
// MachineTimeout. The probe asks herdr whether the machine answers, so a
// machine that cannot be reached fails with a *remotecall.UnreachableError.
func runOnMachine(ctx context.Context, machine, program string, args, env []string) (string, string, int, error) {
	run := func(ctx context.Context, argv []string) (string, string, int, error) {
		res, err := remotecall.RunEnv(ctx, env, program, argv...)
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
// The attempt comes back as a Delivery for Ledger.RecordCompose.
func NotifyOwner(pane, machine, id, text string) (Delivery, error) {
	return NotifyOwnerVerbatim(pane, machine, id, text, "")
}

// NotifyOwnerVerbatim is NotifyOwner with an optional verbatim block.
func NotifyOwnerVerbatim(pane, machine, id, text, verbatim string) (Delivery, error) {
	if strings.TrimSpace(pane) == "" {
		return Delivery{}, fmt.Errorf("decide compose pane is empty")
	}
	if strings.TrimSpace(id) == "" {
		return Delivery{}, fmt.Errorf("decide compose id is empty")
	}
	note := DeliveryNoteVerbatim(id, "human", text, verbatim)
	if name, ok := strings.CutPrefix(pane, BusPanePrefix); ok {
		return deliverBus(id, strings.TrimSpace(name), note)
	}
	cfg, err := LoadConfig()
	if err != nil {
		return Delivery{}, err
	}
	target := pane
	if machine != "" {
		target = pane + "@" + machine
	}
	return SentDelivery(id, target, RunSend(MachineArgv(SendArgv(cfg.SendTemplate, pane, note), machine)))
}

// SendBus runs `agentbus send name note` and describes it as a Delivery.
// BusID is the message id, the first word of stdout. Held or queued
// stdout, including the held exit 3, is waiting, not a failure. A failure
// returns agentbus's error. A nil run uses ExecAgentbus. A nil ctx uses
// context.Background. The call is bounded by BusSendTimeout.
func SendBus(ctx context.Context, run AgentbusRunner, name, note string) (Delivery, error) {
	if run == nil {
		run = ExecAgentbus
	}
	if ctx == nil {
		ctx = context.Background()
	}
	ctx, cancel := context.WithTimeout(ctx, BusSendTimeout)
	defer cancel()
	out, err := run(ctx, []string{"send", name, note})
	target := BusPanePrefix + name
	if _, cerr := classifyBusSend(string(out), err); cerr != nil {
		if err == nil {
			err = cerr
		}
		return failedDelivery(target, err), err
	}
	return busSendOutcome(target, "", string(out), err)
}

// busSendID is the message id agentbus send printed: the first word of
// text output, or the id of JSON output.
func busSendID(stdout string) string {
	raw := strings.TrimSpace(stdout)
	if strings.HasPrefix(raw, "{") {
		var body struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal([]byte(raw), &body); err != nil {
			return ""
		}
		return body.ID
	}
	id, _, _ := strings.Cut(raw, " ")
	return id
}

// OpenBusThread stores a thread addressed to an agentbus name and delivers
// text with SendBus. via is ViaCLI or ViaUI. An empty forward starts a
// new thread; otherwise the note names that item. A failed send deletes
// the thread. The returned item includes the bus message id and the
// delivery record of its first message.
func OpenBusThread(led *Ledger, dir, forward, name, text, via string, run AgentbusRunner) (Item, error) {
	return OpenBusThreadVerbatim(led, dir, forward, name, text, "", via, run)
}

// OpenBusThreadVerbatim is OpenBusThread with an optional verbatim block.
func OpenBusThreadVerbatim(led *Ledger, dir, forward, name, text, verbatim, via string, run AgentbusRunner) (Item, error) {
	pane := BusPanePrefix + name
	item, err := led.ForwardThreadVerbatim(forward, name, pane, text, verbatim, via)
	if err != nil {
		return Item{}, fmt.Errorf("store decide thread: %w", err)
	}
	d, err := SendBus(context.Background(), run, name, busDeliveryNoteVerbatim(item.ID, forward, text, verbatim))
	if err != nil {
		if derr := led.Delete(item.ID); derr != nil {
			return Item{}, fmt.Errorf("%w; %w", err, derr)
		}
		return Item{}, err //nolint:wrapcheck // the inbox status line prints agentbus's error
	}
	if _, err := led.SetBusMessage(item.ID, d.BusID); err != nil {
		return Item{}, fmt.Errorf("record bus message: %w", err)
	}
	updated, err := led.RecordCompose(item.ID, item.Messages[0].Time, d)
	if err != nil {
		return Item{}, fmt.Errorf("record delivery: %w", err)
	}
	if err := AppendSend(dir, name, pane, ComposeNoteText(text, verbatim)); err != nil {
		return Item{}, fmt.Errorf("record decide send: %w", err)
	}
	return updated, nil
}

func busDeliveryNoteVerbatim(id, forward, text, verbatim string) string {
	if forward != "" {
		return ForwardNoteVerbatim(id, forward, "human", text, verbatim)
	}
	return DeliveryNoteVerbatim(id, "human", text, verbatim)
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
