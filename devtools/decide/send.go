package decide

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

const (
	// DefaultSendTemplate is the compose command when config sets none.
	// {pane} is one argument. The note is appended as its own argument
	// and is not interpolated into a shell command.
	DefaultSendTemplate = "herdr agent prompt {pane}"

	// SendsName is the send history file inside the state directory.
	SendsName = "sends.jsonl"
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

// RunSend executes argv. The note is already its own argument.
func RunSend(argv []string) error {
	if len(argv) == 0 {
		return fmt.Errorf("decide send: empty command")
	}
	// The note is already its own argument. Operator config chooses the program.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command(argv[0], argv[1:]...) //nolint:gosec // G204: the note is its own argument, not a shell string
	cmd.Stdout = io.Discard
	cmd.Stderr = io.Discard
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("decide send: %w", err)
	}
	return nil
}

// NotifyOwner runs the compose command so pane sees the thread header,
// the original text, and the reply hint. The author is human.
// An empty template uses DefaultSendTemplate. The text stored on the
// thread is not changed here.
func NotifyOwner(pane, id, text string) error {
	if strings.TrimSpace(pane) == "" {
		return fmt.Errorf("decide compose pane is empty")
	}
	if strings.TrimSpace(id) == "" {
		return fmt.Errorf("decide compose id is empty")
	}
	cfg, err := LoadConfig()
	if err != nil {
		return err
	}
	return RunSend(SendArgv(cfg.SendTemplate, pane, DeliveryNote(id, "human", text)))
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
