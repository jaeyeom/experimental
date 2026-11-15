package decide

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
	"unicode/utf8"
)

// Config is the decide config file. An absent file runs no command.
// OptionsOnly makes relay asks reject free text unless --free-text is set.
// NotifyTemplate is a shell command. It reads "$DECIDE_ID", "$DECIDE_ANSWER",
// "$DECIDE_ANSWER_LABEL", "$DECIDE_OWNER", "$DECIDE_PANE", "$DECIDE_KIND",
// "$DECIDE_QUESTION", "$DECIDE_ANSWERED_AT", "$DECIDE_ANSWERED_BY",
// "$DECIDE_THREAD_HEADER", "$DECIDE_REPLY_HINT", "$DECIDE_MACHINE",
// and "$DECIDE_DELIVERY_KEY".
// DECIDE_THREAD_HEADER is one line such as "[decide D-123 from human]",
// with the thread id and the author.
// DECIDE_REPLY_HINT is one line.
// Reply in this thread: decide reply D-123 "..." (add --option key:label for a follow-up question; decide done D-123 "outcome" when finished). Do not answer only in your terminal.
// DECIDE_MACHINE is empty. The template runs only for a pane on this
// machine. A pane on another machine is delivered with the send command
// and `--machine`, and the template is not run.
// DECIDE_DELIVERY_KEY is <item id>:<answeredAt> for this answer.
// Retries of the same answer reuse it.
// Those values are not written into the command string. An empty template
// runs nothing. There is no default notify command. The example is not a default.
// SendTemplate is the compose argv template. Empty means
// `herdr agent prompt {pane}` plus the note as its own argument.
// The note is not interpolated into sh -c. A pane on another machine
// gets `--machine <m>` after the template's program.
// Colors overrides inbox palette entries. Each value is an ANSI index
// such as "7" or a hex color such as "#b0b0b0". A missing key keeps
// the default for that entry.
type Config struct {
	NotifyTemplate string         `json:"notifyTemplate"`
	NotifyPane     string         `json:"notifyPane"`
	OptionsOnly    bool           `json:"optionsOnly"`
	SendTemplate   string         `json:"sendTemplate,omitempty"`
	Targets        []ConfigTarget `json:"targets,omitempty"`
	Colors         Colors         `json:"colors,omitempty"`
	Recipients     Recipients     `json:"recipients,omitempty"`
	// RecipientsTimeout bounds the inbox's background `agentbus list
	// --json`, such as "3s". Empty is DefaultBusListTimeout.
	RecipientsTimeout string `json:"recipientsTimeout,omitempty"`
	// Hub is an ssh target. When set, commands run on that machine's
	// ledger instead of this one. DECIDE_HUB overrides it.
	Hub string `json:"hub,omitempty"`
	// HubTimeout is the ssh connect timeout in hub mode, such as "5s".
	HubTimeout string `json:"hubTimeout,omitempty"`
	// Machine is this machine's herdr label as the hub knows it. Hub
	// mode sends it with the caller's pane so the hub notifies that pane
	// through `herdr --machine`. DECIDE_MACHINE overrides it.
	Machine string `json:"machine,omitempty"`
	// HubMachine is the hub's herdr machine label on this machine. A
	// hub-mode inbox jumps to an owner pane on the hub through
	// `herdr --machine <hubMachine>`.
	HubMachine string `json:"hubMachine,omitempty"`
	// Avatars maps owner, target, or bus names to avatar files.
	Avatars []AvatarRule `json:"avatars,omitempty"`
	// AvatarDefault shows a built-in avatar, picked by name, for unmatched names.
	AvatarDefault bool `json:"avatarDefault,omitempty"`
	// AvatarMono is AvatarMonoHide to hide avatars without color, under
	// NO_COLOR or a monochrome terminal. Otherwise they show uncolored.
	AvatarMono string `json:"avatarMono,omitempty"`
	// MessageAvatars shows each thread message's sender beside that
	// message. Nil is on. The header avatar stays the item owner.
	MessageAvatars *bool `json:"messageAvatars,omitempty"`
	// Checkouts maps status resource names to checkout paths whose
	// agentbus lease the status board shows.
	Checkouts []ConfigCheckout `json:"checkouts,omitempty"`
	// OpenCommand is the argv the inbox opens a link with; the URL is
	// appended as the last argument and no shell runs. Empty is
	// xdg-open, or open on macOS.
	OpenCommand []string `json:"openCommand,omitempty"`
	// Hyperlinks wraps URLs in the inbox item view in OSC 8 hyperlinks.
	// Unset is on; false turns them off.
	Hyperlinks *bool `json:"hyperlinks,omitempty"`
	// HiddenContacts are agent names the Contacts folder does not show.
	// A name that is active again is dropped from the list.
	HiddenContacts []string `json:"hiddenContacts,omitempty"`
	// ContactAliases maps a ledger name to a canonical agent name when
	// the pane and suffix rules cannot.
	ContactAliases map[string]string `json:"contactAliases,omitempty"`
	// UI is this machine's inbox display. A hub does not send it.
	UI InboxUI `json:"ui,omitempty"`
}

// Clipboard transport values of InboxUI.Clipboard. Empty means auto.
const (
	ClipboardAuto    = "auto"
	ClipboardOSC52   = "osc52"
	ClipboardCommand = "command"
	ClipboardOff     = "off"
)

// InboxUI is local inbox display. UTC shows times in UTC instead of
// the machine's zone. Clipboard chooses how the inbox copies text.
type InboxUI struct {
	UTC bool `json:"utc,omitempty"`
	// Clipboard is auto, osc52, command, or off. Empty is auto.
	Clipboard string `json:"clipboard,omitempty"`
	// FillColumn is the compose fill width. Zero is 72.
	FillColumn int `json:"fillColumn,omitempty"`
	// StaleAnswerAfter is how long an answer sits before a follow-up,
	// such as "30m". Empty is 30 minutes.
	StaleAnswerAfter string `json:"staleAnswerAfter,omitempty"`
	// ArchiveGrace is how long a new message holds inbox archive,
	// such as "2s". Empty is 2 seconds.
	ArchiveGrace string `json:"archiveGrace,omitempty"`
}

// DefaultStaleAnswerAfter is how long an answer sits before a follow-up
// when ui.staleAnswerAfter is unset.
const DefaultStaleAnswerAfter = 30 * time.Minute

// DefaultArchiveGrace is how long a new message holds inbox archive
// when ui.archiveGrace is unset.
const DefaultArchiveGrace = 2 * time.Second

// StaleAnswerAfter is ui.staleAnswerAfter, or DefaultStaleAnswerAfter
// when it is unset. LoadConfig rejects an invalid duration.
func (c Config) StaleAnswerAfter() time.Duration {
	d, err := parseStaleAnswerAfter(c.UI.StaleAnswerAfter)
	if err != nil || d <= 0 {
		return DefaultStaleAnswerAfter
	}
	return d
}

// ArchiveGrace is ui.archiveGrace, or DefaultArchiveGrace when it is
// unset. LoadConfig rejects an invalid duration.
func (c Config) ArchiveGrace() time.Duration {
	d, err := parseArchiveGrace(c.UI.ArchiveGrace)
	if err != nil || d <= 0 {
		return DefaultArchiveGrace
	}
	return d
}

// parseStaleAnswerAfter parses ui.staleAnswerAfter. Empty is zero.
func parseStaleAnswerAfter(raw string) (time.Duration, error) {
	return parsePositiveDuration(raw, "staleAnswerAfter", "30m")
}

// parseArchiveGrace parses ui.archiveGrace. Empty is zero.
func parseArchiveGrace(raw string) (time.Duration, error) {
	return parsePositiveDuration(raw, "archiveGrace", "2s")
}

// parsePositiveDuration parses a ui duration. Empty is zero.
func parsePositiveDuration(raw, field, example string) (time.Duration, error) {
	raw = strings.TrimSpace(raw)
	if raw == "" {
		return 0, nil
	}
	d, err := time.ParseDuration(raw)
	if err != nil || d <= 0 {
		return 0, fmt.Errorf("decide config ui.%s must be a positive duration, such as %q", field, example)
	}
	return d, nil
}

// CheckClipboard reports an error when mode is not a clipboard transport.
// Empty is auto.
func CheckClipboard(mode string) error {
	switch mode {
	case "", ClipboardAuto, ClipboardOSC52, ClipboardCommand, ClipboardOff:
		return nil
	default:
		return fmt.Errorf("decide config ui.clipboard %q must be auto, osc52, command, or off", mode)
	}
}

// HyperlinksOn reports whether the inbox writes OSC 8 hyperlinks.
func (c Config) HyperlinksOn() bool {
	return c.Hyperlinks == nil || *c.Hyperlinks
}

// MessageAvatarsOn reports whether each thread message shows its sender's
// avatar. Unset is on.
func (c Config) MessageAvatarsOn() bool {
	return c.MessageAvatars == nil || *c.MessageAvatars
}

// DefaultBusListTimeout bounds `agentbus list --json` when the config
// has no recipientsTimeout.
const DefaultBusListTimeout = 3 * time.Second

// BusListTimeout is the recipientsTimeout setting, or
// DefaultBusListTimeout when it is unset or invalid. LoadConfig rejects
// an invalid one.
func (c Config) BusListTimeout() time.Duration {
	if d, err := parseRecipientsTimeout(c.RecipientsTimeout); err == nil && d > 0 {
		return d
	}
	return DefaultBusListTimeout
}

// parseRecipientsTimeout parses a recipientsTimeout. Empty is zero.
func parseRecipientsTimeout(raw string) (time.Duration, error) {
	raw = strings.TrimSpace(raw)
	if raw == "" {
		return 0, nil
	}
	d, err := time.ParseDuration(raw)
	if err != nil || d <= 0 {
		return 0, fmt.Errorf("decide config recipientsTimeout must be a positive duration, such as \"3s\"")
	}
	return d, nil
}

// Recipients chooses extra compose recipients. Agentbus adds the names
// from `agentbus list --json` to the configured targets. It is off by
// default.
type Recipients struct {
	Agentbus bool `json:"agentbus,omitempty"`
}

// Colors is the inbox palette in decide config. Each field is an ANSI
// index or a hex color. A missing key keeps the default.
type Colors struct {
	Requester string `json:"requester,omitempty"`
	Human     string `json:"human,omitempty"`
	Agent     string `json:"agent,omitempty"`
	Time      string `json:"time,omitempty"`
	Badge     string `json:"badge,omitempty"`
	Muted     string `json:"muted,omitempty"`
}

// ConfigTarget is one compose destination. The pane id comes from config.
type ConfigTarget struct {
	Name string `json:"name"`
	Pane string `json:"pane"`
}

func configPath() (string, bool) {
	dir := os.Getenv("XDG_CONFIG_HOME")
	if dir == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return "", false
		}
		dir = filepath.Join(home, ".config")
	}
	return filepath.Join(dir, "decide", "config.json"), true
}

// LoadConfig reads the decide config file. A missing file is an empty config.
func LoadConfig() (Config, error) {
	path, ok := configPath()
	if !ok {
		return Config{}, nil
	}
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return Config{}, nil
	}
	if err != nil {
		return Config{}, fmt.Errorf("read decide config: %w", err)
	}
	return parseConfig(data, filepath.Dir(path))
}

// parseConfig parses config data from a file in dir. Nil data is an
// empty config.
func parseConfig(data []byte, dir string) (Config, error) {
	var cfg Config
	if data == nil {
		return cfg, nil
	}
	if err := json.Unmarshal(data, &cfg); err != nil {
		return Config{}, fmt.Errorf("parse decide config: %w", err)
	}
	if err := checkAvatarRules(cfg.Avatars); err != nil {
		return Config{}, err
	}
	if len(cfg.OpenCommand) > 0 && cfg.OpenCommand[0] == "" {
		return Config{}, fmt.Errorf("decide config openCommand: the program is empty")
	}
	if _, err := parseRecipientsTimeout(cfg.RecipientsTimeout); err != nil {
		return Config{}, err
	}
	if err := CheckClipboard(cfg.UI.Clipboard); err != nil {
		return Config{}, err
	}
	if _, err := parseStaleAnswerAfter(cfg.UI.StaleAnswerAfter); err != nil {
		return Config{}, err
	}
	if _, err := parseArchiveGrace(cfg.UI.ArchiveGrace); err != nil {
		return Config{}, err
	}
	resolveAvatarFiles(cfg.Avatars, dir)
	return cfg, nil
}

// Notify runs the configured template after a relay answer is saved.
// An empty template does nothing for a pane. The item notify pane overrides the config pane.
// A pane that starts with agentbus: is delivered with agentbus send, and the
// template is not run. A pane on another machine is delivered with the send
// command and `--machine`, and the template is not run. The template only
// ever sees a pane on this machine.
// The command reads "$DECIDE_ID", "$DECIDE_ANSWER", "$DECIDE_ANSWER_LABEL",
// "$DECIDE_OWNER", "$DECIDE_PANE", "$DECIDE_KIND", "$DECIDE_QUESTION",
// "$DECIDE_ANSWERED_AT", "$DECIDE_ANSWERED_BY", "$DECIDE_THREAD_HEADER",
// "$DECIDE_REPLY_HINT", "$DECIDE_MACHINE", and "$DECIDE_DELIVERY_KEY".
// DECIDE_THREAD_HEADER is one line such as "[decide D-123 from human]",
// with the thread id and the author.
// DECIDE_REPLY_HINT is one line.
// Reply in this thread: decide reply D-123 "..." (add --option key:label for a follow-up question; decide done D-123 "outcome" when finished). Do not answer only in your terminal.
// DECIDE_MACHINE is empty. The template runs only for a pane on this machine.
// DECIDE_DELIVERY_KEY is <item id>:<answeredAt> for this answer. Retries
// of the same answer reuse it. A pane on another machine gets the same
// key in the environment of the send command.
// None of those fields are substituted into the command string.
// A command that exits 0 and prints JSON with a top-level error object,
// or text that says the pane or agent was not found, was not delivered.
// A held or queued agentbus send was accepted for later delivery.
func Notify(item *Item, out io.Writer) error {
	_, _, err := notifyAttempt(item, out)
	return err
}

// notifyAttempt runs one answer notification.
// An empty outcome with a nil error means nothing was sent.
// outcome is OutcomeDelivered, OutcomeFailed, or OutcomeQueued.
func notifyAttempt(item *Item, out io.Writer) (target, outcome string, err error) {
	if item == nil || item.NotifyClosed {
		return "", "", nil
	}
	if out == nil {
		out = io.Discard
	}
	cfg, err := LoadConfig()
	if err != nil {
		return "", OutcomeFailed, err
	}
	pane, machine := NotifyTarget(item, cfg)
	if name, ok := strings.CutPrefix(pane, BusPanePrefix); ok {
		return notifyBus(item, strings.TrimSpace(name))
	}
	if machine != "" {
		return notifyRemote(item, pane, machine, cfg)
	}
	if strings.TrimSpace(cfg.NotifyTemplate) == "" {
		return "", "", nil
	}
	return notifyLocal(item, pane, cfg, out)
}

func notifyLocal(item *Item, pane string, cfg Config, out io.Writer) (string, string, error) {
	answer, answeredBy := answerFields(item)
	// The template is operator config. Fields stay in the environment.
	// Parent values come first. os/exec keeps the last value for a duplicate
	// key, so the item values override a parent DECIDE_* variable.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.Command("sh", "-c", cfg.NotifyTemplate) //nolint:gosec // G204: the notify template is operator config; fields are environment variables
	cmd.Env = append(os.Environ(),
		"DECIDE_ID="+item.ID,
		"DECIDE_ANSWER="+answer,
		"DECIDE_ANSWER_LABEL="+AnswerLabel(item),
		"DECIDE_OWNER="+item.Owner,
		"DECIDE_PANE="+pane,
		"DECIDE_KIND="+item.Kind,
		"DECIDE_QUESTION="+questionLine(item.Question),
		"DECIDE_ANSWERED_AT="+item.AnsweredAt.Format(time.RFC3339),
		"DECIDE_ANSWERED_BY="+answeredBy,
		"DECIDE_THREAD_HEADER="+threadHeader(item.ID, answeredBy),
		"DECIDE_REPLY_HINT="+replyHint(item.ID),
		"DECIDE_MACHINE=",
		"DECIDE_DELIVERY_KEY="+DeliveryKey(item),
	)
	var stdout bytes.Buffer
	cmd.Stdout = io.MultiWriter(out, &stdout)
	cmd.Stderr = out
	if err := cmd.Run(); err != nil {
		return pane, OutcomeFailed, logDelivery(item.ID, fmt.Errorf("notify: %w", err))
	}
	if err := stdoutDeliveryError(stdout.String()); err != nil {
		return pane, OutcomeFailed, logDelivery(item.ID, fmt.Errorf("notify: %w", err))
	}
	return pane, OutcomeDelivered, nil
}

// notifyRemote delivers the answer note to a pane on another machine.
// The user's notify template is not run. The send command sees
// DECIDE_DELIVERY_KEY.
func notifyRemote(item *Item, pane, machine string, cfg Config) (string, string, error) {
	target := pane + "@" + machine
	answer, answeredBy := answerFields(item)
	note := DeliveryNote(item.ID, answeredBy, answer)
	argv := MachineArgv(SendArgv(cfg.SendTemplate, pane, note), machine)
	env := append(os.Environ(), "DECIDE_DELIVERY_KEY="+DeliveryKey(item))
	if err := runSend(argv, env); err != nil {
		return target, OutcomeFailed, logDelivery(item.ID, fmt.Errorf("notify: %w", err))
	}
	return target, OutcomeDelivered, nil
}

// notifyBus delivers the answer note to an agentbus name. The user's
// notify template is not run. The client id is the answer's delivery key.
// A held or queued state is queued, not a failure.
func notifyBus(item *Item, name string) (string, string, error) {
	target := BusPanePrefix + name
	if name == "" {
		return target, OutcomeFailed, logDelivery(item.ID, fmt.Errorf("notify: agentbus name is empty"))
	}
	answer, answeredBy := answerFields(item)
	out, err := ExecAgentbus(context.Background(), []string{
		"send", name, DeliveryNote(item.ID, answeredBy, answer),
		"--client-id", DeliveryKey(item),
	})
	return busSendOutcome(target, item.ID, string(out), err)
}

func answerFields(item *Item) (value, by string) {
	if item != nil && item.Answer != nil {
		return item.Answer.Value, item.Answer.AnsweredBy
	}
	return "", ""
}

// deliverBus runs agentbus send for a compose follow-up. Acceptance is
// classifyBusSend, the same rule as an answer notification.
func deliverBus(id, name, text string) error {
	out, err := ExecAgentbus(context.Background(), []string{"send", name, text})
	_, _, accErr := busSendOutcome("", id, string(out), err)
	return accErr
}

// busSendOutcome applies classifyBusSend. A held or queued message is
// OutcomeQueued with a nil error. id is only used when logging a failure.
func busSendOutcome(target, id, stdout string, runErr error) (string, string, error) {
	queued, err := classifyBusSend(stdout, runErr)
	if err != nil {
		return target, OutcomeFailed, logDelivery(id, fmt.Errorf("notify: %w", err))
	}
	if queued {
		return target, OutcomeQueued, nil
	}
	return target, OutcomeDelivered, nil
}

func logDelivery(id string, err error) error {
	slog.Error("decide notification was not delivered", "id", id, "err", err)
	return err
}

// NotifyTarget is the pane notifications for item go to and the herdr
// machine it is on. The item's pane wins over the config pane, which is
// on this machine, so the machine is empty for it.
func NotifyTarget(item *Item, cfg Config) (pane, machine string) {
	if item.NotifyClosed {
		return "", ""
	}
	if strings.TrimSpace(item.NotifyPane) != "" {
		return item.NotifyPane, item.NotifyMachine
	}
	return cfg.NotifyPane, ""
}

// DeliveryNote is the single pane argument for a human note.
// It is the thread header, the original text, and the reply hint.
// The text stored on the thread is not this note.
func DeliveryNote(id, author, text string) string {
	return threadHeader(id, author) + "\n" + text + "\n" + replyHint(id)
}

// ForwardNote is DeliveryNote for a thread forwarded from item from.
// The header names the source so the agent can reply in context.
func ForwardNote(id, from, author, text string) string {
	return threadHeader(id, author) + " Fwd " + from + "\n" + text + "\n" + replyHint(id)
}

func threadHeader(id, author string) string {
	return "[decide " + id + " from " + author + "]"
}

func replyHint(id string) string {
	return `Reply in this thread: decide reply ` + id + ` "..." (add --option key:label for a follow-up question; decide done ` + id + ` "outcome" when finished). Do not answer only in your terminal.`
}

func questionLine(question string) string {
	line, _, _ := strings.Cut(question, "\n")
	const limit = 200
	if utf8.RuneCountInString(line) <= limit {
		return line
	}
	return string([]rune(line)[:limit])
}
