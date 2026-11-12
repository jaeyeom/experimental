package decide

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
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
// "$DECIDE_THREAD_HEADER", "$DECIDE_REPLY_HINT", and "$DECIDE_MACHINE".
// DECIDE_THREAD_HEADER is one line such as "[decide D-123 from human]",
// with the thread id and the author.
// DECIDE_REPLY_HINT is one line.
// Reply in this thread: decide reply D-123 "..." (add --option key:label for a follow-up question; decide done D-123 "outcome" when finished). Do not answer only in your terminal.
// DECIDE_MACHINE is the herdr machine DECIDE_PANE is on, empty for
// this machine.
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
}

// HyperlinksOn reports whether the inbox writes OSC 8 hyperlinks.
func (c Config) HyperlinksOn() bool {
	return c.Hyperlinks == nil || *c.Hyperlinks
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
	resolveAvatarFiles(cfg.Avatars, dir)
	return cfg, nil
}

// Notify runs the configured template after a relay answer is saved.
// An empty template does nothing. The item notify pane overrides the config pane.
// The command reads "$DECIDE_ID", "$DECIDE_ANSWER", "$DECIDE_ANSWER_LABEL",
// "$DECIDE_OWNER", "$DECIDE_PANE", "$DECIDE_KIND", "$DECIDE_QUESTION",
// "$DECIDE_ANSWERED_AT", "$DECIDE_ANSWERED_BY", "$DECIDE_THREAD_HEADER",
// "$DECIDE_REPLY_HINT", and "$DECIDE_MACHINE". DECIDE_THREAD_HEADER is
// one line such as "[decide D-123 from human]", with the thread id and
// the author.
// DECIDE_REPLY_HINT is one line.
// Reply in this thread: decide reply D-123 "..." (add --option key:label for a follow-up question; decide done D-123 "outcome" when finished). Do not answer only in your terminal.
// DECIDE_MACHINE is the herdr machine of DECIDE_PANE, empty for this machine.
// None of those fields are substituted into the command string.
func Notify(item *Item, out io.Writer) error {
	cfg, err := LoadConfig()
	if err != nil {
		return err
	}
	if strings.TrimSpace(cfg.NotifyTemplate) == "" {
		return nil
	}
	pane, machine := NotifyTarget(item, cfg)
	answer := ""
	answeredBy := ""
	if item.Answer != nil {
		answer = item.Answer.Value
		answeredBy = item.Answer.AnsweredBy
	}
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
		"DECIDE_MACHINE="+machine,
	)
	cmd.Stdout = out
	cmd.Stderr = out
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("notify: %w", err)
	}
	return nil
}

// NotifyTarget is the pane notifications for item go to and the herdr
// machine it is on. The item's pane wins over the config pane, which is
// on this machine, so the machine is empty for it.
func NotifyTarget(item *Item, cfg Config) (pane, machine string) {
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
