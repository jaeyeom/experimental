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
// "$DECIDE_ANSWER_LABEL", "$DECIDE_OWNER", "$DECIDE_WORKER", "$DECIDE_PANE",
// "$DECIDE_KIND", "$DECIDE_QUESTION", "$DECIDE_ANSWERED_AT",
// "$DECIDE_ANSWERED_BY", "$DECIDE_THREAD_HEADER", "$DECIDE_REPLY_HINT",
// "$DECIDE_MACHINE", and "$DECIDE_DELIVERY_KEY".
// DECIDE_WORKER is the agent working on the item, set by decide assign, or
// empty.
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
	NotifyTemplate string `json:"notifyTemplate"`
	NotifyPane     string `json:"notifyPane"`
	OptionsOnly    bool   `json:"optionsOnly"`
	// RequireClass rejects `decide ask` without --class when true.
	RequireClass bool `json:"requireClass,omitempty"`
	// RequireClassAfter rejects `decide ask` without --class on or after
	// this YYYY-MM-DD date. Empty uses DefaultRequireClassAfter.
	RequireClassAfter string         `json:"requireClassAfter,omitempty"`
	SendTemplate      string         `json:"sendTemplate,omitempty"`
	Targets           []ConfigTarget `json:"targets,omitempty"`
	Colors            Colors         `json:"colors,omitempty"`
	Recipients        Recipients     `json:"recipients,omitempty"`
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
	// FavoriteContacts are starred names the compose recipient list
	// shows first, in the order they were starred.
	FavoriteContacts []string `json:"favoriteContacts,omitempty"`
	// ContactAliases maps a ledger name to a canonical agent name when
	// the pane and suffix rules cannot.
	ContactAliases map[string]string `json:"contactAliases,omitempty"`
	// Owners configures who may open items (mode: off, warn, enforce).
	Owners OwnersConfig `json:"owners,omitempty"`
	// UI is this machine's inbox display. A hub does not send it.
	UI InboxUI `json:"ui,omitempty"`
	// Web configures `decide web` on this machine. It is machine-local:
	// a hub never sends it to clients.
	Web WebConfig `json:"web,omitempty"`
}

// WebConfig configures the hub's `decide web` server.
type WebConfig struct {
	// Addr is the listen address. Empty is DefaultWebAddr.
	Addr string `json:"addr,omitempty"`
	// PublicURL is the externally reachable URL, such as the HTTPS proxy.
	PublicURL string        `json:"publicUrl,omitempty"`
	Auth      WebAuthConfig `json:"auth,omitempty"`
	Push      WebPushConfig `json:"push,omitempty"`
}

// WebAuthConfig selects how `decide web` authenticates clients. Mode is
// WebAuthNone (the default) or WebAuthToken, which reads the token from
// TokenFile.
type WebAuthConfig struct {
	Mode      string `json:"mode,omitempty"`
	TokenFile string `json:"tokenFile,omitempty"`
}

// WebPushConfig configures Web Push. Grace is how long a push waits,
// such as "5s", so an item read elsewhere is skipped.
type WebPushConfig struct {
	Enabled bool   `json:"enabled,omitempty"`
	Subject string `json:"subject,omitempty"`
	Grace   string `json:"grace,omitempty"`
}

// Web auth modes.
const (
	WebAuthNone  = "none"
	WebAuthToken = "token"
)

// DefaultWebAddr is the default `decide web` listen address.
const DefaultWebAddr = "127.0.0.1:8787"

// DefaultWebPushGrace is the default push grace delay.
const DefaultWebPushGrace = 5 * time.Second

// WebAddr is the web listen address, or DefaultWebAddr when unset.
func (c Config) WebAddr() string {
	if c.Web.Addr == "" {
		return DefaultWebAddr
	}
	return c.Web.Addr
}

// WebPushGrace is the push grace delay, or DefaultWebPushGrace when it
// is unset or invalid. LoadConfig rejects an invalid one.
func (c Config) WebPushGrace() time.Duration {
	if d, err := parseWebPushGrace(c.Web.Push.Grace); err == nil && d > 0 {
		return d
	}
	return DefaultWebPushGrace
}

// parseWebPushGrace parses a web push grace. Empty is zero.
func parseWebPushGrace(raw string) (time.Duration, error) {
	raw = strings.TrimSpace(raw)
	if raw == "" {
		return 0, nil
	}
	d, err := time.ParseDuration(raw)
	if err != nil || d <= 0 {
		return 0, fmt.Errorf("decide config web.push.grace must be a positive duration, such as \"5s\"")
	}
	return d, nil
}

func checkWebConfig(web WebConfig) error {
	switch web.Auth.Mode {
	case "", WebAuthNone, WebAuthToken:
	default:
		return fmt.Errorf("decide config web.auth.mode %q must be %q or %q", web.Auth.Mode, WebAuthNone, WebAuthToken)
	}
	_, err := parseWebPushGrace(web.Push.Grace)
	return err
}

// Clipboard transport values of InboxUI.Clipboard. Empty means auto.
const (
	ClipboardAuto    = "auto"
	ClipboardOSC52   = "osc52"
	ClipboardCommand = "command"
	ClipboardOff     = "off"
)

// Answer action values of InboxUI.AnswerAction. Empty means stay.
const (
	AnswerActionStay = "stay"
	AnswerActionList = "list"
	AnswerActionEdit = "edit"
)

// Open position values of InboxUI.OpenAt. Empty means new.
const (
	OpenAtNew  = "new"
	OpenAtLast = "last"
	OpenAtTop  = "top"
)

// InboxUI is local inbox display. UTC shows times in UTC instead of
// the machine's zone. Clipboard chooses how the inbox copies text.
type InboxUI struct {
	UTC bool `json:"utc,omitempty"`
	// Clipboard is auto, osc52, command, or off. Empty is auto.
	Clipboard string `json:"clipboard,omitempty"`
	// AnswerAction is stay, list, or edit. Empty is stay.
	AnswerAction string `json:"answerAction,omitempty"`
	// OpenAt is where a thread opens: new (first unread message), last,
	// or top. Empty is new.
	OpenAt string `json:"openAt,omitempty"`
	// FillColumn is the compose fill width. Zero is 72.
	FillColumn int `json:"fillColumn,omitempty"`
	// StaleAnswerAfter is how long an answer sits before a follow-up,
	// such as "30m". Empty is 30 minutes.
	StaleAnswerAfter string `json:"staleAnswerAfter,omitempty"`
	// LateDeliveryAfter is how long a bus message may wait before it
	// needs attention. Empty is five minutes.
	LateDeliveryAfter string `json:"lateDeliveryAfter,omitempty"`
	// OutboxStaleAfter is how long an outbox entry may wait before the
	// inbox shows a status-line warning. Empty is five minutes.
	OutboxStaleAfter string `json:"outboxStaleAfter,omitempty"`
	// ArchiveGrace is how long a new message holds inbox archive,
	// such as "2s". Empty is 2 seconds.
	ArchiveGrace string `json:"archiveGrace,omitempty"`
	// BusHealthInterval is how often the inbox runs `agentbus health
	// --json` for its bus-down banner, such as "15s". Empty is 15 seconds.
	BusHealthInterval string `json:"busHealthInterval,omitempty"`
	// OrgTree lets Contacts switch to the org tree of OrgSpec, a rendered
	// org spec JSON read with `orgctl tree --json`. Off never runs orgctl.
	OrgTree bool   `json:"orgTree,omitempty"`
	OrgSpec string `json:"orgSpec,omitempty"`
	// MaxLineLength is the longest line, in characters, that decide
	// accepts from an author. Nil is DefaultMaxLineLength; zero turns
	// the check off.
	MaxLineLength *int `json:"maxLineLength,omitempty"`
	// MaxFirstLineLength is the longest first line, in characters, that
	// decide accepts from an author. Nil is DefaultMaxFirstLineLength;
	// zero turns the verdict-first check off.
	MaxFirstLineLength *int `json:"maxFirstLineLength,omitempty"`
	// MaxParagraphLines is the most consecutive non-bullet lines that
	// decide accepts from an author. Nil is DefaultMaxParagraphLines;
	// zero turns the check off.
	MaxParagraphLines *int `json:"maxParagraphLines,omitempty"`
	// MaxMessageChars is the longest message, in characters, that
	// decide accepts from an author. Nil is DefaultMaxMessageChars;
	// zero turns the check off.
	MaxMessageChars *int `json:"maxMessageChars,omitempty"`
	// Keys maps inbox action names to key sequences, overriding defaults.
	Keys map[string][]string `json:"keys,omitempty"`
	// ASCIIKeys uses ASCII key labels (such as C-Enter) on buttons instead
	// of Unicode symbols (such as ^⏎).
	ASCIIKeys bool `json:"asciiKeys,omitempty"`
}

const (
	// DefaultMaxLineLength is the line limit when ui.maxLineLength is unset.
	DefaultMaxLineLength = 160
	// DefaultMaxFirstLineLength is the first-line limit when ui.maxFirstLineLength is unset.
	DefaultMaxFirstLineLength = 100
	// DefaultMaxParagraphLines is the paragraph limit when ui.maxParagraphLines is unset.
	DefaultMaxParagraphLines = 3
	// DefaultMaxMessageChars is the message size limit when ui.maxMessageChars is unset.
	DefaultMaxMessageChars = 1200
)

// MaxLineLength is ui.maxLineLength, or DefaultMaxLineLength when it is
// unset. Zero means no limit. LoadConfig rejects a negative value.
func (c Config) MaxLineLength() int {
	if c.UI.MaxLineLength == nil {
		return DefaultMaxLineLength
	}
	return *c.UI.MaxLineLength
}

// MaxFirstLineLength is ui.maxFirstLineLength, or DefaultMaxFirstLineLength
// when it is unset. Zero means no limit. LoadConfig rejects a negative value.
func (c Config) MaxFirstLineLength() int {
	if c.UI.MaxFirstLineLength == nil {
		return DefaultMaxFirstLineLength
	}
	return *c.UI.MaxFirstLineLength
}

// MaxParagraphLines is ui.maxParagraphLines, or DefaultMaxParagraphLines
// when it is unset. Zero means no limit. LoadConfig rejects a negative value.
func (c Config) MaxParagraphLines() int {
	if c.UI.MaxParagraphLines == nil {
		return DefaultMaxParagraphLines
	}
	return *c.UI.MaxParagraphLines
}

// MaxMessageChars is ui.maxMessageChars, or DefaultMaxMessageChars
// when it is unset. Zero means no limit. LoadConfig rejects a negative value.
func (c Config) MaxMessageChars() int {
	if c.UI.MaxMessageChars == nil {
		return DefaultMaxMessageChars
	}
	return *c.UI.MaxMessageChars
}

// DefaultStaleAnswerAfter is how long an answer sits before a follow-up
// when ui.staleAnswerAfter is unset.
const DefaultStaleAnswerAfter = 30 * time.Minute

// DefaultArchiveGrace is how long a new message holds inbox archive
// when ui.archiveGrace is unset.
const DefaultArchiveGrace = 2 * time.Second

// DefaultBusHealthInterval is how often the inbox checks the bus when
// ui.busHealthInterval is unset.
const DefaultBusHealthInterval = 15 * time.Second

// DefaultLateDeliveryAfter is when an undelivered bus message needs attention.
const DefaultLateDeliveryAfter = 5 * time.Minute

// DefaultOutboxStaleAfter is when a pending outbox entry warns on the status line.
const DefaultOutboxStaleAfter = 5 * time.Minute

// OutboxStaleAfter is ui.outboxStaleAfter, or the five minute default.
func (c Config) OutboxStaleAfter() time.Duration {
	d, err := parseOutboxStaleAfter(c.UI.OutboxStaleAfter)
	if err != nil || d <= 0 {
		return DefaultOutboxStaleAfter
	}
	return d
}

func parseOutboxStaleAfter(raw string) (time.Duration, error) {
	return parsePositiveDuration(raw, "outboxStaleAfter", "5m")
}

// LateDeliveryAfter is ui.lateDeliveryAfter, or the five minute default.
func (c Config) LateDeliveryAfter() time.Duration {
	d, err := parseLateDeliveryAfter(c.UI.LateDeliveryAfter)
	if err != nil || d <= 0 {
		return DefaultLateDeliveryAfter
	}
	return d
}

func parseLateDeliveryAfter(raw string) (time.Duration, error) {
	return parsePositiveDuration(raw, "lateDeliveryAfter", "5m")
}

// BusHealthInterval is ui.busHealthInterval, or DefaultBusHealthInterval
// when it is unset. LoadConfig rejects an invalid duration.
func (c Config) BusHealthInterval() time.Duration {
	d, err := parseBusHealthInterval(c.UI.BusHealthInterval)
	if err != nil || d <= 0 {
		return DefaultBusHealthInterval
	}
	return d
}

// parseBusHealthInterval parses ui.busHealthInterval. Empty is zero.
func parseBusHealthInterval(raw string) (time.Duration, error) {
	return parsePositiveDuration(raw, "busHealthInterval", "15s")
}

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

// CheckAnswerAction reports an error when action is not an answer action.
// Empty is stay.
func CheckAnswerAction(action string) error {
	switch action {
	case "", AnswerActionStay, AnswerActionList, AnswerActionEdit:
		return nil
	default:
		return fmt.Errorf("decide config ui.answerAction %q must be stay, list, or edit", action)
	}
}

// CheckOpenAt reports an error when openAt is not an open position.
// Empty is new.
func CheckOpenAt(openAt string) error {
	switch openAt {
	case "", OpenAtNew, OpenAtLast, OpenAtTop:
		return nil
	default:
		return fmt.Errorf("decide config ui.openAt %q must be new, last, or top", openAt)
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

// ConfigPath returns the decide config file path and whether it could be resolved.
func ConfigPath() (string, bool) {
	return configPath()
}

// DefaultRequireClassAfter is the default date (YYYY-MM-DD, 14 days after
// release) on or after which `decide ask` requires --class.
const DefaultRequireClassAfter = "2026-12-08"

// RequireClassAfterDate returns the effective YYYY-MM-DD date on or after
// which `decide ask` requires --class.
func (c Config) RequireClassAfterDate() string {
	if raw := strings.TrimSpace(c.RequireClassAfter); raw != "" {
		return raw
	}
	return DefaultRequireClassAfter
}

// RequireClassAt reports whether `decide ask` without --class should be
// rejected at now.
func (c Config) RequireClassAt(now time.Time) bool {
	if c.RequireClass {
		return true
	}
	cutoff, err := parseRequireClassAfter(c.RequireClassAfterDate())
	if err != nil {
		return false
	}
	return !now.UTC().Before(cutoff)
}

func parseRequireClassAfter(raw string) (time.Time, error) {
	s := strings.TrimSpace(raw)
	if s == "" {
		return time.Time{}, nil
	}
	t, err := time.Parse("2006-01-02", s)
	if err != nil {
		return time.Time{}, fmt.Errorf("decide config requireClassAfter %q must be YYYY-MM-DD", raw)
	}
	return t, nil
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
	if err := validateConfig(cfg); err != nil {
		return Config{}, err
	}
	resolveAvatarFiles(cfg.Avatars, dir)
	return cfg, nil
}

func validateConfig(cfg Config) error {
	if len(cfg.OpenCommand) > 0 && cfg.OpenCommand[0] == "" {
		return fmt.Errorf("decide config openCommand: the program is empty")
	}
	checks := []func() error{
		func() error { return checkAvatarRules(cfg.Avatars) },
		func() error { return checkOwnersConfig(cfg.Owners) },
		func() error { _, err := parseRecipientsTimeout(cfg.RecipientsTimeout); return err },
		func() error { return checkWebConfig(cfg.Web) },
		func() error { return CheckClipboard(cfg.UI.Clipboard) },
		func() error { return CheckAnswerAction(cfg.UI.AnswerAction) },
		func() error { return CheckOpenAt(cfg.UI.OpenAt) },
		func() error { return checkUIDurations(cfg.UI) },
		func() error { return checkShapeConfig(cfg) },
		func() error { _, err := parseRequireClassAfter(cfg.RequireClassAfter); return err },
		func() error { _, err := ResolveKeyMap(cfg.UI.Keys); return err },
	}
	for _, fn := range checks {
		if err := fn(); err != nil {
			return err
		}
	}
	return nil
}

func checkUIDurations(ui InboxUI) error {
	for _, fn := range []func() error{
		func() error { _, err := parseStaleAnswerAfter(ui.StaleAnswerAfter); return err },
		func() error { _, err := parseLateDeliveryAfter(ui.LateDeliveryAfter); return err },
		func() error { _, err := parseOutboxStaleAfter(ui.OutboxStaleAfter); return err },
		func() error { _, err := parseArchiveGrace(ui.ArchiveGrace); return err },
		func() error { _, err := parseBusHealthInterval(ui.BusHealthInterval); return err },
	} {
		if err := fn(); err != nil {
			return err
		}
	}
	return nil
}

func checkShapeConfig(cfg Config) error {
	for _, item := range []struct {
		name string
		val  int
	}{
		{"ui.maxLineLength", cfg.MaxLineLength()},
		{"ui.maxFirstLineLength", cfg.MaxFirstLineLength()},
		{"ui.maxParagraphLines", cfg.MaxParagraphLines()},
		{"ui.maxMessageChars", cfg.MaxMessageChars()},
	} {
		if item.val < 0 {
			return fmt.Errorf("decide config %s must be 0 or more", item.name)
		}
	}
	return nil
}

// Notify runs the configured template after a relay answer is saved.
// An empty template does nothing for a pane. The item notify pane overrides the config pane.
// A pane that starts with agentbus: is delivered with agentbus send, and the
// template is not run. A pane on another machine is delivered with the send
// command and `--machine`, and the template is not run. The template only
// ever sees a pane on this machine.
// The command reads "$DECIDE_ID", "$DECIDE_ANSWER", "$DECIDE_ANSWER_LABEL",
// "$DECIDE_OWNER", "$DECIDE_WORKER", "$DECIDE_PANE", "$DECIDE_KIND",
// "$DECIDE_QUESTION", "$DECIDE_ANSWERED_AT", "$DECIDE_ANSWERED_BY",
// "$DECIDE_THREAD_HEADER", "$DECIDE_REPLY_HINT", "$DECIDE_MACHINE", and
// "$DECIDE_DELIVERY_KEY". DECIDE_WORKER is empty when no agent is assigned.
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
	_, err := notifyAttempt(item, out)
	return err
}

// notifyAttempt runs one answer notification and describes it as a
// Delivery with Target, Outcome, Error, BusID, and Reason set.
// An empty outcome with a nil error means nothing was sent.
// The outcome is OutcomeDelivered, OutcomeFailed, or OutcomeWaiting.
func notifyAttempt(item *Item, out io.Writer) (Delivery, error) {
	if item == nil || item.NotifyClosed {
		return Delivery{}, nil
	}
	if out == nil {
		out = io.Discard
	}
	cfg, err := LoadConfig()
	if err != nil {
		return failedDelivery("", err), err
	}
	pane, machine := NotifyTarget(item, cfg)
	target := ParseTarget(pane, machine)
	return transportFor(target).DeliverAnswer(item, target, cfg, out) //nolint:wrapcheck // transport implementations already wrap delivery errors
}

// failedDelivery is a failed attempt at target with err as its error.
func failedDelivery(target string, err error) Delivery {
	return Delivery{Target: target, Outcome: OutcomeFailed, Error: err.Error()}
}

// SentDelivery is the attempt at target that runErr ended. A nil runErr
// was delivered now. A failure is logged under id.
func SentDelivery(id, target string, runErr error) (Delivery, error) {
	if runErr != nil {
		err := logDelivery(id, runErr)
		return failedDelivery(target, err), err
	}
	return Delivery{Target: target, Outcome: OutcomeDelivered}, nil
}

func notifyLocal(item *Item, pane string, cfg Config, out io.Writer) (Delivery, error) {
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
		"DECIDE_WORKER="+item.Worker,
		"DECIDE_PANE="+pane,
		"DECIDE_KIND="+item.Kind,
		"DECIDE_QUESTION="+questionLine(ComposeNoteText(item.Question, item.Verbatim)),
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
		return SentDelivery(item.ID, pane, fmt.Errorf("notify: %w", err))
	}
	if err := stdoutDeliveryError(stdout.String()); err != nil {
		return SentDelivery(item.ID, pane, fmt.Errorf("notify: %w", err))
	}
	return SentDelivery(item.ID, pane, nil)
}

// notifyRemote delivers the answer note to a pane on another machine.
// The user's notify template is not run. The send command sees
// DECIDE_DELIVERY_KEY.
func notifyRemote(item *Item, pane, machine string, cfg Config) (Delivery, error) {
	target := pane + "@" + machine
	answer, answeredBy := answerFields(item)
	note := DeliveryNote(item.ID, answeredBy, answer)
	argv := MachineArgv(SendArgv(cfg.SendTemplate, pane, note), machine)
	env := append(os.Environ(), "DECIDE_DELIVERY_KEY="+DeliveryKey(item))
	if err := runSend(argv, env); err != nil {
		return SentDelivery(item.ID, target, fmt.Errorf("notify: %w", err))
	}
	return SentDelivery(item.ID, target, nil)
}

// notifyBus delivers the answer note to an agentbus name. The user's
// notify template is not run. The client id is the answer's delivery
// key. A held or queued state is waiting, not a failure. BusID is the
// message id send printed, else the client id.
func notifyBus(item *Item, name string) (Delivery, error) {
	target := BusPanePrefix + name
	if name == "" {
		return SentDelivery(item.ID, target, fmt.Errorf("notify: agentbus name is empty"))
	}
	answer, answeredBy := answerFields(item)
	out, err := ExecAgentbus(context.Background(), []string{
		"send", name, DeliveryNote(item.ID, answeredBy, answer),
		"--client-id", DeliveryKey(item),
	})
	d, err := busSendOutcome(target, item.ID, string(out), err)
	if err == nil && d.BusID == "" {
		d.BusID = DeliveryKey(item)
	}
	return d, err
}

func answerFields(item *Item) (value, by string) {
	if item != nil && item.Answer != nil {
		return item.Answer.Value, item.Answer.AnsweredBy
	}
	return "", ""
}

// deliverBus runs agentbus send for a compose follow-up. Acceptance is
// classifyBusSend, the same rule as an answer notification.
func deliverBus(id, name, text string) (Delivery, error) {
	out, err := ExecAgentbus(context.Background(), []string{"send", name, text})
	return busSendOutcome(BusPanePrefix+name, id, string(out), err)
}

// busSendOutcome applies classifyBusSend to one agentbus send. A held
// or queued message is OutcomeWaiting with a nil error. BusID is the
// message id send printed. id is only used when logging a failure.
func busSendOutcome(target, id, stdout string, runErr error) (Delivery, error) {
	queued, err := classifyBusSend(stdout, runErr)
	if err != nil {
		return SentDelivery(id, target, fmt.Errorf("notify: %w", err))
	}
	d, _ := SentDelivery(id, target, nil)
	d.BusID = busSendID(stdout)
	if queued {
		d.Outcome = OutcomeWaiting
		d.Reason = sendHeldReason(stdout, runErr)
	}
	return d, nil
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

// ComposeNoteText combines plain text and verbatim text for a notification note.
func ComposeNoteText(text, verbatim string) string {
	switch {
	case verbatim == "":
		return text
	case text == "":
		return verbatim
	default:
		return text + "\n\n" + verbatim
	}
}

// DeliveryNote is the single pane argument for a human note.
// It is the thread header, the original text, and the reply hint.
// The text stored on the thread is not this note.
func DeliveryNote(id, author, text string) string {
	return DeliveryNoteVerbatim(id, author, text, "")
}

// DeliveryNoteVerbatim is DeliveryNote with an optional verbatim block.
func DeliveryNoteVerbatim(id, author, text, verbatim string) string {
	return threadHeader(id, author) + "\n" + ComposeNoteText(text, verbatim) + "\n" + replyHint(id)
}

// ForwardNote is DeliveryNote for a thread forwarded from item from.
// The header names the source so the agent can reply in context.
func ForwardNote(id, from, author, text string) string {
	return ForwardNoteVerbatim(id, from, author, text, "")
}

// ForwardNoteVerbatim is ForwardNote with an optional verbatim block.
func ForwardNoteVerbatim(id, from, author, text, verbatim string) string {
	return threadHeader(id, author) + " Fwd " + from + "\n" + ComposeNoteText(text, verbatim) + "\n" + replyHint(id)
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
