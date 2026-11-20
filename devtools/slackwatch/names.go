package slackwatch

import (
	"bytes"
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"log/slog"
	"os/exec"
	"regexp"
	"strings"
	"time"
)

// Defaults of the names config.
const (
	DefaultNameCacheTTL     = 24 * time.Hour
	DefaultNameCommandLimit = 5 * time.Second
	DefaultNameLookupWait   = 10 * time.Second
)

// NamesConfig is the optional "names" config object. Command is an argv
// template whose {id} is replaced by the user id; it runs without a
// shell. UsersInfo nil or true asks Slack users.info when the command
// has no answer. CacheTTL is how long an answer, including no match, is
// trusted. MaxWait bounds the time one lookup waits out rate limits.
type NamesConfig struct {
	Command   Command   `json:"command"`
	Timeout   *Duration `json:"timeout"`
	UsersInfo *bool     `json:"users_info"` //nolint:tagliatelle // the documented key
	CacheTTL  *Duration `json:"cacheTtl"`
	MaxWait   *Duration `json:"maxWait"`
}

// ErrUserNotFound is a users.info answer that names no user.
var ErrUserNotFound = errors.New("user not found")

// UserInfoGetter is Slack users.info. A rate-limited call returns a
// *RateLimitError and an unknown id ErrUserNotFound.
type UserInfoGetter interface {
	UserInfo(ctx context.Context, id string) (NamedUser, error)
}

// CommandRunner runs argv and returns its stdout and exit code. A
// command that could not start or ran past ctx returns an error.
type CommandRunner func(ctx context.Context, argv []string) (stdout []byte, exit int, err error)

// Names turns Slack user ids into names. Resolution order: the external
// command, then users.info, then nothing (the bare id is shown).
// Answers, including no match, are cached in Store for TTL. A nil Names
// resolves nothing.
type Names struct {
	Store   *Store
	Command []string
	Timeout time.Duration
	Info    UserInfoGetter
	TTL     time.Duration
	MaxWait time.Duration
	Run     CommandRunner
	Sleep   func(ctx context.Context, d time.Duration) error
	Now     func() time.Time
	Logger  *slog.Logger
}

// NewNames builds the resolver the config describes. info is users.info
// with the read token; it is dropped when names.users_info is false.
func NewNames(cfg NamesConfig, store *Store, info UserInfoGetter, logger *slog.Logger) *Names {
	n := &Names{
		Store: store, Command: cfg.Command, Info: info, Logger: logger,
		Timeout: DefaultNameCommandLimit, TTL: DefaultNameCacheTTL, MaxWait: DefaultNameLookupWait,
	}
	if cfg.UsersInfo != nil && !*cfg.UsersInfo {
		n.Info = nil
	}
	if cfg.Timeout != nil {
		n.Timeout = cfg.Timeout.Duration
	}
	if cfg.CacheTTL != nil {
		n.TTL = cfg.CacheTTL.Duration
	}
	if cfg.MaxWait != nil {
		n.MaxWait = cfg.MaxWait.Duration
	}
	return n
}

// Name is the user's name, or "" when nothing knows it.
func (n *Names) Name(ctx context.Context, id string) string {
	if n == nil || !looksLikeUserID(id) {
		return ""
	}
	if name, ok := n.cached(id); ok {
		return name
	}
	name, ok := n.lookup(ctx, id)
	if ok {
		n.remember(id, name)
	}
	return name
}

// Label is "Name (U…)", or the bare id when the name is not known.
func (n *Names) Label(ctx context.Context, id string) string {
	if name := n.Name(ctx, id); name != "" {
		return name + " (" + id + ")"
	}
	return id
}

// mentionRE matches <@U…> and <@U…|label> in Slack text.
var mentionRE = regexp.MustCompile(`<@([UW][A-Z0-9]+)(?:\|[^>]*)?>`)

// Mentions rewrites each <@U…> in text to @Name. An unknown id keeps
// its original form.
func (n *Names) Mentions(ctx context.Context, text string) string {
	if n == nil || !strings.Contains(text, "<@") {
		return text
	}
	return mentionRE.ReplaceAllStringFunc(text, func(m string) string {
		id := mentionRE.FindStringSubmatch(m)[1]
		if name := n.Name(ctx, id); name != "" {
			return "@" + name
		}
		return m
	})
}

func looksLikeUserID(id string) bool {
	return len(id) > 1 && (id[0] == 'U' || id[0] == 'W') && strings.ToUpper(id) == id
}

// lookup asks the command, then users.info. ok is false when neither
// gave a definite answer, so nothing is cached.
func (n *Names) lookup(ctx context.Context, id string) (string, bool) {
	name, decided := n.fromCommand(ctx, id)
	if decided && name != "" {
		return name, true
	}
	if n.Info == nil {
		return "", decided
	}
	return n.fromSlack(ctx, id)
}

// fromCommand runs the resolver command. decided is true for exit 0
// with a name and for exit 2 (no match).
func (n *Names) fromCommand(ctx context.Context, id string) (name string, decided bool) {
	if len(n.Command) == 0 {
		return "", false
	}
	argv := make([]string, len(n.Command))
	for i, arg := range n.Command {
		argv[i] = strings.ReplaceAll(arg, "{id}", id)
	}
	timeout := n.Timeout
	if timeout <= 0 {
		timeout = DefaultNameCommandLimit
	}
	cctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	run := n.Run
	if run == nil {
		run = runCommand
	}
	out, code, err := run(cctx, argv)
	switch {
	case err != nil:
		n.warn("slackwatch names: command failed", "id", id, "err", err)
		return "", false
	case code == 2:
		return "", true
	case code != 0:
		n.warn("slackwatch names: command exited", "id", id, "code", code)
		return "", false
	}
	var answer struct {
		Name string `json:"name"`
	}
	if err := json.Unmarshal(out, &answer); err != nil || strings.TrimSpace(answer.Name) == "" {
		n.warn("slackwatch names: command printed no name", "id", id)
		return "", false
	}
	return strings.TrimSpace(answer.Name), true
}

// fromSlack calls users.info, waiting out rate limits within MaxWait.
func (n *Names) fromSlack(ctx context.Context, id string) (string, bool) {
	var waited time.Duration
	for {
		u, err := n.Info.UserInfo(ctx, id)
		if err == nil {
			if u.Display != "" {
				return u.Display, true
			}
			return u.Name, u.Name != ""
		}
		if errors.Is(err, ErrUserNotFound) {
			return "", true
		}
		rl, ok := errors.AsType[*RateLimitError](err)
		if !ok || waited+rl.RetryAfter > n.MaxWait {
			n.warn("slackwatch names: users.info failed", "id", id, "err", err)
			return "", false
		}
		waited += rl.RetryAfter
		sleep := n.Sleep
		if sleep == nil {
			sleep = Wait
		}
		if err := sleep(ctx, rl.RetryAfter); err != nil {
			return "", false
		}
	}
}

func (n *Names) warn(msg string, args ...any) {
	if n.Logger != nil {
		n.Logger.Warn(msg, args...)
	}
}

func (n *Names) now() time.Time {
	if n.Now != nil {
		return n.Now()
	}
	return time.Now()
}

func (n *Names) cached(id string) (string, bool) {
	if n.Store == nil || n.TTL <= 0 {
		return "", false
	}
	name, seen, ok, err := n.Store.UserName(id)
	if err != nil || !ok {
		return "", false
	}
	now := n.now()
	if now.Before(seen) || now.Sub(seen) > n.TTL {
		return "", false
	}
	return name, true
}

func (n *Names) remember(id, name string) {
	if n.Store == nil || n.TTL <= 0 {
		return
	}
	if err := n.Store.PutUserName(id, name, n.now()); err != nil {
		n.warn("slackwatch names: cache write failed", "id", id, "err", err)
	}
}

func runCommand(ctx context.Context, argv []string) ([]byte, int, error) {
	// The operator configured argv; it runs without a shell.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...) //nolint:gosec // G204: operator-configured resolver
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	err := cmd.Run()
	if ctx.Err() != nil {
		return nil, -1, fmt.Errorf("names command: %w", ctx.Err())
	}
	if exit, ok := errors.AsType[*exec.ExitError](err); ok {
		return stdout.Bytes(), exit.ExitCode(), nil
	}
	if err != nil {
		return nil, -1, fmt.Errorf("names command: %w", err)
	}
	return stdout.Bytes(), 0, nil
}

// PutUserName caches id's name; an empty name records no match.
func (s *Store) PutUserName(id, name string, seen time.Time) error {
	_, err := s.db.Exec(`
INSERT INTO user_names (id, name, seen_at) VALUES (?, ?, ?)
ON CONFLICT(id) DO UPDATE SET name = excluded.name, seen_at = excluded.seen_at`,
		id, name, seen.Unix())
	if err != nil {
		return fmt.Errorf("store name of %s: %w", id, err)
	}
	return nil
}

// UserName returns id's cached name ("" for no match) and when it was
// stored.
func (s *Store) UserName(id string) (name string, seen time.Time, ok bool, err error) {
	var sec int64
	err = s.db.QueryRow(`SELECT name, seen_at FROM user_names WHERE id = ?`, id).Scan(&name, &sec)
	if errors.Is(err, sql.ErrNoRows) {
		return "", time.Time{}, false, nil
	}
	if err != nil {
		return "", time.Time{}, false, fmt.Errorf("read name of %s: %w", id, err)
	}
	return name, time.Unix(sec, 0), true, nil
}
