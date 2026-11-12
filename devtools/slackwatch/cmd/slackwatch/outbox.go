package main

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
	"github.com/slack-go/slack"
)

// newDecider returns the decide client that runs cfg's decideCommand.
// Tests replace it with a fake so they never run the real decide.
var newDecider = func(cfg slackwatch.Config) slackwatch.Decider {
	return slackwatch.DecideCLI{Run: slackwatch.ExecDecide(cfg.DecideArgv())}
}

// slackAPIURL overrides the Slack Web API URL for the posting commands.
// Tests point it at a fake server.
var slackAPIURL = ""

func newSlackClient(token string) *slack.Client {
	if slackAPIURL != "" {
		return slack.New(token, slack.OptionAPIURL(slackAPIURL))
	}
	return slack.New(token)
}

// slackWriter is slackwatch.SlackWriter over the Slack Web API.
type slackWriter struct{}

func (slackWriter) AuthTest(ctx context.Context, token string) (slackwatch.AuthInfo, error) {
	res, err := newSlackClient(token).AuthTestContext(ctx)
	if err != nil {
		return slackwatch.AuthInfo{}, apiError("auth.test", err)
	}
	return slackwatch.AuthInfo{UserID: res.UserID, URL: res.URL}, nil
}

func (slackWriter) PostMessage(ctx context.Context, token, channel, threadTS, text string) (string, error) {
	opts := []slack.MsgOption{slack.MsgOptionText(text, false)}
	if threadTS != "" {
		opts = append(opts, slack.MsgOptionTS(threadTS))
	}
	_, ts, err := newSlackClient(token).PostMessageContext(ctx, channel, opts...)
	if err != nil {
		return "", apiError("chat.postMessage", err)
	}
	return ts, nil
}

// readTokenReader is slackwatch.MessageReader with the read token, which
// it loads on first use.
type readTokenReader struct {
	cfg slackwatch.Config
}

func (r readTokenReader) Message(ctx context.Context, channel, ts, threadTS string) (slackwatch.Message, error) {
	api, err := readClient(r.cfg)
	if err != nil {
		return slackwatch.Message{}, err
	}
	return slackReader{api: api}.Message(ctx, channel, ts, threadTS)
}

func readClient(cfg slackwatch.Config) (*slack.Client, error) {
	if err := slackwatch.CheckTokenFile(cfg.TokenFile); err != nil {
		return nil, err //nolint:wrapcheck // Already descriptive.
	}
	tokens, err := slackwatch.LoadTokens(cfg.TokenFile)
	if err != nil {
		return nil, err //nolint:wrapcheck // Already descriptive.
	}
	return newSlackClient(tokens.UserToken), nil
}

// lookupChannelName returns #name for a channel id, or "" when it is
// unknown, such as without a token file or for a DM.
func lookupChannelName(cfg slackwatch.Config, id string) string {
	if cfg.TokenFile == "" {
		return ""
	}
	api, err := readClient(cfg)
	if err != nil {
		return ""
	}
	ch, err := api.GetConversationInfoContext(context.Background(), &slack.GetConversationInfoInput{ChannelID: id})
	if err != nil || ch.Name == "" {
		return ""
	}
	return "#" + ch.Name
}

// newOutbox builds the outbox engine for configPath. It rereads the
// config before posting, so a new slackwatch pin takes effect at once.
func newOutbox(configPath string, cfg slackwatch.Config, store *slackwatch.Store, logger *slog.Logger) *slackwatch.Outbox {
	return &slackwatch.Outbox{
		Store:    store,
		Config:   cfg,
		Reload:   func() (slackwatch.Config, error) { return slackwatch.LoadConfig(configPath) },
		Decide:   newDecider(cfg),
		Notifier: &slackwatch.Notifier{Store: store, Config: cfg, Logger: logger},
		Writer:   slackWriter{},
		Reader:   readTokenReader{cfg: cfg},
		Now:      time.Now,
		Logger:   logger,
	}
}

// withOutbox opens the store and runs f with the outbox engine.
func withOutbox(configPath string, stderr io.Writer, f func(*slackwatch.Outbox) error) int {
	cfg, store, err := openStore(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	defer store.Close()
	if err := f(newOutbox(configPath, cfg, store, slog.New(slog.NewTextHandler(stderr, nil)))); err != nil {
		return fail(stderr, err)
	}
	return 0
}

// destination resolves --to: a channel id, a #name, or a message
// permalink, whose thread the post replies in.
func destination(cfg slackwatch.Config, store *slackwatch.Store, to string) (slackwatch.Draft, error) {
	var d slackwatch.Draft
	switch {
	case strings.HasPrefix(to, "#"):
		id, err := resolveChannel(cfg, strings.TrimPrefix(to, "#"))
		if err != nil {
			return d, err
		}
		d.Channel, d.DestName = id, to
		return d, nil
	case strings.Contains(to, "://"):
		p, err := slackwatch.ParsePost([]string{to})
		if err != nil {
			return d, err //nolint:wrapcheck // Already descriptive.
		}
		d.Channel, d.ThreadTS, d.ParentLink = p.Channel, p.ThreadTS, to
		if d.ThreadTS == "" {
			d.ThreadTS = p.TS
		} else {
			teamURL, err := store.TeamURL()
			if err != nil {
				return d, err //nolint:wrapcheck // Already descriptive.
			}
			if link := slackwatch.Permalink(teamURL, p.Channel, p.ThreadTS, ""); link != "" {
				d.ParentLink = link
			}
		}
	case slackwatch.ValidChannel(to):
		d.Channel = to
	default:
		return d, fmt.Errorf("--to %q is not a channel id, #name, or thread permalink", to)
	}
	d.DestName = lookupChannelName(cfg, d.Channel)
	return d, nil
}

func cmdPost(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("post", stderr)
	to := fs.String("to", "", "channel id, #name, or thread permalink (required)")
	textFile := fs.String("text-file", "", "file holding the exact text to post (required)")
	owner := fs.String("owner", "", "agent that gets the replies (required)")
	purpose := fs.String("purpose", "", "what the post is for (required)")
	var refs stringList
	fs.Var(&refs, "ref", "reference such as an issue or PR (repeatable)")
	asJSON := fs.Bool("json", false, "print the outbox record as JSON")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	switch {
	case len(pos) != 0:
		return usageErr(stderr, errors.New("post takes no arguments"))
	case *to == "", *textFile == "", *owner == "", *purpose == "":
		return usageErr(stderr, errors.New("post needs --to, --text-file, --owner, and --purpose"))
	}
	cfg, err := slackwatch.LoadConfig(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	if err := cfg.CheckPost(); err != nil {
		return fail(stderr, err)
	}
	text, err := os.ReadFile(*textFile)
	if err != nil {
		return fail(stderr, fmt.Errorf("read --text-file: %w", err))
	}
	return withOutbox(configPath, stderr, func(o *slackwatch.Outbox) error {
		d, err := destination(cfg, o.Store, *to)
		if err != nil {
			return err
		}
		d.Text, d.Owner, d.Purpose, d.Refs = string(text), *owner, *purpose, refs
		r, err := o.Submit(context.Background(), d)
		if err != nil {
			return err //nolint:wrapcheck // Already descriptive.
		}
		if *asJSON {
			if code := writeJSON(stdout, stderr, r); code != 0 {
				return errors.New("write JSON")
			}
			return nil
		}
		fmt.Fprintf(stdout, "outbox %d pending approval in decide item %s\n", r.ID, r.DecideID)
		return nil
	})
}

func cmdOutbox(configPath string, args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		return usageErr(stderr, errors.New("outbox needs ls, show, posted, poke, or cancel"))
	}
	sub, ok := map[string]func(string, []string, io.Writer, io.Writer) int{
		"ls":     cmdOutboxLs,
		"show":   cmdOutboxShow,
		"posted": cmdOutboxPosted,
		"poke":   cmdOutboxPoke,
		"cancel": cmdOutboxCancel,
	}[args[0]]
	if !ok {
		return usageErr(stderr, fmt.Errorf("unknown outbox command %q; want ls, show, posted, poke, or cancel", args[0]))
	}
	return sub(configPath, args[1:], stdout, stderr)
}

func outboxID(s string) (int64, error) {
	id, err := strconv.ParseInt(s, 10, 64)
	if err != nil || id <= 0 {
		return 0, fmt.Errorf("invalid outbox id %q", s)
	}
	return id, nil
}

func cmdOutboxLs(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("outbox ls", stderr)
	state := fs.String("state", "", "only records in this state: "+strings.Join(slackwatch.OutboxStates, ", "))
	asJSON := fs.Bool("json", false, "print JSON")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	if len(pos) != 0 {
		return usageErr(stderr, errors.New("outbox ls takes no arguments"))
	}
	if *state != "" && !slices.Contains(slackwatch.OutboxStates, *state) {
		return usageErr(stderr, fmt.Errorf("--state must be one of %s", strings.Join(slackwatch.OutboxStates, ", ")))
	}
	_, store, err := openStore(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	defer store.Close()
	recs, err := store.ListOutbox(*state)
	if err != nil {
		return fail(stderr, err)
	}
	if *asJSON {
		return writeJSON(stdout, stderr, recs)
	}
	tw := tabwriter.NewWriter(stdout, 0, 0, 2, ' ', 0)
	fmt.Fprintln(tw, "ID\tSTATE\tOWNER\tPURPOSE\tDESTINATION\tDECIDE\tWATCH\tREASON")
	for _, r := range recs {
		watch := ""
		if r.WatchID != 0 {
			watch = strconv.FormatInt(r.WatchID, 10)
		}
		fmt.Fprintf(tw, "%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", r.ID, r.State, r.Owner, r.Purpose, dest(r), r.DecideID, watch, r.Reason)
	}
	if err := tw.Flush(); err != nil {
		return fail(stderr, err)
	}
	return 0
}

func dest(r slackwatch.OutboxRecord) string {
	d := r.Channel
	if r.DestName != "" {
		d = r.DestName
	}
	if r.ThreadTS != "" {
		d += " thread " + r.ThreadTS
	}
	return d
}

func cmdOutboxShow(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("outbox show", stderr)
	asJSON := fs.Bool("json", false, "print JSON")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	if len(pos) != 1 {
		return usageErr(stderr, errors.New("outbox show needs one outbox id"))
	}
	id, err := outboxID(pos[0])
	if err != nil {
		return usageErr(stderr, err)
	}
	_, store, err := openStore(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	defer store.Close()
	r, err := store.GetOutbox(id)
	if err != nil {
		return fail(stderr, err)
	}
	if *asJSON {
		return writeJSON(stdout, stderr, r)
	}
	fmt.Fprintf(stdout, "outbox %d  %s\n", r.ID, r.State)
	if r.Reason != "" {
		fmt.Fprintf(stdout, "reason: %s\n", r.Reason)
	}
	fmt.Fprintf(stdout, "destination: %s (channel %s)\n", dest(r), r.Channel)
	if r.ParentLink != "" {
		fmt.Fprintf(stdout, "thread: %s\n", r.ParentLink)
	}
	fmt.Fprintf(stdout, "owner: %s\npurpose: %s\nrefs: %s\ndecide: %s\nsha256: %s\n",
		r.Owner, r.Purpose, strings.Join(r.Refs, ", "), r.DecideID, r.TextHash)
	if r.Answer != "" {
		fmt.Fprintf(stdout, "answer: %s\n", r.Answer)
	}
	if r.Permalink != "" {
		fmt.Fprintf(stdout, "posted: %s\n", r.Permalink)
	}
	if r.WatchID != 0 {
		fmt.Fprintf(stdout, "watch: %d\n", r.WatchID)
	}
	fmt.Fprintln(stdout, "text (post it verbatim):")
	fmt.Fprint(stdout, r.Text)
	if !strings.HasSuffix(r.Text, "\n") {
		fmt.Fprintln(stdout)
	}
	return 0
}

func cmdOutboxPosted(configPath string, args []string, stdout, stderr io.Writer) int {
	pos, err := parseInterspersed(newFlagSet("outbox posted", stderr), args)
	if err != nil {
		return 2
	}
	if len(pos) != 2 {
		return usageErr(stderr, errors.New("outbox posted needs an outbox id and a permalink"))
	}
	id, err := outboxID(pos[0])
	if err != nil {
		return usageErr(stderr, err)
	}
	return withOutbox(configPath, stderr, func(o *slackwatch.Outbox) error {
		r, err := o.Posted(context.Background(), id, pos[1])
		if err != nil {
			return err //nolint:wrapcheck // Already descriptive.
		}
		if r.State == slackwatch.OutboxMismatch {
			fmt.Fprintf(stdout, "outbox %d mismatch: %s; watch %d flagged\n", r.ID, r.Reason, r.WatchID)
			return nil
		}
		fmt.Fprintf(stdout, "outbox %d posted; watch %d\n", r.ID, r.WatchID)
		return nil
	})
}

func cmdOutboxPoke(configPath string, args []string, stdout, stderr io.Writer) int {
	pos, err := parseInterspersed(newFlagSet("outbox poke", stderr), args)
	if err != nil {
		return 2
	}
	if len(pos) != 1 {
		return usageErr(stderr, errors.New("outbox poke needs one decide id"))
	}
	return withOutbox(configPath, stderr, func(o *slackwatch.Outbox) error {
		if err := o.Poke(context.Background(), pos[0]); err != nil {
			return err //nolint:wrapcheck // Already descriptive.
		}
		r, err := o.Store.ListOutbox("")
		if err != nil {
			return err //nolint:wrapcheck // Already descriptive.
		}
		for _, rec := range r {
			if rec.DecideID == pos[0] {
				fmt.Fprintf(stdout, "outbox %d %s\n", rec.ID, rec.State)
			}
		}
		return nil
	})
}

func cmdOutboxCancel(configPath string, args []string, stdout, stderr io.Writer) int {
	fs := newFlagSet("outbox cancel", stderr)
	reason := fs.String("reason", "", "why the draft is withdrawn")
	pos, err := parseInterspersed(fs, args)
	if err != nil {
		return 2
	}
	if len(pos) != 1 {
		return usageErr(stderr, errors.New("outbox cancel needs one outbox id"))
	}
	id, err := outboxID(pos[0])
	if err != nil {
		return usageErr(stderr, err)
	}
	return withOutbox(configPath, stderr, func(o *slackwatch.Outbox) error {
		r, err := o.Cancel(context.Background(), id, *reason)
		if err != nil {
			return err //nolint:wrapcheck // Already descriptive.
		}
		fmt.Fprintf(stdout, "outbox %d cancelled\n", r.ID)
		return nil
	})
}

// cmdPin checks the write token with auth.test and writes its pin to
// the config. It also re-enables posting.
func cmdPin(configPath string, args []string, stdout, stderr io.Writer) int {
	if len(args) != 0 {
		return usageErr(stderr, errors.New("pin takes no arguments"))
	}
	cfg, err := slackwatch.LoadConfig(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	if cfg.Post.ExpectUser == "" || cfg.Post.WriteTokenFile == "" {
		return fail(stderr, errors.New("pin needs post.expectUser and post.writeTokenFile in slackwatch config"))
	}
	token, err := slackwatch.LoadWriteToken(cfg.Post.WriteTokenFile)
	if err != nil {
		return fail(stderr, err)
	}
	auth, err := slackWriter{}.AuthTest(context.Background(), token)
	if err != nil {
		return fail(stderr, err)
	}
	if auth.UserID != cfg.Post.ExpectUser {
		return fail(stderr, fmt.Errorf("the write token belongs to %s, not post.expectUser %s; config unchanged", auth.UserID, cfg.Post.ExpectUser))
	}
	salt := cfg.Post.PinSalt
	if salt == "" {
		b := make([]byte, 16)
		if _, err := rand.Read(b); err != nil {
			return fail(stderr, err)
		}
		salt = hex.EncodeToString(b)
	}
	if err := writePin(configPath, salt, slackwatch.Pin(salt, token)); err != nil {
		return fail(stderr, err)
	}
	store, err := slackwatch.OpenStore(cfg.DBPath)
	if err != nil {
		return fail(stderr, err)
	}
	defer store.Close()
	if err := store.SetPostingDisabled(""); err != nil {
		return fail(stderr, err)
	}
	fmt.Fprintf(stdout, "pinned the write token of %s; posting enabled\n", auth.UserID)
	return 0
}

// writePin sets post.pinSalt and post.tokenPin in the config file and
// keeps every other setting. Keys are rewritten in sorted order.
func writePin(path, salt, pin string) error {
	data, err := os.ReadFile(path)
	if err != nil {
		return fmt.Errorf("read slackwatch config: %w", err)
	}
	var top map[string]json.RawMessage
	if err := json.Unmarshal(data, &top); err != nil {
		return fmt.Errorf("parse slackwatch config: %w", err)
	}
	post := map[string]json.RawMessage{}
	if raw, ok := top["post"]; ok {
		if err := json.Unmarshal(raw, &post); err != nil {
			return fmt.Errorf("parse post in slackwatch config: %w", err)
		}
	}
	for k, v := range map[string]string{"pinSalt": salt, "tokenPin": pin} {
		b, err := json.Marshal(v)
		if err != nil {
			return fmt.Errorf("encode %s: %w", k, err)
		}
		post[k] = b
	}
	b, err := json.Marshal(post)
	if err != nil {
		return fmt.Errorf("encode post: %w", err)
	}
	top["post"] = b
	out, err := json.MarshalIndent(top, "", "  ")
	if err != nil {
		return fmt.Errorf("encode slackwatch config: %w", err)
	}
	info, err := os.Stat(path)
	if err != nil {
		return fmt.Errorf("stat slackwatch config: %w", err)
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), ".config-*.json")
	if err != nil {
		return fmt.Errorf("write slackwatch config: %w", err)
	}
	defer os.Remove(tmp.Name()) //nolint:errcheck // Gone after a successful rename.
	if _, err := tmp.Write(append(out, '\n')); err != nil {
		tmp.Close()
		return fmt.Errorf("write slackwatch config: %w", err)
	}
	if err := tmp.Chmod(info.Mode().Perm()); err != nil {
		tmp.Close()
		return fmt.Errorf("write slackwatch config: %w", err)
	}
	if err := tmp.Close(); err != nil {
		return fmt.Errorf("write slackwatch config: %w", err)
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		return fmt.Errorf("write slackwatch config: %w", err)
	}
	return nil
}

// pollOutbox runs poll every interval until ctx ends and passes each
// result to done. Errors are logged; the next poll retries.
func pollOutbox(ctx context.Context, poll func(context.Context) error, interval time.Duration, done func(error), logger *slog.Logger) {
	tick := time.NewTicker(interval)
	defer tick.Stop()
	for {
		err := poll(ctx)
		if ctx.Err() != nil {
			return
		}
		if err != nil {
			logger.Error("slackwatch serve: outbox poll", "error", err)
		}
		done(err)
		select {
		case <-ctx.Done():
			return
		case <-tick.C:
		}
	}
}

// startOutbox starts posting in serve when post.mode is set. A config
// that does not allow posting is logged and posting stays off; watching
// continues. Each poll first resolves decideCommand, so a missing decide
// fails the first poll at startup and every poll after it, even with
// nothing pending. polled receives each poll result.
func startOutbox(ctx context.Context, configPath string, cfg slackwatch.Config, store *slackwatch.Store, logger *slog.Logger, polled func(error)) {
	if cfg.Post.Mode == "" {
		return
	}
	if err := cfg.CheckPost(); err != nil {
		logger.Error("slackwatch serve: posting not started", "error", err)
		return
	}
	o := newOutbox(configPath, cfg, store, logger)
	if cfg.Post.Mode == slackwatch.PostModeDirect {
		if err := o.CheckIdentity(ctx); err != nil {
			logger.Error("slackwatch serve: write token identity check", "error", err)
		}
	}
	logger.Info("slackwatch serve: posting", "mode", cfg.Post.Mode)
	poll := func(ctx context.Context) error {
		if _, err := cfg.ResolveDecide(); err != nil {
			return err //nolint:wrapcheck // Already names decideCommand.
		}
		return o.Poll(ctx) //nolint:wrapcheck // Already descriptive.
	}
	go pollOutbox(ctx, poll, slackwatch.DefaultOutboxPoll, polled, logger)
}
