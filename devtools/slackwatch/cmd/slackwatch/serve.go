package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"os/signal"
	"time"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
	"github.com/slack-go/slack"
	"github.com/slack-go/slack/slackevents"
	"github.com/slack-go/slack/socketmode"
	"golang.org/x/sys/unix"
)

// eventHandler is what Socket Mode events are dispatched to.
// *slackwatch.Processor is the real one.
type eventHandler interface {
	HandleMessage(ctx context.Context, m slackwatch.Message) error
	HandleEdit(m slackwatch.Message) error
	HandleDelete(channel, ts string) error
	HandleReaction(channel string, r slackwatch.Reaction) error
}

// acker acknowledges a Socket Mode request. *socketmode.Client is the
// real one.
type acker interface {
	Ack(req socketmode.Request, payload ...any) error
}

func cmdServe(configPath string, args []string, _, stderr io.Writer) int {
	if len(args) != 0 {
		fmt.Fprintln(stderr, "slackwatch serve: takes no arguments")
		return 2
	}
	cfg, err := slackwatch.LoadConfig(configPath)
	if err != nil {
		return fail(stderr, err)
	}
	if err := slackwatch.CheckTokenFile(cfg.TokenFile); err != nil {
		return fail(stderr, err)
	}
	store, err := slackwatch.OpenStore(cfg.DBPath)
	if err != nil {
		return fail(stderr, err)
	}
	defer store.Close()
	tokens, err := slackwatch.LoadTokens(cfg.TokenFile)
	if err != nil {
		return fail(stderr, err)
	}
	ctx, stop := signal.NotifyContext(context.Background(), unix.SIGINT, unix.SIGTERM)
	defer stop()
	logger := slog.New(slog.NewTextHandler(stderr, nil))
	if err := serve(ctx, cfg, store, tokens, logger); err != nil && !errors.Is(err, context.Canceled) {
		return fail(stderr, err)
	}
	return 0
}

// sweeper runs a catch-up and expiry pass. *slackwatch.CatchUp is the
// real one.
type sweeper interface {
	Sweep(ctx context.Context) error
}

// serve connects through Socket Mode and handles events until ctx ends.
// Every (re)connect and every sweepInterval triggers a sweep, which runs
// beside event handling so acks are not delayed.
func serve(ctx context.Context, cfg slackwatch.Config, store *slackwatch.Store, tokens slackwatch.Tokens, logger *slog.Logger) error {
	api := slack.New(tokens.UserToken, slack.OptionAppLevelToken(tokens.AppToken))
	proc, err := newProcessor(ctx, api, cfg, store, logger)
	if err != nil {
		return err
	}
	logger.Info("slackwatch serve: authenticated", "user", proc.Self)
	client := socketmode.New(api)
	hb := newHeartbeat(store, slackwatch.DefaultHeartbeatInterval, logger, time.Now)
	defer hb.stop() // Deferred first, so it runs after cancel.
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	go hb.run(ctx)
	sweeps := make(chan struct{}, 1)
	go runSweeps(ctx, &slackwatch.CatchUp{Slack: slackReader{api: api}, Proc: proc}, sweeps, cfg.SweepInterval.Duration, hb.swept, logger)
	done := make(chan error, 1)
	go func() { done <- client.RunContext(ctx) }()
	for {
		select {
		case err := <-done:
			if err != nil {
				return fmt.Errorf("socket mode: %w", err)
			}
			return nil
		case evt := <-client.Events:
			hb.socketEvent(evt.Type)
			if evt.Type == socketmode.EventTypeConnected {
				select {
				case sweeps <- struct{}{}:
				default: // A sweep is already pending.
				}
			}
			handleSocketEvent(ctx, client, proc, evt, logger)
		}
	}
}

// runSweeps sweeps on each trigger and every interval until ctx ends.
// Each result goes to done, with catchUp set for a triggered sweep (a
// connect). Errors are logged; the next sweep retries.
func runSweeps(ctx context.Context, s sweeper, trigger <-chan struct{}, interval time.Duration, done func(catchUp bool, err error), logger *slog.Logger) {
	if interval <= 0 {
		interval = slackwatch.DefaultSweepInterval
	}
	tick := time.NewTicker(interval)
	defer tick.Stop()
	var catchUp bool
	for {
		select {
		case <-ctx.Done():
			return
		case <-trigger:
			catchUp = true
		case <-tick.C:
			catchUp = false
		}
		err := s.Sweep(ctx)
		if err != nil {
			logger.Error("slackwatch serve: sweep", "error", err)
		}
		done(catchUp, err)
	}
}

// handleSocketEvent acknowledges evt and dispatches it to h. Handler
// errors are logged so one bad event does not stop the server.
func handleSocketEvent(ctx context.Context, a acker, h eventHandler, evt socketmode.Event, logger *slog.Logger) {
	switch evt.Type {
	case socketmode.EventTypeConnected:
		logger.Info("slackwatch serve: connected")
		return
	case socketmode.EventTypeConnectionError, socketmode.EventTypeIncomingError:
		logger.Warn("slackwatch serve: connection problem", "type", evt.Type)
		return
	case socketmode.EventTypeEventsAPI:
	default:
		return
	}
	// Ack first: Slack retries a request not acked within 3 seconds, and
	// a notify command may take longer.
	if evt.Request != nil {
		if err := a.Ack(*evt.Request); err != nil {
			logger.Warn("slackwatch serve: ack failed", "error", err)
		}
	}
	api, ok := evt.Data.(slackevents.EventsAPIEvent)
	if !ok {
		return
	}
	if err := dispatch(ctx, h, api.InnerEvent.Data); err != nil {
		logger.Error("slackwatch serve: handle event", "type", api.InnerEvent.Type, "error", err)
	}
}

func dispatch(ctx context.Context, h eventHandler, inner any) error {
	switch ev := inner.(type) {
	case *slackevents.MessageEvent:
		return dispatchMessage(ctx, h, ev)
	case *slackevents.ReactionAddedEvent:
		if ev.Item.Type != "message" {
			return nil
		}
		if err := h.HandleReaction(ev.Item.Channel, slackwatch.Reaction{TS: ev.Item.Timestamp, User: ev.User, Name: ev.Reaction}); err != nil {
			return fmt.Errorf("handle reaction: %w", err)
		}
	}
	return nil
}

func dispatchMessage(ctx context.Context, h eventHandler, ev *slackevents.MessageEvent) error {
	var err error
	switch {
	case newMessageSubtype(ev.SubType):
		err = h.HandleMessage(ctx, slackwatch.Message{
			Channel: ev.Channel, ChannelType: ev.ChannelType, TS: ev.TimeStamp,
			ThreadTS: ev.ThreadTimeStamp, User: messageAuthor(ev.User, ev.BotID), Text: ev.Text,
		})
	case ev.SubType == "message_changed":
		if ev.Message == nil {
			return nil
		}
		m := slackwatch.Message{
			Channel: ev.Channel, ChannelType: ev.ChannelType, TS: ev.Message.Timestamp,
			ThreadTS: ev.Message.ThreadTimestamp, User: ev.Message.User, Text: ev.Message.Text,
		}
		if ev.Message.Edited != nil {
			m.EditedTS = ev.Message.Edited.Timestamp
		}
		err = h.HandleEdit(m)
	case ev.SubType == "message_deleted":
		err = h.HandleDelete(ev.Channel, ev.DeletedTimeStamp)
	}
	if err != nil {
		return fmt.Errorf("handle message %s/%s: %w", ev.Channel, ev.TimeStamp, err)
	}
	return nil
}

// newMessageSubtype reports whether a message subtype is a new message
// that events and catch-up classify. Others, such as joins, are ignored.
func newMessageSubtype(subtype string) bool {
	switch subtype {
	case "", "thread_broadcast", "bot_message", "file_share", "me_message":
		return true
	}
	return false
}

// messageAuthor is the user, or the bot id for a bot message.
func messageAuthor(user, botID string) string {
	if user == "" {
		return botID
	}
	return user
}
