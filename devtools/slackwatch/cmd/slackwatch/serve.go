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

// serve connects through Socket Mode and handles events until ctx ends.
func serve(ctx context.Context, cfg slackwatch.Config, store *slackwatch.Store, tokens slackwatch.Tokens, logger *slog.Logger) error {
	api := slack.New(tokens.UserToken, slack.OptionAppLevelToken(tokens.AppToken))
	auth, err := api.AuthTestContext(ctx)
	if err != nil {
		return fmt.Errorf("slack auth.test: %w", err)
	}
	logger.Info("slackwatch serve: authenticated", "user", auth.UserID, "team", auth.TeamID)
	proc := &slackwatch.Processor{
		Store:    store,
		Notifier: &slackwatch.Notifier{Store: store, Config: cfg, Logger: logger},
		Config:   cfg,
		Self:     auth.UserID,
		TeamURL:  auth.URL,
		Now:      time.Now,
	}
	client := socketmode.New(api)
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
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
			handleSocketEvent(ctx, client, proc, evt, logger)
		}
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
	switch ev.SubType {
	case "", "thread_broadcast", "bot_message", "file_share", "me_message":
		user := ev.User
		if user == "" {
			user = ev.BotID
		}
		err = h.HandleMessage(ctx, slackwatch.Message{
			Channel: ev.Channel, ChannelType: ev.ChannelType, TS: ev.TimeStamp,
			ThreadTS: ev.ThreadTimeStamp, User: user, Text: ev.Text,
		})
	case "message_changed":
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
	case "message_deleted":
		err = h.HandleDelete(ev.Channel, ev.DeletedTimeStamp)
	}
	if err != nil {
		return fmt.Errorf("handle message %s/%s: %w", ev.Channel, ev.TimeStamp, err)
	}
	return nil
}
