package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"time"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
	"github.com/slack-go/slack"
)

// pageLimit is the page size asked of the Web API.
const pageLimit = 200

// slackReader is slackwatch.SlackReader over the Slack Web API.
type slackReader struct {
	api *slack.Client
}

func (r slackReader) Replies(ctx context.Context, channel, threadTS, oldest string) ([]slackwatch.Fetched, error) {
	p := &slack.GetConversationRepliesParameters{ChannelID: channel, Timestamp: threadTS, Oldest: oldest, Limit: pageLimit}
	var out []slackwatch.Fetched
	for {
		msgs, more, next, err := r.api.GetConversationRepliesContext(ctx, p)
		if err != nil {
			return nil, apiError("conversations.replies", err)
		}
		out = appendFetched(out, channel, msgs)
		if !more || next == "" {
			return out, nil
		}
		p.Cursor = next
	}
}

func (r slackReader) History(ctx context.Context, channel, oldest string) ([]slackwatch.Fetched, error) {
	p := &slack.GetConversationHistoryParameters{ChannelID: channel, Oldest: oldest, Limit: pageLimit}
	var out []slackwatch.Fetched
	for {
		res, err := r.api.GetConversationHistoryContext(ctx, p)
		if err != nil {
			return nil, apiError("conversations.history", err)
		}
		out = appendFetched(out, channel, res.Messages)
		if !res.HasMore || res.ResponseMetaData.NextCursor == "" {
			return out, nil
		}
		p.Cursor = res.ResponseMetaData.NextCursor
	}
}

func (r slackReader) Search(ctx context.Context, query string) ([]slackwatch.Fetched, error) {
	params := slack.SearchParameters{Sort: "timestamp", SortDirection: "asc", Count: 100, Page: 1}
	var out []slackwatch.Fetched
	for {
		res, err := r.api.SearchMessagesContext(ctx, query, params)
		if err != nil {
			return nil, apiError("search.messages", err)
		}
		for _, m := range res.Matches {
			out = append(out, searchFetched(m))
		}
		if params.Page >= res.Pages {
			return out, nil
		}
		params.Page++
	}
}

func apiError(method string, err error) error {
	if rl, ok := errors.AsType[*slack.RateLimitedError](err); ok {
		return &slackwatch.RateLimitError{RetryAfter: rl.RetryAfter}
	}
	return fmt.Errorf("slack %s: %w", method, err)
}

// appendFetched converts the messages that live events would also handle.
func appendFetched(out []slackwatch.Fetched, channel string, msgs []slack.Message) []slackwatch.Fetched {
	for _, m := range msgs {
		if !newMessageSubtype(m.SubType) {
			continue
		}
		f := slackwatch.Fetched{Message: slackwatch.Message{
			Channel: channel, TS: m.Timestamp, ThreadTS: m.ThreadTimestamp, User: messageAuthor(m.User, m.BotID), Text: m.Text,
		}}
		for _, r := range m.Reactions {
			for _, u := range r.Users {
				f.Reactions = append(f.Reactions, slackwatch.Reaction{TS: m.Timestamp, User: u, Name: r.Name})
			}
		}
		out = append(out, f)
	}
	return out
}

// searchFetched converts a search match. Its thread comes from the
// permalink's thread_ts.
func searchFetched(m slack.SearchMessage) slackwatch.Fetched {
	f := slackwatch.Fetched{Message: slackwatch.Message{Channel: m.Channel.ID, TS: m.Timestamp, User: m.User, Text: m.Text}}
	if m.Channel.IsMPIM {
		f.ChannelType = "mpim"
	}
	if p, err := slackwatch.ParsePost([]string{m.Permalink}); err == nil {
		f.ThreadTS = p.ThreadTS
	}
	return f
}

// newProcessor learns the token owner from auth.test and builds the
// processor shared by live events and catch-up.
func newProcessor(ctx context.Context, api *slack.Client, cfg slackwatch.Config, store *slackwatch.Store, logger *slog.Logger) (*slackwatch.Processor, error) {
	auth, err := api.AuthTestContext(ctx)
	if err != nil {
		return nil, fmt.Errorf("slack auth.test: %w", err)
	}
	return &slackwatch.Processor{
		Store:    store,
		Notifier: &slackwatch.Notifier{Store: store, Config: cfg, Logger: logger},
		Config:   cfg,
		Self:     auth.UserID,
		TeamURL:  auth.URL,
		Now:      time.Now,
	}, nil
}

// slackCatchUp is the catch-up that add runs, with the user token.
func slackCatchUp(cfg slackwatch.Config, store *slackwatch.Store, stderr io.Writer) (*slackwatch.CatchUp, error) {
	tokens, err := slackwatch.LoadTokens(cfg.TokenFile)
	if err != nil {
		return nil, err //nolint:wrapcheck // Already descriptive.
	}
	api := slack.New(tokens.UserToken)
	proc, err := newProcessor(context.Background(), api, cfg, store, slog.New(slog.NewTextHandler(stderr, nil)))
	if err != nil {
		return nil, err
	}
	return &slackwatch.CatchUp{Slack: slackReader{api: api}, Proc: proc}, nil
}
