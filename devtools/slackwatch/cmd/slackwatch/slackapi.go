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
// channels, when set, stores channel names seen on search results.
type slackReader struct {
	api      *slack.Client
	channels *slackwatch.Store
	now      func() time.Time
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
			if err := r.remember(m.Channel.Name, m.Channel.ID); err != nil {
				return nil, err
			}
			out = append(out, searchFetched(m))
		}
		if params.Page >= res.Pages {
			return out, nil
		}
		params.Page++
	}
}

// ListChannels reads one page of conversations.list.
func (r slackReader) ListChannels(ctx context.Context, cursor string) (slackwatch.ChannelPage, error) {
	p := &slack.GetConversationsParameters{
		ExcludeArchived: true, Limit: pageLimit, Cursor: cursor,
		Types: []string{"public_channel", "private_channel"},
	}
	channels, next, err := r.api.GetConversationsContext(ctx, p)
	if err != nil {
		return slackwatch.ChannelPage{}, apiError("conversations.list", err)
	}
	page := slackwatch.ChannelPage{Next: next}
	for _, c := range channels {
		if c.Name == "" || c.ID == "" {
			continue
		}
		page.Channels = append(page.Channels, slackwatch.NamedChannel{Name: c.Name, ID: c.ID})
	}
	return page, nil
}

// ChannelID finds a channel by name with conversations.list. It does not
// use the state cache; resolveChannel does.
func (r slackReader) ChannelID(ctx context.Context, name string) (string, error) {
	return slackwatch.ChannelLookup{ //nolint:wrapcheck // Already names the channel and the fix.
		List: r, TTL: slackwatch.DefaultChannelCacheTTL, MaxWait: slackwatch.DefaultChannelLookupWait,
	}.ChannelID(ctx, name)
}

// remember stores a channel name seen by the service.
func (r slackReader) remember(name, id string) error {
	if r.channels == nil || name == "" || id == "" {
		return nil
	}
	now := time.Now()
	if r.now != nil {
		now = r.now()
	}
	if err := r.channels.PutChannel(name, id, now); err != nil {
		return fmt.Errorf("remember channel #%s: %w", name, err)
	}
	return nil
}

// Message reads one message back: a thread reply from
// conversations.replies, a top-level message from conversations.history.
func (r slackReader) Message(ctx context.Context, channel, ts, threadTS string) (slackwatch.Message, error) {
	var msgs []slack.Message
	if threadTS != "" {
		got, _, _, err := r.api.GetConversationRepliesContext(ctx, &slack.GetConversationRepliesParameters{
			ChannelID: channel, Timestamp: threadTS, Oldest: ts, Latest: ts, Inclusive: true, Limit: 2,
		})
		if err != nil {
			return slackwatch.Message{}, apiError("conversations.replies", err)
		}
		msgs = got
	} else {
		res, err := r.api.GetConversationHistoryContext(ctx, &slack.GetConversationHistoryParameters{
			ChannelID: channel, Oldest: ts, Latest: ts, Inclusive: true, Limit: 1,
		})
		if err != nil {
			return slackwatch.Message{}, apiError("conversations.history", err)
		}
		msgs = res.Messages
	}
	for _, m := range msgs {
		if m.Timestamp == ts {
			return slackwatch.Message{
				Channel: channel, TS: ts, ThreadTS: m.ThreadTimestamp, User: messageAuthor(m.User, m.BotID), Text: m.Text,
			}, nil
		}
	}
	return slackwatch.Message{}, fmt.Errorf("message %s %s not found", channel, ts)
}

// Recent lists messages at channel since oldest. A threadTS selects
// conversations.replies; otherwise conversations.history. Unlike History
// and Replies, every message is returned, including subtypes, so a post
// that already landed can be matched.
func (r slackReader) Recent(ctx context.Context, channel, threadTS, oldest string) ([]slackwatch.Message, error) {
	msgs, err := r.recentMessages(ctx, channel, threadTS, oldest)
	if err != nil {
		return nil, err
	}
	out := make([]slackwatch.Message, 0, len(msgs))
	for _, m := range msgs {
		out = append(out, slackwatch.Message{
			Channel: channel, TS: m.Timestamp, ThreadTS: m.ThreadTimestamp,
			User: messageAuthor(m.User, m.BotID), Text: m.Text,
		})
	}
	return out, nil
}

func (r slackReader) recentMessages(ctx context.Context, channel, threadTS, oldest string) ([]slack.Message, error) {
	if threadTS != "" {
		return r.replyMessages(ctx, channel, threadTS, oldest)
	}
	return r.historyMessages(ctx, channel, oldest)
}

func (r slackReader) historyMessages(ctx context.Context, channel, oldest string) ([]slack.Message, error) {
	p := &slack.GetConversationHistoryParameters{ChannelID: channel, Oldest: oldest, Limit: pageLimit}
	var out []slack.Message
	for {
		res, err := r.api.GetConversationHistoryContext(ctx, p)
		if err != nil {
			return nil, apiError("conversations.history", err)
		}
		out = append(out, res.Messages...)
		if !res.HasMore || res.ResponseMetaData.NextCursor == "" {
			return out, nil
		}
		p.Cursor = res.ResponseMetaData.NextCursor
	}
}

func (r slackReader) replyMessages(ctx context.Context, channel, threadTS, oldest string) ([]slack.Message, error) {
	p := &slack.GetConversationRepliesParameters{ChannelID: channel, Timestamp: threadTS, Oldest: oldest, Limit: pageLimit}
	var out []slack.Message
	for {
		msgs, more, next, err := r.api.GetConversationRepliesContext(ctx, p)
		if err != nil {
			return nil, apiError("conversations.replies", err)
		}
		out = append(out, msgs...)
		if !more || next == "" {
			return out, nil
		}
		p.Cursor = next
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
		}, LatestReply: m.LatestReply}
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
	if auth.URL != "" {
		if err := store.SetTeamURL(auth.URL); err != nil {
			return nil, err //nolint:wrapcheck // Already descriptive.
		}
	}
	return &slackwatch.Processor{
		Store:    store,
		Notifier: &slackwatch.Notifier{Store: store, Config: cfg, Logger: logger},
		Config:   cfg,
		Self:     auth.UserID,
		TeamURL:  auth.URL,
		Now:      time.Now,
		Logger:   logger,
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
	return &slackwatch.CatchUp{Slack: slackReader{api: api, channels: store}, Proc: proc}, nil
}
