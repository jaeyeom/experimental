package main

import (
	"bytes"
	"context"
	"fmt"
	"log/slog"
	"reflect"
	"testing"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
	"github.com/slack-go/slack"
	"github.com/slack-go/slack/slackevents"
	"github.com/slack-go/slack/socketmode"
)

type fakeHandler struct {
	calls []string
}

func (f *fakeHandler) HandleMessage(_ context.Context, m slackwatch.Message) error {
	f.calls = append(f.calls, fmt.Sprintf("message %+v", m))
	return nil
}

func (f *fakeHandler) HandleEdit(m slackwatch.Message) error {
	f.calls = append(f.calls, fmt.Sprintf("edit %+v", m))
	return nil
}

func (f *fakeHandler) HandleDelete(channel, ts string) error {
	f.calls = append(f.calls, "delete "+channel+" "+ts)
	return nil
}

func (f *fakeHandler) HandleReaction(channel string, r slackwatch.Reaction) error {
	f.calls = append(f.calls, fmt.Sprintf("reaction %s %+v", channel, r))
	return nil
}

type fakeAcker struct {
	acked []string
}

func (f *fakeAcker) Ack(req socketmode.Request, _ ...any) error {
	f.acked = append(f.acked, req.EnvelopeID)
	return nil
}

func eventsAPI(inner any) socketmode.Event {
	return socketmode.Event{
		Type:    socketmode.EventTypeEventsAPI,
		Request: &socketmode.Request{EnvelopeID: "env-1"},
		Data: slackevents.EventsAPIEvent{
			Type:       slackevents.CallbackEvent,
			InnerEvent: slackevents.EventsAPIInnerEvent{Data: inner},
		},
	}
}

func TestHandleSocketEvent(t *testing.T) {
	tests := []struct {
		name string
		evt  socketmode.Event
		want []string
	}{
		{
			name: "message",
			evt: eventsAPI(&slackevents.MessageEvent{
				Channel: "C1", ChannelType: "channel", TimeStamp: "2.0", ThreadTimeStamp: "1.0", User: "U9", Text: "hi",
			}),
			want: []string{"message {Channel:C1 ChannelType:channel TS:2.0 ThreadTS:1.0 User:U9 Text:hi EditedTS:}"},
		},
		{
			name: "bot message uses the bot id",
			evt: eventsAPI(&slackevents.MessageEvent{
				SubType: "bot_message", Channel: "C1", ChannelType: "channel", TimeStamp: "2.0", BotID: "B1", Text: "hi",
			}),
			want: []string{"message {Channel:C1 ChannelType:channel TS:2.0 ThreadTS: User:B1 Text:hi EditedTS:}"},
		},
		{
			name: "edit",
			evt: eventsAPI(&slackevents.MessageEvent{
				SubType: "message_changed", Channel: "D1", ChannelType: "im",
				Message: &slack.Msg{Timestamp: "2.0", ThreadTimestamp: "1.0", User: "U9", Text: "fixed", Edited: &slack.Edited{Timestamp: "3.0"}},
			}),
			want: []string{"edit {Channel:D1 ChannelType:im TS:2.0 ThreadTS:1.0 User:U9 Text:fixed EditedTS:3.0}"},
		},
		{
			name: "delete",
			evt:  eventsAPI(&slackevents.MessageEvent{SubType: "message_deleted", Channel: "C1", DeletedTimeStamp: "2.0"}),
			want: []string{"delete C1 2.0"},
		},
		{
			name: "other subtypes are ignored",
			evt:  eventsAPI(&slackevents.MessageEvent{SubType: "channel_join", Channel: "C1", TimeStamp: "2.0", User: "U9"}),
		},
		{
			name: "reaction",
			evt: eventsAPI(&slackevents.ReactionAddedEvent{
				User: "U9", Reaction: "eyes", Item: slackevents.Item{Type: "message", Channel: "C1", Timestamp: "1.0"},
			}),
			want: []string{"reaction C1 {TS:1.0 User:U9 Name:eyes}"},
		},
		{
			name: "file reaction is ignored",
			evt:  eventsAPI(&slackevents.ReactionAddedEvent{User: "U9", Reaction: "eyes", Item: slackevents.Item{Type: "file"}}),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			h := &fakeHandler{}
			a := &fakeAcker{}
			handleSocketEvent(context.Background(), a, h, tt.evt, slog.New(slog.NewTextHandler(&bytes.Buffer{}, nil)))
			if !reflect.DeepEqual(h.calls, tt.want) {
				t.Errorf("calls = %q, want %q", h.calls, tt.want)
			}
			if !reflect.DeepEqual(a.acked, []string{"env-1"}) {
				t.Errorf("acked = %q, want [env-1]", a.acked)
			}
		})
	}
}
