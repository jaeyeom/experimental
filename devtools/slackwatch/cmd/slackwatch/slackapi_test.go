package main

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"net/http/httptest"
	"reflect"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
	"github.com/slack-go/slack"
)

// fakeSlackAPI serves canned Web API responses keyed by method.
func fakeSlackAPI(t *testing.T, handle func(method string, r *http.Request) (int, string)) slackReader {
	t.Helper()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if err := r.ParseForm(); err != nil {
			t.Error(err)
		}
		code, body := handle(r.URL.Path[1:], r)
		if code == http.StatusTooManyRequests {
			w.Header().Set("Retry-After", "7")
		}
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(code)
		fmt.Fprint(w, body)
	}))
	t.Cleanup(srv.Close)
	return slackReader{api: slack.New("xoxp-test", slack.OptionAPIURL(srv.URL+"/"))}
}

func TestSlackReaderReplies(t *testing.T) {
	var oldest []string
	r := fakeSlackAPI(t, func(method string, req *http.Request) (int, string) {
		if method != "conversations.replies" || req.Form.Get("channel") != "C1" || req.Form.Get("ts") != "1.000100" {
			t.Errorf("unexpected call %s %v", method, req.Form)
		}
		oldest = append(oldest, req.Form.Get("oldest"))
		if req.Form.Get("cursor") == "" {
			return 200, `{"ok":true,"has_more":true,"response_metadata":{"next_cursor":"p2"},"messages":[
{"type":"message","user":"USELF","text":"post","ts":"1.000100","thread_ts":"1.000100",
 "reactions":[{"name":"eyes","users":["U8","U9"],"count":2}]}]}`
		}
		return 200, `{"ok":true,"has_more":false,"messages":[
{"type":"message","bot_id":"B1","text":"bot","ts":"2.000100","thread_ts":"1.000100"},
{"type":"message","subtype":"channel_join","user":"U7","text":"joined","ts":"3.000100","thread_ts":"1.000100"}]}`
	})
	got, err := r.Replies(context.Background(), "C1", "1.000100", "0.5")
	if err != nil {
		t.Fatal(err)
	}
	want := []slackwatch.Fetched{
		{
			Message: slackwatch.Message{Channel: "C1", TS: "1.000100", ThreadTS: "1.000100", User: "USELF", Text: "post"},
			Reactions: []slackwatch.Reaction{
				{TS: "1.000100", User: "U8", Name: "eyes"}, {TS: "1.000100", User: "U9", Name: "eyes"},
			},
		},
		{Message: slackwatch.Message{Channel: "C1", TS: "2.000100", ThreadTS: "1.000100", User: "B1", Text: "bot"}},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("Replies = %+v, want %+v", got, want)
	}
	if !reflect.DeepEqual(oldest, []string{"0.5", "0.5"}) {
		t.Errorf("oldest = %q, want it on every page", oldest)
	}
}

func TestSlackReaderHistory(t *testing.T) {
	r := fakeSlackAPI(t, func(method string, req *http.Request) (int, string) {
		if method != "conversations.history" || req.Form.Get("oldest") != "1.000100" {
			t.Errorf("unexpected call %s %v", method, req.Form)
		}
		return 200, `{"ok":true,"messages":[{"type":"message","user":"U9","text":"hi","ts":"2.000100"}]}`
	})
	got, err := r.History(context.Background(), "D1", "1.000100")
	if err != nil {
		t.Fatal(err)
	}
	want := []slackwatch.Fetched{{Message: slackwatch.Message{Channel: "D1", TS: "2.000100", User: "U9", Text: "hi"}}}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("History = %+v, want %+v", got, want)
	}
}

func TestSlackReaderHistoryLatestReply(t *testing.T) {
	r := fakeSlackAPI(t, func(string, *http.Request) (int, string) {
		return 200, `{"ok":true,"messages":[{"type":"message","user":"U9","text":"q","ts":"2.000100","thread_ts":"2.000100","latest_reply":"4.000100"}]}`
	})
	got, err := r.History(context.Background(), "C2", "1.000100")
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || got[0].LatestReply != "4.000100" {
		t.Errorf("History = %+v, want latest_reply 4.000100", got)
	}
}

func TestSlackReaderChannelID(t *testing.T) {
	r := fakeSlackAPI(t, func(method string, req *http.Request) (int, string) {
		if method != "conversations.list" || req.Form.Get("types") != "public_channel,private_channel" {
			t.Errorf("unexpected call %s %v", method, req.Form)
		}
		if req.Form.Get("cursor") == "" {
			return 200, `{"ok":true,"channels":[{"id":"C1","name":"general"}],"response_metadata":{"next_cursor":"p2"}}`
		}
		return 200, `{"ok":true,"channels":[{"id":"C2","name":"deploys"}],"response_metadata":{"next_cursor":""}}`
	})
	if id, err := r.ChannelID(context.Background(), "deploys"); err != nil || id != "C2" {
		t.Errorf("ChannelID(deploys) = %q, %v, want C2", id, err)
	}
	if _, err := r.ChannelID(context.Background(), "missing"); err == nil {
		t.Error("ChannelID(missing) succeeded")
	}
}

func TestSlackReaderSearch(t *testing.T) {
	r := fakeSlackAPI(t, func(method string, req *http.Request) (int, string) {
		if method != "search.messages" || req.Form.Get("query") != "from:me" || req.Form.Get("sort") != "timestamp" {
			t.Errorf("unexpected call %s %v", method, req.Form)
		}
		if req.Form.Get("page") == "" { // slack-go omits the default page 1.
			return 200, `{"ok":true,"messages":{"paging":{"page":1,"pages":2},"matches":[
{"type":"message","user":"USELF","text":"top","ts":"5.000100","channel":{"id":"C5"},
 "permalink":"https://x.slack.com/archives/C5/p5000100"}]}}`
		}
		return 200, `{"ok":true,"messages":{"paging":{"page":2,"pages":2},"matches":[
{"type":"message","user":"USELF","text":"reply","ts":"6.000100","channel":{"id":"G6","is_mpim":true},
 "permalink":"https://x.slack.com/archives/G6/p6000100?thread_ts=5.000100&cid=G6"}]}}`
	})
	got, err := r.Search(context.Background(), "from:me")
	if err != nil {
		t.Fatal(err)
	}
	want := []slackwatch.Fetched{
		{Message: slackwatch.Message{Channel: "C5", TS: "5.000100", User: "USELF", Text: "top"}},
		{Message: slackwatch.Message{Channel: "G6", ChannelType: "mpim", TS: "6.000100", ThreadTS: "5.000100", User: "USELF", Text: "reply"}},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("Search = %+v, want %+v", got, want)
	}
}

func TestSlackReaderRateLimit(t *testing.T) {
	r := fakeSlackAPI(t, func(string, *http.Request) (int, string) {
		return http.StatusTooManyRequests, `{"ok":false,"error":"ratelimited"}`
	})
	_, err := r.History(context.Background(), "D1", "1.000100")
	var rl *slackwatch.RateLimitError
	if !errors.As(err, &rl) || rl.RetryAfter != 7*time.Second {
		t.Errorf("err = %v, want a rate limit of 7s", err)
	}
}
