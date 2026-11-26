package jira

import (
	"bytes"
	"context"
	"embed"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"net/http"
	"net/http/httptest"
	"net/url"
	"os"
	"path"
	"path/filepath"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

//go:embed testdata
var fixtures embed.FS

const (
	testEmail   = "dev@example.com"
	testToken   = "secret-token"
	testJQL     = "project = PRJ AND statusCategory != Done"
	testAccount = "5b10ac8d82e05b22cc7d4ef5"
)

func TestSyncPaginatesFullJQL(t *testing.T) {
	fake := startJira(t, map[string][]response{
		http.MethodPost + " " + searchPath: {
			okFixture(t, "search_page1.json"),
			okFixture(t, "search_page2.json"),
		},
	})
	src, clk := fake.open(testJQL)

	got, err := src.Sync(context.Background(), time.Time{})
	if err != nil {
		t.Fatalf("Sync() error = %v", err)
	}
	recs := fake.requests()
	assertAuth(t, recs)
	if len(clk.durations()) != 0 {
		t.Fatalf("sleeps = %v, want none", clk.durations())
	}
	if len(recs) != 2 {
		t.Fatalf("requests = %d, want 2", len(recs))
	}
	for _, rec := range recs {
		if rec.path == "/rest/api/3/search" {
			t.Fatalf("called deprecated search: %s", rec.path)
		}
		if rec.path != searchPath {
			t.Fatalf("path = %s, want %s", rec.path, searchPath)
		}
	}

	wantJQL := "(" + testJQL + ") ORDER BY updated ASC"
	wantFields := []string{
		"summary", "status", "issuetype", "parent", "labels",
		"assignee", "priority", "issuelinks", "created", "updated", "duedate",
	}
	first := decodePost(t, recs[0])
	second := decodePost(t, recs[1])
	if first.JQL != wantJQL || second.JQL != wantJQL {
		t.Fatalf("JQL = %q and %q, want %q", first.JQL, second.JQL, wantJQL)
	}
	if strings.Contains(first.JQL, "updated >=") || strings.Contains(second.JQL, "updated >=") {
		t.Fatalf("full sync JQL contains an updated clause: %q", first.JQL)
	}
	if first.NextPageToken != nil {
		t.Fatalf("first nextPageToken = %q, want omitted", *first.NextPageToken)
	}
	if bytes.Contains(recs[0].body, []byte("nextPageToken")) {
		t.Fatalf("first body includes nextPageToken: %s", recs[0].body)
	}
	if second.NextPageToken == nil || *second.NextPageToken != "page-2" {
		t.Fatalf("second nextPageToken = %v, want page-2", second.NextPageToken)
	}
	if first.MaxResults != pageSize || second.MaxResults != pageSize {
		t.Fatalf("maxResults = %d and %d, want %d", first.MaxResults, second.MaxResults, pageSize)
	}
	if !reflect.DeepEqual(first.Fields, wantFields) || !reflect.DeepEqual(second.Fields, wantFields) {
		t.Fatalf("fields = %#v and %#v", first.Fields, second.Fields)
	}

	want := []source.Item{
		{
			Source:    "jira:acme",
			ID:        "PRJ-10",
			Title:     "Ship the reader",
			URL:       fake.srv.URL + "/browse/PRJ-10",
			Type:      "Task",
			State:     "To Do",
			Parent:    "PRJ-1",
			Labels:    []string{"agent-ready", "backend"},
			Assignee:  testAccount,
			Priority:  1,
			BlockedBy: []source.BlockRef{{ID: "PRJ-9", LinkID: "10001"}},
			Created:   mustTime(t, "2026-01-02T03:04:05.000-0700"),
			Updated:   mustTime(t, "2026-01-03T04:05:06.000-0700"),
		},
		{
			Source:   "jira:acme",
			ID:       "PRJ-20",
			Title:    "Already finished",
			URL:      fake.srv.URL + "/browse/PRJ-20",
			Type:     "Bug",
			State:    "Done",
			Done:     true,
			Priority: 2,
			Created:  mustTime(t, "2026-02-02T03:04:05.000-0700"),
			Updated:  mustTime(t, "2026-02-03T04:05:06.000-0700"),
		},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Sync() = %#v, want %#v", got, want)
	}

	t.Run("link direction", func(t *testing.T) {
		if len(got[0].BlockedBy) != 1 || got[0].BlockedBy[0] != want[0].BlockedBy[0] {
			t.Fatalf("BlockedBy = %+v, want only inward PRJ-9", got[0].BlockedBy)
		}
	})
	t.Run("done status category", func(t *testing.T) {
		if got[0].Done {
			t.Fatal("PRJ-10 Done = true, want false")
		}
		if !got[1].Done || got[1].State != "Done" {
			t.Fatalf("PRJ-20 = done:%v state:%q", got[1].Done, got[1].State)
		}
	})
}

func TestSyncIncrementalJQLInSeoul(t *testing.T) {
	page := fixture(t, "search_one.json")
	fake := startJira(t, map[string][]response{
		http.MethodGet + " " + myselfPath: {okFixture(t, "myself.json")},
		http.MethodPost + " " + searchPath: {
			{status: http.StatusOK, body: page},
			{status: http.StatusOK, body: page},
		},
	})
	src, _ := fake.open(testJQL)
	since := time.Date(2026, 1, 15, 0, 1, 0, 0, time.UTC)
	wantJQL := `(` + testJQL + `) AND updated >= "2026-01-15 08:59" ORDER BY updated ASC`

	if _, err := src.Sync(context.Background(), since); err != nil {
		t.Fatalf("Sync() error = %v", err)
	}
	if _, err := src.Sync(context.Background(), since); err != nil {
		t.Fatalf("Sync() again error = %v", err)
	}
	account, err := src.Me(context.Background())
	if err != nil {
		t.Fatalf("Me() error = %v", err)
	}
	if account != testAccount {
		t.Fatalf("Me() = %q, want %q", account, testAccount)
	}

	recs := fake.requests()
	if len(recs) != 3 {
		t.Fatalf("requests = %d, want myself plus two searches: %#v", len(recs), recs)
	}
	if recs[0].path != myselfPath {
		t.Fatalf("first path = %s, want %s", recs[0].path, myselfPath)
	}
	for _, rec := range recs[1:] {
		got := decodePost(t, rec)
		if got.JQL != wantJQL {
			t.Fatalf("JQL = %q, want %q", got.JQL, wantJQL)
		}
	}
	myselfs := 0
	for _, rec := range recs {
		if rec.path == myselfPath {
			myselfs++
		}
	}
	if myselfs != 1 {
		t.Fatalf("myself calls = %d, want 1 cached profile", myselfs)
	}
}

func TestPriorityMapping(t *testing.T) {
	var buf bytes.Buffer
	prev := slog.Default()
	slog.SetDefault(slog.New(slog.NewTextHandler(&buf, nil)))
	t.Cleanup(func() { slog.SetDefault(prev) })

	fake := startJira(t, map[string][]response{
		http.MethodPost + " " + searchPath: {okFixture(t, "search_priorities.json")},
	})
	src, _ := fake.open(testJQL)
	got, err := src.Sync(context.Background(), time.Time{})
	if err != nil {
		t.Fatalf("Sync() error = %v", err)
	}
	want := []struct {
		id       string
		priority int
	}{
		{"PRJ-P0", 0},
		{"PRJ-P1", 1},
		{"PRJ-P2", 2},
		{"PRJ-P3", 3},
		{"PRJ-P4", 4},
		{"PRJ-PNONE", 4},
		{"PRJ-PUNK", 2},
	}
	if len(got) != len(want) {
		t.Fatalf("len = %d, want %d", len(got), len(want))
	}
	for i, item := range want {
		if got[i].ID != item.id || got[i].Priority != item.priority {
			t.Fatalf("item %d = %s priority %d, want %s priority %d", i, got[i].ID, got[i].Priority, item.id, item.priority)
		}
	}
	logText := buf.String()
	if strings.Count(logText, "unknown jira priority") != 1 {
		t.Fatalf("warn count in %q", logText)
	}
	if !strings.Contains(logText, "PRJ-PUNK") || !strings.Contains(logText, "Critical") {
		t.Fatalf("log = %q, want the issue and the priority", logText)
	}
	if strings.Contains(logText, "PRJ-PNONE") {
		t.Fatalf("missing priority was warned: %s", logText)
	}
}

func TestPriorityRanksOverride(t *testing.T) {
	buf := captureWarns(t)
	src := prioritySource(map[string]int{"Highest": 9}, nil)
	mustPriority(t, src, "PRJ-1", &jiraNamed{Name: "Highest"}, 9)
	mustPriority(t, src, "PRJ-2", &jiraNamed{Name: "High"}, 1)
	mustPriority(t, src, "PRJ-3", &jiraNamed{Name: "Low"}, 3)
	if buf.Len() != 0 {
		t.Fatalf("log = %q, want none", buf.String())
	}
}

func TestKnownPriorityDoesNotWarn(t *testing.T) {
	buf := captureWarns(t)
	src := prioritySource(map[string]int{"Untriaged": 8}, []string{"Untriaged", "Queued"})
	mustPriority(t, src, "PRJ-1", &jiraNamed{Name: "Untriaged"}, 8)
	mustPriority(t, src, "PRJ-2", &jiraNamed{Name: "Queued"}, 2)
	mustPriority(t, src, "PRJ-3", nil, 4)
	mustPriority(t, src, "PRJ-4", &jiraNamed{Name: ""}, 4)
	mustPriority(t, src, "PRJ-5", &jiraNamed{Name: "Medium"}, 2)
	if buf.Len() != 0 {
		t.Fatalf("log = %q, want none", buf.String())
	}
}

func TestUnknownPriorityWarnsOncePerValue(t *testing.T) {
	buf := captureWarns(t)
	src := prioritySource(nil, nil)
	mustPriority(t, src, "PRJ-1", &jiraNamed{Name: "Odd"}, 2)
	mustPriority(t, src, "PRJ-2", &jiraNamed{Name: "Odd"}, 2)
	if strings.Count(buf.String(), "unknown jira priority") != 1 {
		t.Fatalf("warn count in %q", buf.String())
	}
	if !strings.Contains(buf.String(), "issue=PRJ-1") || !strings.Contains(buf.String(), "priority=Odd") {
		t.Fatalf("log = %q, want the first issue and the priority", buf.String())
	}
	mustPriority(t, src, "PRJ-3", &jiraNamed{Name: "Strange"}, 2)
	if strings.Count(buf.String(), "unknown jira priority") != 2 || !strings.Contains(buf.String(), "priority=Strange") {
		t.Fatalf("log = %q, want one warning per unknown name", buf.String())
	}
	other := prioritySource(nil, nil)
	mustPriority(t, other, "PRJ-4", &jiraNamed{Name: "Odd"}, 2)
	if strings.Count(buf.String(), "priority=Odd") != 2 {
		t.Fatalf("log = %q, want the second source to warn once", buf.String())
	}
}

func TestMissingStatus(t *testing.T) {
	err := getIssue(t, "PRJ-30", "issue_missing_status.json")
	if err == nil || !strings.Contains(err.Error(), "PRJ-30") || !strings.Contains(err.Error(), "status") {
		t.Fatalf("Get() error = %v, want PRJ-30 status missing", err)
	}
}

func TestMissingIssueType(t *testing.T) {
	err := getIssue(t, "PRJ-31", "issue_missing_issuetype.json")
	if err == nil || !strings.Contains(err.Error(), "PRJ-31") || !strings.Contains(err.Error(), "issuetype") {
		t.Fatalf("Get() error = %v, want PRJ-31 issuetype missing", err)
	}
}

func TestMissingKey(t *testing.T) {
	fake := startJira(t, map[string][]response{
		http.MethodPost + " " + searchPath: {okFixture(t, "search_missing_key.json")},
	})
	src, _ := fake.open(testJQL)
	_, err := src.Sync(context.Background(), time.Time{})
	if err == nil || !strings.Contains(err.Error(), "expected key, found empty") {
		t.Fatalf("Sync() error = %v", err)
	}
}

func TestBadCreatedTime(t *testing.T) {
	err := getIssue(t, "PRJ-33", "issue_bad_time.json")
	if err == nil || !strings.Contains(err.Error(), "PRJ-33") || !strings.Contains(err.Error(), "created") || !strings.Contains(err.Error(), "yesterday") {
		t.Fatalf("Get() error = %v", err)
	}
}

func TestSearchRequiresIssues(t *testing.T) {
	fake := startJira(t, map[string][]response{
		http.MethodPost + " " + searchPath: {okFixture(t, "search_no_issues.json")},
	})
	src, _ := fake.open(testJQL)
	_, err := src.Sync(context.Background(), time.Time{})
	if err == nil || !strings.Contains(err.Error(), "expected issues") {
		t.Fatalf("Sync() error = %v", err)
	}
}

func TestGetRetryThenSuccess(t *testing.T) {
	tests := []struct {
		name       string
		status     int
		retryAfter string
		wantWait   time.Duration
	}{
		{name: "429 capped at 30s", status: http.StatusTooManyRequests, retryAfter: "45", wantWait: 30 * time.Second},
		{name: "503 honors Retry-After", status: http.StatusServiceUnavailable, retryAfter: "10", wantWait: 10 * time.Second},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			key := http.MethodGet + " /rest/api/3/issue/PRJ-40"
			fake := startJira(t, map[string][]response{
				key: {
					{status: tc.status, body: []byte("slow down"), retryAfter: tc.retryAfter},
					okFixture(t, "issue_ok.json"),
				},
			})
			src, clk := fake.open(testJQL)
			got, err := src.Get(context.Background(), "PRJ-40")
			if err != nil {
				t.Fatalf("Get() error = %v", err)
			}
			if got.ID != "PRJ-40" || got.Priority != 3 || got.Done {
				t.Fatalf("Get() = %+v", got)
			}
			if waits := clk.durations(); !reflect.DeepEqual(waits, []time.Duration{tc.wantWait}) {
				t.Fatalf("sleeps = %v, want %v", waits, []time.Duration{tc.wantWait})
			}
			recs := fake.requests()
			if len(recs) != 2 {
				t.Fatalf("requests = %d, want 2", len(recs))
			}
			query, err := url.ParseQuery(recs[0].rawQuery)
			if err != nil {
				t.Fatalf("query: %v", err)
			}
			if query.Get("fields") != strings.Join(issueFields, ",") {
				t.Fatalf("fields = %q", query.Get("fields"))
			}
		})
	}
}

func TestGetRetriesExhausted(t *testing.T) {
	body := append([]byte("START"), bytes.Repeat([]byte("x"), 600)...)
	body = append(body, []byte("END")...)
	retries := make([]response, 4)
	for i := range retries {
		retries[i] = response{status: http.StatusTooManyRequests, body: body, retryAfter: "1"}
	}
	fake := startJira(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-40": retries,
	})
	src, clk := fake.open(testJQL)
	_, err := src.Get(context.Background(), "PRJ-40")
	if err == nil {
		t.Fatal("Get() error = nil, want 429")
	}
	text := err.Error()
	marker := "429: "
	idx := strings.Index(text, marker)
	if idx < 0 || !strings.Contains(text, "START") || strings.Contains(text, "END") {
		t.Fatalf("Get() error = %q", text)
	}
	if got := text[idx+len(marker):]; len(got) != errSnippetLen {
		t.Fatalf("snippet length = %d, want %d", len(got), errSnippetLen)
	}
	wantWaits := []time.Duration{time.Second, time.Second, time.Second}
	if waits := clk.durations(); !reflect.DeepEqual(waits, wantWaits) {
		t.Fatalf("sleeps = %v, want %v", waits, wantWaits)
	}
	if len(fake.requests()) != 4 {
		t.Fatalf("requests = %d, want 4", len(fake.requests()))
	}
}

func TestRetryAfterNotSeconds(t *testing.T) {
	tests := []struct {
		name       string
		retryAfter string
		want       string
	}{
		{name: "missing", want: "expected Retry-After seconds, found none"},
		{name: "not a number", retryAfter: "Wednesday", want: `expected Retry-After seconds, found "Wednesday"`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			fake := startJira(t, map[string][]response{
				http.MethodGet + " /rest/api/3/issue/PRJ-40": {
					{status: http.StatusTooManyRequests, body: []byte("slow"), retryAfter: tc.retryAfter},
				},
			})
			src, clk := fake.open(testJQL)
			_, err := src.Get(context.Background(), "PRJ-40")
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Get() error = %v, want %s", err, tc.want)
			}
			if waits := clk.durations(); len(waits) != 0 {
				t.Fatalf("sleeps = %v, want none", waits)
			}
		})
	}
}

func TestGetHTTPError(t *testing.T) {
	fake := startJira(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-40": {
			{status: http.StatusInternalServerError, body: []byte("nope")},
		},
	})
	src, clk := fake.open(testJQL)
	_, err := src.Get(context.Background(), "PRJ-40")
	if err == nil || !strings.Contains(err.Error(), "500: nope") {
		t.Fatalf("Get() error = %v", err)
	}
	if waits := clk.durations(); len(waits) != 0 {
		t.Fatalf("sleeps = %v, want none", waits)
	}
	if len(fake.requests()) != 1 {
		t.Fatalf("requests = %d, want 1", len(fake.requests()))
	}
}

func TestGetNotFound(t *testing.T) {
	fake := startJira(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/PRJ-404": {
			{status: http.StatusNotFound, body: []byte(`{"errorMessages":["Issue does not exist or you do not have permission to see it."],"errors":{}}`)},
		},
	})
	src, clk := fake.open(testJQL)
	_, err := src.Get(context.Background(), "PRJ-404")
	if !errors.Is(err, source.ErrNotFound) {
		t.Fatalf("Get() error = %v, want ErrNotFound", err)
	}
	if waits := clk.durations(); len(waits) != 0 {
		t.Fatalf("sleeps = %v", waits)
	}
}

func TestGetEmptyKey(t *testing.T) {
	src := New(config.Source{Name: "jira:acme"}, nil)
	_, err := src.Get(context.Background(), "")
	if err == nil || !strings.Contains(err.Error(), "expected an issue key, found empty") {
		t.Fatalf("Get() error = %v", err)
	}
}

func TestNilHTTPClient(t *testing.T) {
	src := New(config.Source{Name: "jira:acme", Site: "https://example.atlassian.net"}, nil)
	_, err := src.Me(context.Background())
	if err == nil || !strings.Contains(err.Error(), "expected an *http.Client, found nil") {
		t.Fatalf("Me() error = %v", err)
	}
}

func TestMeCanceled(t *testing.T) {
	fake := startJira(t, map[string][]response{
		http.MethodGet + " " + myselfPath: {okFixture(t, "myself.json")},
	})
	src, _ := fake.open(testJQL)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	_, err := src.Me(ctx)
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Me() error = %v, want canceled", err)
	}
}

func getIssue(t *testing.T, key, fixtureName string) error {
	t.Helper()
	fake := startJira(t, map[string][]response{
		http.MethodGet + " /rest/api/3/issue/" + key: {okFixture(t, fixtureName)},
	})
	src, _ := fake.open(testJQL)
	_, err := src.Get(context.Background(), key)
	return err
}

func mustTime(t *testing.T, value string) time.Time {
	t.Helper()
	parsed, err := time.Parse(jiraTimeLayout, value)
	if err != nil {
		t.Fatal(err)
	}
	return parsed
}

func okFixture(t *testing.T, name string) response {
	t.Helper()
	return response{status: http.StatusOK, body: fixture(t, name)}
}

func fixture(t *testing.T, name string) []byte {
	t.Helper()
	data, err := fixtures.ReadFile(path.Join("testdata", name))
	if err != nil {
		t.Fatalf("read fixture %s: %v", name, err)
	}
	return data
}

type response struct {
	status     int
	body       []byte
	retryAfter string
}

type recorded struct {
	method      string
	path        string
	rawQuery    string
	body        []byte
	user        string
	pass        string
	accept      string
	contentType string
}

type posted struct {
	JQL           string   `json:"jql"`
	Fields        []string `json:"fields"`
	MaxResults    int      `json:"maxResults"`
	NextPageToken *string  `json:"nextPageToken"`
}

type jiraFake struct {
	t      *testing.T
	mu     sync.Mutex
	queues map[string][]response
	reqs   []recorded
	srv    *httptest.Server
	// fallback handles a request the queue does not list.
	// A nil fallback reports that request as unexpected.
	fallback func(recorded) (response, bool)
}

func startJira(t *testing.T, queues map[string][]response) *jiraFake {
	t.Helper()
	fake := &jiraFake{t: t, queues: queues}
	fake.srv = httptest.NewServer(fake)
	t.Cleanup(fake.srv.Close)
	return fake
}

func (f *jiraFake) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	body, err := io.ReadAll(r.Body)
	if err != nil {
		f.t.Errorf("read request: %v", err)
		http.Error(w, "read", http.StatusInternalServerError)
		return
	}
	user, pass, _ := r.BasicAuth()
	rec := recorded{
		method:      r.Method,
		path:        r.URL.Path,
		rawQuery:    r.URL.RawQuery,
		body:        body,
		user:        user,
		pass:        pass,
		accept:      r.Header.Get("Accept"),
		contentType: r.Header.Get("Content-Type"),
	}
	key := r.Method + " " + r.URL.Path
	resp, ok := f.pop(key, rec)
	if !ok && f.fallback != nil {
		resp, ok = f.fallback(rec)
	}
	if !ok {
		f.t.Errorf("unexpected %s", key)
		http.Error(w, "unexpected "+key, http.StatusInternalServerError)
		return
	}
	if resp.retryAfter != "" {
		w.Header().Set("Retry-After", resp.retryAfter)
	}
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(resp.status)
	if _, err := w.Write(resp.body); err != nil {
		f.t.Errorf("write response: %v", err)
	}
}

func (f *jiraFake) pop(key string, rec recorded) (response, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.reqs = append(f.reqs, rec)
	queue := f.queues[key]
	if len(queue) == 0 {
		return response{}, false
	}
	head := queue[0]
	f.queues[key] = queue[1:]
	return head, true
}

func (f *jiraFake) requests() []recorded {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]recorded, len(f.reqs))
	copy(out, f.reqs)
	return out
}

func (f *jiraFake) open(jql string) (*Source, *clock) {
	clk := &clock{}
	src := New(config.Source{
		Name:           "jira:acme",
		Site:           f.srv.URL,
		Email:          testEmail,
		Token:          testToken,
		JQL:            jql,
		BlocksLinkType: "Blocks",
	}, f.srv.Client())
	src.sleep = clk.sleep
	return src, clk
}

type clock struct {
	mu    sync.Mutex
	waits []time.Duration
}

func (c *clock) sleep(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.waits = append(c.waits, d)
}

func (c *clock) durations() []time.Duration {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]time.Duration, len(c.waits))
	copy(out, c.waits)
	return out
}

func assertAuth(t *testing.T, recs []recorded) {
	t.Helper()
	if len(recs) == 0 {
		t.Fatal("no requests")
	}
	for i, rec := range recs {
		if rec.user != testEmail || rec.pass != testToken {
			t.Fatalf("request %d auth = %s:%s", i, rec.user, rec.pass)
		}
		if rec.accept != "application/json" {
			t.Fatalf("request %d Accept = %q", i, rec.accept)
		}
	}
}

func decodePost(t *testing.T, rec recorded) posted {
	t.Helper()
	if rec.method != http.MethodPost {
		t.Fatalf("method = %s, want POST", rec.method)
	}
	if rec.contentType != "application/json" {
		t.Fatalf("Content-Type = %q", rec.contentType)
	}
	var body posted
	if err := json.Unmarshal(rec.body, &body); err != nil {
		t.Fatalf("decode body: %v", err)
	}
	return body
}

func TestSyncReadsTokenFileOnFirstRequest(t *testing.T) {
	dir := t.TempDir()
	tokenPath := filepath.Join(dir, "token")
	if err := os.WriteFile(tokenPath, []byte("  from-file \n"), 0o600); err != nil {
		t.Fatal(err)
	}
	cfg := loadJiraConfig(t, tokenPath)
	if cfg.Token != "" {
		t.Fatalf("Load read the token, Token = %q", cfg.Token)
	}
	var hits int
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		hits++
		user, pass, ok := r.BasicAuth()
		if !ok || user != "dev@example.com" || pass != "from-file" {
			t.Errorf("auth = %s:%s ok=%v", user, pass, ok)
		}
		w.Header().Set("Content-Type", "application/json")
		if _, err := io.WriteString(w, `{"issues":[],"isLast":true}`); err != nil {
			t.Errorf("write: %v", err)
		}
	}))
	t.Cleanup(srv.Close)
	cfg.Site = srv.URL
	if _, err := New(cfg, srv.Client()).Sync(context.Background(), time.Time{}); err != nil {
		t.Fatalf("Sync: %v", err)
	}
	if hits != 1 {
		t.Fatalf("requests = %d, want 1", hits)
	}
}

func TestSyncMissingTokenFileDoesNotCallHTTP(t *testing.T) {
	tokenPath := filepath.Join(t.TempDir(), "missing-token")
	cfg := loadJiraConfig(t, tokenPath)
	hits := 0
	srv := httptest.NewServer(http.HandlerFunc(func(http.ResponseWriter, *http.Request) {
		hits++
	}))
	t.Cleanup(srv.Close)
	cfg.Site = srv.URL
	_, err := New(cfg, srv.Client()).Sync(context.Background(), time.Time{})
	if err == nil {
		t.Fatal("Sync error = nil, want missing token file")
	}
	for _, part := range []string{`source "acme"`, `field "token_file"`, tokenPath} {
		if !strings.Contains(err.Error(), part) {
			t.Fatalf("Sync error = %q, want substring %q", err.Error(), part)
		}
	}
	if hits != 0 {
		t.Fatalf("requests = %d, want 0", hits)
	}
}

func loadJiraConfig(t *testing.T, tokenPath string) config.Source {
	t.Helper()
	dir := t.TempDir()
	cwd := t.TempDir()
	body := fmt.Sprintf(`
[[source]]
name = "acme"
kind = "jira"
site = "https://example.atlassian.net"
email = "dev@example.com"
token_file = %q
jql = "project = PRJ"
claim_status = "In Progress"
cwd = %q
base = "main"
start_prompt = "work {id}"
`, tokenPath, cwd)
	cfgPath := filepath.Join(dir, "factory.toml")
	if err := os.WriteFile(cfgPath, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	cfg, err := config.Load(cfgPath, config.OSEnv())
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if len(cfg.Sources) != 1 {
		t.Fatalf("sources = %d, want 1", len(cfg.Sources))
	}
	return cfg.Sources[0]
}

func prioritySource(ranks map[string]int, known []string) *Source {
	return New(config.Source{
		Name:            "acme",
		BlocksLinkType:  "Blocks",
		PriorityRanks:   ranks,
		KnownPriorities: known,
	}, nil)
}

func priorityIssue(key string, priority *jiraNamed) jiraIssue {
	return jiraIssue{
		Key: key,
		Fields: jiraFields{
			Summary: "Ship it",
			Status: &jiraStatus{
				Name:           "To Do",
				StatusCategory: &jiraCategory{Key: "new"},
			},
			IssueType: &jiraNamed{Name: "Task"},
			Priority:  priority,
			Created:   "2026-01-02T03:04:05.000-0700",
			Updated:   "2026-01-03T03:04:05.000-0700",
		},
	}
}

func mustPriority(t *testing.T, src *Source, key string, priority *jiraNamed, want int) {
	t.Helper()
	got, err := src.mapIssue(priorityIssue(key, priority))
	if err != nil {
		t.Fatalf("mapIssue(%s) error = %v", key, err)
	}
	if got.Priority != want {
		t.Fatalf("%s priority = %d, want %d", key, got.Priority, want)
	}
}

func TestMapDueDate(t *testing.T) {
	src := prioritySource(nil, nil)
	tests := []struct {
		name    string
		raw     string
		want    string
		wantErr string
	}{
		{name: "absent"},
		{name: "null", raw: `null`},
		{name: "empty", raw: `""`},
		{name: "date", raw: `"2026-11-07"`, want: "2026-11-07"},
		{name: "datetime", raw: `"2026-11-07T00:00:00.000-0700"`, wantErr: `expected YYYY-MM-DD, found "2026-11-07T00:00:00.000-0700"`},
		{name: "number", raw: `1`, wantErr: "expected YYYY-MM-DD or null, found"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			issue := priorityIssue("PRJ-1", &jiraNamed{Name: "Medium"})
			if tc.raw != "" {
				issue.Fields.DueDate = json.RawMessage(tc.raw)
			}
			got, err := src.mapIssue(issue)
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("mapIssue() error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("mapIssue() error = %v", err)
			}
			if tc.want == "" {
				if got.Due != nil {
					t.Fatalf("Due = %s, want nil", got.Due.Format("2006-01-02"))
				}
				return
			}
			if got.Due == nil || got.Due.Format("2006-01-02") != tc.want || got.Due.Location() != time.UTC {
				t.Fatalf("Due = %v, want %s UTC", got.Due, tc.want)
			}
		})
	}
}

func captureWarns(t *testing.T) *bytes.Buffer {
	t.Helper()
	var buf bytes.Buffer
	prev := slog.Default()
	slog.SetDefault(slog.New(slog.NewTextHandler(&buf, nil)))
	t.Cleanup(func() { slog.SetDefault(prev) })
	return &buf
}
