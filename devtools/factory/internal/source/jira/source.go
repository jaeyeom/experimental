// Package jira is a Jira source.
// Sync and Get read issues over REST. Claim, Release, Finish, Link, Unlink,
// and Create write through the same HTTP client. Finish returns
// source.ErrNotSupported when finish_status is empty.
package jira

import (
	"context"
	"net/http"
	"slices"
	"sync"
	"time"
	_ "time/tzdata" // IANA zones for JQL dates, including Asia/Seoul.

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

const (
	// maxRetries is how many extra attempts follow the first 429 or 503.
	maxRetries = 3
	// maxRetryWait caps a single Retry-After wait.
	maxRetryWait  = 30 * time.Second
	errSnippetLen = 500
	pageSize      = 100

	jiraTimeLayout = "2006-01-02T15:04:05.000-0700"
	jiraDateLayout = "2006-01-02"
	jqlTimeLayout  = "2006-01-02 15:04"
	searchPath     = "/rest/api/3/search/jql"
	myselfPath     = "/rest/api/3/myself"
)

// issueFields is the field set Sync and Get request.
var issueFields = []string{
	"summary",
	"status",
	"issuetype",
	"parent",
	"labels",
	"assignee",
	"priority",
	"issuelinks",
	"created",
	"updated",
	"duedate",
}

// fields is issueFields plus the configured defer_field, when set.
func (s *Source) fields() []string {
	if s.cfg.DeferField == "" || slices.Contains(issueFields, s.cfg.DeferField) {
		return issueFields
	}
	return append(slices.Clone(issueFields), s.cfg.DeferField)
}

// idFields is the field set IDs requests. The key is always returned.
var idFields = []string{"key"}

// Source reads issues from one Jira site.
type Source struct {
	cfg  config.Source
	http *http.Client

	// sleep waits between 429 and 503 retries. Tests replace it.
	sleep func(time.Duration)

	mu     sync.Mutex
	cached bool
	info   profile
	// warned records unknown priority names already logged.
	warned map[string]struct{}
}

// profile is the cached /myself payload Sync needs for JQL dates.
type profile struct {
	accountID string
	timeZone  string
	loc       *time.Location
}

// New returns a Jira source that sends every request through hc.
// The first request calls cfg.ResolveToken when Load deferred the credential.
// Requests use basic auth cfg.Email:cfg.Token and Accept: application/json.
func New(cfg config.Source, hc *http.Client) *Source {
	return &Source{
		cfg:   cfg,
		http:  hc,
		sleep: time.Sleep,
	}
}

// Compile-time check that Source implements source.Source.
var _ source.Source = (*Source)(nil)

// Name returns the configured source name.
func (s *Source) Name() string {
	return s.cfg.Name
}

// Me returns the authenticated user's account id.
// The first successful call caches the account id and the profile time zone.
func (s *Source) Me(ctx context.Context) (string, error) {
	info, err := s.profile(ctx)
	if err != nil {
		return "", err
	}
	return info.accountID, nil
}
