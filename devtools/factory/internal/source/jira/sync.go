package jira

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"net/url"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

// Sync returns issues updated at or after since, oldest first.
// The zero time returns every issue the configured JQL matches.
// An incremental sync shifts since back two minutes and formats that instant
// in the profile time zone, because Jira reads JQL dates in that zone.
func (s *Source) Sync(ctx context.Context, since time.Time) ([]source.Item, error) {
	jql, err := s.jql(ctx, since)
	if err != nil {
		return nil, err
	}
	return s.searchAll(ctx, jql)
}

// Get reads one issue by key. A 404 returns source.ErrNotFound.
func (s *Source) Get(ctx context.Context, key string) (source.Item, error) {
	if key == "" {
		return source.Item{}, errors.New("jira get: expected an issue key, found empty")
	}
	path := "/rest/api/3/issue/" + url.PathEscape(key) + "?fields=" + strings.Join(s.fields(), ",")
	raw, err := s.do(ctx, http.MethodGet, path, nil)
	if err != nil {
		return source.Item{}, err
	}
	issue, err := decodeIssue(raw)
	if err != nil {
		return source.Item{}, err
	}
	return s.mapIssue(issue)
}

func (s *Source) jql(ctx context.Context, since time.Time) (string, error) {
	if since.IsZero() {
		return "(" + s.cfg.JQL + ") ORDER BY updated ASC", nil
	}
	info, err := s.profile(ctx)
	if err != nil {
		return "", err
	}
	stamp := since.Add(-2 * time.Minute).In(info.loc).Format(jqlTimeLayout)
	return "(" + s.cfg.JQL + ") AND updated >= \"" + stamp + "\" ORDER BY updated ASC", nil
}

func (s *Source) searchAll(ctx context.Context, jql string) ([]source.Item, error) {
	items := []source.Item{}
	token := ""
	for {
		page, err := s.searchPage(ctx, jql, s.fields(), token)
		if err != nil {
			return nil, err
		}
		mapped, err := s.mapIssues(page.Issues)
		if err != nil {
			return nil, err
		}
		items = append(items, mapped...)
		if page.IsLast || page.NextPageToken == "" {
			return items, nil
		}
		token = page.NextPageToken
	}
}

// IDs returns the key of every issue the configured JQL matches now.
// It sends the same JQL as a full Sync and requests only the key field.
func (s *Source) IDs(ctx context.Context) ([]string, error) {
	jql, err := s.jql(ctx, time.Time{})
	if err != nil {
		return nil, err
	}
	keys := []string{}
	seen := map[string]struct{}{}
	token := ""
	for {
		page, err := s.searchPage(ctx, jql, idFields, token)
		if err != nil {
			return nil, err
		}
		for i := range page.Issues {
			key := page.Issues[i].Key
			if key == "" {
				return nil, fmt.Errorf("jira ids: expected an issue key, found empty at position %d", len(keys))
			}
			if _, ok := seen[key]; ok {
				continue
			}
			seen[key] = struct{}{}
			keys = append(keys, key)
		}
		if page.IsLast || page.NextPageToken == "" {
			return keys, nil
		}
		token = page.NextPageToken
	}
}

func (s *Source) searchPage(ctx context.Context, jql string, fields []string, token string) (searchPage, error) {
	body, err := json.Marshal(searchBody{
		JQL:           jql,
		Fields:        fields,
		MaxResults:    pageSize,
		NextPageToken: token,
	})
	if err != nil {
		return searchPage{}, fmt.Errorf("jira search: encode request: %w", err)
	}
	raw, err := s.do(ctx, http.MethodPost, searchPath, body)
	if err != nil {
		return searchPage{}, err
	}
	return decodeSearch(raw)
}

func (s *Source) profile(ctx context.Context) (profile, error) {
	s.mu.Lock()
	if s.cached {
		info := s.info
		s.mu.Unlock()
		return info, nil
	}
	s.mu.Unlock()

	info, err := s.loadProfile(ctx)
	if err != nil {
		return profile{}, err
	}

	s.mu.Lock()
	defer s.mu.Unlock()
	if !s.cached {
		s.info = info
		s.cached = true
	}
	return s.info, nil
}

func (s *Source) loadProfile(ctx context.Context) (profile, error) {
	raw, err := s.do(ctx, http.MethodGet, myselfPath, nil)
	if err != nil {
		return profile{}, err
	}
	body, err := decodeMyself(raw)
	if err != nil {
		return profile{}, err
	}
	if body.AccountID == "" {
		return profile{}, errors.New("jira myself: expected accountId, found empty")
	}
	if body.TimeZone == "" {
		return profile{}, errors.New("jira myself: expected timeZone, found empty")
	}
	loc, err := time.LoadLocation(body.TimeZone)
	if err != nil {
		return profile{}, fmt.Errorf("jira myself field \"timeZone\": expected an IANA name, found %q: %w", body.TimeZone, err)
	}
	return profile{accountID: body.AccountID, timeZone: body.TimeZone, loc: loc}, nil
}
