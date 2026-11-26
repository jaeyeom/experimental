package jira

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func (s *Source) requireClient() error {
	if s.http == nil {
		return errors.New("jira: expected an *http.Client, found nil")
	}
	return nil
}

func (s *Source) do(ctx context.Context, method, path string, body []byte) ([]byte, error) {
	if err := s.requireClient(); err != nil {
		return nil, err
	}
	if err := s.cfg.ResolveToken(); err != nil {
		return nil, err //nolint:wrapcheck // internal factory error already carries context
	}
	var wait time.Duration
	for attempt := 0; ; attempt++ {
		if attempt > 0 {
			if err := s.pause(ctx, wait); err != nil {
				return nil, err
			}
		}
		raw, status, retryAfter, err := s.roundTrip(ctx, method, path, body)
		if err != nil {
			return nil, err
		}
		done, next, result, err := outcome(method, path, status, raw, retryAfter, attempt)
		if done {
			return result, err
		}
		wait = next
	}
}

func (s *Source) pause(ctx context.Context, d time.Duration) error {
	if err := ctx.Err(); err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	if s.sleep == nil {
		return errors.New("jira: expected a sleep function, found nil")
	}
	s.sleep(d)
	return ctx.Err() //nolint:wrapcheck // propagate context cancellation
}

func (s *Source) roundTrip(ctx context.Context, method, path string, body []byte) ([]byte, int, string, error) {
	endpoint, err := s.endpoint(path)
	if err != nil {
		return nil, 0, "", err
	}
	var reader io.Reader
	if len(body) > 0 {
		reader = bytes.NewReader(body)
	}
	req, err := http.NewRequestWithContext(ctx, method, endpoint, reader)
	if err != nil {
		return nil, 0, "", fmt.Errorf("jira %s %s: %w", method, path, err)
	}
	req.SetBasicAuth(s.cfg.Email, s.cfg.Token)
	req.Header.Set("Accept", "application/json")
	if len(body) > 0 {
		req.Header.Set("Content-Type", "application/json")
	}
	resp, err := s.http.Do(req)
	if err != nil {
		return nil, 0, "", fmt.Errorf("jira %s %s: %w", method, path, err)
	}
	defer func() {
		_ = resp.Body.Close()
	}()
	raw, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, resp.StatusCode, "", fmt.Errorf("jira %s %s: read body: %w", method, path, err)
	}
	return raw, resp.StatusCode, resp.Header.Get("Retry-After"), nil
}

func (s *Source) endpoint(path string) (string, error) {
	if strings.TrimSpace(s.cfg.Site) == "" {
		return "", errors.New("jira: expected site URL, found empty")
	}
	base, err := url.Parse(s.cfg.Site)
	if err != nil {
		return "", fmt.Errorf("jira site: expected a URL, found %q: %w", s.cfg.Site, err)
	}
	if base.Scheme == "" || base.Host == "" {
		return "", fmt.Errorf("jira site: expected an absolute URL, found %q", s.cfg.Site)
	}
	ref, err := url.Parse(path)
	if err != nil {
		return "", fmt.Errorf("jira path: expected a path, found %q: %w", path, err)
	}
	return base.ResolveReference(ref).String(), nil
}

// outcome reports whether the call is finished.
// A 429 or 503 before the retry limit returns done false and the wait.
func outcome(method, path string, status int, raw []byte, retryAfter string, attempt int) (bool, time.Duration, []byte, error) {
	if status >= 200 && status < 300 {
		return true, 0, raw, nil
	}
	if issueNotFound(method, path, status) {
		return true, 0, nil, fmt.Errorf("jira GET %s: %w", path, source.ErrNotFound)
	}
	if !retryable(status) || attempt >= maxRetries {
		return true, 0, nil, httpError(method, path, status, raw)
	}
	wait, err := retryWait(retryAfter)
	if err != nil {
		return true, 0, nil, fmt.Errorf("jira %s %s: %w", method, path, err)
	}
	return false, wait, nil, nil
}

func issueNotFound(method, path string, status int) bool {
	if method != http.MethodGet || status != http.StatusNotFound {
		return false
	}
	base, _, _ := strings.Cut(path, "?")
	return strings.Contains(base, "/rest/api/3/issue/")
}

func retryable(status int) bool {
	return status == http.StatusTooManyRequests || status == http.StatusServiceUnavailable
}

func retryWait(header string) (time.Duration, error) {
	header = strings.TrimSpace(header)
	if header == "" {
		return 0, errors.New("expected Retry-After seconds, found none")
	}
	seconds, err := strconv.Atoi(header)
	if err != nil {
		return 0, fmt.Errorf("expected Retry-After seconds, found %q", header)
	}
	if seconds < 0 {
		return 0, fmt.Errorf("expected Retry-After seconds >= 0, found %d", seconds)
	}
	wait := time.Duration(seconds) * time.Second
	if wait > maxRetryWait {
		return maxRetryWait, nil
	}
	return wait, nil
}

// statusError is a non-2xx Jira response.
type statusError struct {
	method string
	path   string
	status int
	body   []byte
}

func (e *statusError) Error() string {
	return fmt.Sprintf("jira %s %s: %d: %s", e.method, e.path, e.status, clip(e.body, errSnippetLen))
}

func httpError(method, path string, status int, raw []byte) error {
	return &statusError{method: method, path: path, status: status, body: append([]byte(nil), raw...)}
}

func clip(body []byte, n int) string {
	if len(body) > n {
		body = body[:n]
	}
	return string(body)
}
