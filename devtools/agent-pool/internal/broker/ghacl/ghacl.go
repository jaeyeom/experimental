// Package ghacl verifies GitHub repository pull access for a requester-supplied
// GitHub token with positive-only, SHA-256-keyed in-memory caching.
package ghacl

import (
	"context"
	"crypto/sha256"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"regexp"
	"strings"
	"sync"
	"time"
)

const (
	defaultAPIBaseURL = "https://api.github.com"
	defaultCacheTTL   = 5 * time.Minute
	defaultTimeout    = 10 * time.Second
	githubAPIVersion  = "2022-11-28"
	userAgent         = "agent-poold"
	maxResponseBytes  = 1 << 20 // 1 MiB.
)

var (
	// ErrMissingGitHubToken indicates no GitHub token was supplied by the requester.
	ErrMissingGitHubToken = errors.New("missing GitHub token")
	// ErrInvalidGitHubToken indicates GitHub rejected the token with 401 Unauthorized.
	ErrInvalidGitHubToken = errors.New("invalid GitHub token")
	// ErrAccessDenied indicates the GitHub token lacks pull access to the repository.
	ErrAccessDenied = errors.New("GitHub repository access denied")
	// ErrInvalidRepo indicates the repository name does not match owner/repo format.
	ErrInvalidRepo = errors.New("invalid repository name")

	reRepo = regexp.MustCompile(`^[A-Za-z0-9._-]+/[A-Za-z0-9._-]+$`)
)

// Verifier checks whether a requester-supplied GitHub token has pull access to
// a target repository.
type Verifier interface {
	VerifyPullAccess(ctx context.Context, githubToken string, repo string) error
}

// Config configures a GitHubVerifier.
type Config struct {
	APIBaseURL string           // Defaults to "https://api.github.com" (injectable for httptest).
	CacheTTL   time.Duration    // Defaults to 5m; positive cache only.
	Timeout    time.Duration    // Defaults to 10s.
	Now        func() time.Time // Optional clock; defaults to time.Now.
}

// GitHubVerifier verifies repository pull permissions against the GitHub REST
// API and caches positive results keyed by SHA-256(token + "\x00" + repo).
type GitHubVerifier struct {
	apiBaseURL string
	cacheTTL   time.Duration
	timeout    time.Duration
	client     *http.Client
	now        func() time.Time

	mu    sync.Mutex
	cache map[[sha256.Size]byte]time.Time
}

var _ Verifier = (*GitHubVerifier)(nil)

// NewGitHubVerifier constructs a GitHubVerifier with defaults applied for
// zero-value config fields.
func NewGitHubVerifier(cfg Config, httpClient *http.Client) *GitHubVerifier {
	baseURL := strings.TrimRight(strings.TrimSpace(cfg.APIBaseURL), "/")
	if baseURL == "" {
		baseURL = defaultAPIBaseURL
	}
	cacheTTL := cfg.CacheTTL
	if cacheTTL <= 0 {
		cacheTTL = defaultCacheTTL
	}
	timeout := cfg.Timeout
	if timeout <= 0 {
		timeout = defaultTimeout
	}
	nowFn := cfg.Now
	if nowFn == nil {
		nowFn = time.Now
	}
	client := httpClient
	if client == nil {
		client = &http.Client{Timeout: timeout}
	}
	return &GitHubVerifier{
		apiBaseURL: baseURL,
		cacheTTL:   cacheTTL,
		timeout:    timeout,
		client:     client,
		now:        nowFn,
		cache:      make(map[[sha256.Size]byte]time.Time),
	}
}

type repoPermissionsResponse struct {
	Permissions struct {
		Pull bool `json:"pull"`
	} `json:"permissions"`
}

// VerifyPullAccess verifies that githubToken grants pull permission on repo.
// Positive results are cached for CacheTTL keyed by SHA-256(token + "\x00" + repo);
// raw tokens are never stored in cache keys, logs, or returned errors.
func (v *GitHubVerifier) VerifyPullAccess(ctx context.Context, githubToken, repo string) error {
	if !reRepo.MatchString(repo) {
		return fmt.Errorf("%w: %q", ErrInvalidRepo, repo)
	}
	if githubToken == "" {
		return ErrMissingGitHubToken
	}

	key := cacheKey(githubToken, repo)
	if v.hasFreshCache(key) {
		return nil
	}

	if err := v.fetchPullAccess(ctx, githubToken, repo); err != nil {
		return err
	}
	v.putCache(key)
	return nil
}

func cacheKey(githubToken, repo string) [sha256.Size]byte {
	h := sha256.New()
	_, _ = h.Write([]byte(githubToken))
	_, _ = h.Write([]byte{0})
	_, _ = h.Write([]byte(repo))
	var sum [sha256.Size]byte
	h.Sum(sum[:0])
	return sum
}

func (v *GitHubVerifier) hasFreshCache(key [sha256.Size]byte) bool {
	v.mu.Lock()
	defer v.mu.Unlock()
	exp, ok := v.cache[key]
	if !ok {
		return false
	}
	if v.now().Before(exp) {
		return true
	}
	delete(v.cache, key)
	return false
}

func (v *GitHubVerifier) putCache(key [sha256.Size]byte) {
	v.mu.Lock()
	defer v.mu.Unlock()
	v.cache[key] = v.now().Add(v.cacheTTL)
}

func (v *GitHubVerifier) fetchPullAccess(ctx context.Context, githubToken, repo string) error {
	reqCtx, cancel := context.WithTimeout(ctx, v.timeout)
	defer cancel()

	endpoint := v.apiBaseURL + "/repos/" + repo
	req, err := http.NewRequestWithContext(reqCtx, http.MethodGet, endpoint, nil)
	if err != nil {
		return fmt.Errorf("build github repo request for %s: %w", repo, err)
	}
	req.Header.Set("Authorization", "Bearer "+githubToken)
	req.Header.Set("Accept", "application/vnd.github+json")
	req.Header.Set("X-GitHub-Api-Version", githubAPIVersion)
	req.Header.Set("User-Agent", userAgent)

	resp, err := v.client.Do(req)
	if err != nil {
		return fmt.Errorf("query github repo access for %s: %w", repo, err)
	}
	defer resp.Body.Close()

	return interpretRepoResponse(repo, resp)
}

func interpretRepoResponse(repo string, resp *http.Response) error {
	switch resp.StatusCode {
	case http.StatusOK:
		var body repoPermissionsResponse
		if err := json.NewDecoder(io.LimitReader(resp.Body, maxResponseBytes)).Decode(&body); err != nil {
			return fmt.Errorf("decode github repo permissions for %s: %w", repo, err)
		}
		if !body.Permissions.Pull {
			return ErrAccessDenied
		}
		return nil
	case http.StatusUnauthorized:
		return ErrInvalidGitHubToken
	case http.StatusForbidden, http.StatusNotFound:
		return ErrAccessDenied
	default:
		return fmt.Errorf("github repo access check for %s returned status %d", repo, resp.StatusCode)
	}
}
