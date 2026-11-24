package ghacl_test

import (
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"sync/atomic"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/broker/ghacl"
)

func TestVerifyPullAccessAllowsAndCaches(t *testing.T) {
	var hits atomic.Int32
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		hits.Add(1)
		if r.Method != http.MethodGet {
			t.Errorf("method = %s, want GET", r.Method)
		}
		if r.URL.Path != "/repos/org/secret-repo" {
			t.Errorf("path = %s, want /repos/org/secret-repo", r.URL.Path)
		}
		if got := r.Header.Get("Authorization"); got != "Bearer test-token-allow" {
			t.Errorf("Authorization = %q, want Bearer test-token-allow", got)
		}
		if got := r.Header.Get("Accept"); got != "application/vnd.github+json" {
			t.Errorf("Accept = %q, want application/vnd.github+json", got)
		}
		if got := r.Header.Get("X-GitHub-Api-Version"); got != "2022-11-28" {
			t.Errorf("X-GitHub-Api-Version = %q, want 2022-11-28", got)
		}
		if got := r.Header.Get("User-Agent"); got != "agent-poold" {
			t.Errorf("User-Agent = %q, want agent-poold", got)
		}
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte(`{"permissions":{"pull":true}}`))
	}))
	defer srv.Close()

	now := time.Date(2026, 11, 24, 12, 0, 0, 0, time.UTC)
	verifier := ghacl.NewGitHubVerifier(ghacl.Config{
		APIBaseURL: srv.URL,
		CacheTTL:   5 * time.Minute,
		Now:        func() time.Time { return now },
	}, srv.Client())

	ctx := context.Background()
	if err := verifier.VerifyPullAccess(ctx, "test-token-allow", "org/secret-repo"); err != nil {
		t.Fatalf("first VerifyPullAccess() error = %v", err)
	}
	if got := hits.Load(); got != 1 {
		t.Fatalf("server hits after first call = %d, want 1", got)
	}

	// Second call within CacheTTL hits cache.
	now = now.Add(4 * time.Minute)
	if err := verifier.VerifyPullAccess(ctx, "test-token-allow", "org/secret-repo"); err != nil {
		t.Fatalf("second VerifyPullAccess() error = %v", err)
	}
	if got := hits.Load(); got != 1 {
		t.Fatalf("server hits within TTL = %d, want 1", got)
	}

	// After TTL expires, re-queries server.
	now = now.Add(2 * time.Minute)
	if err := verifier.VerifyPullAccess(ctx, "test-token-allow", "org/secret-repo"); err != nil {
		t.Fatalf("third VerifyPullAccess() after TTL error = %v", err)
	}
	if got := hits.Load(); got != 2 {
		t.Fatalf("server hits after TTL expiry = %d, want 2", got)
	}
}

func TestVerifyPullAccessDeniesPullFalse(t *testing.T) {
	var hits atomic.Int32
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		hits.Add(1)
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte(`{"permissions":{"pull":false}}`))
	}))
	defer srv.Close()

	verifier := ghacl.NewGitHubVerifier(ghacl.Config{
		APIBaseURL: srv.URL,
		CacheTTL:   5 * time.Minute,
	}, srv.Client())

	ctx := context.Background()
	err := verifier.VerifyPullAccess(ctx, "test-token-nopull", "org/secret-repo")
	if !errors.Is(err, ghacl.ErrAccessDenied) {
		t.Fatalf("first error = %v, want ErrAccessDenied", err)
	}

	// Negative results must NOT be cached.
	err = verifier.VerifyPullAccess(ctx, "test-token-nopull", "org/secret-repo")
	if !errors.Is(err, ghacl.ErrAccessDenied) {
		t.Fatalf("second error = %v, want ErrAccessDenied", err)
	}
	if got := hits.Load(); got != 2 {
		t.Fatalf("server hits = %d, want 2 (negative result must not be cached)", got)
	}
}

func TestVerifyPullAccessDenies404And403(t *testing.T) {
	tests := []struct {
		name   string
		status int
	}{
		{name: "404 Not Found", status: http.StatusNotFound},
		{name: "403 Forbidden", status: http.StatusForbidden},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var hits atomic.Int32
			srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
				hits.Add(1)
				w.WriteHeader(tc.status)
			}))
			defer srv.Close()

			verifier := ghacl.NewGitHubVerifier(ghacl.Config{
				APIBaseURL: srv.URL,
				CacheTTL:   5 * time.Minute,
			}, srv.Client())

			err := verifier.VerifyPullAccess(context.Background(), "test-token", "org/secret-repo")
			if !errors.Is(err, ghacl.ErrAccessDenied) {
				t.Fatalf("VerifyPullAccess() error = %v, want ErrAccessDenied", err)
			}
			// Must not cache negative result.
			err = verifier.VerifyPullAccess(context.Background(), "test-token", "org/secret-repo")
			if !errors.Is(err, ghacl.ErrAccessDenied) {
				t.Fatalf("second VerifyPullAccess() error = %v, want ErrAccessDenied", err)
			}
			if got := hits.Load(); got != 2 {
				t.Fatalf("server hits = %d, want 2", got)
			}
		})
	}
}

func TestVerifyPullAccessInvalidToken401(t *testing.T) {
	var hits atomic.Int32
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		hits.Add(1)
		w.WriteHeader(http.StatusUnauthorized)
	}))
	defer srv.Close()

	verifier := ghacl.NewGitHubVerifier(ghacl.Config{
		APIBaseURL: srv.URL,
		CacheTTL:   5 * time.Minute,
	}, srv.Client())

	err := verifier.VerifyPullAccess(context.Background(), "bad-token", "org/secret-repo")
	if !errors.Is(err, ghacl.ErrInvalidGitHubToken) {
		t.Fatalf("VerifyPullAccess() error = %v, want ErrInvalidGitHubToken", err)
	}
	// Must not cache 401.
	err = verifier.VerifyPullAccess(context.Background(), "bad-token", "org/secret-repo")
	if !errors.Is(err, ghacl.ErrInvalidGitHubToken) {
		t.Fatalf("second VerifyPullAccess() error = %v, want ErrInvalidGitHubToken", err)
	}
	if got := hits.Load(); got != 2 {
		t.Fatalf("server hits = %d, want 2", got)
	}
}

func TestVerifyPullAccessMissingToken(t *testing.T) {
	var hits atomic.Int32
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		hits.Add(1)
		w.WriteHeader(http.StatusOK)
	}))
	defer srv.Close()

	verifier := ghacl.NewGitHubVerifier(ghacl.Config{
		APIBaseURL: srv.URL,
	}, srv.Client())

	err := verifier.VerifyPullAccess(context.Background(), "", "org/secret-repo")
	if !errors.Is(err, ghacl.ErrMissingGitHubToken) {
		t.Fatalf("VerifyPullAccess() error = %v, want ErrMissingGitHubToken", err)
	}
	if got := hits.Load(); got != 0 {
		t.Fatalf("server hits = %d, want 0", got)
	}
}

func TestVerifyPullAccessDifferentTokensDoNotShareCache(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Header.Get("Authorization") == "Bearer token-a" {
			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(http.StatusOK)
			_, _ = w.Write([]byte(`{"permissions":{"pull":true}}`))
			return
		}
		w.WriteHeader(http.StatusNotFound)
	}))
	defer srv.Close()

	verifier := ghacl.NewGitHubVerifier(ghacl.Config{
		APIBaseURL: srv.URL,
		CacheTTL:   5 * time.Minute,
	}, srv.Client())

	ctx := context.Background()
	if err := verifier.VerifyPullAccess(ctx, "token-a", "org/secret-repo"); err != nil {
		t.Fatalf("token-a error = %v, want nil", err)
	}
	err := verifier.VerifyPullAccess(ctx, "token-b", "org/secret-repo")
	if !errors.Is(err, ghacl.ErrAccessDenied) {
		t.Fatalf("token-b error = %v, want ErrAccessDenied", err)
	}
}

func TestVerifyPullAccessInvalidRepo(t *testing.T) {
	var hits atomic.Int32
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		hits.Add(1)
		w.WriteHeader(http.StatusOK)
	}))
	defer srv.Close()

	verifier := ghacl.NewGitHubVerifier(ghacl.Config{
		APIBaseURL: srv.URL,
	}, srv.Client())

	for _, badRepo := range []string{"", "no-slash", "a/b/c", "../etc/passwd", "org/repo?foo=bar"} {
		err := verifier.VerifyPullAccess(context.Background(), "token-a", badRepo)
		if !errors.Is(err, ghacl.ErrInvalidRepo) {
			t.Errorf("VerifyPullAccess(%q) error = %v, want ErrInvalidRepo", badRepo, err)
		}
	}
	if got := hits.Load(); got != 0 {
		t.Fatalf("server hits = %d, want 0", got)
	}
}
