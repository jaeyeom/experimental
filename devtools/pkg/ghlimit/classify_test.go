package ghlimit

import (
	"strings"
	"testing"
	"time"
)

func TestIsMutation(t *testing.T) {
	for _, tt := range []struct {
		args []string
		want bool
	}{
		{strings.Fields("pr review 1 --approve"), true},
		{strings.Fields("pr edit 1 --add-label x"), true},
		{strings.Fields("pr merge 1 --squash"), true},
		{strings.Fields("pr close 1"), true},
		{strings.Fields("pr comment 1 --body hi"), true},
		{strings.Fields("pr ready 1"), true},
		{strings.Fields("pr view 1"), false},
		{strings.Fields("pr list"), false},
		{strings.Fields("pr checks 1"), false},
		{strings.Fields("issue create --title t"), true},
		{strings.Fields("issue edit 1"), true},
		{strings.Fields("issue comment 1 --body-file -"), true},
		{strings.Fields("issue close 1"), true},
		{strings.Fields("issue view 1"), false},
		{strings.Fields("label create bug"), true},
		{strings.Fields("label list"), true},
		{strings.Fields("api -X POST repos/o/r/issues"), true},
		{strings.Fields("api -XPATCH repos/o/r"), true},
		{strings.Fields("api --method PUT repos/o/r/pulls/1/merge"), true},
		{strings.Fields("api --method=delete repos/o/r/labels/x"), true},
		{strings.Fields("api -X GET repos/o/r -f per_page=1"), false},
		{strings.Fields("api repos/o/r/issues -f title=t"), true},
		{strings.Fields("api repos/o/r/issues -F number=1"), true},
		{strings.Fields("api repos/o/r/issues --raw-field title=t"), true},
		{strings.Fields("api repos/o/r/issues --field=n=1"), true},
		{strings.Fields("api repos/o/r/issues -ftitle=t"), true},
		{strings.Fields("api repos/o/r"), false},
		{strings.Fields("api -i repos/o/r"), false},
		{[]string{"api", "graphql", "-f", "query=mutation{addStar(input:{}){clientMutationId}}"}, true},
		{[]string{"api", "graphql", "-f", "query=\n  mutation M { x }"}, true},
		{[]string{"api", "graphql", "--raw-field=query=mutation{x}"}, true},
		{[]string{"api", "graphql", "-F", "owner=o", "-f", "query=query { viewer { login } }"}, false},
		{[]string{"api", "graphql", "-f", "query={viewer{login}}"}, false},
		{strings.Fields("repo view"), false},
		{nil, false},
	} {
		if got := IsMutation(tt.args); got != tt.want {
			t.Errorf("IsMutation(%q) = %v, want %v", tt.args, got, tt.want)
		}
	}
}

func TestRedact(t *testing.T) {
	got := strings.Join(Redact([]string{"pr", "comment", "123", "--body", "secret", "--repo=o/r", "-R", "o/r"}), " ")
	if want := "pr comment _ --body _ --repo=_ -R _"; got != want {
		t.Fatalf("Redact = %q, want %q", got, want)
	}
	got = strings.Join(Redact([]string{"api", "graphql", "-f", "query=mutation{x}"}), " ")
	if want := "api graphql -f _"; got != want {
		t.Fatalf("Redact = %q, want %q", got, want)
	}
}

func TestUsesStdin(t *testing.T) {
	for _, tt := range []struct {
		args string
		want bool
	}{
		{"pr comment 1 --body-file -", true},
		{"pr comment 1 --body-file=-", true},
		{"api graphql -F query=@-", true},
		{"api repos/o/r --input -", true},
		{"pr comment 1 --body x-", false},
		{"pr view 1", false},
	} {
		if got := UsesStdin(strings.Fields(tt.args)); got != tt.want {
			t.Errorf("UsesStdin(%q) = %v, want %v", tt.args, got, tt.want)
		}
	}
}

func TestRateLimited(t *testing.T) {
	for _, tt := range []struct {
		args, stderr, stdout string
		want                 bool
	}{
		{"pr merge 1", "You have exceeded a secondary rate limit", "", true},
		{"pr view 1", "API rate limit exceeded for user", "", true},
		{"pr view 1", "You have triggered an abuse detection mechanism", "", true},
		{"api repos/o/r", "gh: Forbidden (HTTP 403)", "", true},
		{"api -i repos/o/r", "", "HTTP/2.0 429 Too Many Requests\nRetry-After: 3\n", true},
		{"pr view 1", "gh: Forbidden (HTTP 403)", "", false},
		{"pr view 1", "no pull requests found", "", false},
	} {
		if got := RateLimited(strings.Fields(tt.args), tt.stderr, tt.stdout); got != tt.want {
			t.Errorf("RateLimited(%q, %q, %q) = %v, want %v", tt.args, tt.stderr, tt.stdout, got, tt.want)
		}
	}
}

func TestRetryAfter(t *testing.T) {
	now := time.Unix(1000, 0)
	if d, ok := RetryAfter("HTTP/2.0 403 Forbidden\nRetry-After: 7\n", now); !ok || d != 7*time.Second {
		t.Fatalf("RetryAfter = %v, %v; want 7s", d, ok)
	}
	if d, ok := RetryAfter("x-ratelimit-reset: 1030\r\n", now); !ok || d != 30*time.Second {
		t.Fatalf("RetryAfter(reset) = %v, %v; want 30s", d, ok)
	}
	if _, ok := RetryAfter("secondary rate limit", now); ok {
		t.Fatal("RetryAfter found a header in plain text")
	}
}

func TestConfigFromEnv(t *testing.T) {
	cfg, err := ConfigFromEnv(func(string) string { return "" })
	if err != nil || cfg != DefaultConfig() {
		t.Fatalf("empty env = %+v, %v; want defaults", cfg, err)
	}
	if cfg.MaxConcurrent != 1 || cfg.MutationGap != time.Second || cfg.Backoff != time.Minute || cfg.MaxBackoff != 5*time.Minute || cfg.MaxWait != 10*time.Minute {
		t.Fatalf("defaults = %+v", cfg)
	}

	env := map[string]string{
		"GH_THROTTLE_MAX_CONCURRENT": "3",
		"GH_THROTTLE_MUTATION_GAP":   "1500ms",
		"GH_THROTTLE_BACKOFF":        "30",
		"GH_THROTTLE_MAX_BACKOFF":    "2m",
	}
	cfg, err = ConfigFromEnv(func(k string) string { return env[k] })
	if err != nil {
		t.Fatal(err)
	}
	want := DefaultConfig()
	want.MaxConcurrent, want.MutationGap, want.Backoff, want.MaxBackoff = 3, 1500*time.Millisecond, 30*time.Second, 2*time.Minute
	if cfg != want {
		t.Fatalf("ConfigFromEnv = %+v, want %+v", cfg, want)
	}

	for _, bad := range []map[string]string{
		{"GH_THROTTLE_MAX_CONCURRENT": "0"},
		{"GH_THROTTLE_MUTATION_GAP": "soon"},
	} {
		if _, err := ConfigFromEnv(func(k string) string { return bad[k] }); err == nil {
			t.Errorf("ConfigFromEnv(%v) succeeded", bad)
		}
	}
}
