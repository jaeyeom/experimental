package lease

import (
	"context"
	"testing"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

func TestNoopProvider(t *testing.T) {
	t.Parallel()
	var p NoopProvider
	if p.Name() != "off" {
		t.Fatalf("Name() = %q, want off", p.Name())
	}
	res, err := p.Claim(context.Background(), "repo", time.Minute)
	if err != nil || res.Status != ClaimAcquired {
		t.Fatalf("Claim() = %+v, %v", res, err)
	}
	if err := p.Release(context.Background(), "repo"); err != nil {
		t.Fatalf("Release() error = %v", err)
	}
}

func TestFileProvider(t *testing.T) {
	t.Parallel()
	now := time.Date(2026, 5, 1, 12, 0, 0, 0, time.UTC)
	p := &FileProvider{
		Dir: t.TempDir(),
		Now: func() time.Time { return now },
	}

	res, err := p.Claim(context.Background(), "acme/widgets", 5*time.Minute)
	if err != nil || res.Status != ClaimAcquired {
		t.Fatalf("first Claim() = %+v, %v, want acquired", res, err)
	}

	// Second claim while unexpired must be refused immediately.
	res, err = p.Claim(context.Background(), "acme/widgets", 5*time.Minute)
	if err != nil || res.Status != ClaimRefused {
		t.Fatalf("second Claim() = %+v, %v, want refused", res, err)
	}

	// Advance past expiry; claim should reclaim.
	now = now.Add(6 * time.Minute)
	res, err = p.Claim(context.Background(), "acme/widgets", 5*time.Minute)
	if err != nil || res.Status != ClaimAcquired {
		t.Fatalf("expired Claim() = %+v, %v, want acquired", res, err)
	}

	// Release then re-claim.
	if err := p.Release(context.Background(), "acme/widgets"); err != nil {
		t.Fatalf("Release() error = %v", err)
	}
	res, err = p.Claim(context.Background(), "acme/widgets", 5*time.Minute)
	if err != nil || res.Status != ClaimAcquired {
		t.Fatalf("after Release Claim() = %+v, %v, want acquired", res, err)
	}
}

func TestAgentbusProvider(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name       string
		stdout     string
		stderr     string
		exitCode   int
		wantStatus ClaimStatus
	}{
		{name: "acquired", stdout: `{"status":"acquired"}`, exitCode: 0, wantStatus: ClaimAcquired},
		{name: "queued json", stdout: `{"status":"queued","holder":"pm"}`, exitCode: 0, wantStatus: ClaimQueued},
		{name: "refused exit", stderr: "held by other", exitCode: 1, wantStatus: ClaimRefused},
		{name: "queued exit", stderr: "request queued", exitCode: 1, wantStatus: ClaimQueued},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			mock := executor.NewMockExecutor()
			exp := mock.ExpectCommandWithArgs("agentbus", "lease", "claim", "--path", "/repo", "--ttl", "10m0s", "--no-wait", "--json")
			if tc.exitCode == 0 {
				exp.WillSucceed(tc.stdout, 0).Build()
			} else {
				exp.WillFail(tc.stderr, tc.exitCode).Build()
			}
			p := &AgentbusProvider{Exec: mock, Bin: "agentbus"}
			res, err := p.Claim(context.Background(), "/repo", 10*time.Minute)
			if err != nil {
				t.Fatalf("Claim() error = %v", err)
			}
			if res.Status != tc.wantStatus {
				t.Fatalf("Claim().Status = %q, want %q", res.Status, tc.wantStatus)
			}
		})
	}
}

func TestCommandProvider(t *testing.T) {
	t.Parallel()
	mock := executor.NewMockExecutor()
	mock.ExpectCommandWithArgs("sh", "-c", "claim-it").WillSucceed("", 0).Build()
	mock.ExpectCommandWithArgs("sh", "-c", "release-it").WillSucceed("", 0).Build()

	p := &CommandProvider{Exec: mock, ClaimCmd: "claim-it", ReleaseCmd: "release-it"}
	res, err := p.Claim(context.Background(), "work", time.Minute)
	if err != nil || res.Status != ClaimAcquired {
		t.Fatalf("Claim() = %+v, %v", res, err)
	}
	if err := p.Release(context.Background(), "work"); err != nil {
		t.Fatalf("Release() error = %v", err)
	}
}
