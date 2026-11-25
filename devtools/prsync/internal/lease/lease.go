// Package lease provides a pluggable lock/lease hook around dispatches that
// touch a shared working copy.
package lease

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

// ClaimStatus is the non-blocking result of Claim.
type ClaimStatus string

// Claim status values.
const (
	ClaimAcquired ClaimStatus = "acquired"
	ClaimQueued   ClaimStatus = "queued"
	ClaimRefused  ClaimStatus = "refused"
)

// ClaimResult reports the outcome of a single non-blocking lease claim.
type ClaimResult struct {
	Status ClaimStatus
	Holder string
	Reason string
}

// NoopProvider is the default "off" provider. It never takes a lock.
type NoopProvider struct{}

// Name returns "off".
func (NoopProvider) Name() string { return "off" }

// Claim always reports ClaimAcquired without taking a lock.
func (NoopProvider) Claim(context.Context, string, time.Duration) (ClaimResult, error) {
	return ClaimResult{Status: ClaimAcquired}, nil
}

// Release is a no-op.
func (NoopProvider) Release(context.Context, string) error { return nil }

// FileProvider implements a local named lease using atomic O_EXCL files with
// expiry timestamps. It never waits on a held, unexpired lock.
type FileProvider struct {
	Dir string
	Now func() time.Time
}

type fileLeaseRecord struct {
	Name      string    `json:"name"`
	PID       int       `json:"pid"`
	ClaimedAt time.Time `json:"claimed_at"` //nolint:tagliatelle // lease metadata
	ExpiresAt time.Time `json:"expires_at"` //nolint:tagliatelle // lease metadata
}

// Name returns "file".
func (p *FileProvider) Name() string { return "file" }

func (p *FileProvider) now() time.Time {
	if p.Now != nil {
		return p.Now()
	}
	return time.Now()
}

func (p *FileProvider) lockPath(name string) string {
	safe := strings.NewReplacer("/", "_", "\\", "_", ":", "_", " ", "_").Replace(name)
	if safe == "" {
		safe = "default"
	}
	return filepath.Join(p.Dir, safe+".lease")
}

// Claim atomically claims name for timeout. If an unexpired lock exists,
// Claim returns ClaimRefused immediately.
func (p *FileProvider) Claim(_ context.Context, name string, timeout time.Duration) (ClaimResult, error) {
	if err := os.MkdirAll(p.Dir, 0o700); err != nil {
		return ClaimResult{}, fmt.Errorf("create lease dir: %w", err)
	}
	path := p.lockPath(name)
	now := p.now().UTC()
	if err := p.tryCreate(path, name, now, timeout); err == nil {
		return ClaimResult{Status: ClaimAcquired}, nil
	} else if !errors.Is(err, os.ErrExist) {
		return ClaimResult{}, err
	}

	// Lock file exists; check if expired.
	existing, err := readLeaseFile(path)
	if err == nil && !existing.ExpiresAt.IsZero() && !now.Before(existing.ExpiresAt) {
		_ = os.Remove(path)
		if err := p.tryCreate(path, name, now, timeout); err == nil {
			return ClaimResult{Status: ClaimAcquired}, nil
		} else if !errors.Is(err, os.ErrExist) {
			return ClaimResult{}, err
		}
		existing, _ = readLeaseFile(path)
	}
	holder := ""
	if existing.PID > 0 {
		holder = fmt.Sprintf("pid %d", existing.PID)
	}
	return ClaimResult{
		Status: ClaimRefused,
		Holder: holder,
		Reason: "lease already held",
	}, nil
}

func (p *FileProvider) tryCreate(path, name string, now time.Time, timeout time.Duration) error {
	f, err := os.OpenFile(path, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0o600) //nolint:gosec // lease path under configured dir
	if err != nil {
		return fmt.Errorf("create lease file: %w", err)
	}
	rec := fileLeaseRecord{
		Name:      name,
		PID:       os.Getpid(),
		ClaimedAt: now,
		ExpiresAt: now.Add(timeout),
	}
	data, err := json.Marshal(rec)
	if err != nil {
		_ = f.Close()
		_ = os.Remove(path)
		return fmt.Errorf("encode lease: %w", err)
	}
	if _, err := f.Write(append(data, '\n')); err != nil {
		_ = f.Close()
		_ = os.Remove(path)
		return fmt.Errorf("write lease: %w", err)
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("close lease file: %w", err)
	}
	return nil
}

func readLeaseFile(path string) (fileLeaseRecord, error) {
	data, err := os.ReadFile(path) //nolint:gosec // lease path under configured dir
	if err != nil {
		return fileLeaseRecord{}, fmt.Errorf("read lease file: %w", err)
	}
	var rec fileLeaseRecord
	if err := json.Unmarshal(data, &rec); err != nil {
		return fileLeaseRecord{}, fmt.Errorf("decode lease file: %w", err)
	}
	return rec, nil
}

// Release removes the lease file for name.
func (p *FileProvider) Release(_ context.Context, name string) error {
	err := os.Remove(p.lockPath(name))
	if errors.Is(err, os.ErrNotExist) {
		return nil
	}
	if err != nil {
		return fmt.Errorf("remove lease file: %w", err)
	}
	return nil
}

// AgentbusProvider claims and releases a named workdir lease via the agentbus CLI.
type AgentbusProvider struct {
	Exec executor.Executor
	Bin  string
}

// Name returns "agentbus".
func (p *AgentbusProvider) Name() string { return "agentbus" }

// Claim runs `agentbus lease claim --path <name> --ttl <timeout> --no-wait --json`.
func (p *AgentbusProvider) Claim(ctx context.Context, name string, timeout time.Duration) (ClaimResult, error) {
	bin := p.Bin
	if bin == "" {
		bin = "agentbus"
	}
	res, err := p.Exec.Execute(ctx, executor.ToolConfig{
		Command: bin,
		Args:    []string{"lease", "claim", "--path", name, "--ttl", timeout.String(), "--no-wait", "--json"},
		Timeout: 10 * time.Second,
	})
	if err != nil {
		return ClaimResult{}, fmt.Errorf("agentbus lease claim: %w", err)
	}
	var payload struct {
		Status string `json:"status"`
		Holder string `json:"holder"`
		Reason string `json:"reason"`
	}
	_ = json.Unmarshal([]byte(res.Output), &payload)
	if res.ExitCode == 0 {
		switch ClaimStatus(payload.Status) {
		case ClaimQueued:
			return ClaimResult{Status: ClaimQueued, Holder: payload.Holder, Reason: payload.Reason}, nil
		case ClaimRefused:
			return ClaimResult{Status: ClaimRefused, Holder: payload.Holder, Reason: payload.Reason}, nil
		default:
			return ClaimResult{Status: ClaimAcquired}, nil
		}
	}
	reason := strings.TrimSpace(payload.Reason)
	if reason == "" {
		reason = strings.TrimSpace(res.Stderr)
	}
	if strings.Contains(strings.ToLower(reason), "queued") {
		return ClaimResult{Status: ClaimQueued, Holder: payload.Holder, Reason: reason}, nil
	}
	return ClaimResult{Status: ClaimRefused, Holder: payload.Holder, Reason: reason}, nil
}

// Release runs `agentbus lease release --path <name>`.
func (p *AgentbusProvider) Release(ctx context.Context, name string) error {
	bin := p.Bin
	if bin == "" {
		bin = "agentbus"
	}
	res, err := p.Exec.Execute(ctx, executor.ToolConfig{
		Command: bin,
		Args:    []string{"lease", "release", "--path", name},
		Timeout: 10 * time.Second,
	})
	if err != nil {
		return fmt.Errorf("agentbus lease release: %w", err)
	}
	if res.ExitCode != 0 {
		return fmt.Errorf("agentbus lease release exited %d: %s", res.ExitCode, strings.TrimSpace(res.Stderr))
	}
	return nil
}

// CommandProvider delegates lease claim/release to user-configured shell commands.
// PRSYNC_LEASE_NAME and PRSYNC_LEASE_TIMEOUT_MS are passed in the environment.
type CommandProvider struct {
	Exec       executor.Executor
	ClaimCmd   string
	ReleaseCmd string
}

// Name returns "command".
func (p *CommandProvider) Name() string { return "command" }

// Claim runs ClaimCmd. Exit 0 = acquired; non-zero = refused (or queued if stderr contains "queued").
func (p *CommandProvider) Claim(ctx context.Context, name string, timeout time.Duration) (ClaimResult, error) {
	if strings.TrimSpace(p.ClaimCmd) == "" {
		return ClaimResult{}, errors.New("lease_claim_cmd is empty")
	}
	res, err := p.Exec.Execute(ctx, executor.ToolConfig{
		Command: "sh",
		Args:    []string{"-c", p.ClaimCmd},
		Env: map[string]string{
			"PRSYNC_LEASE_NAME":       name,
			"PRSYNC_LEASE_TIMEOUT_MS": fmt.Sprintf("%d", timeout.Milliseconds()),
		},
		Timeout: 15 * time.Second,
	})
	if err != nil {
		return ClaimResult{}, fmt.Errorf("lease claim command: %w", err)
	}
	if res.ExitCode == 0 {
		return ClaimResult{Status: ClaimAcquired}, nil
	}
	reason := strings.TrimSpace(res.Stderr)
	if reason == "" {
		reason = strings.TrimSpace(res.Output)
	}
	if strings.Contains(strings.ToLower(reason), "queued") {
		return ClaimResult{Status: ClaimQueued, Reason: reason}, nil
	}
	return ClaimResult{Status: ClaimRefused, Reason: reason}, nil
}

// Release runs ReleaseCmd.
func (p *CommandProvider) Release(ctx context.Context, name string) error {
	if strings.TrimSpace(p.ReleaseCmd) == "" {
		return nil
	}
	res, err := p.Exec.Execute(ctx, executor.ToolConfig{
		Command: "sh",
		Args:    []string{"-c", p.ReleaseCmd},
		Env: map[string]string{
			"PRSYNC_LEASE_NAME": name,
		},
		Timeout: 15 * time.Second,
	})
	if err != nil {
		return fmt.Errorf("lease release command: %w", err)
	}
	if res.ExitCode != 0 {
		return fmt.Errorf("lease release exited %d: %s", res.ExitCode, strings.TrimSpace(res.Stderr))
	}
	return nil
}

// FakeProvider is an in-memory Provider for tests.
type FakeProvider struct {
	mu         sync.Mutex
	Result     ClaimResult
	ClaimErr   error
	ReleaseErr error
	Claims     []string
	Releases   []string
	Held       bool
}

// Name returns "fake".
func (f *FakeProvider) Name() string { return "fake" }

// Claim records the claim and returns Result (default ClaimAcquired).
func (f *FakeProvider) Claim(_ context.Context, name string, _ time.Duration) (ClaimResult, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.Claims = append(f.Claims, name)
	if f.ClaimErr != nil {
		return ClaimResult{}, f.ClaimErr
	}
	res := f.Result
	if res.Status == "" {
		res.Status = ClaimAcquired
	}
	if res.Status == ClaimAcquired {
		f.Held = true
	}
	return res, nil
}

// Release records the release.
func (f *FakeProvider) Release(_ context.Context, name string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.Releases = append(f.Releases, name)
	f.Held = false
	return f.ReleaseErr
}
