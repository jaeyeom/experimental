package memstore

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"testing"
	"time"
)

func setupInboxTestConfig(t *testing.T) Config {
	t.Helper()
	dir := t.TempDir()
	workPath := filepath.Join(dir, "vaults", "work.db")
	personalPath := filepath.Join(dir, "vaults", "personal.db")

	cfg := Config{
		Vaults: map[string]VaultConfig{
			"work":     {Name: "work", Path: workPath, Residency: ResidencyLocalOnly},
			"personal": {Name: "personal", Path: personalPath, Residency: ResidencyLocalOnly},
		},
		Profiles: map[string]ProfileConfig{
			"admin": {
				Name:           "admin",
				Vaults:         []string{"work", "personal"},
				Compartments:   []string{"general", "engineering", "personal"},
				MaxSensitivity: SensitivityRestricted,
				Capabilities:   []Capability{CapabilityRead, CapabilityWrite, CapabilityWriteEpisodic, CapabilityProposeFact},
			},
			"work-bot": {
				Name:           "work-bot",
				Vaults:         []string{"work"},
				Compartments:   []string{"general", "engineering"},
				MaxSensitivity: SensitivityNormal,
				Capabilities:   []Capability{CapabilityRead, CapabilityWriteEpisodic, CapabilityProposeFact},
			},
		},
	}
	for _, vc := range cfg.Vaults {
		v, err := InitVault(vc)
		if err != nil {
			t.Fatalf("InitVault(%s): %v", vc.Name, err)
		}
		if err := v.Close(); err != nil {
			t.Fatalf("Close(%s): %v", vc.Name, err)
		}
	}
	return cfg
}

type failingAckTransport struct {
	inner   InboxTransport
	failAck bool
	acked   []string
}

func (f *failingAckTransport) Push(ctx context.Context, env InboxEnvelope) error {
	if err := f.inner.Push(ctx, env); err != nil {
		return fmt.Errorf("inner push: %w", err)
	}
	return nil
}

func (f *failingAckTransport) List(ctx context.Context, limit int) ([]InboxEnvelope, error) {
	envs, err := f.inner.List(ctx, limit)
	if err != nil {
		return nil, fmt.Errorf("inner list: %w", err)
	}
	return envs, nil
}

func (f *failingAckTransport) Ack(ctx context.Context, id string) error {
	if f.failAck {
		return errors.New("simulated ack failure")
	}
	f.acked = append(f.acked, id)
	if err := f.inner.Ack(ctx, id); err != nil {
		return fmt.Errorf("inner ack: %w", err)
	}
	return nil
}

func TestDirInboxTransportRoundTrip(t *testing.T) {
	ctx := context.Background()
	dir := filepath.Join(t.TempDir(), "inbox")
	tr := NewDirInboxTransport(dir)

	// Empty dir returns empty slice.
	got, err := tr.List(ctx, 0)
	if err != nil {
		t.Fatalf("List empty: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("len(List empty) = %d, want 0", len(got))
	}

	env1 := InboxEnvelope{ID: "env_001", Source: "cloud-run", Summary: "First event"}
	env2 := InboxEnvelope{ID: "env_002", Source: "cloud-run", Summary: "Second event"}
	if err := tr.Push(ctx, env1); err != nil {
		t.Fatalf("Push env1: %v", err)
	}
	if err := tr.Push(ctx, env2); err != nil {
		t.Fatalf("Push env2: %v", err)
	}

	got, err = tr.List(ctx, 1)
	if err != nil {
		t.Fatalf("List limit 1: %v", err)
	}
	if len(got) != 1 || got[0].ID != "env_001" {
		t.Fatalf("List limit 1 = %+v, want [env_001]", got)
	}

	if err := tr.Ack(ctx, "env_001"); err != nil {
		t.Fatalf("Ack env_001: %v", err)
	}
	// Idempotent ack on already removed ID.
	if err := tr.Ack(ctx, "env_001"); err != nil {
		t.Fatalf("second Ack env_001: %v", err)
	}

	got, err = tr.List(ctx, 0)
	if err != nil {
		t.Fatalf("List after ack: %v", err)
	}
	if len(got) != 1 || got[0].ID != "env_002" {
		t.Fatalf("List after ack = %+v, want [env_002]", got)
	}

	if err := tr.Push(ctx, InboxEnvelope{ID: "../escape", Source: "x", Summary: "bad"}); err == nil {
		t.Fatal("expected error for path traversal envelope ID")
	}
}

func TestPullInboxValidRefusedAndIdempotentRetry(t *testing.T) {
	ctx := context.Background()
	cfg := setupInboxTestConfig(t)

	svc, err := OpenService(cfg, "work-bot")
	if err != nil {
		t.Fatalf("OpenService: %v", err)
	}
	defer svc.Close()

	dirTr := NewDirInboxTransport(filepath.Join(t.TempDir(), "inbox"))
	tr := &failingAckTransport{inner: dirTr, failAck: true}

	validEnv := InboxEnvelope{
		ID:          "env_valid_1",
		Source:      "gcp-pubsub",
		OccurredAt:  time.Date(2026, 6, 1, 10, 0, 0, 0, time.UTC),
		Compartment: "engineering",
		Sensitivity: SensitivityNormal,
		Summary:     "Deployed service v2",
		DetailsMD:   "Rollout succeeded",
		Facts: []FactInput{
			{EntityID: "project:alpha", Key: "version", ValueJSON: `"v2"`, SourceType: "gcp"},
		},
	}
	badCompEnv := InboxEnvelope{
		ID:          "env_bad_comp",
		Source:      "gcp-pubsub",
		Compartment: "personal",
		Sensitivity: SensitivityNormal,
		Summary:     "Personal note",
	}
	badSensEnv := InboxEnvelope{
		ID:          "env_bad_sens",
		Source:      "gcp-pubsub",
		Compartment: "engineering",
		Sensitivity: SensitivityRestricted,
		Summary:     "Secret key",
	}

	for _, env := range []InboxEnvelope{badCompEnv, badSensEnv, validEnv} {
		if err := tr.Push(ctx, env); err != nil {
			t.Fatalf("Push %s: %v", env.ID, err)
		}
	}

	// First run: Ack fails after local SQLite commit succeeds.
	_, err = PullInbox(ctx, svc, "work", tr, PullOptions{Actor: Actor{Caller: "sync-test"}})
	if err == nil {
		t.Fatal("expected error when Ack fails")
	}

	// Second run: Ack now succeeds; validEnv is skipped as duplicate (not re-inserted) and acked.
	// Refused envelopes remain un-acked in the inbox.
	tr.failAck = false
	report, err := PullInbox(ctx, svc, "work", tr, PullOptions{Actor: Actor{Caller: "sync-test"}})
	if err != nil {
		t.Fatalf("PullInbox retry: %v", err)
	}
	if report.Listed != 3 || report.Skipped != 1 || report.Refused != 2 || report.Acked != 1 {
		t.Fatalf("unexpected report on retry: %+v", report)
	}

	// Verify only 1 episodic event and 1 triage item were recorded.
	events, err := svc.ListEvents(ctx, EventFilter{Vault: "work"}, Actor{Caller: "test"})
	if err != nil {
		t.Fatalf("ListEvents: %v", err)
	}
	if len(events) != 1 || events[0].Summary != "Deployed service v2" {
		t.Fatalf("events = %+v, want 1 event", events)
	}

	triage, err := svc.ListTriage(ctx, TriageFilter{Vault: "work"})
	if err != nil {
		t.Fatalf("ListTriage: %v", err)
	}
	if len(triage) != 1 {
		t.Fatalf("triage len = %d, want 1", len(triage))
	}

	// Verify refused envelopes were NOT acked and remain in the transport.
	remaining, err := dirTr.List(ctx, 0)
	if err != nil {
		t.Fatalf("List remaining: %v", err)
	}
	if len(remaining) != 2 {
		t.Fatalf("remaining envelopes = %d, want 2 (refused)", len(remaining))
	}
}
