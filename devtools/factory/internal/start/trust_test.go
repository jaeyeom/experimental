package start

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func setupGitHubTrustHarness(t *testing.T, env string) *harness {
	t.Helper()
	h := newHarness(t)
	const ghID = "acme/widgets#42"
	h.id = ghID
	h.starter.Kind = "github"
	h.starter.Cfg = config.Source{
		Name:          "widgets",
		Kind:          "github",
		Repo:          "acme/widgets",
		Cwd:           "~/src/widgets",
		Base:          "main",
		Workspace:     "factory",
		StartPrompt:   "Work on {id} {title}",
		ReadyStatuses: config.DefaultGitHubReadyStatuses(),
		ClaimStatus:   config.DefaultGitHubClaimStatus,
		AssigneeOK:    "me",
		Trust:         config.Trust{Environment: env},
	}
	it := source.Item{
		Source:            "github:widgets",
		ID:                ghID,
		Title:             "External fix",
		URL:               "https://github.com/acme/widgets/issues/42",
		Type:              "Issue",
		State:             "Todo",
		Author:            "stranger",
		AuthorAssociation: "NONE",
		Locked:            true,
	}
	h.snap.Items = map[string]source.Item{ghID: it}
	h.snap.Me = "maintainer"
	h.src.inner.SourceName = "github:widgets"
	h.src.inner.Account = "maintainer"
	h.src.inner.ClaimStatus = config.DefaultGitHubClaimStatus
	h.src.inner.Items = map[string]source.Item{ghID: it}
	return h
}

func TestTrustPromptInjectionOnlyForUntrusted(t *testing.T) {
	t.Parallel()

	t.Run("trusted source leaves prompt clean", func(t *testing.T) {
		t.Parallel()
		h := setupGitHubTrustHarness(t, config.TrustEnvTrusted)
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		if strings.Contains(h.herdr.promptText, "Untrusted-input rule") {
			t.Fatalf("trusted prompt unexpectedly hardened:\n%s", h.herdr.promptText)
		}
		if h.herdr.promptText != "Work on acme/widgets#42 External fix" {
			t.Fatalf("prompt = %q", h.herdr.promptText)
		}
	})

	t.Run("unset trust environment leaves prompt clean", func(t *testing.T) {
		t.Parallel()
		h := setupGitHubTrustHarness(t, "")
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		if h.herdr.promptText != "Work on acme/widgets#42 External fix" {
			t.Fatalf("prompt = %q", h.herdr.promptText)
		}
	})

	t.Run("untrusted source prepends rule when no placeholder", func(t *testing.T) {
		t.Parallel()
		h := setupGitHubTrustHarness(t, config.TrustEnvUntrusted)
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		want := UntrustedInputRule + "\n\nWork on acme/widgets#42 External fix"
		if h.herdr.promptText != want {
			t.Fatalf("prompt =\n%s\nwant =\n%s", h.herdr.promptText, want)
		}
		if !strings.Contains(h.out.String(), "trust    untrusted, rule at the top of the prompt:\n") {
			t.Fatalf("plan =\n%s", h.out.String())
		}
	})

	t.Run("untrusted source places rule at {{trust_rule}}", func(t *testing.T) {
		t.Parallel()
		h := setupGitHubTrustHarness(t, config.TrustEnvUntrusted)
		h.starter.Cfg.StartPrompt = "Work on {id}.\n{{trust_rule}}\nDone."
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		want := "Work on acme/widgets#42.\n" + UntrustedInputRule + "\nDone."
		if h.herdr.promptText != want {
			t.Fatalf("prompt =\n%s\nwant =\n%s", h.herdr.promptText, want)
		}
		if !strings.Contains(h.out.String(), "trust    untrusted, rule at {{trust_rule}}:\n") {
			t.Fatalf("plan =\n%s", h.out.String())
		}
	})

	t.Run("trusted source removes {{trust_rule}} placeholder", func(t *testing.T) {
		t.Parallel()
		h := setupGitHubTrustHarness(t, config.TrustEnvTrusted)
		h.starter.Cfg.StartPrompt = "Work on {id}.{{trust_rule}}"
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		if h.herdr.promptText != "Work on acme/widgets#42." {
			t.Fatalf("prompt = %q", h.herdr.promptText)
		}
	})

	t.Run("title cannot inject {{trust_rule}}", func(t *testing.T) {
		t.Parallel()
		h := setupGitHubTrustHarness(t, config.TrustEnvUntrusted)
		it := h.snap.Items[h.id]
		it.Title = "{{trust_rule}}"
		h.snap.Items[h.id] = it
		h.src.inner.Items[h.id] = it
		h.starter.Cfg.StartPrompt = "{title}"
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		if h.herdr.promptText != UntrustedInputRule+"\n\n{{trust_rule}}" {
			t.Fatalf("prompt = %q", h.herdr.promptText)
		}
	})
}

func TestTrustLiveRecheckAndForce(t *testing.T) {
	t.Parallel()

	t.Run("live unlocked issue refuses before claim even if snapshot was locked", func(t *testing.T) {
		t.Parallel()
		h := setupGitHubTrustHarness(t, config.TrustEnvUntrusted)
		// Snapshot has Locked: true, but live issue on backend was unlocked.
		live := h.src.inner.Items[h.id]
		live.Locked = false
		h.src.inner.Items[h.id] = live

		err := h.start(h.snap, okGate, applyOpt)
		if err == nil || !strings.Contains(err.Error(), `untrusted author "stranger" requires locked issue`) {
			t.Fatalf("err = %v, want locked issue refusal", err)
		}
		assertNoClaim(t, h.st, "github:widgets", h.id)
		assertCalls(t, h.log, []string{"Tabs", "Workspaces", "Get"})
	})

	t.Run("force does not bypass trust gating or live unlock check", func(t *testing.T) {
		t.Parallel()
		h := setupGitHubTrustHarness(t, config.TrustEnvUntrusted)
		live := h.src.inner.Items[h.id]
		live.Locked = false
		h.src.inner.Items[h.id] = live

		err := h.start(h.snap, okGate, forceOpt)
		if err == nil || !strings.Contains(err.Error(), `untrusted author "stranger" requires locked issue`) {
			t.Fatalf("err = %v, want locked issue refusal with --force", err)
		}
		assertNoClaim(t, h.st, "github:widgets", h.id)
		assertCalls(t, h.log, []string{"Tabs", "Workspaces", "Get"})
	})

	t.Run("force does not bypass untrusted Backlog gating", func(t *testing.T) {
		t.Parallel()
		h := setupGitHubTrustHarness(t, config.TrustEnvUntrusted)
		it := h.snap.Items[h.id]
		it.State = "Backlog"
		h.snap.Items[h.id] = it
		h.src.inner.Items[h.id] = it

		err := h.start(h.snap, okGate, forceOpt)
		if err == nil || !strings.Contains(err.Error(), `untrusted author "stranger" requires status in [Todo]`) {
			t.Fatalf("err = %v, want status refusal with --force", err)
		}
		assertNoClaim(t, h.st, "github:widgets", h.id)
	})

	t.Run("own interrupted claim in In progress still enforces locked guard", func(t *testing.T) {
		t.Parallel()
		h := setupGitHubTrustHarness(t, config.TrustEnvUntrusted)
		it := h.snap.Items[h.id]
		it.State = config.DefaultGitHubClaimStatus
		it.Assignee = "maintainer"
		h.snap.Items[h.id] = it
		h.src.inner.Items[h.id] = it

		ctx := context.Background()
		if err := h.st.InsertClaim(ctx, store.Claim{
			Source:    "github:widgets",
			ID:        h.id,
			ClaimedAt: fixedNow.Add(-time.Minute),
			Machine:   "laptop",
			State:     store.Failed,
			Detail:    "launch failed",
			UpdatedAt: fixedNow.Add(-time.Minute),
		}); err != nil {
			t.Fatal(err)
		}

		// When locked, own claim restarts cleanly.
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatalf("restart locked own claim failed: %v", err)
		}
	})

	t.Run("own interrupted claim still fails if unlocked", func(t *testing.T) {
		t.Parallel()
		h := setupGitHubTrustHarness(t, config.TrustEnvUntrusted)
		it := h.snap.Items[h.id]
		it.State = config.DefaultGitHubClaimStatus
		it.Assignee = "maintainer"
		it.Locked = false
		h.snap.Items[h.id] = it
		h.src.inner.Items[h.id] = it

		ctx := context.Background()
		if err := h.st.InsertClaim(ctx, store.Claim{
			Source:    "github:widgets",
			ID:        h.id,
			ClaimedAt: fixedNow.Add(-time.Minute),
			Machine:   "laptop",
			State:     store.Failed,
			Detail:    "launch failed",
			UpdatedAt: fixedNow.Add(-time.Minute),
		}); err != nil {
			t.Fatal(err)
		}

		err := h.start(h.snap, okGate, applyOpt)
		if err == nil || !strings.Contains(err.Error(), `untrusted author "stranger" requires locked issue`) {
			t.Fatalf("err = %v, want locked issue refusal", err)
		}
	})
}
