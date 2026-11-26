package start

import (
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func TestGitHubNativeStatusStartAndSafety(t *testing.T) {
	t.Parallel()

	t.Run("dry run and apply confirm native GitHub status", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.useGitHub(false)
		h.src.inner.ClaimStatus = config.DefaultGitHubClaimStatus
		h.starter.Cfg.ClaimStatus = config.DefaultGitHubClaimStatus
		h.starter.Cfg.ClaimLabel = ""
		h.starter.Cfg.ReadyStatuses = config.DefaultGitHubReadyStatuses()
		h.starter.Cfg.Claim = fullClaim(config.DefaultGitHubClaimStatus, "")

		it := h.snap.Items[h.id]
		it.State = "Todo"
		h.snap.Items[h.id] = it
		h.src.inner.Items[h.id] = it

		if err := h.start(h.snap, okGate, Options{}); err != nil {
			t.Fatal(err)
		}
		if !strings.Contains(h.out.String(), "acme/widgets#12 → In progress (assign to me)") {
			t.Fatalf("dry-run plan =\n%s", h.out.String())
		}

		h.out.Reset()
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		if got := h.src.inner.Items[h.id].State; got != "In progress" {
			t.Fatalf("state = %q, want In progress", got)
		}
		assertStarted(t, h.st, "github:experimental", h.id, "tg", "wg")
	})

	t.Run("already in progress GitHub native status blocks and --force-taking-ownership succeeds", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.useGitHub(false)
		h.src.inner.ClaimStatus = config.DefaultGitHubClaimStatus
		h.starter.Cfg.ClaimStatus = config.DefaultGitHubClaimStatus
		h.starter.Cfg.ClaimLabel = ""
		h.starter.Cfg.ReadyStatuses = config.DefaultGitHubReadyStatuses()
		h.starter.Cfg.Claim = fullClaim(config.DefaultGitHubClaimStatus, "")

		it := h.snap.Items[h.id]
		it.State = "In progress"
		h.snap.Items[h.id] = it
		h.src.inner.Items[h.id] = it

		for _, opts := range []Options{applyOpt, forceOpt} {
			if err := h.start(h.snap, okGate, opts); err == nil || !strings.Contains(err.Error(), "In progress") {
				t.Fatalf("opts=%+v error = %v", opts, err)
			}
		}

		if err := h.start(h.snap, okGate, Options{Apply: true, ForceTakeOwnership: true}); err != nil {
			t.Fatal(err)
		}
		assertStarted(t, h.st, "github:experimental", h.id, "tg", "wg")
	})

	t.Run("in review status is not ready by default", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.useGitHub(false)
		h.src.inner.ClaimStatus = config.DefaultGitHubClaimStatus
		h.starter.Cfg.ClaimStatus = config.DefaultGitHubClaimStatus
		h.starter.Cfg.ClaimLabel = ""
		h.starter.Cfg.ReadyStatuses = config.DefaultGitHubReadyStatuses()
		h.starter.Cfg.Claim = fullClaim(config.DefaultGitHubClaimStatus, "")

		it := h.snap.Items[h.id]
		it.State = "In review"
		h.snap.Items[h.id] = it
		h.src.inner.Items[h.id] = it

		err := h.start(h.snap, okGate, applyOpt)
		if err == nil || !strings.Contains(err.Error(), `status "In review" not in ready_statuses`) {
			t.Fatalf("error = %v", err)
		}
	})
}
