package start

import (
	"context"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

func fullClaim(status, label string) config.Claim {
	return config.Claim{
		RequireLeaf:       true,
		RequireUnblocked:  true,
		SourceContainment: true,
		Assign:            true,
		OwnershipRule:     config.OwnershipUnassignedOrMe,
		MarkInProgress:    true,
		InProgressRule:    config.InProgressBlock,
		Comment:           true,
		Status:            status,
		Label:             label,
	}
}

func TestSafetyRequireLeaf(t *testing.T) {
	t.Parallel()

	t.Run("skip_types refused even with --force", func(t *testing.T) {
		t.Parallel()
		for _, opts := range []Options{applyOpt, forceOpt} {
			h := newHarness(t)
			h.starter.Cfg.SkipTypes = []string{"Epic"}
			it := jiraItem()
			it.Type = "Epic"
			h.snap.Items[it.ID] = it
			h.src.inner.Items[it.ID] = it
			err := h.start(h.snap, okGate, opts)
			if err == nil || !strings.Contains(err.Error(), `type "Epic" in skip_types`) {
				t.Fatalf("opts=%+v error = %v", opts, err)
			}
			assertCalls(t, h.log, nil)
		}
	})

	t.Run("open child refused even with --force", func(t *testing.T) {
		t.Parallel()
		for _, opts := range []Options{applyOpt, forceOpt} {
			h := newHarness(t)
			h.snap.Items["PRJ-124"] = source.Item{ID: "PRJ-124", Parent: h.id, Done: false}
			err := h.start(h.snap, okGate, opts)
			if err == nil || !strings.Contains(err.Error(), "has open child PRJ-124") {
				t.Fatalf("opts=%+v error = %v", opts, err)
			}
			assertCalls(t, h.log, nil)
		}
	})

	t.Run("require_leaf false allows skip_types and open children", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Cfg.SkipTypes = []string{"Epic"}
		h.starter.Cfg.Claim = fullClaim("In Progress", "")
		h.starter.Cfg.Claim.RequireLeaf = false
		h.starter.Cfg.Claim.Comment = false
		it := jiraItem()
		it.Type = "Epic"
		h.snap.Items[it.ID] = it
		h.snap.Items["PRJ-124"] = source.Item{ID: "PRJ-124", Parent: it.ID, Done: false}
		h.src.inner.Items[it.ID] = it
		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		assertStarted(t, h.st, "jira:acme", h.id, "w1:t1", "w1")
	})
}

func TestSafetyRequireUnblockedLive(t *testing.T) {
	t.Parallel()
	h := newHarness(t)
	h.snap.Items["PRJ-8"] = source.Item{ID: "PRJ-8", State: "To Do", Done: false}
	live := jiraItem()
	live.BlockedBy = []source.BlockRef{{ID: "PRJ-8"}}
	h.src.inner.Items[live.ID] = live

	err := h.start(h.snap, okGate, forceOpt)
	if err == nil || !strings.Contains(err.Error(), "blocker PRJ-8 is not done") {
		t.Fatalf("error = %v", err)
	}
	assertCalls(t, h.log, []string{"Tabs", "Get"})
	assertNoClaim(t, h.st, "jira:acme", h.id)
}

func TestSafetyOwnershipRuleAndTakeover(t *testing.T) {
	t.Parallel()

	t.Run("foreign assignee blocks and --force-taking-ownership succeeds with RemoveAssignees", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.useGitHub(true)
		h.starter.Cfg.Claim = fullClaim("", "in-progress")
		it := h.snap.Items[h.id]
		it.Assignee = "bob"
		h.snap.Items[h.id] = it
		h.src.inner.Items[h.id] = it

		// Blocked by default and with --force.
		for _, opts := range []Options{applyOpt, forceOpt} {
			err := h.start(h.snap, okGate, opts)
			if err == nil || !strings.Contains(err.Error(), "bob") {
				t.Fatalf("opts=%+v error = %v", opts, err)
			}
		}

		// Dry-run with --force-taking-ownership prints takeover warning and claim comment preview.
		h.out.Reset()
		if err := h.start(h.snap, okGate, Options{ForceTakeOwnership: true}); err != nil {
			t.Fatal(err)
		}
		if !strings.Contains(h.out.String(), `warning: --force-taking-ownership takes over acme/widgets#12 (assignee "bob")`) {
			t.Fatalf("dry-run output =\n%s", h.out.String())
		}
		if !strings.Contains(h.out.String(), "comment  Claimed by claude at <timestamp>.\n") {
			t.Fatalf("dry-run output missing comment preview:\n%s", h.out.String())
		}

		// Apply with --force-taking-ownership removes prior assignee and claims for me.
		h.out.Reset()
		if err := h.start(h.snap, okGate, Options{Apply: true, ForceTakeOwnership: true}); err != nil {
			t.Fatal(err)
		}
		gotOpts := h.src.inner.LastClaimOptions[h.id]
		if !slices.Equal(gotOpts.RemoveAssignees, []string{"bob"}) {
			t.Fatalf("RemoveAssignees = %#v, want [bob]", gotOpts.RemoveAssignees)
		}
		if h.src.inner.Items[h.id].Assignee != "ada" {
			t.Fatalf("assignee = %q, want ada", h.src.inner.Items[h.id].Assignee)
		}
		assertStarted(t, h.st, "github:experimental", h.id, "tg", "wg")
	})

	t.Run("ownership_rule unassigned blocks even me", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Cfg.AssigneeOK = "unassigned"
		h.starter.Cfg.Claim = fullClaim("In Progress", "")
		h.starter.Cfg.Claim.OwnershipRule = config.OwnershipUnassigned
		it := jiraItem()
		it.Assignee = "ada"
		h.snap.Items[it.ID] = it
		h.src.inner.Items[it.ID] = it

		err := h.start(h.snap, okGate, forceOpt)
		if err == nil || !strings.Contains(err.Error(), `expected assignee empty, found "ada"`) {
			t.Fatalf("error = %v", err)
		}
		if err := h.start(h.snap, okGate, Options{Apply: true, ForceTakeOwnership: true}); err != nil {
			t.Fatal(err)
		}
	})
}

func TestSafetyInProgressAndOwnClaim(t *testing.T) {
	t.Parallel()

	t.Run("github claim_label blocks by default and succeeds with --force-taking-ownership", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.useGitHub(true)
		h.starter.Cfg.Claim = fullClaim("", "in-progress")
		it := h.snap.Items[h.id]
		it.Labels = []string{"in-progress"}
		h.snap.Items[h.id] = it
		h.src.inner.Items[h.id] = it

		for _, opts := range []Options{applyOpt, forceOpt} {
			if err := h.start(h.snap, okGate, opts); err == nil || !strings.Contains(err.Error(), "in-progress") {
				t.Fatalf("opts=%+v error = %v", opts, err)
			}
		}

		if err := h.start(h.snap, okGate, Options{Apply: true, ForceTakeOwnership: true}); err != nil {
			t.Fatal(err)
		}
		assertStarted(t, h.st, "github:experimental", h.id, "tg", "wg")
	})

	t.Run("re-running on this machine's own failed claim succeeds and skips duplicate comment", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.useGitHub(true)
		h.starter.Cfg.Claim = fullClaim("", "in-progress")
		it := h.snap.Items[h.id]
		it.Assignee = "ada"
		it.Labels = []string{"in-progress"}
		h.snap.Items[h.id] = it
		h.src.inner.Items[h.id] = it

		// Seed a failed claim row owned by this machine ("laptop").
		if err := h.st.InsertClaim(context.Background(), store.Claim{
			Source:    "github:experimental",
			ID:        h.id,
			ClaimedAt: fixedNow.Add(-time.Minute),
			Machine:   "laptop",
			State:     store.Failed,
			UpdatedAt: fixedNow.Add(-time.Minute),
		}); err != nil {
			t.Fatal(err)
		}

		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		if len(h.src.inner.Comments[h.id]) != 0 {
			t.Fatalf("comments = %#v, want none when re-running own claim", h.src.inner.Comments[h.id])
		}
		assertStarted(t, h.st, "github:experimental", h.id, "tg", "wg")
	})
}

func TestSafetyForeignNicknameComment(t *testing.T) {
	t.Parallel()

	h := newHarness(t)
	h.useGitHub(true)
	h.starter.Cfg.Claim = fullClaim("", "in-progress")
	h.starter.Cfg.Claim.InProgressRule = config.InProgressAllow
	h.starter.Cfg.Claim.Nickname = "mac-studio"
	it := h.snap.Items[h.id]
	it.Assignee = "ada"
	it.Labels = []string{"in-progress"}
	h.snap.Items[h.id] = it
	h.src.inner.Items[h.id] = it
	h.src.inner.ClaimComments = map[string][]source.ClaimComment{
		h.id: {{
			Author:    "ada",
			CreatedAt: fixedNow.Add(-10 * time.Minute),
			Body:      "Claimed by claude on work-mbp at 2026-11-01T11:50:00Z.",
		}},
	}

	// Foreign nickname blocks even with --force.
	for _, opts := range []Options{applyOpt, forceOpt} {
		err := h.start(h.snap, okGate, opts)
		if err == nil || !strings.Contains(err.Error(), `machine "work-mbp"`) {
			t.Fatalf("opts=%+v error = %v", opts, err)
		}
	}

	// --force-taking-ownership succeeds.
	if err := h.start(h.snap, okGate, Options{Apply: true, ForceTakeOwnership: true}); err != nil {
		t.Fatal(err)
	}
	assertStarted(t, h.st, "github:experimental", h.id, "tg", "wg")
}

func TestClaimCommentRenderingAndPosting(t *testing.T) {
	t.Parallel()

	t.Run("jira default with nickname matches claims.claimed_at", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Cfg.Claim = fullClaim("In Progress", "")
		h.starter.Cfg.Claim.Nickname = "mac-studio"

		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		wantComment := "Claimed by claude on mac-studio at 2026-11-01T12:00:00Z."
		if got := h.src.inner.Comments[h.id]; !slices.Equal(got, []string{wantComment}) {
			t.Fatalf("comments = %#v, want [%q]", got, wantComment)
		}
		if !strings.Contains(h.out.String(), "comment  Claimed by claude on mac-studio at <timestamp>.\n") {
			t.Fatalf("plan =\n%s", h.out.String())
		}
		cl := assertStarted(t, h.st, "jira:acme", h.id, "w1:t1", "w1")
		if !strings.Contains(wantComment, cl.ClaimedAt.UTC().Format(time.RFC3339)) {
			t.Fatalf("comment %q does not match claimed_at %s", wantComment, cl.ClaimedAt.UTC().Format(time.RFC3339))
		}
	})

	t.Run("github custom template", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.useGitHub(true)
		h.starter.Cfg.Claim = fullClaim("", "in-progress")
		h.starter.Cfg.Claim.Nickname = "devbox"
		h.starter.Cfg.Claim.CommentTemplate = "Claimed by {agent} ({nickname}) for {id} in {source} @ {timestamp}"

		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		want := "Claimed by claude (devbox) for acme/widgets#12 in github:experimental @ 2026-11-01T12:00:00Z"
		if got := h.src.inner.Comments[h.id]; !slices.Equal(got, []string{want}) {
			t.Fatalf("comments = %#v, want [%q]", got, want)
		}
	})

	t.Run("comment false skips preview and posting", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Cfg.Claim = fullClaim("In Progress", "")
		h.starter.Cfg.Claim.Comment = false

		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		if strings.Contains(h.out.String(), "comment  ") {
			t.Fatalf("plan should omit comment line:\n%s", h.out.String())
		}
		if len(h.src.inner.Comments[h.id]) != 0 {
			t.Fatalf("comments = %#v, want none", h.src.inner.Comments[h.id])
		}
	})

	t.Run("mark_in_progress false and assign true", func(t *testing.T) {
		t.Parallel()
		h := newHarness(t)
		h.starter.Cfg.ClaimStatus = ""
		h.starter.Cfg.Claim = fullClaim("", "")
		h.starter.Cfg.Claim.MarkInProgress = false
		h.starter.Cfg.Claim.Comment = false

		if err := h.start(h.snap, okGate, applyOpt); err != nil {
			t.Fatal(err)
		}
		if !strings.Contains(h.out.String(), "claim    PRJ-123 (assign to me)\n") {
			t.Fatalf("plan =\n%s", h.out.String())
		}
		assertStarted(t, h.st, "jira:acme", h.id, "w1:t1", "w1")
	})
}
