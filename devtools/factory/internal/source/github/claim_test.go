package github

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
)

func boolPtr(v bool) *bool { return &v }

func TestClaimConfigurableBehavior(t *testing.T) {
	t.Run("assign false only adds label", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         false,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: true,
			InProgressRule: config.InProgressBlock,
			Label:          "in-progress",
		}
		want := src.editArgs(7, "--add-label", "in-progress")
		mock.ExpectCommandWithArgs(testBin, want...).WillSucceed("", 0).Once().Build()
		if err := src.Claim(context.Background(), "acme/widgets#7"); err != nil {
			t.Fatalf("Claim() error = %v", err)
		}
		assertArgs(t, mock, [][]string{want})
	})

	t.Run("mark_in_progress false only assigns me even without claim_label", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         true,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: false,
			InProgressRule: config.InProgressBlock,
			Label:          "",
		}
		want := src.editArgs(7, "--add-assignee", "@me")
		mock.ExpectCommandWithArgs(testBin, want...).WillSucceed("", 0).Once().Build()
		if err := src.Claim(context.Background(), "acme/widgets#7"); err != nil {
			t.Fatalf("Claim() error = %v", err)
		}
		assertArgs(t, mock, [][]string{want})
	})

	t.Run("both assign and mark_in_progress false is a no-op", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         false,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: false,
			InProgressRule: config.InProgressBlock,
			Label:          "in-progress",
		}
		if err := src.Claim(context.Background(), "acme/widgets#7"); err != nil {
			t.Fatalf("Claim() error = %v", err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})

	t.Run("ownership takeover removes prior non-me assignees", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         true,
			OwnershipRule:  config.OwnershipAny,
			MarkInProgress: true,
			InProgressRule: config.InProgressBlock,
			Label:          "in-progress",
		}
		src.cached = true
		src.login = "me-user"

		want := src.editArgs(
			7,
			"--add-label", "in-progress",
			"--add-assignee", "@me",
			"--remove-assignee", "alice",
			"--remove-assignee", "bob",
		)
		mock.ExpectCommandWithArgs(testBin, want...).WillSucceed("", 0).Once().Build()
		err := src.ClaimWithOptions(context.Background(), "acme/widgets#7", source.ClaimOptions{
			RemoveAssignees: []string{"alice", "@me", "me-user", "ALICE", "", "bob"},
		})
		if err != nil {
			t.Fatalf("ClaimWithOptions() error = %v", err)
		}
		assertArgs(t, mock, [][]string{want})
	})

	t.Run("create_label true creates missing label and retries", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         true,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: true,
			InProgressRule: config.InProgressBlock,
			CreateLabel:    true,
			Label:          "in-progress",
		}
		edit := src.editArgs(7, "--add-label", "in-progress", "--add-assignee", "@me")
		create := []string{
			"label", "create", "in-progress",
			"--description", "Work is actively underway",
			"--color", "1D76DB",
			"-R", "acme/widgets",
		}
		mock.ExpectCommandWithArgs(testBin, edit...).
			WillFail("could not add label: 'in-progress' not found", 1).Once().Build()
		mock.ExpectCommandWithArgs(testBin, create...).WillSucceed("", 0).Once().Build()
		mock.ExpectCommandWithArgs(testBin, edit...).WillSucceed("", 0).Once().Build()

		if err := src.Claim(context.Background(), "acme/widgets#7"); err != nil {
			t.Fatalf("Claim() error = %v", err)
		}
		assertArgs(t, mock, [][]string{edit, create, edit})
	})

	t.Run("create_label true reports create failure", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         true,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: true,
			InProgressRule: config.InProgressBlock,
			CreateLabel:    true,
			Label:          "in-progress",
		}
		edit := src.editArgs(7, "--add-label", "in-progress", "--add-assignee", "@me")
		create := []string{
			"label", "create", "in-progress",
			"--description", "Work is actively underway",
			"--color", "1D76DB",
			"-R", "acme/widgets",
		}
		mock.ExpectCommandWithArgs(testBin, edit...).
			WillFail("could not add label: 'in-progress' not found", 1).Once().Build()
		mock.ExpectCommandWithArgs(testBin, create...).
			WillFail("HTTP 403: Forbidden", 1).Once().Build()

		err := src.Claim(context.Background(), "acme/widgets#7")
		if err == nil || !strings.Contains(err.Error(), `create label "in-progress"`) || !strings.Contains(err.Error(), "403") {
			t.Fatalf("Claim() error = %v", err)
		}
		assertArgs(t, mock, [][]string{edit, create})
	})

	t.Run("create_label false keeps existing missing label error", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         true,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: true,
			InProgressRule: config.InProgressBlock,
			CreateLabel:    false,
			Label:          "in-progress",
		}
		edit := src.editArgs(7, "--add-label", "in-progress", "--add-assignee", "@me")
		mock.ExpectCommandWithArgs(testBin, edit...).
			WillFail("could not add label: 'in-progress' not found", 1).Once().Build()

		err := src.Claim(context.Background(), "acme/widgets#7")
		const want = `create the "in-progress" label in acme/widgets first`
		if err == nil || !strings.Contains(err.Error(), want) {
			t.Fatalf("Claim() error = %v, want %s", err, want)
		}
		assertArgs(t, mock, [][]string{edit})
	})

	t.Run("claim options override config", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         true,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: true,
			InProgressRule: config.InProgressBlock,
			Label:          "in-progress",
		}
		want := src.editArgs(7, "--add-label", "in-progress")
		mock.ExpectCommandWithArgs(testBin, want...).WillSucceed("", 0).Once().Build()
		err := src.ClaimWithOptions(context.Background(), "acme/widgets#7", source.ClaimOptions{
			Assign: boolPtr(false),
		})
		if err != nil {
			t.Fatalf("ClaimWithOptions() error = %v", err)
		}
		assertArgs(t, mock, [][]string{want})
	})
}

func TestReleaseMirrorsClaimConfig(t *testing.T) {
	t.Run("assign false only removes label", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         false,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: true,
			InProgressRule: config.InProgressBlock,
			Label:          "in-progress",
		}
		want := src.editArgs(7, "--remove-label", "in-progress")
		mock.ExpectCommandWithArgs(testBin, want...).WillSucceed("", 0).Once().Build()
		if err := src.Release(context.Background(), "acme/widgets#7"); err != nil {
			t.Fatalf("Release() error = %v", err)
		}
		assertArgs(t, mock, [][]string{want})
	})

	t.Run("mark_in_progress false only removes assignee", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         true,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: false,
			InProgressRule: config.InProgressBlock,
			Label:          "",
		}
		want := src.editArgs(7, "--remove-assignee", "@me")
		mock.ExpectCommandWithArgs(testBin, want...).WillSucceed("", 0).Once().Build()
		if err := src.Release(context.Background(), "acme/widgets#7"); err != nil {
			t.Fatalf("Release() error = %v", err)
		}
		assertArgs(t, mock, [][]string{want})
	})

	t.Run("both disabled is a no-op", func(t *testing.T) {
		mock := newMock()
		src := newTestSource(mock)
		src.cfg.Claim = config.Claim{
			Assign:         false,
			OwnershipRule:  config.OwnershipUnassignedOrMe,
			MarkInProgress: false,
			InProgressRule: config.InProgressBlock,
		}
		if err := src.Release(context.Background(), "acme/widgets#7"); err != nil {
			t.Fatalf("Release() error = %v", err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
}

func TestPostClaimComment(t *testing.T) {
	t.Run("posts comment", func(t *testing.T) {
		mock := newMock()
		body := "Claimed by claude on mac1 at 2026-11-24T00:00:00Z"
		want := []string{"issue", "comment", "7", "-R", "acme/widgets", "--body", body}
		mock.ExpectCommandWithArgs(testBin, want...).WillSucceed("", 0).Once().Build()
		if err := newTestSource(mock).PostClaimComment(context.Background(), "acme/widgets#7", body); err != nil {
			t.Fatalf("PostClaimComment() error = %v", err)
		}
		assertArgs(t, mock, [][]string{want})
	})

	t.Run("rejects nil context", func(t *testing.T) {
		var ctx context.Context
		err := newTestSource(newMock()).PostClaimComment(ctx, "acme/widgets#7", "Claimed by claude")
		if err == nil || !strings.Contains(err.Error(), "found nil") {
			t.Fatalf("PostClaimComment() error = %v", err)
		}
	})

	t.Run("rejects empty body", func(t *testing.T) {
		mock := newMock()
		err := newTestSource(mock).PostClaimComment(context.Background(), "acme/widgets#7", "   \n")
		if err == nil || !strings.Contains(err.Error(), "found empty") {
			t.Fatalf("PostClaimComment() error = %v", err)
		}
		if len(mock.Executions()) != 0 {
			t.Fatalf("executions = %d, want 0", len(mock.Executions()))
		}
	})
}

func TestLatestClaimComment(t *testing.T) {
	viewArgs := []string{"issue", "view", "7", "-R", "acme/widgets", "--json", "comments"}

	t.Run("returns latest matching claim comment", func(t *testing.T) {
		mock := newMock()
		payload := mustJSON(t, map[string]any{
			"comments": []map[string]any{
				{
					"author":    map[string]any{"login": "alice"},
					"body":      "Claimed by claude on mac1 at 2026-11-24T01:00:00Z",
					"createdAt": "2026-11-24T01:00:00Z",
				},
				{
					"author":    map[string]any{"login": "bob"},
					"body":      "Looks good to me!",
					"createdAt": "2026-11-24T02:00:00Z",
				},
				{
					"author":    map[string]any{"login": "carol"},
					"body":      "  Claimed by claude on mac2 at 2026-11-24T03:00:00Z",
					"createdAt": "2026-11-24T03:00:00Z",
				},
			},
		})
		mock.ExpectCommandWithArgs(testBin, viewArgs...).WillSucceed(payload, 0).Once().Build()
		got, err := newTestSource(mock).LatestClaimComment(context.Background(), "acme/widgets#7")
		if err != nil {
			t.Fatalf("LatestClaimComment() error = %v", err)
		}
		if got == nil {
			t.Fatal("LatestClaimComment() = nil, want comment")
			return
		}
		wantTime := time.Date(2026, 11, 24, 3, 0, 0, 0, time.UTC)
		if got.Author != "carol" || !got.CreatedAt.Equal(wantTime) || !strings.Contains(got.Body, "mac2") {
			t.Fatalf("LatestClaimComment() = %+v", got)
		}
		assertArgs(t, mock, [][]string{viewArgs})
	})

	t.Run("returns nil when no claim comment exists", func(t *testing.T) {
		mock := newMock()
		payload := mustJSON(t, map[string]any{
			"comments": []map[string]any{
				{
					"author":    nil,
					"body":      "Just a regular discussion comment",
					"createdAt": "2026-11-24T01:00:00Z",
				},
			},
		})
		mock.ExpectCommandWithArgs(testBin, viewArgs...).WillSucceed(payload, 0).Once().Build()
		got, err := newTestSource(mock).LatestClaimComment(context.Background(), "acme/widgets#7")
		if err != nil {
			t.Fatalf("LatestClaimComment() error = %v", err)
		}
		if got != nil {
			t.Fatalf("LatestClaimComment() = %+v, want nil", got)
		}
	})

	t.Run("rejects malformed comments payload", func(t *testing.T) {
		mock := newMock()
		mock.ExpectCommandWithArgs(testBin, viewArgs...).WillSucceed(`{"comments":null}`, 0).Once().Build()
		if _, err := newTestSource(mock).LatestClaimComment(context.Background(), "acme/widgets#7"); err == nil || !strings.Contains(err.Error(), "comments") {
			t.Fatalf("LatestClaimComment() error = %v", err)
		}
	})

	t.Run("rejects bad createdAt timestamp", func(t *testing.T) {
		mock := newMock()
		payload := `{"comments":[{"author":{"login":"a"},"body":"Claimed by x","createdAt":"not-a-time"}]}`
		mock.ExpectCommandWithArgs(testBin, viewArgs...).WillSucceed(payload, 0).Once().Build()
		if _, err := newTestSource(mock).LatestClaimComment(context.Background(), "acme/widgets#7"); err == nil || !strings.Contains(err.Error(), "RFC3339") {
			t.Fatalf("LatestClaimComment() error = %v", err)
		}
	})
}
