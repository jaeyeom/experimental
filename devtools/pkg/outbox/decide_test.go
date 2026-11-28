package outbox

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/chorong-sh/chorong/decide"
)

func TestDecideCLIAskWithLegacyFallback(t *testing.T) {
	var calls [][]string
	run := func(_ context.Context, _ []byte, args ...string) ([]byte, error) {
		calls = append(calls, slices.Clone(args))
		if slices.Contains(args, "--long-lines") {
			return nil, errors.New("exit status 2: flag provided but not defined: -long-lines")
		}
		if slices.Contains(args, "--owner-replies-only") {
			return nil, errors.New("exit status 2: flag provided but not defined: -owner-replies-only")
		}
		return []byte(`{"id":"D-11"}`), nil
	}
	cli := DecideCLI{Run: run}
	id, err := cli.Ask(context.Background(), AskRequest{
		Owner:            "worker-1",
		NotifyTarget:     "target-1",
		Class:            "approval",
		OwnerRepliesOnly: true,
		LongLines:        true,
		Options: []decide.Option{
			{Key: "post", Label: "Post", Recommended: true},
			{Key: "cancel", Label: "Cancel"},
		},
		FreeText: true,
		Verbatim: "exact body",
		Question: "Approve action?",
	})
	if err != nil || id != "D-11" {
		t.Fatalf("Ask = %q, %v", id, err)
	}
	if len(calls) != 3 {
		t.Fatalf("calls = %d, want 3 (retry -long-lines then -owner-replies-only): %v", len(calls), calls)
	}
	last := calls[2]
	if slices.Contains(last, "--long-lines") || slices.Contains(last, "--owner-replies-only") {
		t.Fatalf("retried call still had removed flags: %v", last)
	}
	if !slices.Contains(last, "--class") || !slices.Contains(last, "--verbatim") {
		t.Fatalf("missing expected flags: %v", last)
	}
}

func TestEvaluateApprovalProvenance(t *testing.T) {
	policy := DefaultEvalPolicy()
	cases := []struct {
		name string
		item decide.Item
		want Action
	}{
		{
			name: "no human messages",
			item: decide.Item{ID: "D-1", State: decide.StateOpen},
			want: ActionPending,
		},
		{
			name: "cli post rejected in default policy as pending",
			item: decide.Item{
				ID: "D-1", State: decide.StateAnswered,
				Answer:   &decide.Answer{Key: "post", Value: "post", AnsweredBy: decide.Human, Via: decide.ViaCLI},
				Messages: []decide.Message{{Author: decide.Human, OptionKey: "post", Via: decide.ViaCLI}},
			},
			want: ActionPending,
		},
		{
			name: "agent acting as human via ui ignored",
			item: decide.Item{
				ID: "D-1", State: decide.StateOpen,
				Answer:   &decide.Answer{Key: "post", Value: "post", AnsweredBy: decide.Human, Via: decide.ViaUI},
				Messages: []decide.Message{{Author: decide.Human, OptionKey: "post", Via: decide.ViaUI, By: "worker/a"}},
			},
			want: ActionPending,
		},
		{
			name: "real human ui post approved",
			item: decide.Item{
				ID: "D-1", State: decide.StateAnswered,
				Answer:   &decide.Answer{Key: "post", Value: "post", AnsweredBy: decide.Human, Via: decide.ViaUI},
				Messages: []decide.Message{{Author: decide.Human, OptionKey: "post", Via: decide.ViaUI}},
			},
			want: ActionApprove,
		},
		{
			name: "later free text overrides earlier approve",
			item: decide.Item{
				ID: "D-1", State: decide.StateAnswered,
				Answer: &decide.Answer{Value: "tighten wording", AnsweredBy: decide.Human, Via: decide.ViaUI},
				Messages: []decide.Message{
					{Author: decide.Human, OptionKey: "post", Via: decide.ViaUI},
					{Author: decide.Human, Text: "tighten wording", Via: decide.ViaUI},
				},
			},
			want: ActionEdit,
		},
		{
			name: "withdrawn cancels even after ui approve",
			item: decide.Item{
				ID: "D-1", State: decide.StateWithdrawn,
				Answer:   &decide.Answer{Key: "post", Value: "post", AnsweredBy: decide.Human, Via: decide.ViaUI},
				Messages: []decide.Message{{Author: decide.Human, OptionKey: "post", Via: decide.ViaUI}},
			},
			want: ActionClosedWithoutApproval,
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			v := EvaluateApproval(tc.item, policy)
			if v.Action != tc.want {
				t.Fatalf("Action = %s, want %s (verdict=%+v)", v.Action, tc.want, v)
			}
		})
	}
}

func TestEvaluateApprovalStrictPolicy(t *testing.T) {
	base := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	policy := EvalPolicy{
		ApproveKeys:         []string{"yes"},
		EditKeys:            []string{"edit"},
		CancelKeys:          []string{"no"},
		RejectNonUIApprove:  true,
		RejectUnknownKey:    true,
		CancelClosedEdits:   true,
		RequireBeforeDoneAt: true,
		UnknownKeyMessageFn: func(k string) string { return "bad option " + k },
	}

	v := EvaluateApproval(decide.Item{
		State:    decide.StateOpen,
		Messages: []decide.Message{{Author: decide.Human, OptionKey: "yes", Via: decide.ViaCLI}},
	}, policy)
	if v.Action != ActionRejected || !strings.Contains(v.Refusal, `came via "cli"`) {
		t.Fatalf("non-ui approve = %+v", v)
	}

	v = EvaluateApproval(decide.Item{
		State:    decide.StateOpen,
		Messages: []decide.Message{{Author: decide.Human, OptionKey: "other", Via: decide.ViaUI}},
	}, policy)
	if v.Action != ActionRejected || v.Refusal != "bad option other" {
		t.Fatalf("unknown key = %+v", v)
	}

	v = EvaluateApproval(decide.Item{
		State:    decide.StateDone,
		DoneAt:   base,
		Messages: []decide.Message{{Author: decide.Human, OptionKey: "yes", Via: decide.ViaUI, Time: base}},
	}, policy)
	if v.Action != ActionClosedWithoutApproval || !strings.Contains(v.Refusal, "before") {
		t.Fatalf("equal DoneAt = %+v", v)
	}

	v = EvaluateApproval(decide.Item{
		State:    decide.StateDoneUnread,
		DoneAt:   base.Add(time.Second),
		Messages: []decide.Message{{Author: decide.Human, OptionKey: "yes", Via: decide.ViaUI, Time: base}},
	}, policy)
	if v.Action != ActionApprove {
		t.Fatalf("before DoneAt = %+v, want ActionApprove", v)
	}
}
