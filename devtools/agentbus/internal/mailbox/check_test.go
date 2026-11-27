package mailbox

import (
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
)

func TestHeldMessagesFiltersByAgeReasonAndTo(t *testing.T) {
	now := time.Date(2026, 11, 27, 4, 0, 0, 0, time.UTC)
	ad := &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{"herdr:local/s1/w1:p1": adapter.StateIdle, "herdr:local/s1/w1:p2": adapter.StateIdle},
		PaneHandles: map[string]string{"p1": "herdr:local/s1/w1:p1", "p2": "herdr:local/s1/w1:p2"},
	}
	box, _ := openAmbiguous(t, ad)
	box.now = func() time.Time { return now }
	if _, err := box.reg.Register(t.Context(), []string{"HERDR_PANE_ID=p1"}, "ops/inbox", "inbox"); err != nil {
		t.Fatal(err)
	}
	if _, err := box.reg.Register(t.Context(), []string{"HERDR_PANE_ID=p2"}, "ops/reviewer", "review"); err != nil {
		t.Fatal(err)
	}

	oldDraft := now.Add(-20 * time.Minute)
	newOrder := now.Add(-2 * time.Minute)
	oldAuth := now.Add(-15 * time.Minute)
	box.msgs = map[string]message{
		"m-draft": {
			ID: "m-draft", From: "worker", To: "ops/inbox", Handle: "herdr:local/s1/w1:p1",
			SentAt: oldDraft, HeldAt: &oldDraft, State: "held: " + adapter.HoldDraft,
		},
		"m-order": {
			ID: "m-order", From: "worker", To: "ops/inbox", Handle: "herdr:local/s1/w1:p1",
			SentAt: newOrder, HeldAt: &newOrder, State: "held: waiting for an earlier message m-draft",
		},
		"m-auth": {
			ID: "m-auth", From: "worker", To: "ops/reviewer", Handle: "herdr:local/s1/w1:p2",
			SentAt: oldAuth, HeldAt: &oldAuth, State: "held: " + HoldLoginExpired,
		},
		"m-delivered": {
			ID: "m-delivered", From: "worker", To: "ops/inbox", Handle: "herdr:local/s1/w1:p1",
			SentAt: oldDraft, State: stateDelivered,
		},
	}

	all := box.HeldMessages(HeldFilter{})
	if len(all) != 3 {
		t.Fatalf("HeldMessages(all) = %+v, want 3", all)
	}
	if all[0].ID != "m-draft" || all[0].ReasonCode != "draft" || all[1].ID != "m-auth" || all[1].ReasonCode != "login-expired" || all[2].ID != "m-order" || all[2].ReasonCode != "ordering" {
		t.Fatalf("order/codes = %+v", all)
	}

	older := box.HeldMessages(HeldFilter{OlderThan: 10 * time.Minute})
	if len(older) != 2 || older[0].ID != "m-draft" || older[1].ID != "m-auth" {
		t.Fatalf("OlderThan 10m = %+v", older)
	}

	noDraft := box.HeldMessages(HeldFilter{NotReasons: []string{"draft"}})
	if len(noDraft) != 2 || noDraft[0].ID != "m-auth" || noDraft[1].ID != "m-order" {
		t.Fatalf("NotReasons draft = %+v", noDraft)
	}

	onlyOrder := box.HeldMessages(HeldFilter{Reasons: []string{"ordering"}})
	if len(onlyOrder) != 1 || onlyOrder[0].ID != "m-order" {
		t.Fatalf("Reasons ordering = %+v", onlyOrder)
	}

	toReviewer := box.HeldMessages(HeldFilter{To: "ops/reviewer"})
	if len(toReviewer) != 1 || toReviewer[0].ID != "m-auth" {
		t.Fatalf("To ops/reviewer = %+v", toReviewer)
	}
}

func TestAsksRulesReplyClearingAndAge(t *testing.T) {
	now := time.Date(2026, 11, 27, 4, 0, 0, 0, time.UTC)
	box, _ := openAmbiguous(t, ambiguousFake())
	box.now = func() time.Time { return now }
	box.SetExpectReplySenders([]string{"coord/*"})

	box.msgs = map[string]message{
		"m-explicit": {
			ID: "m-explicit", From: "worker", FromNames: []string{"worker"}, To: "ops/inbox",
			SentAt: now.Add(-15 * time.Minute), Body: "Please confirm receipt\nMore details",
			State: stateDelivered, ExpectReply: true,
		},
		"m-question": {
			ID: "m-question", From: "lead", FromNames: []string{"lead"}, To: "ops/reviewer",
			SentAt: now.Add(-3 * time.Minute), Body: "Have you finished reviewing?\n",
			State: stateDelivered,
		},
		"m-answered": {
			ID: "m-answered", From: "worker", FromNames: []string{"worker"}, To: "ops/inbox",
			SentAt: now.Add(-20 * time.Minute), Body: "Are you up?",
			State: stateDelivered,
		},
		"m-reply": {
			ID: "m-reply", From: "ops/inbox", To: "worker", Re: "m-answered",
			SentAt: now.Add(-19 * time.Minute), Body: "yes",
			State: stateDelivered,
		},
		"m-skill": {
			ID: "m-skill", From: "coord/main", FromNames: []string{"coord/main"}, To: "ops/inbox",
			SentAt: now.Add(-10 * time.Minute), Body: "Check the build.",
			State: stateDelivered,
		},
		"m-fyi": {
			ID: "m-fyi", From: "worker", To: "ops/inbox",
			SentAt: now.Add(-30 * time.Minute), Body: "FYI build passed.",
			State: stateDelivered,
		},
		"m-held": {
			ID: "m-held", From: "worker", To: "ops/inbox",
			SentAt: now.Add(-30 * time.Minute), Body: "Still stuck?",
			State: "held: user draft in composer", ExpectReply: true,
		},
	}

	all := box.Asks(AskFilter{})
	if len(all) != 3 {
		t.Fatalf("Asks(all) = %+v, want 3", all)
	}
	if all[0].ID != "m-explicit" || all[0].Rule != "explicit" || all[0].FirstLine != "Please confirm receipt" {
		t.Fatalf("all[0] = %+v", all[0])
	}
	if all[1].ID != "m-skill" || all[1].Rule != "sender-skill" {
		t.Fatalf("all[1] = %+v", all[1])
	}
	if all[2].ID != "m-question" || all[2].Rule != "question-mark" {
		t.Fatalf("all[2] = %+v", all[2])
	}

	older := box.Asks(AskFilter{Unanswered: 5 * time.Minute})
	if len(older) != 2 || older[0].ID != "m-explicit" || older[1].ID != "m-skill" {
		t.Fatalf("Unanswered 5m = %+v", older)
	}

	fromWorker := box.Asks(AskFilter{From: "worker"})
	if len(fromWorker) != 1 || fromWorker[0].ID != "m-explicit" {
		t.Fatalf("From worker = %+v", fromWorker)
	}

	toReviewer := box.Asks(AskFilter{To: "ops/reviewer"})
	if len(toReviewer) != 1 || toReviewer[0].ID != "m-question" {
		t.Fatalf("To ops/reviewer = %+v", toReviewer)
	}
}
