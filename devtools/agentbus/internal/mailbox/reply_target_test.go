package mailbox

import (
	"context"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
)

func replyBox(t *testing.T, state adapter.State) *Mailbox {
	t.Helper()
	box, _ := openAmbiguous(t, &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{ambiguousHandle: state},
		PaneHandles: map[string]string{"p1": ambiguousHandle},
	})
	return box
}

func replyTo(t *testing.T, box *Mailbox, orig message) (message, error) {
	t.Helper()
	orig.ID, orig.Replyable = "m-orig", true
	box.msgs = map[string]message{"m-orig": orig}
	res, err := box.Reply(context.Background(), []string{"HERDR_PANE_ID=p1"}, 0, "m-orig", "re", Options{})
	if err != nil {
		return message{}, err
	}
	return box.msgs[res.ID], nil
}

func TestReplyToSenderWithTwoNamesUsesFirstName(t *testing.T) {
	box := replyBox(t, adapter.StateIdle)
	got, err := replyTo(t, box, message{From: "a/x, ops/x", FromNames: []string{"a/x", "ops/x"}, SenderHandle: ambiguousHandle})
	if err != nil {
		t.Fatal(err)
	}
	// a/x is unregistered, so the first resolving name is ops/x.
	if got.To != "ops/x" || got.Handle != ambiguousHandle {
		t.Fatalf("To=%q Handle=%q", got.To, got.Handle)
	}
}

func TestReplyUsesFirstRegisteredOfTwoNames(t *testing.T) {
	box := replyBox(t, adapter.StateIdle)
	if _, err := box.reg.Register(context.Background(), []string{"HERDR_PANE_ID=p1"}, "ops/y", "reviews"); err != nil {
		t.Fatal(err)
	}
	got, err := replyTo(t, box, message{From: "ops/x, ops/y", FromNames: []string{"ops/x", "ops/y"}})
	if err != nil {
		t.Fatal(err)
	}
	if got.To != "ops/x" {
		t.Fatalf("To = %q, want ops/x", got.To)
	}
}

func TestReplyToRawHandleSenderUsesSenderHandle(t *testing.T) {
	box := replyBox(t, adapter.StateIdle)
	got, err := replyTo(t, box, message{From: "herdr:other", SenderHandle: ambiguousHandle})
	if err != nil {
		t.Fatal(err)
	}
	if got.To != ambiguousHandle || got.Handle != ambiguousHandle {
		t.Fatalf("To=%q Handle=%q", got.To, got.Handle)
	}
}

func TestReplyToLegacyJoinedFrom(t *testing.T) {
	box := replyBox(t, adapter.StateIdle)
	got, err := replyTo(t, box, message{From: "a/x, ops/x"})
	if err != nil {
		t.Fatal(err)
	}
	if got.To != "ops/x" {
		t.Fatalf("To = %q, want ops/x", got.To)
	}
}

func TestReplyFailsListingNamesWhenHandleGone(t *testing.T) {
	box := replyBox(t, adapter.StateGone)
	_, err := replyTo(t, box, message{From: "a/x, b/x", FromNames: []string{"a/x", "b/x"}, SenderHandle: "herdr:local/s9/w9:p9"})
	if err == nil || !strings.Contains(err.Error(), "a/x, b/x") {
		t.Fatalf("err = %v, want names tried", err)
	}
}
