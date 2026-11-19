package mailbox

import (
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/adapter/fake"
)

func TestRepliesTellTheAddressedNameAndItsHolderFromOthers(t *testing.T) {
	box, _ := openAmbiguous(t, &fake.Adapter{
		AdapterName: "herdr",
		Reach:       map[string]adapter.State{ambiguousHandle: adapter.StateIdle},
		PaneHandles: map[string]string{"p1": ambiguousHandle},
	})
	at := time.Date(2026, 11, 16, 12, 0, 0, 0, time.UTC)
	box.msgs = map[string]message{
		"m-1": {ID: "m-1", From: "worker", To: "ops/x", SentAt: at, Body: "which way?"},
		"m-2": {ID: "m-2", From: "intern", SenderHandle: "herdr:local/s1/w2:p9", Re: "m-1", SentAt: at.Add(time.Minute), Body: "ship it"},
		"m-3": {ID: "m-3", From: "lead, ops/x", SenderHandle: "herdr:local/s1/w3:p7", Re: "m-1", SentAt: at.Add(2 * time.Minute), Body: "old API"},
		"m-4": {ID: "m-4", From: "herdr:x", SenderHandle: ambiguousHandle, Re: "m-1", SentAt: at.Add(3 * time.Minute), Body: "new API"},
		"m-5": {ID: "m-5", From: "ops/x", Re: "m-9", SentAt: at, Body: "elsewhere"},
	}
	to, err := box.Recipient("m-1")
	if err != nil || to != "ops/x" {
		t.Fatalf("Recipient = %q, %v", to, err)
	}
	got, err := box.Replies("m-1")
	if err != nil {
		t.Fatal(err)
	}
	want := []Answer{
		{ID: "m-2", From: "intern", Text: "ship it"},
		{ID: "m-3", From: "lead, ops/x", Text: "old API", Addressed: true},
		{ID: "m-4", From: "herdr:x", Text: "new API", Addressed: true},
	}
	if len(got) != len(want) {
		t.Fatalf("Replies = %+v, want %+v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Errorf("reply %d = %+v, want %+v", i, got[i], want[i])
		}
	}
	if _, err := box.Replies("m-404"); err == nil {
		t.Fatal("replies to an unknown message")
	}
}
