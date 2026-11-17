package decide

import (
	"encoding/json"
	"errors"
	"strings"
	"testing"
	"time"
)

// stepClock is a ledger clock that moves one second per call, so each
// write lands after the one before it.
func stepClock(led *Ledger) {
	at := time.Date(2026, 11, 17, 9, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time {
		at = at.Add(time.Second)
		return at
	})
}

func unreadResult(t *testing.T) (*Ledger, Item) {
	t.Helper()
	led, _ := openTemp(t)
	stepClock(led)
	item, err := led.Create(&Item{Question: "deploy?", Kind: KindRelay, Owner: "ada"})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.MarkRead(item.ID); err != nil {
		t.Fatal(err)
	}
	if _, err := led.Reply(item.ID, "ada", "deployed to prod", nil, false); err != nil {
		t.Fatal(err)
	}
	return led, item
}

func TestAgentDoneOnUnreadMessageStaysInTheInbox(t *testing.T) {
	led, item := unreadResult(t)

	got, err := led.Done(item.ID, "shipped")
	if err != nil {
		t.Fatal(err)
	}
	if got.State != StateDoneUnread || !got.Unread || got.Outcome != "shipped" || got.DoneAt.IsZero() {
		t.Fatalf("done = state %s unread %v outcome %q doneAt %v", got.State, got.Unread, got.Outcome, got.DoneAt)
	}
	last := got.Messages[len(got.Messages)-1]
	if last.Author != "ada" || !strings.Contains(last.Text, "shipped") {
		t.Fatalf("closing note = %+v", last)
	}
	items, err := led.List()
	if err != nil {
		t.Fatal(err)
	}
	if inbox, _ := Select(items, StateOpen); len(inbox) != 1 || inbox[0].ID != item.ID {
		t.Fatalf("inbox = %+v, want the done-unread item", inbox)
	}
	if done, _ := Select(items, StateDone); len(done) != 0 {
		t.Fatalf("done folder = %+v, want empty", done)
	}
	if only, _ := Select(items, StateDoneUnread); len(only) != 1 {
		t.Fatalf("ls --state done-unread = %+v", only)
	}
}

func TestAgentDoneAgainWithTheSameOutcomeAddsNoNote(t *testing.T) {
	led, item := unreadResult(t)
	first, err := led.Done(item.ID, "shipped")
	if err != nil {
		t.Fatal(err)
	}
	again, err := led.Done(item.ID, "shipped")
	if err != nil {
		t.Fatal(err)
	}
	if again.State != StateDoneUnread || len(again.Messages) != len(first.Messages) {
		t.Fatalf("second done = state %s, %d messages, want %d", again.State, len(again.Messages), len(first.Messages))
	}
}

func TestUserArchiveAfterReadingDoneUnreadKeepsTheOutcome(t *testing.T) {
	led, item := unreadResult(t)
	pending, err := led.Done(item.ID, "shipped")
	if err != nil {
		t.Fatal(err)
	}
	before := pending.Messages[len(pending.Messages)-2].Time
	if _, err := led.Archive(item.ID, before); !errors.Is(err, ErrNewerMessage) {
		t.Fatalf("Archive(before the closing note) = %v, want ErrNewerMessage", err)
	}
	if _, err := led.MarkRead(item.ID); err != nil {
		t.Fatal(err)
	}
	got, err := led.Archive(item.ID, pending.LastMessageAt())
	if err != nil {
		t.Fatal(err)
	}
	if got.State != StateDone || got.Outcome != "shipped" || !got.DoneAt.Equal(pending.DoneAt) {
		t.Fatalf("archived = state %s outcome %q doneAt %v", got.State, got.Outcome, got.DoneAt)
	}
}

func TestUIDoneClosesAtOnceAndArchivedKeepsTheAgentOutcome(t *testing.T) {
	led, item := unreadResult(t)
	ui, err := led.DoneVia(item.ID, "archived", ViaUI)
	if err != nil {
		t.Fatal(err)
	}
	if ui.State != StateDone || ui.Outcome != "archived" {
		t.Fatalf("ui done on unread = state %s outcome %q", ui.State, ui.Outcome)
	}

	led2, item2 := unreadResult(t)
	if _, err := led2.Done(item2.ID, "shipped"); err != nil {
		t.Fatal(err)
	}
	got, err := led2.DoneVia(item2.ID, "archived", ViaUI)
	if err != nil {
		t.Fatal(err)
	}
	if got.State != StateDone || got.Outcome != "shipped" {
		t.Fatalf("ui archive of done-unread = state %s outcome %q", got.State, got.Outcome)
	}
	if _, err := led2.DoneVia(item2.ID, "x", "bot"); err == nil {
		t.Fatal("done with an unknown via succeeded")
	}
}

func TestAgentDoneOnReadItemClosesAtOnce(t *testing.T) {
	led, item := unreadResult(t)
	if _, err := led.MarkRead(item.ID); err != nil {
		t.Fatal(err)
	}
	read, err := led.Get(item.ID)
	if err != nil {
		t.Fatal(err)
	}
	got, err := led.Done(item.ID, "shipped")
	if err != nil {
		t.Fatal(err)
	}
	if got.State != StateDone || got.Outcome != "shipped" || len(got.Messages) != len(read.Messages) {
		t.Fatalf("done = state %s outcome %q messages %d", got.State, got.Outcome, len(got.Messages))
	}
}

func TestMessagePostedAndDoneInTheSameStepStays(t *testing.T) {
	led, _ := openTemp(t)
	at := time.Date(2026, 11, 17, 9, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time { return at })
	item, err := led.Create(&Item{Question: "status", Kind: KindRelay, Owner: "ada"})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.MarkRead(item.ID); err != nil {
		t.Fatal(err)
	}
	at = at.Add(time.Second)
	if _, err := led.Reply(item.ID, "ada", "result: all green", nil, false); err != nil {
		t.Fatal(err)
	}
	got, err := led.Done(item.ID, "reported")
	if err != nil {
		t.Fatal(err)
	}
	if got.State != StateDoneUnread {
		t.Fatalf("state = %s, want %s", got.State, StateDoneUnread)
	}

	never, err := led.Create(&Item{Question: "fyi: build fixed", Kind: KindRelay, Owner: "ada"})
	if err != nil {
		t.Fatal(err)
	}
	got, err = led.Done(never.ID, "noted")
	if err != nil {
		t.Fatal(err)
	}
	if got.State != StateDoneUnread {
		t.Fatalf("never-read note state = %s, want %s", got.State, StateDoneUnread)
	}
}

func TestHumanAnswerCountsAsReadingWhatCameBefore(t *testing.T) {
	led, _ := openTemp(t)
	stepClock(led)
	item, err := led.Create(&Item{
		Question: "post it?", Kind: KindRelay, Owner: "ada", Options: postChoice(),
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Answer(item.ID, "post", ViaCLI); err != nil {
		t.Fatal(err)
	}
	got, err := led.Done(item.ID, "posted")
	if err != nil {
		t.Fatal(err)
	}
	if got.State != StateDone {
		t.Fatalf("state = %s, want done", got.State)
	}
}

func TestDoneUnreadAcceptsAnswerAndReplyButNotWithdraw(t *testing.T) {
	led, _ := openTemp(t)
	stepClock(led)
	item, err := led.Create(&Item{
		Question: "post it?", Kind: KindRelay, Owner: "ada", Options: postChoice(),
	})
	if err != nil {
		t.Fatal(err)
	}
	if got, err := led.Done(item.ID, "dropped"); err != nil || got.State != StateDoneUnread {
		t.Fatalf("done = %s %v", got.State, err)
	}
	if _, err := led.Withdraw(item.ID, "gone"); err == nil {
		t.Fatal("withdraw of done-unread succeeded")
	}
	answered, err := led.Answer(item.ID, "post", ViaUI)
	if err != nil || answered.State != StateAnswered {
		t.Fatalf("answer = %s %v", answered.State, err)
	}

	other, err := led.Create(&Item{
		Question: "again?", Kind: KindRelay, Owner: "ada", Options: postChoice(),
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Done(other.ID, "dropped"); err != nil {
		t.Fatal(err)
	}
	reopened, err := led.Reply(other.ID, "ada", "one more thing", nil, false)
	if err != nil || reopened.State != StateOpen || len(reopened.Options) != 0 {
		t.Fatalf("reply = state %s options %+v err %v", reopened.State, reopened.Options, err)
	}
}

func TestDoneUnreadJSONReportsTheState(t *testing.T) {
	led, item := unreadResult(t)
	got, err := led.Done(item.ID, "shipped")
	if err != nil {
		t.Fatal(err)
	}
	data, err := json.Marshal(got)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(data), `"state":"done-unread"`) {
		t.Fatalf("json = %s", data)
	}
}
