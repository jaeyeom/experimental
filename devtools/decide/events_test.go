package decide

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestAnswerOnChoiceItemWritesSingleEventInSameWrite(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	led, dir := openTemp(t)
	now := time.Date(2026, 11, 26, 10, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time { return now })

	it, err := led.Create(&Item{
		Question:    "Pick database",
		Kind:        KindRelay,
		Owner:       "ada",
		AskingAgent: "ada/planner",
		Options: []Option{
			{Key: "a", Label: "Postgres", Recommended: true},
			{Key: "b", Label: "SQLite"},
		},
	})
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	now = now.Add(time.Minute)
	answered, err := led.Answer(it.ID, "a", ViaUI)
	if err != nil {
		t.Fatalf("Answer: %v", err)
	}

	events, err := led.ListRawEvents()
	if err != nil {
		t.Fatalf("ListRawEvents: %v", err)
	}
	if len(events) != 1 {
		t.Fatalf("event count = %d, want 1: %+v", len(events), events)
	}
	ev := events[0]
	if ev.Kind != EventChoice || ev.ItemID != answered.ID || ev.Who != Human || ev.Via != ViaUI {
		t.Fatalf("event = %+v", ev)
	}
	if !ev.Time.Equal(answered.AnsweredAt) {
		t.Fatalf("event time = %v, want %v", ev.Time, answered.AnsweredAt)
	}
	if ev.ChoiceKey != "a" || ev.ChoiceLabel != "Postgres" || ev.ReplyText != "" {
		t.Fatalf("choice fields = %+v", ev)
	}
	if ev.AskingAgent != "ada/planner" || ev.Misfit {
		t.Fatalf("askingAgent/misfit = %+v", ev)
	}
	if len(ev.Options) != 2 || ev.Options[0].Label != "Postgres" || ev.Options[1].Label != "SQLite" {
		t.Fatalf("options snapshot = %+v", ev.Options)
	}
	if _, err := os.Stat(filepath.Join(dir, eventsName)); err != nil {
		t.Fatalf("events.jsonl stat: %v", err)
	}

	// Emulating AnswerActionEdit (Answer followed by Compose note on now-answered thread)
	// must NOT append a second choice or misfit event.
	now = now.Add(time.Second)
	if _, err := led.Compose(it.ID, "extra rationale", ViaUI, ""); err != nil {
		t.Fatalf("Compose follow-up: %v", err)
	}
	events, err = led.ListRawEvents()
	if err != nil {
		t.Fatal(err)
	}
	if len(events) != 1 {
		t.Fatalf("event count after follow-up compose = %d, want 1", len(events))
	}
}

func TestFreeTextReplyOnChoiceItemFlagsOptionsDidNotFit(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	led, _ := openTemp(t)
	now := time.Date(2026, 11, 26, 11, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time { return now })

	it, err := led.Create(&Item{
		Question: "Deploy strategy?",
		Kind:     KindRelay,
		Owner:    "bea",
		Options: []Option{
			{Key: "a", Label: "Blue/green", Recommended: true},
			{Key: "b", Label: "Rolling"},
		},
	})
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	now = now.Add(2 * time.Minute)
	if _, err := led.Compose(it.ID, "Use canary first", ViaUI, ""); err != nil {
		t.Fatalf("Compose (Reply 0): %v", err)
	}

	events, err := led.ListRawEvents()
	if err != nil {
		t.Fatal(err)
	}
	if len(events) != 1 {
		t.Fatalf("events = %d, want 1", len(events))
	}
	ev := events[0]
	if !ev.Misfit || ev.MisfitFlag != MisfitFlagOptionsDidNotFit {
		t.Fatalf("misfit = %v flag = %q, want true / %q", ev.Misfit, ev.MisfitFlag, MisfitFlagOptionsDidNotFit)
	}
	if ev.ReplyText != "Use canary first" || ev.ChoiceKey != "" {
		t.Fatalf("replyText/choiceKey = %+v", ev)
	}
	if ev.AskingAgent != "bea" || len(ev.Options) != 2 {
		t.Fatalf("askingAgent/options = %+v", ev)
	}

	misfits, err := led.QueryEvents(EventFilter{Misfit: true})
	if err != nil {
		t.Fatal(err)
	}
	if len(misfits) != 1 || misfits[0].ID != ev.ID {
		t.Fatalf("misfits query = %+v", misfits)
	}

	// Free-text via Answer on FreeText-enabled item with options also records misfit.
	it2, err := led.Create(&Item{
		Question: "Pick color?",
		Kind:     KindRelay,
		Owner:    "carl",
		FreeText: true,
		Options: []Option{
			{Key: "r", Label: "Red", Recommended: true},
			{Key: "g", Label: "Green"},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	now = now.Add(time.Minute)
	if _, err := led.Answer(it2.ID, "Blue", ViaCLI); err != nil {
		t.Fatalf("Answer free text: %v", err)
	}
	misfits, err = led.QueryEvents(EventFilter{Misfit: true, Agent: "carl"})
	if err != nil {
		t.Fatal(err)
	}
	if len(misfits) != 1 || misfits[0].ItemID != it2.ID || misfits[0].Via != ViaCLI {
		t.Fatalf("carl misfits = %+v", misfits)
	}
}

func TestCauseAnnotationAppendsAndLeavesOriginalEventUnchanged(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	led, dir := openTemp(t)
	now := time.Date(2026, 11, 26, 12, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time { return now })

	it, err := led.Create(&Item{
		Question: "Who pushes tag?",
		Kind:     KindRelay,
		Owner:    "ada",
		Options: []Option{
			{Key: "a", Label: "I push it", Recommended: true},
			{Key: "b", Label: "You push it"},
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	if _, err := led.Compose(it.ID, "Wait until tests pass", ViaUI, ""); err != nil {
		t.Fatal(err)
	}
	rawBefore, err := os.ReadFile(filepath.Join(dir, eventsName))
	if err != nil {
		t.Fatal(err)
	}
	beforeLines := strings.Split(strings.TrimSpace(string(rawBefore)), "\n")
	if len(beforeLines) != 1 {
		t.Fatalf("lines before = %d", len(beforeLines))
	}

	rawEvs, err := led.ListRawEvents()
	if err != nil {
		t.Fatal(err)
	}
	now = now.Add(time.Hour)
	causeEv, err := led.AddCause(rawEvs[0].ID, []string{"actor-unclear", "timing-unclear"}, "Unclear who pushes or when", Human, ViaCLI)
	if err != nil {
		t.Fatalf("AddCause: %v", err)
	}
	if causeEv.Kind != EventCause || causeEv.TargetEventID != rawEvs[0].ID {
		t.Fatalf("causeEv = %+v", causeEv)
	}

	rawAfter, err := os.ReadFile(filepath.Join(dir, eventsName))
	if err != nil {
		t.Fatal(err)
	}
	afterLines := strings.Split(strings.TrimSpace(string(rawAfter)), "\n")
	if len(afterLines) != 2 {
		t.Fatalf("lines after = %d, want 2", len(afterLines))
	}
	if afterLines[0] != beforeLines[0] {
		t.Fatalf("original line mutated!\nbefore: %s\nafter:  %s", beforeLines[0], afterLines[0])
	}

	misfits, err := led.QueryEvents(EventFilter{Misfit: true, Cause: "actor-unclear"})
	if err != nil {
		t.Fatal(err)
	}
	if len(misfits) != 1 || len(misfits[0].Causes) != 1 || misfits[0].Causes[0].Note != "Unclear who pushes or when" {
		t.Fatalf("filtered misfits = %+v", misfits)
	}
	none, err := led.QueryEvents(EventFilter{Misfit: true, Cause: "too-long"})
	if err != nil {
		t.Fatal(err)
	}
	if len(none) != 0 {
		t.Fatalf("unexpected misfits for too-long: %+v", none)
	}
}

func TestFlagItemSavesReasonsAndRequiresNoteForOther(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	led, _ := openTemp(t)
	now := time.Date(2026, 11, 26, 13, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time { return now })

	it, err := led.Create(&Item{
		Question: "Check PR #99",
		Kind:     KindRelay,
		Owner:    "ada",
	})
	if err != nil {
		t.Fatal(err)
	}
	now = now.Add(time.Minute)
	if _, err := led.Reply(it.ID, "worker-1", "Please inspect PR #99 now", nil, false); err != nil {
		t.Fatal(err)
	}

	// Reject "other" without note.
	msgIdx := 1
	if _, err := led.FlagItem(it.ID, &msgIdx, []string{"other"}, "", ViaUI); err == nil {
		t.Fatal("expected error flagging with other and empty note")
	}

	// Flag specific agent message (index 1, sent by worker-1).
	flagEv, err := led.FlagItem(it.ID, &msgIdx, []string{"no-exact-action", "unexplained-jargon"}, "missing exact gh command", ViaUI)
	if err != nil {
		t.Fatalf("FlagItem: %v", err)
	}
	if flagEv.Kind != EventFlag || flagEv.SendingAgent != "worker-1" || flagEv.ItemID != it.ID {
		t.Fatalf("flagEv = %+v", flagEv)
	}
	if flagEv.MessageIndex == nil || *flagEv.MessageIndex != 1 {
		t.Fatalf("MessageIndex = %v, want 1", flagEv.MessageIndex)
	}
	if len(flagEv.Reasons) != 2 || flagEv.Note != "missing exact gh command" {
		t.Fatalf("reasons/note = %+v", flagEv)
	}

	flags, err := led.QueryEvents(EventFilter{Flags: true})
	if err != nil {
		t.Fatal(err)
	}
	if len(flags) != 1 || flags[0].ID != flagEv.ID {
		t.Fatalf("flags = %+v", flags)
	}
	groups := GroupEventsByAgent(flags)
	if len(groups) != 1 || groups[0].Agent != "worker-1" {
		t.Fatalf("groups = %+v", groups)
	}
}
