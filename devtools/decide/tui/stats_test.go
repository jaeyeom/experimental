package tui

import (
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestTUIInboxRecordsAllFourAnswerTypes(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	led := openLedger(t, dir)

	optItem := mustCreate(t, led, &decide.Item{
		Question: "pick option?",
		Owner:    "ada",
		Kind:     decide.KindRelay,
		FreeText: true,
		Options:  []decide.Option{{Key: "y", Label: "Yes"}, {Key: "n", Label: "No"}},
	})
	ftOptItem := mustCreate(t, led, &decide.Item{
		Question: "pick or type?",
		Owner:    "bea",
		Kind:     decide.KindRelay,
		FreeText: true,
		Options:  []decide.Option{{Key: "a", Label: "Alpha"}, {Key: "b", Label: "Beta"}},
	})
	ftOnlyItem := mustCreate(t, led, &decide.Item{
		Question: "which bucket?",
		Owner:    "carl",
		Kind:     decide.KindRelay,
		Class:    decide.ClassFact,
		FreeText: true,
	})
	noteItem := mustCreate(t, led, &decide.Item{
		Question: "build finished",
		Owner:    "dana",
		Kind:     decide.KindRelay,
		Class:    decide.ClassNote,
	})

	m := New(dir)
	m.width, m.height = 100, 30
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}

	openByID := func(id string) {
		t.Helper()
		for i, it := range m.items {
			if it.ID == id {
				m.cursor = i
				press(t, m, "\r")
				return
			}
		}
		t.Fatalf("item %s not in list %+v", id, m.items)
	}

	// 1. option pick via digit key
	openByID(optItem.ID)
	press(t, m, "1")
	gotOpt, err := led.Get(optItem.ID)
	if err != nil {
		t.Fatal(err)
	}
	if gotOpt.Answer == nil || gotOpt.Answer.AnswerType != decide.AnswerTypeOption || gotOpt.Answer.AskingAgent != "ada" {
		t.Fatalf("TUI option Answer = %+v", gotOpt.Answer)
	}
	if last := gotOpt.Messages[len(gotOpt.Messages)-1]; last.AnswerType != decide.AnswerTypeOption || last.AskingAgent != "ada" {
		t.Fatalf("TUI option Message = %+v", last)
	}

	// Return to list
	press(t, m, "\x1b")

	// 2. free-text-over-options via reply box on open question
	openByID(ftOptItem.ID)
	press(t, m, "0neither\t\r")
	gotFTO, err := led.Get(ftOptItem.ID)
	if err != nil {
		t.Fatal(err)
	}
	if gotFTO.Answer == nil || gotFTO.Answer.AnswerType != decide.AnswerTypeFreeTextOverOptions || gotFTO.Answer.AskingAgent != "bea" {
		t.Fatalf("TUI free-text-over-options Answer = %+v", gotFTO.Answer)
	}
	if last := gotFTO.Messages[len(gotFTO.Messages)-1]; last.AnswerType != decide.AnswerTypeFreeTextOverOptions || last.AskingAgent != "bea" {
		t.Fatalf("TUI free-text-over-options Message = %+v", last)
	}

	press(t, m, "\x1b")

	// 3. free-text-only on fact item
	openByID(ftOnlyItem.ID)
	press(t, m, "0my-bucket\t\r")
	gotFTOnly, err := led.Get(ftOnlyItem.ID)
	if err != nil {
		t.Fatal(err)
	}
	if gotFTOnly.Answer == nil || gotFTOnly.Answer.AnswerType != decide.AnswerTypeFreeTextOnly || gotFTOnly.Answer.AskingAgent != "carl" {
		t.Fatalf("TUI free-text-only Answer = %+v", gotFTOnly.Answer)
	}
	if last := gotFTOnly.Messages[len(gotFTOnly.Messages)-1]; last.AnswerType != decide.AnswerTypeFreeTextOnly || last.AskingAgent != "carl" {
		t.Fatalf("TUI free-text-only Message = %+v", last)
	}

	press(t, m, "\x1b")

	// 4. reply-no-question on note item
	openByID(noteItem.ID)
	press(t, m, "0thanks\t\r")
	gotNote, err := led.Get(noteItem.ID)
	if err != nil {
		t.Fatal(err)
	}
	if gotNote.Answer != nil {
		t.Fatalf("TUI note Answer = %+v, want nil", gotNote.Answer)
	}
	if last := gotNote.Messages[len(gotNote.Messages)-1]; last.AnswerType != decide.AnswerTypeReplyNoQuestion || last.AskingAgent != "" {
		t.Fatalf("TUI reply-no-question Message = %+v", last)
	}
}
