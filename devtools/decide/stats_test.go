package decide

import (
	"slices"
	"testing"
	"time"
)

func TestAnswerRecordsTypeAndAskingAgentMetadata(t *testing.T) {
	led, _ := openTemp(t)
	t0 := time.Date(2026, 11, 20, 10, 0, 0, 0, time.UTC)
	t1 := t0.Add(5 * time.Minute)
	t2 := t0.Add(10 * time.Minute)
	t3 := t0.Add(15 * time.Minute)
	t4 := t0.Add(20 * time.Minute)
	t5 := t0.Add(25 * time.Minute)

	// 1. option pick
	led.SetClock(func() time.Time { return t0 })
	optItem, err := led.Create(&Item{
		Question: "ship it?",
		Owner:    "ada",
		Kind:     KindRelay,
		FreeText: true,
		Options:  []Option{{Key: "a", Label: "Alpha", Recommended: true}, {Key: "b", Label: "Beta"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	led.SetClock(func() time.Time { return t1 })
	optGot, err := led.Answer(optItem.ID, "a", ViaCLI)
	if err != nil {
		t.Fatal(err)
	}
	assertAnswerMeta(t, optGot.Answer, AnswerTypeOption, "ada", t0, t1)
	assertMessageMeta(t, optGot.Messages[1], AnswerTypeOption, "ada", t0, t1)

	// 2. free-text-over-options via Answer and via Compose
	led.SetClock(func() time.Time { return t0 })
	ftOptItem, err := led.Create(&Item{
		Question: "which db?",
		Owner:    "ada",
		Kind:     KindRelay,
		FreeText: true,
		Options:  []Option{{Key: "a", Label: "SQLite", Recommended: true}, {Key: "b", Label: "Postgres"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	led.SetClock(func() time.Time { return t2 })
	ftOptGot, err := led.Answer(ftOptItem.ID, "use DuckDB", ViaUI)
	if err != nil {
		t.Fatal(err)
	}
	assertAnswerMeta(t, ftOptGot.Answer, AnswerTypeFreeTextOverOptions, "ada", t0, t2)
	assertMessageMeta(t, ftOptGot.Messages[1], AnswerTypeFreeTextOverOptions, "ada", t0, t2)

	// 3. free-text-only (fact / no options)
	led.SetClock(func() time.Time { return t0 })
	ftOnlyItem, err := led.Create(&Item{
		Question: "which AWS account?",
		Owner:    "bea",
		Kind:     KindRelay,
		Class:    ClassFact,
		FreeText: true,
	})
	if err != nil {
		t.Fatal(err)
	}
	led.SetClock(func() time.Time { return t3 })
	ftOnlyGot, err := led.Compose(ftOnlyItem.ID, "prod-123", ViaUI, "")
	if err != nil {
		t.Fatal(err)
	}
	assertAnswerMeta(t, ftOnlyGot.Answer, AnswerTypeFreeTextOnly, "bea", t0, t3)
	assertMessageMeta(t, ftOnlyGot.Messages[1], AnswerTypeFreeTextOnly, "bea", t0, t3)

	// 4. reply-no-question (reply after answered, and reply on a note)
	led.SetClock(func() time.Time { return t4 })
	afterAns, err := led.Compose(optItem.ID, "also check logs", ViaUI, "")
	if err != nil {
		t.Fatal(err)
	}
	// Answer stays option; new message is reply-no-question.
	assertAnswerMeta(t, afterAns.Answer, AnswerTypeOption, "ada", t0, t1)
	assertMessageMeta(t, afterAns.Messages[2], AnswerTypeReplyNoQuestion, "", time.Time{}, t4)

	led.SetClock(func() time.Time { return t0 })
	noteItem, err := led.Create(&Item{
		Question: "build green",
		Owner:    "carl",
		Kind:     KindRelay,
		Class:    ClassNote,
	})
	if err != nil {
		t.Fatal(err)
	}
	led.SetClock(func() time.Time { return t5 })
	noteGot, err := led.Compose(noteItem.ID, "thanks", ViaUI, "")
	if err != nil {
		t.Fatal(err)
	}
	if noteGot.Answer != nil {
		t.Fatalf("note got Answer = %+v, want nil", noteGot.Answer)
	}
	assertMessageMeta(t, noteGot.Messages[1], AnswerTypeReplyNoQuestion, "", time.Time{}, t5)
}

func TestAskingAgentUsesLatestOptionsSenderNotOwner(t *testing.T) {
	led, _ := openTemp(t)
	t0 := time.Date(2026, 11, 20, 10, 0, 0, 0, time.UTC)
	t1 := t0.Add(2 * time.Minute)
	t2 := t0.Add(4 * time.Minute)
	t3 := t0.Add(6 * time.Minute)
	t4 := t0.Add(8 * time.Minute)

	led.SetClock(func() time.Time { return t0 })
	it, err := led.Create(&Item{
		Question: "deploy?",
		Owner:    "ada",
		Kind:     KindRelay,
		FreeText: true,
		Options:  []Option{{Key: "a", Label: "Staging", Recommended: true}, {Key: "b", Label: "Prod"}},
	})
	if err != nil {
		t.Fatal(err)
	}

	// Assign to worker bea; worker sends new options.
	led.SetClock(func() time.Time { return t1 })
	if _, err := led.Assign(it.ID, Assign{Worker: "bea", By: "ada"}); err != nil {
		t.Fatal(err)
	}
	led.SetClock(func() time.Time { return t2 })
	if _, err := led.Reply(it.ID, "bea", "updated choices", []Option{{Key: "x", Label: "Canary", Recommended: true}, {Key: "y", Label: "All"}}, true); err != nil {
		t.Fatal(err)
	}
	// Plain reply from carl without options keeps bea's pending options and asking time.
	led.SetClock(func() time.Time { return t3 })
	if _, err := led.Reply(it.ID, "carl", "fyi canary looks ready", nil, false); err != nil {
		t.Fatal(err)
	}
	led.SetClock(func() time.Time { return t4 })
	got, err := led.Answer(it.ID, "x", ViaUI)
	if err != nil {
		t.Fatal(err)
	}
	if got.Owner != "ada" {
		t.Fatalf("Owner = %q, want ada", got.Owner)
	}
	assertAnswerMeta(t, got.Answer, AnswerTypeOption, "bea", t2, t4)
	last := got.Messages[len(got.Messages)-1]
	assertMessageMeta(t, last, AnswerTypeOption, "bea", t2, t4)
}

func TestClassifyOldRecordsAndUnknown(t *testing.T) {
	t0 := time.Date(2026, 11, 1, 9, 0, 0, 0, time.UTC)
	t1 := t0.Add(time.Minute)
	t2 := t0.Add(2 * time.Minute)

	items := []Item{
		{
			// Old option pick via Answer.Key (no AnswerType, no messages).
			ID:         "D-001",
			Owner:      "ada",
			Kind:       KindRelay,
			CreatedAt:  t0,
			AnsweredAt: t1,
			Options:    []Option{{Key: "a", Label: "Alpha"}},
			Answer:     &Answer{Value: "a", Key: "a", Label: "Alpha", AnsweredBy: "human"},
		},
		{
			// Old option pick via Message.OptionKey only (Answer.Key empty).
			ID:        "D-002",
			Owner:     "ada",
			Kind:      KindRelay,
			CreatedAt: t0,
			Messages: []Message{
				{Author: "ada", Time: t0, Text: "pick", Options: []Option{{Key: "a", Label: "A"}}},
				{Author: "human", Time: t1, Text: "a: A", OptionKey: "a"},
			},
			Answer: &Answer{Value: "a", AnsweredBy: "human"},
		},
		{
			// Old free-text-over-options: pending options on ask message, no Key/OptionKey, non-option text.
			ID:         "D-003",
			Owner:      "bea",
			Kind:       KindRelay,
			FreeText:   true,
			CreatedAt:  t0,
			AnsweredAt: t1,
			Options:    []Option{{Key: "a", Label: "A"}, {Key: "b", Label: "B"}},
			Messages: []Message{
				{Author: "bea", Time: t0, Text: "pick", Options: []Option{{Key: "a", Label: "A"}, {Key: "b", Label: "B"}}},
				{Author: "human", Time: t1, Text: "do c instead", Via: ViaUI},
				{Author: "human", Time: t2, Text: "follow-up note", Via: ViaUI},
			},
			Answer: &Answer{Value: "do c instead", AnsweredBy: "human", Via: ViaUI},
		},
		{
			// Old free-text-only (fact with FreeText and no options).
			ID:         "Q-004",
			Owner:      "carl",
			Kind:       KindRelay,
			Class:      ClassFact,
			FreeText:   true,
			CreatedAt:  t0,
			AnsweredAt: t1,
			Messages: []Message{
				{Author: "carl", Time: t0, Text: "which region?"},
				{Author: "human", Time: t1, Text: "us-west-2", Via: ViaUI},
			},
			Answer: &Answer{Value: "us-west-2", AnsweredBy: "human", Via: ViaUI},
		},
		{
			// Old note with human reply -> reply-no-question.
			ID:        "N-005",
			Owner:     "carl",
			Kind:      KindRelay,
			Class:     ClassNote,
			CreatedAt: t0,
			Messages: []Message{
				{Author: "carl", Time: t0, Text: "deployed"},
				{Author: "human", Time: t1, Text: "cool", Via: ViaUI},
			},
		},
		{
			// Unclassifiable old item: answered, Key empty, FreeText false, no options recorded anywhere.
			ID:         "D-006",
			Owner:      "dana",
			Kind:       KindRelay,
			CreatedAt:  t0,
			AnsweredAt: t1,
			Messages: []Message{
				{Author: "dana", Time: t0, Text: "legacy"},
				{Author: "human", Time: t1, Text: "custom"},
			},
			Answer: &Answer{Value: "custom", AnsweredBy: "human"},
		},
	}

	// Check individual classifications.
	if got := ClassifyAnswer(&items[0]); got.Type != AnswerTypeOption || got.AskingAgent != "ada" {
		t.Fatalf("D-001 = %+v", got)
	}
	if got := ClassifyMessage(&items[1], 1); got.Type != AnswerTypeOption || got.AskingAgent != "ada" {
		t.Fatalf("D-002 msg = %+v", got)
	}
	if got := ClassifyMessage(&items[2], 1); got.Type != AnswerTypeFreeTextOverOptions || got.AskingAgent != "bea" {
		t.Fatalf("D-003 msg[1] = %+v", got)
	}
	if got := ClassifyMessage(&items[2], 2); got.Type != AnswerTypeReplyNoQuestion {
		t.Fatalf("D-003 msg[2] = %+v, want reply-no-question", got)
	}
	if got := ClassifyMessage(&items[3], 1); got.Type != AnswerTypeFreeTextOnly || got.AskingAgent != "carl" {
		t.Fatalf("Q-004 msg[1] = %+v", got)
	}
	if got := ClassifyMessage(&items[4], 1); got.Type != AnswerTypeReplyNoQuestion {
		t.Fatalf("N-005 msg[1] = %+v, want reply-no-question", got)
	}
	if got := ClassifyMessage(&items[5], 1); got.Type != AnswerTypeUnknown || got.AskingAgent != "dana" {
		t.Fatalf("D-006 msg[1] = %+v, want unknown", got)
	}

	rows := ComputeAnswerStats(items, AnswerStatsOptions{})
	wantOrder := []string{"bea", "ada", "carl", "dana"}
	if len(rows) != len(wantOrder) {
		t.Fatalf("rows = %+v", rows)
	}
	for i, w := range wantOrder {
		if rows[i].Agent != w {
			t.Fatalf("rows[%d].Agent = %q, want %q (all: %+v)", i, rows[i].Agent, w, rows)
		}
	}
	if rows[0].FreeTextOverOptions != 1 || rows[0].FreeTextShare != 1.0 || !slices.Equal(rows[0].FreeTextOverOptionsIDs, []string{"D-003"}) {
		t.Fatalf("bea row = %+v", rows[0])
	}
	if rows[1].Option != 2 || rows[1].FreeTextShare != 0 || !slices.Equal(rows[1].OptionIDs, []string{"D-001", "D-002"}) {
		t.Fatalf("ada row = %+v", rows[1])
	}
	if rows[2].FreeTextOnly != 1 || rows[2].Total != 1 || !slices.Equal(rows[2].FreeTextOnlyIDs, []string{"Q-004"}) {
		t.Fatalf("carl row = %+v", rows[2])
	}
	if rows[3].Unknown != 1 || rows[3].Total != 1 || !slices.Equal(rows[3].UnknownIDs, []string{"D-006"}) {
		t.Fatalf("dana row = %+v", rows[3])
	}
}

func TestComputeAnswerStatsWindowsAndAgentFilter(t *testing.T) {
	t1 := time.Date(2026, 11, 10, 12, 0, 0, 0, time.UTC)
	t2 := time.Date(2026, 11, 15, 12, 0, 0, 0, time.UTC)
	t3 := time.Date(2026, 11, 20, 12, 0, 0, 0, time.UTC)
	at := func(t time.Time) *time.Time { return &t }

	items := []Item{
		{
			ID: "D-001", Owner: "ada", Kind: KindRelay, CreatedAt: t1,
			Messages: []Message{
				{Author: "ada", Time: t1, Text: "q1", Options: []Option{{Key: "a", Label: "A"}}},
				{Author: "human", Time: t1, Text: "a: A", OptionKey: "a", AnswerType: AnswerTypeOption, AskingAgent: "ada", AskedAt: at(t1)},
			},
		},
		{
			ID: "D-002", Owner: "ada", Kind: KindRelay, CreatedAt: t2,
			Messages: []Message{
				{Author: "ada", Time: t2, Text: "q2", Options: []Option{{Key: "a", Label: "A"}}},
				{Author: "human", Time: t2, Text: "free", AnswerType: AnswerTypeFreeTextOverOptions, AskingAgent: "ada", AskedAt: at(t2)},
			},
		},
		{
			ID: "D-003", Owner: "bea", Kind: KindRelay, CreatedAt: t3,
			Messages: []Message{
				{Author: "bea", Time: t3, Text: "q3", Options: []Option{{Key: "a", Label: "A"}}},
				{Author: "human", Time: t3, Text: "free", AnswerType: AnswerTypeFreeTextOverOptions, AskingAgent: "bea", AskedAt: at(t3)},
			},
		},
	}

	since := t2
	until := t2
	rows := ComputeAnswerStats(items, AnswerStatsOptions{Since: &since, Until: &until})
	if len(rows) != 1 || rows[0].Agent != "ada" || rows[0].FreeTextOverOptions != 1 || rows[0].Option != 0 {
		t.Fatalf("windowed rows = %+v", rows)
	}

	adaRows := ComputeAnswerStats(items, AnswerStatsOptions{Agent: "ada"})
	if len(adaRows) != 1 || adaRows[0].Option != 1 || adaRows[0].FreeTextOverOptions != 1 || adaRows[0].FreeTextShare != 0.5 {
		t.Fatalf("agent filtered rows = %+v", adaRows)
	}
}

func TestComputeAnswerStatsAliasesAndUnmapped(t *testing.T) {
	t0 := time.Date(2026, 11, 20, 10, 0, 0, 0, time.UTC)
	at := func(t time.Time) *time.Time { return &t }

	cfg := Config{
		ContactAliases: map[string]string{
			"tab-a": "team/agent",
			"tab-b": "team/agent",
		},
		Targets: []ConfigTarget{
			{Name: "target-bot", Pane: "agentbus:team/bot"},
		},
	}
	bus := []BusRecipient{
		{Name: "team/live", Handle: "herdr:local/s/w1:p9"},
	}
	res := NewNameResolver(cfg, bus)

	items := []Item{
		{
			// Old record via alias tab-a -> team/agent (option)
			ID: "D-001", Owner: "tab-a", Kind: KindRelay, CreatedAt: t0,
			Messages: []Message{
				{Author: "tab-a", Time: t0, Text: "q1", Options: []Option{{Key: "a", Label: "A"}}},
				{Author: "human", Time: t0.Add(time.Minute), Text: "a: A", OptionKey: "a", AnswerType: AnswerTypeOption, AskingAgent: "tab-a", AskedAt: at(t0)},
			},
		},
		{
			// Old record via alias tab-b -> team/agent (free-text-over-options)
			ID: "D-002", Owner: "tab-b", Kind: KindRelay, FreeText: true, CreatedAt: t0,
			Messages: []Message{
				{Author: "tab-b", Time: t0, Text: "q2", Options: []Option{{Key: "a", Label: "A"}}},
				{Author: "human", Time: t0.Add(2 * time.Minute), Text: "custom", AnswerType: AnswerTypeFreeTextOverOptions, AskingAgent: "tab-b", AskedAt: at(t0)},
			},
		},
		{
			// Old record matching target agentbus:team/bot -> team/bot
			ID: "D-003", Owner: "target-bot", Kind: KindRelay, CreatedAt: t0,
			Messages: []Message{
				{Author: "target-bot", Time: t0, Text: "q3", Options: []Option{{Key: "a", Label: "A"}}},
				{Author: "human", Time: t0.Add(3 * time.Minute), Text: "a: A", OptionKey: "a", AnswerType: AnswerTypeOption, AskingAgent: "target-bot", AskedAt: at(t0)},
			},
		},
		{
			// Old record matching live pane handle -> team/live
			ID: "D-004", Owner: "live-tab", NotifyPane: "w1:p9", Kind: KindRelay, CreatedAt: t0,
			Messages: []Message{
				{Author: "live-tab", Time: t0, Text: "q4", Options: []Option{{Key: "a", Label: "A"}}},
				{Author: "human", Time: t0.Add(4 * time.Minute), Text: "a: A", OptionKey: "a", AnswerType: AnswerTypeOption, AskingAgent: "live-tab", AskedAt: at(t0)},
			},
		},
		{
			// New record with recorded AskingAgent and RawAskingAgent (no config alias needed at report time)
			ID: "D-005", Owner: "recorded-tab", RawOwner: "recorded-tab", AskingAgent: "team/recorded", Kind: KindRelay, CreatedAt: t0,
			Messages: []Message{
				{Author: "recorded-tab", Time: t0, Text: "q5", Options: []Option{{Key: "a", Label: "A"}}},
				{Author: "human", Time: t0.Add(5 * time.Minute), Text: "a: A", OptionKey: "a", AnswerType: AnswerTypeOption, AskingAgent: "team/recorded", RawAskingAgent: "recorded-tab", AskedAt: at(t0)},
			},
		},
		{
			// Unmapped name stays unchanged with no aliases.
			ID: "D-006", Owner: "solo", Kind: KindRelay, CreatedAt: t0,
			Messages: []Message{
				{Author: "solo", Time: t0, Text: "q6", Options: []Option{{Key: "a", Label: "A"}}},
				{Author: "human", Time: t0.Add(6 * time.Minute), Text: "a: A", OptionKey: "a", AnswerType: AnswerTypeOption, AskingAgent: "solo", AskedAt: at(t0)},
			},
		},
	}

	rows := ComputeAnswerStats(items, AnswerStatsOptions{Resolver: &res})
	byAgent := map[string]AnswerStatsRow{}
	for _, r := range rows {
		byAgent[r.Agent] = r
	}
	if len(rows) != 5 {
		t.Fatalf("got %d rows, want 5: %+v", len(rows), rows)
	}
	teamAgent := byAgent["team/agent"]
	if teamAgent.Option != 1 || teamAgent.FreeTextOverOptions != 1 || teamAgent.Total != 2 || teamAgent.FreeTextShare != 0.5 {
		t.Fatalf("team/agent row = %+v", teamAgent)
	}
	if !slices.Equal(teamAgent.Aliases, []string{"tab-a", "tab-b"}) {
		t.Fatalf("team/agent aliases = %v, want [tab-a tab-b]", teamAgent.Aliases)
	}
	if byAgent["team/bot"].Total != 1 || !slices.Equal(byAgent["team/bot"].Aliases, []string{"target-bot"}) {
		t.Fatalf("team/bot = %+v", byAgent["team/bot"])
	}
	if byAgent["team/live"].Total != 1 || !slices.Equal(byAgent["team/live"].Aliases, []string{"live-tab"}) {
		t.Fatalf("team/live = %+v", byAgent["team/live"])
	}
	if byAgent["team/recorded"].Total != 1 || !slices.Equal(byAgent["team/recorded"].Aliases, []string{"recorded-tab"}) {
		t.Fatalf("team/recorded = %+v", byAgent["team/recorded"])
	}
	if byAgent["solo"].Total != 1 || len(byAgent["solo"].Aliases) != 0 {
		t.Fatalf("solo = %+v, want unchanged with no aliases", byAgent["solo"])
	}

	// --agent <alias> and --agent <canonical> return the same row.
	for _, q := range []string{"tab-a", "tab-b", "team/agent"} {
		filtered := ComputeAnswerStats(items, AnswerStatsOptions{Agent: q, Resolver: &res})
		if len(filtered) != 1 || filtered[0].Agent != "team/agent" || filtered[0].Total != 2 || !slices.Equal(filtered[0].Aliases, []string{"tab-a", "tab-b"}) {
			t.Fatalf("--agent %q = %+v", q, filtered)
		}
	}
	// Recorded raw name also matches without config alias.
	for _, q := range []string{"recorded-tab", "team/recorded"} {
		filtered := ComputeAnswerStats(items, AnswerStatsOptions{Agent: q, Resolver: &res})
		if len(filtered) != 1 || filtered[0].Agent != "team/recorded" || filtered[0].Total != 1 {
			t.Fatalf("--agent %q = %+v", q, filtered)
		}
	}
}

func TestAnswerRecordsCanonicalAndRawAskingAgent(t *testing.T) {
	writeConfig(t, `{"contactAliases": {"old-ada": "team/ada", "worker-alias": "team/worker"}}`)
	led, _ := openTemp(t)
	t0 := time.Date(2026, 11, 20, 10, 0, 0, 0, time.UTC)
	led.SetClock(func() time.Time { return t0 })

	it, err := led.Create(&Item{
		Question: "ship?",
		Owner:    "old-ada",
		Kind:     KindRelay,
		FreeText: true,
		Options:  []Option{{Key: "a", Label: "Alpha", Recommended: true}, {Key: "b", Label: "Beta"}},
	})
	if err != nil {
		t.Fatal(err)
	}
	if it.Owner != "old-ada" || it.RawOwner != "old-ada" || it.AskingAgent != "team/ada" {
		t.Fatalf("created item = %+v", it)
	}

	led.SetClock(func() time.Time { return t0.Add(time.Minute) })
	got, err := led.Answer(it.ID, "a", ViaUI)
	if err != nil {
		t.Fatal(err)
	}
	if got.Answer == nil || got.Answer.AskingAgent != "team/ada" || got.Answer.RawAskingAgent != "old-ada" {
		t.Fatalf("Answer = %+v", got.Answer)
	}
	last := got.Messages[len(got.Messages)-1]
	if last.AskingAgent != "team/ada" || last.RawAskingAgent != "old-ada" {
		t.Fatalf("Message = %+v", last)
	}

	// Worker reply also resolves alias for Answer.AskingAgent + RawAskingAgent.
	led.SetClock(func() time.Time { return t0.Add(2 * time.Minute) })
	if _, err := led.Reply(it.ID, "worker-alias", "choose", []Option{{Key: "x", Label: "X", Recommended: true}, {Key: "y", Label: "Y"}}, false); err != nil {
		t.Fatal(err)
	}
	led.SetClock(func() time.Time { return t0.Add(3 * time.Minute) })
	got2, err := led.Answer(it.ID, "x", ViaUI)
	if err != nil {
		t.Fatal(err)
	}
	if got2.Answer == nil || got2.Answer.AskingAgent != "team/worker" || got2.Answer.RawAskingAgent != "worker-alias" {
		t.Fatalf("worker Answer = %+v", got2.Answer)
	}
}

func TestFindUnaliasedDuplicates(t *testing.T) {
	t0 := time.Date(2026, 11, 20, 10, 0, 0, 0, time.UTC)
	items := []Item{
		{ID: "D-001", Owner: "review", NotifyPane: "w1:p3", CreatedAt: t0},
		{ID: "D-002", Owner: "ops/inbox", NotifyPane: "w1:p3", CreatedAt: t0.Add(10 * time.Minute)},
		// Outside short window on another pane
		{ID: "D-003", Owner: "tab-old", NotifyPane: "w1:p4", CreatedAt: t0},
		{ID: "D-004", Owner: "tab-new", NotifyPane: "w1:p4", CreatedAt: t0.Add(3 * time.Hour)},
		// Different machine, same pane id
		{ID: "D-005", Owner: "laptop-agent", NotifyPane: "w1:p3", NotifyMachine: "laptop", CreatedAt: t0.Add(5 * time.Minute)},
		// Pull items do not share a pane
		{ID: "D-006", Owner: "ci-a", NotifyPane: "pull", CreatedAt: t0},
		{ID: "D-007", Owner: "ci-b", NotifyPane: "pull", CreatedAt: t0.Add(time.Minute)},
	}

	dups := FindUnaliasedDuplicates(items, NewNameResolver(Config{}, nil), DefaultDuplicateAgentWindow)
	if len(dups) != 1 {
		t.Fatalf("dups = %+v, want 1", dups)
	}
	if dups[0].Pane != "w1:p3" || dups[0].Machine != "" || !slices.Equal(dups[0].Names, []string{"ops/inbox", "review"}) {
		t.Fatalf("dup[0] = %+v", dups[0])
	}

	// Aliasing them via contactAliases clears the warning.
	resAliased := NewNameResolver(Config{ContactAliases: map[string]string{"review": "ops/inbox"}}, nil)
	if got := FindUnaliasedDuplicates(items, resAliased, DefaultDuplicateAgentWindow); len(got) != 0 {
		t.Fatalf("after alias dups = %+v, want empty", got)
	}
}

func assertAnswerMeta(t *testing.T, a *Answer, wantType, wantAgent string, wantAsked, wantAnswered time.Time) {
	t.Helper()
	if a == nil {
		t.Fatal("Answer is nil")
		return
	}
	if a.AnswerType != wantType || a.AskingAgent != wantAgent {
		t.Fatalf("Answer = %+v, want type %q agent %q", a, wantType, wantAgent)
	}
	if a.AskedAt == nil || !a.AskedAt.Equal(wantAsked) {
		t.Fatalf("Answer.AskedAt = %v, want %v", a.AskedAt, wantAsked)
	}
	if a.AnsweredAt == nil || !a.AnsweredAt.Equal(wantAnswered) {
		t.Fatalf("Answer.AnsweredAt = %v, want %v", a.AnsweredAt, wantAnswered)
	}
}

func assertMessageMeta(t *testing.T, m Message, wantType, wantAgent string, wantAsked, wantTime time.Time) {
	t.Helper()
	if m.AnswerType != wantType || m.AskingAgent != wantAgent {
		t.Fatalf("Message = %+v, want type %q agent %q", m, wantType, wantAgent)
	}
	if wantAsked.IsZero() {
		if m.AskedAt != nil {
			t.Fatalf("Message.AskedAt = %v, want nil", m.AskedAt)
		}
	} else if m.AskedAt == nil || !m.AskedAt.Equal(wantAsked) {
		t.Fatalf("Message.AskedAt = %v, want %v", m.AskedAt, wantAsked)
	}
	if !m.Time.Equal(wantTime) {
		t.Fatalf("Message.Time = %v, want %v", m.Time, wantTime)
	}
}
