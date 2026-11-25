package main

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

func TestCLIAnswerAndSendRecordAnswerTypesAndAskingAgent(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	t.Setenv("HERDR_PANE_ID", "")

	// 1. option via `decide answer`
	optID := askRelay(t, []string{"--owner", "ada"}, "ship?")
	if code := answer(t, optID, "a"); code != 0 {
		t.Fatalf("answer option code = %d", code)
	}
	gotOpt := showJSON(t, optID)
	if gotOpt.Answer == nil || gotOpt.Answer.AnswerType != decide.AnswerTypeOption || gotOpt.Answer.AskingAgent != "ada" {
		t.Fatalf("option Answer = %+v", gotOpt.Answer)
	}
	if last := gotOpt.Messages[len(gotOpt.Messages)-1]; last.AnswerType != decide.AnswerTypeOption || last.AskingAgent != "ada" {
		t.Fatalf("option Message = %+v", last)
	}

	// 2. free-text-over-options via `decide answer`
	ftOptID := askRelay(t, []string{"--owner", "ada"}, "which one?")
	if code := answer(t, ftOptID, "neither, wait"); code != 0 {
		t.Fatalf("answer free text code = %d", code)
	}
	gotFTO := showJSON(t, ftOptID)
	if gotFTO.Answer == nil || gotFTO.Answer.AnswerType != decide.AnswerTypeFreeTextOverOptions || gotFTO.Answer.AskingAgent != "ada" {
		t.Fatalf("free-text-over-options Answer = %+v", gotFTO.Answer)
	}
	if last := gotFTO.Messages[len(gotFTO.Messages)-1]; last.AnswerType != decide.AnswerTypeFreeTextOverOptions || last.AskingAgent != "ada" {
		t.Fatalf("free-text-over-options Message = %+v", last)
	}

	// 3. free-text-only via `decide answer` on a fact item
	out, stderr, code := runCLI("ask", "--class", "fact", "--owner", "bea", "which env?")
	if code != 0 {
		t.Fatalf("ask fact = %d\n%s", code, stderr)
	}
	factID := strings.TrimSpace(out)
	if code := answer(t, factID, "staging"); code != 0 {
		t.Fatalf("answer fact code = %d", code)
	}
	gotFact := showJSON(t, factID)
	if gotFact.Answer == nil || gotFact.Answer.AnswerType != decide.AnswerTypeFreeTextOnly || gotFact.Answer.AskingAgent != "bea" {
		t.Fatalf("free-text-only Answer = %+v", gotFact.Answer)
	}
	if last := gotFact.Messages[len(gotFact.Messages)-1]; last.AnswerType != decide.AnswerTypeFreeTextOnly || last.AskingAgent != "bea" {
		t.Fatalf("free-text-only Message = %+v", last)
	}

	// 4. reply-no-question via `decide send --id` on an answered item
	if _, stderr, code := runCLI("send", "--id", optID, "also check metrics"); code != 0 {
		t.Fatalf("send --id = %d\n%s", code, stderr)
	}
	gotReply := showJSON(t, optID)
	if gotReply.Answer == nil || gotReply.Answer.AnswerType != decide.AnswerTypeOption {
		t.Fatalf("Answer changed after reply = %+v", gotReply.Answer)
	}
	if last := gotReply.Messages[len(gotReply.Messages)-1]; last.AnswerType != decide.AnswerTypeReplyNoQuestion || last.AskingAgent != "" {
		t.Fatalf("reply-no-question Message = %+v", last)
	}

	// 5. Asking agent after `decide assign` + worker `decide reply --option`
	t.Run("asking agent after assign and worker reply", func(t *testing.T) {
		inHerdrPane(t)
		assignID := askRelay(t, []string{"--owner", "ada"}, "initial?")
		if _, stderr, code := runCLI("assign", assignID, "worker-bea"); code != 0 {
			t.Fatalf("assign = %d\n%s", code, stderr)
		}
		fakeWhoami(t, "worker-bea\nwhen: x\n", nil)
		if _, stderr, code := runCLI("reply", assignID, "pick new", "--option", "x:X", "--option", "y:Y", "--rec", "x"); code != 0 {
			t.Fatalf("worker reply = %d\n%s", code, stderr)
		}
		if code := answer(t, assignID, "x"); code != 0 {
			t.Fatalf("answer worker option = %d", code)
		}
		gotAssigned := showJSON(t, assignID)
		if gotAssigned.Owner != "ada" {
			t.Fatalf("Owner = %q, want ada", gotAssigned.Owner)
		}
		if gotAssigned.Answer == nil || gotAssigned.Answer.AskingAgent != "worker-bea" || gotAssigned.Answer.AnswerType != decide.AnswerTypeOption {
			t.Fatalf("assigned Answer = %+v", gotAssigned.Answer)
		}
	})
}

func TestStatsAnswersReportWindowsAndJSONParity(t *testing.T) {
	state := t.TempDir()
	t.Setenv("XDG_STATE_HOME", state)
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())

	now := time.Date(2026, 11, 25, 12, 0, 0, 0, time.UTC)
	d1 := now.Add(-10 * 24 * time.Hour) // 10d ago
	d2 := now.Add(-3 * 24 * time.Hour)  // 3d ago
	d3 := now.Add(-2 * time.Hour)       // 2h ago

	// Fixture ledger with new records, legacy derivable records, and an unknown record.
	ledgerDir := filepath.Join(state, "decide")
	if err := os.MkdirAll(ledgerDir, 0o755); err != nil {
		t.Fatal(err)
	}
	fixture := `{
  "next": 8,
  "items": [
    {
      "id": "D-001", "question": "q1", "owner": "ada", "kind": "relay", "state": "answered",
      "createdAt": "` + d1.Format(time.RFC3339) + `", "answeredAt": "` + d1.Format(time.RFC3339) + `",
      "options": [{"key": "a", "label": "A"}],
      "answer": {"value": "a", "key": "a", "label": "A", "answeredBy": "human", "answerType": "option", "askingAgent": "ada", "askedAt": "` + d1.Format(time.RFC3339) + `", "answeredAt": "` + d1.Format(time.RFC3339) + `"},
      "messages": [
        {"author": "ada", "time": "` + d1.Format(time.RFC3339) + `", "text": "q1", "options": [{"key": "a", "label": "A"}]},
        {"author": "human", "time": "` + d1.Format(time.RFC3339) + `", "text": "a: A", "optionKey": "a", "answerType": "option", "askingAgent": "ada", "askedAt": "` + d1.Format(time.RFC3339) + `"}
      ]
    },
    {
      "id": "D-002", "question": "q2", "owner": "ada", "kind": "relay", "freeText": true, "state": "answered",
      "createdAt": "` + d2.Format(time.RFC3339) + `", "answeredAt": "` + d2.Format(time.RFC3339) + `",
      "options": [{"key": "a", "label": "A"}],
      "answer": {"value": "custom", "answeredBy": "human", "answerType": "free-text-over-options", "askingAgent": "ada", "askedAt": "` + d2.Format(time.RFC3339) + `", "answeredAt": "` + d2.Format(time.RFC3339) + `"},
      "messages": [
        {"author": "ada", "time": "` + d2.Format(time.RFC3339) + `", "text": "q2", "options": [{"key": "a", "label": "A"}]},
        {"author": "human", "time": "` + d2.Format(time.RFC3339) + `", "text": "custom", "answerType": "free-text-over-options", "askingAgent": "ada", "askedAt": "` + d2.Format(time.RFC3339) + `"}
      ]
    },
    {
      "id": "D-003", "question": "q3", "owner": "bea", "kind": "relay", "freeText": true, "state": "answered",
      "createdAt": "` + d2.Format(time.RFC3339) + `", "answeredAt": "` + d2.Format(time.RFC3339) + `",
      "options": [{"key": "a", "label": "A"}],
      "answer": {"value": "nope", "answeredBy": "human"},
      "messages": [
        {"author": "bea", "time": "` + d2.Format(time.RFC3339) + `", "text": "q3", "options": [{"key": "a", "label": "A"}]},
        {"author": "human", "time": "` + d2.Format(time.RFC3339) + `", "text": "nope", "via": "ui"}
      ]
    },
    {
      "id": "Q-004", "question": "q4", "owner": "carl", "kind": "relay", "class": "fact", "freeText": true, "state": "answered",
      "createdAt": "` + d3.Format(time.RFC3339) + `", "answeredAt": "` + d3.Format(time.RFC3339) + `",
      "answer": {"value": "prod", "answeredBy": "human"},
      "messages": [
        {"author": "carl", "time": "` + d3.Format(time.RFC3339) + `", "text": "q4"},
        {"author": "human", "time": "` + d3.Format(time.RFC3339) + `", "text": "prod", "via": "ui"}
      ]
    },
    {
      "id": "D-005", "question": "q5", "owner": "dana", "kind": "relay", "state": "answered",
      "createdAt": "` + d3.Format(time.RFC3339) + `", "answeredAt": "` + d3.Format(time.RFC3339) + `",
      "answer": {"value": "mystery", "answeredBy": "human"},
      "messages": [
        {"author": "dana", "time": "` + d3.Format(time.RFC3339) + `", "text": "q5"},
        {"author": "human", "time": "` + d3.Format(time.RFC3339) + `", "text": "mystery"}
      ]
    }
  ]
}`
	if err := os.WriteFile(filepath.Join(ledgerDir, "ledger.json"), []byte(fixture), 0o600); err != nil {
		t.Fatal(err)
	}

	// Full report: text & JSON match.
	var textOut, textErr bytes.Buffer
	if code := cmdStatsAt([]string{"answers"}, &textOut, &textErr, now); code != 0 {
		t.Fatalf("stats answers text code = %d\n%s", code, textErr.String())
	}
	var jsonOut, jsonErr bytes.Buffer
	if code := cmdStatsAt([]string{"answers", "--json"}, &jsonOut, &jsonErr, now); code != 0 {
		t.Fatalf("stats answers json code = %d\n%s", code, jsonErr.String())
	}
	var rows []decide.AnswerStatsRow
	if err := json.Unmarshal(jsonOut.Bytes(), &rows); err != nil {
		t.Fatalf("unmarshal json: %v\n%s", err, jsonOut.String())
	}
	// Expected order by share desc: bea (100%), ada (50%), carl (0% / no opts), dana (0% / unknown)
	if len(rows) != 4 {
		t.Fatalf("rows = %+v", rows)
	}
	if rows[0].Agent != "bea" || rows[0].FreeTextShare != 1.0 || !slices.Equal(rows[0].FreeTextOverOptionsIDs, []string{"D-003"}) {
		t.Fatalf("rows[0] = %+v", rows[0])
	}
	if rows[1].Agent != "ada" || rows[1].Option != 1 || rows[1].FreeTextOverOptions != 1 || rows[1].FreeTextShare != 0.5 {
		t.Fatalf("rows[1] = %+v", rows[1])
	}
	if !slices.Equal(rows[1].OptionIDs, []string{"D-001"}) || !slices.Equal(rows[1].FreeTextOverOptionsIDs, []string{"D-002"}) {
		t.Fatalf("ada ids = %+v", rows[1])
	}
	if rows[2].Agent != "carl" || rows[2].FreeTextOnly != 1 || !slices.Equal(rows[2].FreeTextOnlyIDs, []string{"Q-004"}) {
		t.Fatalf("rows[2] = %+v", rows[2])
	}
	if rows[3].Agent != "dana" || rows[3].Unknown != 1 || !slices.Equal(rows[3].UnknownIDs, []string{"D-005"}) {
		t.Fatalf("rows[3] = %+v", rows[3])
	}

	// Verify text lines match JSON order and numbers.
	lines := strings.Split(strings.TrimSpace(textOut.String()), "\n")
	if len(lines) != len(rows)+1 {
		t.Fatalf("text lines = %d, want %d:\n%s", len(lines), len(rows)+1, textOut.String())
	}
	for i, r := range rows {
		fields := strings.Fields(lines[i+1])
		if len(fields) != 7 || fields[0] != r.Agent {
			t.Fatalf("line %d = %q, want agent %q", i+1, lines[i+1], r.Agent)
		}
	}

	// Window --since 7d --until 1d excludes D-001 (10d ago) and Q-004/D-005 (2h ago).
	var winOut, winErr bytes.Buffer
	if code := cmdStatsAt([]string{"answers", "--since", "7d", "--until", "1d", "--json"}, &winOut, &winErr, now); code != 0 {
		t.Fatalf("windowed stats code = %d\n%s", code, winErr.String())
	}
	var winRows []decide.AnswerStatsRow
	if err := json.Unmarshal(winOut.Bytes(), &winRows); err != nil {
		t.Fatal(err)
	}
	if len(winRows) != 2 || winRows[0].Agent != "ada" || winRows[1].Agent != "bea" {
		t.Fatalf("windowed rows = %+v", winRows)
	}

	// --agent filter
	var agentOut, agentErr bytes.Buffer
	if code := cmdStatsAt([]string{"answers", "--agent", "ada", "--json"}, &agentOut, &agentErr, now); code != 0 {
		t.Fatalf("agent stats code = %d\n%s", code, agentErr.String())
	}
	var agentRows []decide.AnswerStatsRow
	if err := json.Unmarshal(agentOut.Bytes(), &agentRows); err != nil {
		t.Fatal(err)
	}
	if len(agentRows) != 1 || agentRows[0].Agent != "ada" || agentRows[0].Total != 2 {
		t.Fatalf("agent rows = %+v", agentRows)
	}

	// Usage errors exit 2.
	for _, bad := range [][]string{
		{"stats"},
		{"stats", "unknown"},
		{"stats", "answers", "--since", "bogus"},
		{"stats", "answers", "--since", "1d", "--until", "7d"},
		{"stats", "answers", "extra"},
	} {
		if _, _, code := runCLI(bad...); code != 2 {
			t.Fatalf("%v code = %d, want 2", bad, code)
		}
	}
}
