package claude

import (
	"bytes"
	"encoding/json"
	"strings"
	"testing"
	"time"
)

func TestFormatHuman(t *testing.T) {
	t.Parallel()

	reset5 := time.Date(2026, 7, 24, 3, 29, 59, 312000000, time.UTC)
	reset7 := time.Date(2026, 7, 27, 22, 59, 59, 312000000, time.UTC)

	tests := []struct {
		name string
		data *Data
		want string
	}{
		{
			name: "full data",
			data: &Data{
				PlanName:        "Team",
				FiveHour:        Float(2),
				SevenDay:        Float(12),
				FiveHourResetAt: Time(reset5),
				SevenDayResetAt: Time(reset7),
			},
			want: strings.Join([]string{
				"plan: Team",
				"  5-hour    2%   resets 2026-07-24 03:29 UTC",
				"  7-day    12%   resets 2026-07-27 22:59 UTC",
				"",
			}, "\n"),
		},
		{
			name: "fractional values round in human output",
			data: &Data{
				PlanName:        "Team",
				FiveHour:        Float(2.4),
				SevenDay:        Float(33.6),
				FiveHourResetAt: Time(reset5),
				SevenDayResetAt: Time(reset7),
			},
			want: strings.Join([]string{
				"plan: Team",
				"  5-hour    2%   resets 2026-07-24 03:29 UTC",
				"  7-day    34%   resets 2026-07-27 22:59 UTC",
				"",
			}, "\n"),
		},
		{
			name: "null percentages and resets",
			data: &Data{
				PlanName: "Pro",
			},
			want: strings.Join([]string{
				"plan: Pro",
				"  5-hour    ?   resets n/a",
				"  7-day     ?   resets n/a",
				"",
			}, "\n"),
		},
		{
			name: "empty plan name becomes unknown",
			data: &Data{
				FiveHour: Float(100),
				SevenDay: Float(0),
			},
			want: strings.Join([]string{
				"plan: unknown",
				"  5-hour  100%   resets n/a",
				"  7-day     0%   resets n/a",
				"",
			}, "\n"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			var buf bytes.Buffer
			if err := FormatHuman(&buf, tt.data); err != nil {
				t.Fatalf("FormatHuman: %v", err)
			}
			if got := buf.String(); got != tt.want {
				t.Fatalf("FormatHuman mismatch\ngot:\n%q\nwant:\n%q", got, tt.want)
			}
		})
	}
}

func TestFormatHumanNil(t *testing.T) {
	t.Parallel()
	var buf bytes.Buffer
	if err := FormatHuman(&buf, nil); err == nil {
		t.Fatal("expected error for nil data")
	}
}

func TestFormatJSON(t *testing.T) {
	t.Parallel()

	reset5 := time.Date(2026, 7, 24, 3, 29, 59, 312000000, time.UTC)
	reset7 := time.Date(2026, 7, 27, 22, 59, 59, 312000000, time.UTC)
	fetchedAt := time.Date(2026, 7, 24, 1, 0, 0, 0, time.UTC)
	data := &Data{
		PlanName:        "Team",
		FiveHour:        Float(33.3),
		SevenDay:        Float(12),
		FiveHourResetAt: Time(reset5),
		SevenDayResetAt: Time(reset7),
		FetchedAt:       Time(fetchedAt),
		Source:          "keychain:/Users/alice/.claude",
	}

	var buf bytes.Buffer
	if err := FormatJSON(&buf, data); err != nil {
		t.Fatalf("FormatJSON: %v", err)
	}

	var got map[string]any
	if err := json.Unmarshal(buf.Bytes(), &got); err != nil {
		t.Fatalf("unmarshal output: %v\nraw: %s", err, buf.String())
	}

	if got["planName"] != "Team" {
		t.Errorf("planName = %v", got["planName"])
	}
	if got["fiveHour"] != 33.3 {
		t.Errorf("fiveHour = %v, want 33.3", got["fiveHour"])
	}
	if got["sevenDay"] != float64(12) {
		t.Errorf("sevenDay = %v, want 12", got["sevenDay"])
	}
	if !strings.Contains(buf.String(), `"sevenDay": 12,`) {
		t.Errorf("whole number should serialize as integer in JSON:\n%s", buf.String())
	}
	if got["fiveHourResetAt"] != "2026-07-24T03:29:59.312Z" {
		t.Errorf("fiveHourResetAt = %v", got["fiveHourResetAt"])
	}
	if got["sevenDayResetAt"] != "2026-07-27T22:59:59.312Z" {
		t.Errorf("sevenDayResetAt = %v", got["sevenDayResetAt"])
	}
	if got["fetchedAt"] != "2026-07-24T01:00:00Z" {
		t.Errorf("fetchedAt = %v", got["fetchedAt"])
	}
	if got["source"] != "keychain:/Users/alice/.claude" {
		t.Errorf("source = %v", got["source"])
	}
}

func TestFormatJSONNullFields(t *testing.T) {
	t.Parallel()

	var buf bytes.Buffer
	if err := FormatJSON(&buf, &Data{PlanName: "Team"}); err != nil {
		t.Fatalf("FormatJSON: %v", err)
	}

	var got map[string]any
	if err := json.Unmarshal(buf.Bytes(), &got); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	for _, key := range []string{"fiveHour", "sevenDay", "fiveHourResetAt", "sevenDayResetAt"} {
		if v, ok := got[key]; !ok || v != nil {
			t.Errorf("%s: want null, got %v (present=%v)", key, v, ok)
		}
	}
}

func TestFormatJSONNil(t *testing.T) {
	t.Parallel()
	var buf bytes.Buffer
	if err := FormatJSON(&buf, nil); err == nil {
		t.Fatal("expected error for nil data")
	}
}

func TestFormatQuotapace(t *testing.T) {
	t.Parallel()

	reset5 := time.Date(2026, 7, 24, 3, 29, 59, 312000000, time.UTC)
	reset7 := time.Date(2026, 7, 27, 22, 59, 59, 0, time.UTC)
	fetchedAt := time.Date(2026, 7, 24, 1, 0, 0, 0, time.UTC)

	data := &Data{
		PlanName:        "Team",
		FiveHour:        Float(0),
		SevenDay:        Float(33.3),
		FiveHourResetAt: Time(reset5),
		SevenDayResetAt: Time(reset7),
		FetchedAt:       Time(fetchedAt),
		Source:          "credentials-file:/home/alice/.claude",
	}

	var buf bytes.Buffer
	if err := FormatQuotapace(&buf, data); err != nil {
		t.Fatalf("FormatQuotapace: %v", err)
	}

	lines := strings.Split(strings.TrimSuffix(buf.String(), "\n"), "\n")
	if len(lines) != 2 {
		t.Fatalf("got %d lines, want 2:\n%s", len(lines), buf.String())
	}

	var first, second map[string]any
	if err := json.Unmarshal([]byte(lines[0]), &first); err != nil {
		t.Fatalf("line 0 json: %v", err)
	}
	if err := json.Unmarshal([]byte(lines[1]), &second); err != nil {
		t.Fatalf("line 1 json: %v", err)
	}
	if first["window"] != "5h" || first["percent"] != float64(0) ||
		first["resetsAt"] != "2026-07-24T03:29:59.312Z" ||
		first["source"] != "credentials-file:/home/alice/.claude" ||
		first["at"] != "2026-07-24T01:00:00Z" {
		t.Fatalf("unexpected 5h line: %v", first)
	}
	if second["window"] != "7d" || second["percent"] != 33.3 ||
		second["resetsAt"] != "2026-07-27T22:59:59Z" ||
		second["source"] != "credentials-file:/home/alice/.claude" ||
		second["at"] != "2026-07-24T01:00:00Z" {
		t.Fatalf("unexpected 7d line: %v", second)
	}
}

func TestFormatQuotapaceSkipsIncompleteAndRejectsEmpty(t *testing.T) {
	t.Parallel()

	reset7 := time.Date(2026, 7, 27, 22, 59, 59, 0, time.UTC)
	var buf bytes.Buffer
	err := FormatQuotapace(&buf, &Data{
		FiveHour:        Float(10),
		SevenDay:        Float(45.5),
		SevenDayResetAt: Time(reset7),
	})
	if err != nil {
		t.Fatalf("FormatQuotapace partial: %v", err)
	}
	lines := strings.Split(strings.TrimSuffix(buf.String(), "\n"), "\n")
	if len(lines) != 1 || !strings.Contains(lines[0], `"window":"7d"`) {
		t.Fatalf("got %q", buf.String())
	}

	buf.Reset()
	if err := FormatQuotapace(&buf, &Data{PlanName: "Pro"}); err != ErrNoData {
		t.Fatalf("got %v, want ErrNoData", err)
	}
}
