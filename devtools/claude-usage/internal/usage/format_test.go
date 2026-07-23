package usage

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
				FiveHour:        Int(2),
				SevenDay:        Int(12),
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
				FiveHour: Int(100),
				SevenDay: Int(0),
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
	data := &Data{
		PlanName:        "Team",
		FiveHour:        Int(2),
		SevenDay:        Int(12),
		FiveHourResetAt: Time(reset5),
		SevenDayResetAt: Time(reset7),
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
	if got["fiveHour"] != float64(2) {
		t.Errorf("fiveHour = %v", got["fiveHour"])
	}
	if got["sevenDay"] != float64(12) {
		t.Errorf("sevenDay = %v", got["sevenDay"])
	}
	if got["fiveHourResetAt"] != "2026-07-24T03:29:59.312Z" {
		t.Errorf("fiveHourResetAt = %v", got["fiveHourResetAt"])
	}
	if got["sevenDayResetAt"] != "2026-07-27T22:59:59.312Z" {
		t.Errorf("sevenDayResetAt = %v", got["sevenDayResetAt"])
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
