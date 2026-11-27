package healthsnap

import (
	"testing"
	"time"
)

func TestParseDuration(t *testing.T) {
	tests := []struct {
		name    string
		input   string
		want    time.Duration
		wantErr bool
	}{
		{name: "seconds", input: "30s", want: 30 * time.Second},
		{name: "hours and minutes", input: "2h15m", want: 2*time.Hour + 15*time.Minute},
		{name: "zero", input: "0s", want: 0},
		{name: "bare zero", input: "0", want: 0},
		{name: "days", input: "2d", want: 48 * time.Hour},
		{name: "days and hours", input: "1d12h", want: 36 * time.Hour},
		{name: "weeks", input: "1w", want: 7 * 24 * time.Hour},
		{name: "weeks days hours", input: "1w2d3h", want: (9*24 + 3) * time.Hour},
		{name: "fractional days", input: "1.5d", want: 36 * time.Hour},
		{name: "negative rejected", input: "-5s", wantErr: true},
		{name: "empty rejected", input: "", wantErr: true},
		{name: "invalid unit", input: "10x", wantErr: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got, err := ParseDuration(tc.input)
			if (err != nil) != tc.wantErr {
				t.Fatalf("ParseDuration(%q) error = %v, wantErr %v", tc.input, err, tc.wantErr)
			}
			if !tc.wantErr && got != tc.want {
				t.Errorf("ParseDuration(%q) = %v, want %v", tc.input, got, tc.want)
			}
		})
	}
}
