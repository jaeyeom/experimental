package slackwatch

import "testing"

func TestParsePost(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want PostRef
	}{
		{
			name: "top-level permalink",
			args: []string{"https://acme.slack.com/archives/C0123ABC/p1700000000000100"},
			want: PostRef{Channel: "C0123ABC", TS: "1700000000.000100"},
		},
		{
			name: "reply permalink",
			args: []string{"https://acme.slack.com/archives/C0123ABC/p1700000005000200?thread_ts=1700000000.000100&cid=C0123ABC"},
			want: PostRef{Channel: "C0123ABC", TS: "1700000005.000200", ThreadTS: "1700000000.000100"},
		},
		{
			name: "permalink to the parent with its own thread_ts",
			args: []string{"https://acme.slack.com/archives/C0123ABC/p1700000000000100?thread_ts=1700000000.000100"},
			want: PostRef{Channel: "C0123ABC", TS: "1700000000.000100"},
		},
		{
			name: "channel and ts",
			args: []string{"D0456", "1700000000.000100"},
			want: PostRef{Channel: "D0456", TS: "1700000000.000100"},
		},
		{
			name: "watch id",
			args: []string{"42"},
			want: PostRef{WatchID: 42},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := ParsePost(tt.args)
			if err != nil {
				t.Fatal(err)
			}
			if got != tt.want {
				t.Errorf("ParsePost(%q) = %+v, want %+v", tt.args, got, tt.want)
			}
		})
	}
}

func TestParsePostErrors(t *testing.T) {
	for _, args := range [][]string{
		nil,
		{"a", "b", "c"},
		{"https://acme.slack.com/archives/C0123ABC"},
		{"https://acme.slack.com/archives/C0123ABC/p123"},
		{"https://acme.slack.com/archives/C0123ABC/x1700000000000100"},
		{"not a post"},
		{"0"},
		{"C0123ABC", "not-a-ts"},
		{"bad channel", "1700000000.000100"},
	} {
		if got, err := ParsePost(args); err == nil {
			t.Errorf("ParsePost(%q) = %+v, want error", args, got)
		}
	}
}

func TestCompareTS(t *testing.T) {
	tests := []struct {
		a, b string
		want int
	}{
		{"1700000000.000100", "1700000000.000100", 0},
		{"1700000000.000100", "1700000000.000200", -1},
		{"999999999.900000", "1700000000.000100", -1},
		{"1700000001.000000", "1700000000.999999", 1},
	}
	for _, tt := range tests {
		if got := CompareTS(tt.a, tt.b); got != tt.want {
			t.Errorf("CompareTS(%q, %q) = %d, want %d", tt.a, tt.b, got, tt.want)
		}
	}
}
