package diskwatch

import (
	"strings"
	"testing"
	"time"
)

func TestParseConfigDefaultsAndThresholds(t *testing.T) {
	t.Parallel()
	cfg, err := ParseConfig([]byte(`
on_low = ["/tmp/notify", "disk"]
[[mounts]]
path = "/"
low = "15%"
high = "25%"
urgent = "5%"
`))
	if err != nil {
		t.Fatal(err)
	}
	if cfg.Cooldown != DefaultCooldown || cfg.SummaryTop != DefaultSummaryTop {
		t.Fatalf("defaults = %s top %d", cfg.Cooldown, cfg.SummaryTop)
	}
	if cfg.Mounts[0].Urgent == nil || cfg.Mounts[0].Urgent.Percent != 5 {
		t.Fatalf("urgent = %+v", cfg.Mounts[0].Urgent)
	}
	if len(cfg.OnLow) != 2 {
		t.Fatalf("argv = %#v", cfg.OnLow)
	}
}

func TestInvalidConfig(t *testing.T) {
	t.Parallel()
	cases := []struct {
		name string
		toml string
		want string
	}{
		{
			name: "low above high",
			toml: `
on_low = ["notify"]
[[mounts]]
path = "/"
low = "40%"
high = "15%"
`,
			want: "low must be below high",
		},
		{
			name: "empty argv",
			toml: `
on_low = []
[[mounts]]
path = "/"
low = "15%"
high = "25%"
`,
			want: "on_low is empty",
		},
		{
			name: "mixed units",
			toml: `
on_low = ["notify"]
[[mounts]]
path = "/"
low = "15%"
high = "40G"
`,
			want: "same unit",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, err := ParseConfig([]byte(tc.toml))
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("error = %v, want %q", err, tc.want)
			}
		})
	}
}

func TestMountCooldownOverride(t *testing.T) {
	t.Parallel()
	cfg, err := ParseConfig([]byte(`
cooldown = "1h"
on_low = ["notify"]
[[mounts]]
path = "/"
low = "10%"
high = "20%"
cooldown = "30m"
`))
	if err != nil {
		t.Fatal(err)
	}
	if cfg.Mounts[0].Cooldown != 30*time.Minute {
		t.Fatalf("cooldown = %s", cfg.Mounts[0].Cooldown)
	}
}
