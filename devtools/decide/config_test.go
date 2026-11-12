package decide

import (
	"slices"
	"strings"
	"testing"
	"time"
)

func TestLoadConfigOpenCommand(t *testing.T) {
	writeConfig(t, `{"openCommand": ["my-open", "--new"]}`)
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(cfg.OpenCommand, []string{"my-open", "--new"}) {
		t.Fatalf("OpenCommand = %q", cfg.OpenCommand)
	}
}

func TestLoadConfigOpenCommandNeedsAProgram(t *testing.T) {
	writeConfig(t, `{"openCommand": ["", "--new"]}`)
	if _, err := LoadConfig(); err == nil || !strings.Contains(err.Error(), "openCommand") {
		t.Fatalf("err = %v, want an openCommand error", err)
	}
}

func TestLoadConfigHyperlinks(t *testing.T) {
	for _, tc := range []struct {
		data string
		want bool
	}{
		{`{}`, true},
		{`{"hyperlinks": true}`, true},
		{`{"hyperlinks": false}`, false},
	} {
		writeConfig(t, tc.data)
		cfg, err := LoadConfig()
		if err != nil {
			t.Fatal(err)
		}
		if got := cfg.HyperlinksOn(); got != tc.want {
			t.Fatalf("%s: HyperlinksOn() = %v, want %v", tc.data, got, tc.want)
		}
	}
}

func TestLoadConfigRecipientsTimeout(t *testing.T) {
	for _, tc := range []struct {
		data string
		want time.Duration
	}{
		{`{}`, 3 * time.Second},
		{`{"recipientsTimeout": "5s"}`, 5 * time.Second},
		{`{"recipientsTimeout": "250ms"}`, 250 * time.Millisecond},
	} {
		writeConfig(t, tc.data)
		cfg, err := LoadConfig()
		if err != nil {
			t.Fatal(err)
		}
		if got := cfg.BusListTimeout(); got != tc.want {
			t.Fatalf("%s: BusListTimeout() = %s, want %s", tc.data, got, tc.want)
		}
	}
}

func TestLoadConfigUIUTC(t *testing.T) {
	writeConfig(t, `{"ui": {"utc": true}}`)
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	if !cfg.UI.UTC {
		t.Fatal("ui.utc was not set")
	}
	writeConfig(t, `{}`)
	cfg, err = LoadConfig()
	if err != nil || cfg.UI.UTC {
		t.Fatalf("default ui = %+v, err %v", cfg.UI, err)
	}
}

func TestLoadConfigRecipientsTimeoutMustBePositive(t *testing.T) {
	for _, data := range []string{`{"recipientsTimeout": "soon"}`, `{"recipientsTimeout": "0s"}`} {
		writeConfig(t, data)
		if _, err := LoadConfig(); err == nil || !strings.Contains(err.Error(), "recipientsTimeout") {
			t.Fatalf("%s: err = %v, want a recipientsTimeout error", data, err)
		}
	}
}
