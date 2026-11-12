package decide

import (
	"slices"
	"strings"
	"testing"
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
