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

func TestLateDeliveryAfterDefaultsAndHonorsConfig(t *testing.T) {
	writeConfig(t, `{}`)
	cfg, err := LoadConfig()
	if err != nil || cfg.LateDeliveryAfter() != 5*time.Minute {
		t.Fatalf("default = %s, err %v", cfg.LateDeliveryAfter(), err)
	}
	writeConfig(t, `{"ui":{"lateDeliveryAfter":"90s"}}`)
	cfg, err = LoadConfig()
	if err != nil || cfg.LateDeliveryAfter() != 90*time.Second {
		t.Fatalf("configured = %s, err %v", cfg.LateDeliveryAfter(), err)
	}
	writeConfig(t, `{"ui":{"lateDeliveryAfter":"soon"}}`)
	if _, err := LoadConfig(); err == nil || !strings.Contains(err.Error(), "lateDeliveryAfter") {
		t.Fatalf("invalid duration error = %v", err)
	}
}

func TestLoadConfigClipboard(t *testing.T) {
	for _, mode := range []string{"auto", "osc52", "command", "off"} {
		writeConfig(t, `{"ui": {"clipboard": "`+mode+`"}}`)
		cfg, err := LoadConfig()
		if err != nil {
			t.Fatal(err)
		}
		if cfg.UI.Clipboard != mode {
			t.Fatalf("clipboard %q, want %q", cfg.UI.Clipboard, mode)
		}
	}
	writeConfig(t, `{}`)
	cfg, err := LoadConfig()
	if err != nil || cfg.UI.Clipboard != "" {
		t.Fatalf("default clipboard %q err %v", cfg.UI.Clipboard, err)
	}
	writeConfig(t, `{"ui": {"clipboard": "paste"}}`)
	_, err = LoadConfig()
	if err == nil || !strings.Contains(err.Error(), "ui.clipboard") || !strings.Contains(err.Error(), "paste") {
		t.Fatalf("err = %v, want a ui.clipboard error naming paste", err)
	}
}

func TestLoadConfigFillColumn(t *testing.T) {
	writeConfig(t, `{"ui": {"fillColumn": 40}}`)
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	if cfg.UI.FillColumn != 40 {
		t.Fatalf("fillColumn %d, want 40", cfg.UI.FillColumn)
	}
	writeConfig(t, `{}`)
	cfg, err = LoadConfig()
	if err != nil || cfg.UI.FillColumn != 0 {
		t.Fatalf("default fillColumn %d err %v", cfg.UI.FillColumn, err)
	}
}

func TestLoadConfigAnswerAction(t *testing.T) {
	for _, action := range []string{"stay", "list", "edit"} {
		writeConfig(t, `{"ui": {"answerAction": "`+action+`"}}`)
		cfg, err := LoadConfig()
		if err != nil {
			t.Fatal(err)
		}
		if cfg.UI.AnswerAction != action {
			t.Fatalf("answerAction %q, want %q", cfg.UI.AnswerAction, action)
		}
	}
	writeConfig(t, `{}`)
	cfg, err := LoadConfig()
	if err != nil || cfg.UI.AnswerAction != "" {
		t.Fatalf("default answerAction %q err %v", cfg.UI.AnswerAction, err)
	}
	writeConfig(t, `{"ui": {"answerAction": "close"}}`)
	_, err = LoadConfig()
	if err == nil || !strings.Contains(err.Error(), "ui.answerAction") || !strings.Contains(err.Error(), "close") {
		t.Fatalf("err = %v, want a ui.answerAction error naming close", err)
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
