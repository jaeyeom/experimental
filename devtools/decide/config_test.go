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

func TestLoadConfigOpenAt(t *testing.T) {
	for _, openAt := range []string{"new", "last", "top"} {
		writeConfig(t, `{"ui": {"openAt": "`+openAt+`"}}`)
		cfg, err := LoadConfig()
		if err != nil || cfg.UI.OpenAt != openAt {
			t.Fatalf("openAt %q err %v, want %q", cfg.UI.OpenAt, err, openAt)
		}
	}
	writeConfig(t, `{"ui": {"openAt": "middle"}}`)
	_, err := LoadConfig()
	if err == nil || !strings.Contains(err.Error(), "ui.openAt") || !strings.Contains(err.Error(), "middle") {
		t.Fatalf("err = %v, want a ui.openAt error naming middle", err)
	}
}

func TestLoadConfigWebDefaults(t *testing.T) {
	writeConfig(t, `{}`)
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	if cfg.WebAddr() != "127.0.0.1:8787" || cfg.WebPushGrace() != 5*time.Second {
		t.Fatalf("defaults = %q, %s", cfg.WebAddr(), cfg.WebPushGrace())
	}
	writeConfig(t, `{"web": {"addr": ":9000", "publicUrl": "https://x", "auth": {"mode": "token", "tokenFile": "/t"}, "push": {"enabled": true, "subject": "mailto:a@b", "grace": "250ms"}}}`)
	cfg, err = LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	if cfg.WebAddr() != ":9000" || cfg.WebPushGrace() != 250*time.Millisecond || cfg.Web.Auth.Mode != "token" || !cfg.Web.Push.Enabled {
		t.Fatalf("web = %+v", cfg.Web)
	}
}

func TestLoadConfigWebRejectsInvalid(t *testing.T) {
	for _, data := range []string{
		`{"web": {"auth": {"mode": "oauth"}}}`,
		`{"web": {"push": {"grace": "soon"}}}`,
		`{"web": {"push": {"grace": "-1s"}}}`,
	} {
		writeConfig(t, data)
		if _, err := LoadConfig(); err == nil {
			t.Fatalf("%s: LoadConfig() succeeded", data)
		}
	}
}

func TestDisplayNeverExportsWeb(t *testing.T) {
	f := hubFile(t, `{"web": {"addr": ":9000", "auth": {"mode": "token", "tokenFile": "/secret"}}}`)
	if _, ok := f.Display().Keys["web"]; ok {
		t.Fatal("display settings carry web")
	}
	for _, k := range DisplayKeys() {
		if k == "web" {
			t.Fatal("web is a display key")
		}
	}
}

func TestLoadConfigTargetsCommand(t *testing.T) {
	writeConfig(t, `{"targets": [{"name": "mytool", "command": ["/usr/bin/mytool", "--flag"]}, {"name": "builder", "pane": "w1:p2"}]}`)
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	if len(cfg.Targets) != 2 {
		t.Fatalf("targets = %+v, want 2", cfg.Targets)
	}
	if cfg.Targets[0].Name != "mytool" || cfg.Targets[0].Pane != "" || !slices.Equal(cfg.Targets[0].Command, []string{"/usr/bin/mytool", "--flag"}) {
		t.Fatalf("target[0] = %+v", cfg.Targets[0])
	}
	if cfg.Targets[1].Name != "builder" || cfg.Targets[1].Pane != "w1:p2" || len(cfg.Targets[1].Command) != 0 {
		t.Fatalf("target[1] = %+v", cfg.Targets[1])
	}
}

func TestLoadConfigTargetsValidation(t *testing.T) {
	tests := []struct {
		name string
		data string
	}{
		{
			name: "both pane and command",
			data: `{"targets": [{"name": "mytool", "pane": "w1:p2", "command": ["/usr/bin/mytool"]}]}`,
		},
		{
			name: "neither pane nor command",
			data: `{"targets": [{"name": "mytool"}]}`,
		},
		{
			name: "empty pane and empty command",
			data: `{"targets": [{"name": "mytool", "pane": "  ", "command": []}]}`,
		},
		{
			name: "empty command program",
			data: `{"targets": [{"name": "mytool", "command": ["", "--flag"]}]}`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			writeConfig(t, tc.data)
			if _, err := LoadConfig(); err == nil || !strings.Contains(err.Error(), "targets") {
				t.Fatalf("LoadConfig(%s) error = %v, want a targets error", tc.data, err)
			}
		})
	}
}

func TestLoadConfigTransportsValid(t *testing.T) {
	writeConfig(t, `{"transports": {"tmux": ["/usr/local/bin/decide-tmux", "--json"], "my-hook2": ["/bin/hook"]}}`)
	cfg, err := LoadConfig()
	if err != nil {
		t.Fatalf("LoadConfig() error = %v", err)
	}
	if !slices.Equal(cfg.Transports["tmux"], []string{"/usr/local/bin/decide-tmux", "--json"}) {
		t.Fatalf("transports[tmux] = %q", cfg.Transports["tmux"])
	}
	if !slices.Equal(cfg.Transports["my-hook2"], []string{"/bin/hook"}) {
		t.Fatalf("transports[my-hook2] = %q", cfg.Transports["my-hook2"])
	}
}

func TestLoadConfigTransportsValidation(t *testing.T) {
	tests := []struct {
		name string
		data string
	}{
		{
			name: "empty argv",
			data: `{"transports": {"tmux": []}}`,
		},
		{
			name: "empty program",
			data: `{"transports": {"tmux": ["", "--flag"]}}`,
		},
		{
			name: "whitespace program",
			data: `{"transports": {"tmux": ["   "]}}`,
		},
		{
			name: "uppercase scheme",
			data: `{"transports": {"Tmux": ["/bin/tmux-plugin"]}}`,
		},
		{
			name: "leading hyphen scheme",
			data: `{"transports": {"-tmux": ["/bin/tmux-plugin"]}}`,
		},
		{
			name: "underscore scheme",
			data: `{"transports": {"my_plugin": ["/bin/plugin"]}}`,
		},
		{
			name: "empty scheme",
			data: `{"transports": {"": ["/bin/plugin"]}}`,
		},
		{
			name: "shadow herdr",
			data: `{"transports": {"herdr": ["/bin/plugin"]}}`,
		},
		{
			name: "shadow agentbus",
			data: `{"transports": {"agentbus": ["/bin/plugin"]}}`,
		},
		{
			name: "shadow pull",
			data: `{"transports": {"pull": ["/bin/plugin"]}}`,
		},
		{
			name: "shadow target",
			data: `{"transports": {"target": ["/bin/plugin"]}}`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			writeConfig(t, tc.data)
			if _, err := LoadConfig(); err == nil || !strings.Contains(err.Error(), "transports") {
				t.Fatalf("LoadConfig(%s) error = %v, want a transports error", tc.data, err)
			}
		})
	}
}

func TestDisplayNeverExportsTransports(t *testing.T) {
	f := hubFile(t, `{"transports": {"tmux": ["/usr/local/bin/decide-tmux"]}}`)
	if _, ok := f.Display().Keys["transports"]; ok {
		t.Fatal("display settings carry transports")
	}
	for _, k := range DisplayKeys() {
		if k == "transports" {
			t.Fatal("transports is a display key")
		}
	}
}
