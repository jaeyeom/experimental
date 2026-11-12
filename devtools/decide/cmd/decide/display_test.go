package main

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

// hubAvatarConfig is a hub config with a file avatar, avatarDefault,
// and keys that must stay on the hub.
func hubAvatarConfig(t *testing.T) string {
	t.Helper()
	file := filepath.Join(t.TempDir(), "ada.txt")
	if err := os.WriteFile(file, []byte("AB"), 0o600); err != nil {
		t.Fatal(err)
	}
	return `{"avatars": [{"match": "ada", "file": ` + strings.TrimSpace(mustJSON(t, file)) + `}], "avatarDefault": true,
		"openCommand": ["hub-open"], "checkouts": [{"name": "repo", "path": "/hub/repo"}], "notifyTemplate": "hub-notify"}`
}

func mustJSON(t *testing.T, v any) string {
	t.Helper()
	data, err := json.Marshal(v)
	if err != nil {
		t.Fatal(err)
	}
	return string(data)
}

func TestHubInboxUsesTheHubDisplaySettings(t *testing.T) {
	setupHub(t, `{"hub": "me@hub", "avatarDefault": false}`)
	hubDecide(t, hubAvatarConfig(t))
	var got tui.Display
	var changed, again bool
	stubInbox(t, func(opts tui.Options) {
		got, changed = opts.Display.Display()
		_, again = opts.Display.Display()
	})
	var stderr bytes.Buffer
	if code := run([]string{"inbox"}, &bytes.Buffer{}, &stderr); code != 0 {
		t.Fatalf("inbox = %d, stderr %q", code, stderr.String())
	}
	if !changed || again {
		t.Fatalf("display changed %v, again %v", changed, again)
	}
	if got.AvatarDefault {
		t.Fatal("the hub's avatarDefault won over the local one")
	}
	if len(got.Avatars) != 1 || got.Avatars[0].File != decide.HubAvatarPrefix+"0.txt" {
		t.Fatalf("avatars = %+v", got.Avatars)
	}
	if string(got.AvatarFiles[decide.HubAvatarPrefix+"0.txt"].Data) != "AB" {
		t.Fatalf("avatar files = %+v", got.AvatarFiles)
	}
	if got.Note != "display settings: hub (2 keys), local overrides: avatarDefault" {
		t.Fatalf("note = %q", got.Note)
	}
}

func TestHubDisplayFollowsEachConnect(t *testing.T) {
	gen := uint64(1)
	settings := decide.DisplaySettings{Keys: map[string]json.RawMessage{"avatarDefault": json.RawMessage(`true`)}}
	h := &hubDisplay{fetch: func() (decide.DisplaySettings, uint64) { return settings, gen }}
	first, ok := h.Display()
	if !ok || !first.AvatarDefault || first.Note == "" {
		t.Fatalf("first = %+v, %v", first, ok)
	}
	if _, ok := h.Display(); ok {
		t.Fatal("an unchanged connect changed the display")
	}
	gen = 2
	settings = decide.DisplaySettings{Keys: map[string]json.RawMessage{"avatarMono": json.RawMessage(`"hide"`)}}
	next, ok := h.Display()
	if !ok || next.AvatarDefault || next.AvatarMono != decide.AvatarMonoHide || next.Note != "" {
		t.Fatalf("after reconnect = %+v, %v", next, ok)
	}
}

func TestConfigShowEffectiveLabelsEachKey(t *testing.T) {
	setupHub(t, `{"hub": "me@hub", "avatarDefault": false}`)
	hubDecide(t, hubAvatarConfig(t))
	var stdout, stderr bytes.Buffer
	if code := run([]string{"config", "show", "--effective"}, &stdout, &stderr); code != 0 {
		t.Fatalf("config show = %d, stderr %q", code, stderr.String())
	}
	out := stdout.String()
	for _, want := range []string{
		"avatarDefault\tlocal\tfalse\n",
		"avatars\thub\t[{\"match\":\"ada\",\"file\":\"hub:0.txt\"}]\n",
		"hub\tlocal\t\"me@hub\"\n",
	} {
		if !strings.Contains(out, want) {
			t.Fatalf("output lacks %q:\n%s", want, out)
		}
	}
	for _, never := range []string{"openCommand", "hub-open", "/hub/repo", "hub-notify"} {
		if strings.Contains(out, never) {
			t.Fatalf("output carries %q:\n%s", never, out)
		}
	}
}

func TestConfigShowWithoutAHubIsLocal(t *testing.T) {
	setupHub(t, `{"avatarDefault": true}`)
	var stdout, stderr bytes.Buffer
	if code := run([]string{"config", "show", "--effective"}, &stdout, &stderr); code != 0 {
		t.Fatalf("config show = %d, stderr %q", code, stderr.String())
	}
	if stdout.String() != "avatarDefault\tlocal\ttrue\n" {
		t.Fatalf("output = %q", stdout.String())
	}
}
