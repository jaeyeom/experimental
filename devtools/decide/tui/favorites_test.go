package tui

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// pickName selects name in the open recipient picker.
func pickName(t *testing.T, m *Model, name string) {
	t.Helper()
	if !m.selectName(name) {
		t.Fatalf("%s missing from %q", name, candidateNames(m))
	}
}

func TestStarMovesBusNameAndTargetToTheTop(t *testing.T) {
	m := busModel(t, &fakeBus{list: busList})
	loadBus(t, m)
	press(t, m, "c")
	before := strings.Join(candidateNames(m), ",")

	pickName(t, m, "ops/inbox")
	press(t, m, "*")
	if got, _ := m.selectedTarget(); got.Name != "ops/inbox" {
		t.Fatalf("selection moved to %q after starring", got.Name)
	}
	pickName(t, m, "inbox")
	press(t, m, "*")

	names := candidateNames(m)
	want := []string{
		"ops/inbox", "inbox",
		"coord/backup", "coord/primary",
		"ops/pager",
		"pm/alpha", "pm/beta", "pm/gamma",
		"tools/build", "tools/deploy", "tools/lint",
		"coord/old", "pm/retired",
	}
	if strings.Join(names, ",") != strings.Join(want, ",") {
		t.Fatalf("candidates =\n%q\nwant\n%q", names, want)
	}
	cands := m.candidates()
	if !cands[0].Fav || !cands[1].Fav || cands[2].Fav {
		t.Fatalf("favorite flags = %+v", cands[:3])
	}
	if !cands[0].Bus || cands[0].When != "reviewing the queue" || cands[1].Bus {
		t.Fatalf("starred entries lost their data: %+v", cands[:2])
	}
	m.width, m.height = 100, 60
	lines, _ := m.pickerLines()
	var marked int
	for _, l := range lines {
		if strings.Contains(visibleLine(l.text), "★") {
			marked++
		}
	}
	if marked != 2 {
		t.Fatalf("%d rows marked ★, want 2", marked)
	}
	m.pickFilter = "queue"
	if got := strings.Join(visibleNames(m), ","); got != "ops/inbox" {
		t.Fatalf("filter on a favorite's --when = %s", got)
	}
	m.pickFilter = ""

	pickName(t, m, "ops/inbox")
	press(t, m, "*")
	pickName(t, m, "inbox")
	press(t, m, "*")
	if got := strings.Join(candidateNames(m), ","); got != before {
		t.Fatalf("after unstar\n%s\nwant\n%s", got, before)
	}
}

func TestStarsSurviveARestart(t *testing.T) {
	bus := &fakeBus{list: busList}
	m := busModel(t, bus)
	loadBus(t, m)
	press(t, m, "c")
	pickName(t, m, "tools/lint")
	press(t, m, "*")

	cfg, err := decide.LoadConfig()
	if err != nil {
		t.Fatal(err)
	}
	if strings.Join(cfg.FavoriteContacts, ",") != "tools/lint" {
		t.Fatalf("stored favorites = %q", cfg.FavoriteContacts)
	}
	again, err := prepare(Options{
		Dir: t.TempDir(), In: strings.NewReader(""), Out: io.Discard,
		Targets: configuredTargets(), Agentbus: true, BusRunner: bus.run,
		FavoriteContacts: cfg.FavoriteContacts,
	})
	if err != nil {
		t.Fatal(err)
	}
	loadBus(t, again)
	if got := candidateNames(again); got[0] != "tools/lint" || !again.candidates()[0].Fav {
		t.Fatalf("after restart candidates = %q", got)
	}
}

func TestStarredNameNotLiveShowsInFavorites(t *testing.T) {
	m := busModel(t, &fakeBus{list: busList})
	m.favorites = []string{"ghost", "coord/old"}
	loadBus(t, m)
	cands := m.candidates()
	if cands[0].Name != "ghost" || cands[1].Name != "coord/old" {
		t.Fatalf("candidates = %q", candidateNames(m))
	}
	for _, tg := range cands[:2] {
		if !tg.Fav || !tg.Dead || !tg.Bus {
			t.Fatalf("not-live favorite = %+v", tg)
		}
		if label := visibleLine(m.targetLabel(tg, false)); !strings.Contains(label, "★") || !strings.Contains(label, "✗ dead") {
			t.Fatalf("label %q", label)
		}
	}
	if n := strings.Count(strings.Join(candidateNames(m), ","), "coord/old"); n != 1 {
		t.Fatalf("coord/old listed %d times", n)
	}
}

type hubFavorites struct {
	*LocalBackend
	calls [][]string
}

func (h *hubFavorites) SetFavorites(names []string) (decide.DisplaySettings, error) {
	h.calls = append(h.calls, append([]string(nil), names...))
	raw, err := json.Marshal(names)
	if err != nil {
		return decide.DisplaySettings{}, fmt.Errorf("favorite contacts: %w", err)
	}
	return decide.DisplaySettings{Keys: map[string]json.RawMessage{"favoriteContacts": raw}}, nil
}

func TestContactsStarKeyTogglesAFavoriteOnTheHub(t *testing.T) {
	t.Setenv("XDG_CONFIG_HOME", t.TempDir())
	dir := t.TempDir()
	led := openLedger(t, dir)
	mustCreate(t, led, &decide.Item{Question: "q", Owner: "ada", Kind: decide.KindRelay, State: decide.StateOpen})
	bus := &fakeBus{list: `[{"name":"ada","liveness":"alive"},{"name":"bea","liveness":"alive"}]`}
	m := New(dir)
	m.busRun = bus.run
	if err := m.Reload(); err != nil {
		t.Fatal(err)
	}
	hub := &hubFavorites{}
	m.backend = hub
	m.width, m.height = 100, 30
	if err := m.showContacts(); err != nil {
		t.Fatal(err)
	}
	finishContactBus(t, m)
	selectContact(t, m, "bea")
	press(t, m, "*")
	if len(hub.calls) != 1 || strings.Join(hub.calls[0], ",") != "bea" {
		t.Fatalf("hub favorites %q", hub.calls)
	}
	if strings.Join(m.favorites, ",") != "bea" {
		t.Fatalf("favorites %q", m.favorites)
	}
	if view := visibleLine(m.View()); !strings.Contains(view, "★ bea") {
		t.Fatalf("starred row not marked\n%s", view)
	}
	press(t, m, "*")
	if len(hub.calls) != 2 || len(hub.calls[1]) != 0 || len(m.favorites) != 0 {
		t.Fatalf("unstar: hub %q favorites %q", hub.calls, m.favorites)
	}
}
