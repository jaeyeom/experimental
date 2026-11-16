package tui

import (
	"fmt"
	"slices"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// favoriteMark marks a starred name in the picker and in Contacts.
const favoriteMark = "★"

// favoritesHub stores the starred contact list on the hub, as SetHidden
// does for hidden names. A local backend does not implement it.
type favoritesHub interface {
	SetFavorites(names []string) (decide.DisplaySettings, error)
}

func (m *Model) isFavorite(name string) bool {
	return slices.Contains(m.favorites, name)
}

// withFavorites puts the starred names first, in the order they were
// starred, and drops them from the rest. A starred name nobody lists is
// a dead bus name, so it stays selectable and shows as not live.
func (m *Model) withFavorites(cands []Target) []Target {
	if len(m.favorites) == 0 {
		return cands
	}
	out := make([]Target, 0, len(cands)+len(m.favorites))
	for _, name := range m.favorites {
		tg := Target{Name: name, Bus: true, Dead: true, Live: "dead"}
		if i := slices.IndexFunc(cands, func(c Target) bool { return c.Name == name }); i >= 0 {
			tg = cands[i]
		}
		tg.Fav = true
		out = append(out, tg)
	}
	for _, tg := range cands {
		if !m.isFavorite(tg.Name) {
			out = append(out, tg)
		}
	}
	return out
}

// toggleFavorite stars name, or unstars a starred one, and stores the
// list. A failed write keeps the previous list.
func (m *Model) toggleFavorite(name string) {
	if name == "" {
		return
	}
	names := slices.DeleteFunc(slices.Clone(m.favorites), func(n string) bool { return n == name })
	action := "unstarred " + name
	if len(names) == len(m.favorites) {
		names = append(names, name)
		action = "starred " + name
	}
	if err := m.persistFavorites(names); err != nil {
		m.status = err.Error()
		return
	}
	m.status = action
	if _, ok := m.backend.(favoritesHub); ok {
		m.status += " (saved on hub)"
	}
}

// persistFavorites stores names on the hub in hub mode, otherwise in
// this machine's config, and keeps the stored list.
func (m *Model) persistFavorites(names []string) error {
	if hub, ok := m.backend.(favoritesHub); ok {
		settings, err := hub.SetFavorites(names)
		if err != nil {
			return fmt.Errorf("save favorite contacts: %w", err)
		}
		eff, err := decide.MergeDisplay(m.localConfig, settings)
		if err != nil {
			return fmt.Errorf("merge favorite contacts: %w", err)
		}
		m.favorites = append([]string(nil), eff.Config.FavoriteContacts...)
		return nil
	}
	stored, err := decide.SetFavoriteContacts(names)
	if err != nil {
		return fmt.Errorf("save favorite contacts: %w", err)
	}
	m.favorites = append([]string(nil), stored...)
	return nil
}

// starPicked toggles the star on the picked recipient and keeps it
// picked at its new position.
func (m *Model) starPicked() {
	tg, ok := m.selectedTarget()
	if !ok {
		return
	}
	m.toggleFavorite(tg.Name)
	m.selectName(tg.Name)
}

// starContact toggles the star on the selected contact.
func (m *Model) starContact() {
	if c, ok := m.selectedContact(); ok {
		m.toggleFavorite(c.Name)
	}
}
