package decide

import (
	"encoding/json"
	"fmt"
	"slices"
	"strings"
)

// KeyStroke is one normalized key press.
type KeyStroke struct {
	Key  string
	Ctrl bool
	Alt  bool
}

// KeySeq is a sequence of one or more key strokes (a single key or a chord).
type KeySeq []KeyStroke

// String returns the canonical config notation for a stroke (e.g. "ctrl+x", "alt+q", "enter", "G").
func (s KeyStroke) String() string {
	var parts []string
	if s.Ctrl {
		parts = append(parts, "ctrl")
	}
	if s.Alt {
		parts = append(parts, "alt")
	}
	parts = append(parts, s.Key)
	return strings.Join(parts, "+")
}

// String returns the canonical config notation for a key sequence.
func (seq KeySeq) String() string {
	parts := make([]string, len(seq))
	for i, s := range seq {
		parts[i] = s.String()
	}
	return strings.Join(parts, " ")
}

// HelpString formats a key sequence in the notation used by the help screen.
func (seq KeySeq) HelpString() string {
	parts := make([]string, len(seq))
	for i, s := range seq {
		parts[i] = s.HelpString()
	}
	return strings.Join(parts, " ")
}

// HelpString formats a key stroke in the notation used by the help screen.
func (s KeyStroke) HelpString() string {
	if s.Ctrl && !s.Alt && len(s.Key) == 1 {
		return "C-" + s.Key
	}
	var prefix string
	if s.Ctrl {
		prefix += "Ctrl+"
	}
	if s.Alt {
		prefix += "Alt+"
	}
	switch s.Key {
	case "enter", "esc", "tab", "shift+tab", "up", "down", "left", "right", "pgup", "pgdn", "home", "end", "backspace", "delete", "space":
		if prefix == "" {
			return s.Key
		}
		return prefix + strings.ToUpper(s.Key[:1]) + s.Key[1:]
	default:
		return prefix + s.Key
	}
}

// ButtonString formats a key sequence for display inside a button label.
// When ascii is false, Ctrl is rendered as ^ and Enter as ⏎ (e.g. ^⏎, ^G, ⏎).
// When ascii is true, ASCII notation is used (e.g. C-Enter, C-g, Enter).
func (seq KeySeq) ButtonString(ascii bool) string {
	parts := make([]string, len(seq))
	for i, s := range seq {
		parts[i] = s.ButtonString(ascii)
	}
	return strings.Join(parts, " ")
}

// ButtonString formats a single key stroke for display inside a button label.
func (s KeyStroke) ButtonString(ascii bool) string {
	// Ctrl+Enter is normalized to Alt+Enter by ParseKeySeq and programInput.
	if s.Key == "enter" && s.Alt && !s.Ctrl {
		if ascii {
			return "C-Enter"
		}
		return "^⏎"
	}
	if s.Ctrl && !s.Alt {
		if ascii {
			return "C-" + asciiButtonKey(s.Key)
		}
		if len(s.Key) == 1 && s.Key[0] >= 'a' && s.Key[0] <= 'z' {
			return "^" + string(s.Key[0]-'a'+'A')
		}
		return "^" + unicodeButtonKey(s.Key)
	}
	var prefix string
	if s.Ctrl {
		if ascii {
			prefix += "C-"
		} else {
			prefix += "^"
		}
	}
	if s.Alt {
		prefix += "M-"
	}
	if ascii {
		return prefix + asciiButtonKey(s.Key)
	}
	return prefix + unicodeButtonKey(s.Key)
}

func unicodeButtonKey(key string) string {
	switch key {
	case "enter":
		return "⏎"
	case "esc":
		return "Esc"
	case "tab":
		return "Tab"
	case "shift+tab":
		return "S-Tab"
	case "space":
		return "Space"
	case "up":
		return "↑"
	case "down":
		return "↓"
	case "left":
		return "←"
	case "right":
		return "→"
	default:
		if len(key) == 1 && key[0] >= 'a' && key[0] <= 'z' {
			return key
		}
		return key
	}
}

func asciiButtonKey(key string) string {
	switch key {
	case "enter":
		return "Enter"
	case "esc":
		return "Esc"
	case "tab":
		return "Tab"
	case "shift+tab":
		return "S-Tab"
	case "space":
		return "Space"
	case "up", "down", "left", "right", "pgup", "pgdn", "home", "end", "backspace", "delete":
		return strings.ToUpper(key[:1]) + key[1:]
	default:
		return key
	}
}

func lookupNamedKey(tok string) (string, bool) {
	names := map[string]string{
		"enter":     "enter",
		"return":    "enter",
		"esc":       "esc",
		"escape":    "esc",
		"tab":       "tab",
		"shift+tab": "shift+tab",
		"s-tab":     "shift+tab",
		"backtab":   "shift+tab",
		"up":        "up",
		"down":      "down",
		"left":      "left",
		"right":     "right",
		"pgup":      "pgup",
		"pageup":    "pgup",
		"pgdn":      "pgdn",
		"pagedown":  "pgdn",
		"home":      "home",
		"end":       "end",
		"backspace": "backspace",
		"bs":        "backspace",
		"delete":    "delete",
		"del":       "delete",
		"space":     "space",
	}
	v, ok := names[strings.ToLower(tok)]
	return v, ok
}

// ParseKeySeq parses a key or space-separated chord in config/help notation
// (e.g. "j", "G", "enter", "ctrl+x ctrl+e", "C-x C-e", "alt+q", "shift+tab").
func ParseKeySeq(raw string) (KeySeq, error) {
	if raw == " " {
		return KeySeq{{Key: "space"}}, nil
	}
	fields := strings.Fields(raw)
	if len(fields) == 0 {
		return nil, fmt.Errorf("empty key")
	}
	seq := make(KeySeq, 0, len(fields))
	for _, tok := range fields {
		s, err := parseKeyStroke(tok)
		if err != nil {
			return nil, err
		}
		seq = append(seq, s)
	}
	return seq, nil
}

func parseKeyStroke(tok string) (KeyStroke, error) {
	if tok == "" {
		return KeyStroke{}, fmt.Errorf("empty key")
	}
	if canonical, ok := lookupNamedKey(tok); ok {
		return KeyStroke{Key: canonical}, nil
	}
	if len([]rune(tok)) == 1 {
		return KeyStroke{Key: tok}, nil
	}
	ctrl, alt, rest := parseKeyModifiers(tok)
	if rest == "" {
		return KeyStroke{}, fmt.Errorf("missing key after modifier in %q", tok)
	}
	if !ctrl && !alt {
		return KeyStroke{}, fmt.Errorf("unknown key %q", tok)
	}
	if canonical, ok := lookupNamedKey(rest); ok {
		return buildModifiedNamedKey(tok, canonical, ctrl, alt)
	}
	runes := []rune(rest)
	if len(runes) != 1 {
		return KeyStroke{}, fmt.Errorf("unknown key %q", tok)
	}
	r, err := normalizeCtrlRune(tok, runes[0], ctrl)
	if err != nil {
		return KeyStroke{}, err
	}
	return KeyStroke{Key: string(r), Ctrl: ctrl, Alt: alt}, nil
}

func parseKeyModifiers(tok string) (ctrl, alt bool, rest string) {
	rest = tok
	for {
		lower := strings.ToLower(rest)
		switch {
		case strings.HasPrefix(lower, "ctrl+"):
			ctrl = true
			rest = rest[5:]
		case strings.HasPrefix(lower, "c-"):
			ctrl = true
			rest = rest[2:]
		case strings.HasPrefix(lower, "alt+"):
			alt = true
			rest = rest[4:]
		case strings.HasPrefix(lower, "m-"):
			alt = true
			rest = rest[2:]
		default:
			return ctrl, alt, rest
		}
	}
}

func buildModifiedNamedKey(tok, canonical string, ctrl, alt bool) (KeyStroke, error) {
	if canonical == "shift+tab" {
		return KeyStroke{}, fmt.Errorf("unsupported modifier combination %q", tok)
	}
	// Ctrl+Enter is rewritten to Alt+Enter by programInput.
	if ctrl && !alt && canonical == "enter" {
		return KeyStroke{Key: "enter", Alt: true}, nil
	}
	return KeyStroke{Key: canonical, Ctrl: ctrl, Alt: alt}, nil
}

func normalizeCtrlRune(tok string, r rune, ctrl bool) (rune, error) {
	if !ctrl {
		return r, nil
	}
	if r >= 'A' && r <= 'Z' {
		r = r - 'A' + 'a'
	}
	if r < 'a' || r > 'z' {
		return 0, fmt.Errorf("unsupported ctrl key %q", tok)
	}
	return r, nil
}

// ActionDef describes one configurable inbox action.
type ActionDef struct {
	Name        string
	View        string
	Scopes      []string
	Default     []string
	Description string
	Where       string
}

// ActionCatalog returns all configurable inbox actions in deterministic order.
func ActionCatalog() []ActionDef {
	browse := []string{"list", "detail", "status", "contacts"}
	return []ActionDef{
		// Global actions active in all browse views.
		{Name: "quit", View: "global", Scopes: browse, Default: []string{"q"}, Description: "Quit.", Where: "any"},
		{Name: "help", View: "global", Scopes: browse, Default: []string{"?"}, Description: "Show this inbox's shortcuts. ? or Esc closes them.", Where: "any"},
		{Name: "mouse", View: "global", Scopes: browse, Default: []string{"M"}, Description: "Toggle mouse capture.", Where: "any"},
		{Name: "search", View: "global", Scopes: browse, Default: []string{"/"}, Description: "Filter the list, or highlight an item. Empty Enter clears.", Where: "any"},
		{Name: "goto.inbox", View: "global", Scopes: browse, Default: []string{"g i"}, Description: "Go to Inbox.", Where: "any"},
		{Name: "goto.snoozed", View: "global", Scopes: browse, Default: []string{"g z"}, Description: "Go to Snoozed.", Where: "any"},
		{Name: "goto.answered", View: "global", Scopes: browse, Default: []string{"g a"}, Description: "Go to Answered.", Where: "any"},
		{Name: "goto.done", View: "global", Scopes: browse, Default: []string{"g d"}, Description: "Go to Done.", Where: "any"},
		{Name: "goto.withdrawn", View: "global", Scopes: browse, Default: []string{"g w"}, Description: "Go to Withdrawn.", Where: "any"},
		{Name: "goto.status", View: "global", Scopes: browse, Default: []string{"g s"}, Description: "Go to Status.", Where: "any"},
		{Name: "goto.contacts", View: "global", Scopes: browse, Default: []string{"g c"}, Description: "Go to Contacts.", Where: "any"},
		{Name: "goto.outbox", View: "global", Scopes: browse, Default: []string{"g o"}, Description: "Go to Outbox.", Where: "any"},
		{Name: "goto.top", View: "global", Scopes: browse, Default: []string{"g g"}, Description: "Top of the list.", Where: "list, status, contacts"},

		// List view.
		{Name: "list.next", View: "list", Scopes: []string{"list"}, Default: []string{"j"}, Description: "Move to the next row.", Where: "list"},
		{Name: "list.prev", View: "list", Scopes: []string{"list"}, Default: []string{"k"}, Description: "Move to the previous row.", Where: "list"},
		{Name: "list.pageDown", View: "list", Scopes: []string{"list"}, Default: []string{"pgdn"}, Description: "Page down.", Where: "list"},
		{Name: "list.pageUp", View: "list", Scopes: []string{"list"}, Default: []string{"pgup"}, Description: "Page up.", Where: "list"},
		{Name: "list.top", View: "list", Scopes: []string{"list"}, Default: []string{"home"}, Description: "Jump to the top of the list.", Where: "list"},
		{Name: "list.bottom", View: "list", Scopes: []string{"list"}, Default: []string{"G", "end"}, Description: "Jump to the bottom of the list.", Where: "list"},
		{Name: "list.open", View: "list", Scopes: []string{"list"}, Default: []string{"o", "enter"}, Description: "Open the selected row.", Where: "list"},
		{Name: "list.compose", View: "list", Scopes: []string{"list"}, Default: []string{"c"}, Description: "Compose a note.", Where: "list"},
		{Name: "list.archive", View: "list", Scopes: []string{"list"}, Default: []string{"e", "y"}, Description: "Archive: mark done with outcome archived.", Where: "list"},
		{Name: "list.snooze", View: "list", Scopes: []string{"list"}, Default: []string{"b"}, Description: "Snooze. In Snoozed, also offers unsnooze.", Where: "list"},
		{Name: "list.clearFilter", View: "list", Scopes: []string{"list"}, Default: []string{"esc"}, Description: "Clear the agent filter.", Where: "list"},

		// Detail view.
		{Name: "detail.back", View: "detail", Scopes: []string{"detail"}, Default: []string{"u", "esc"}, Description: "Back to the list.", Where: "item"},
		{Name: "detail.scrollDown", View: "detail", Scopes: []string{"detail"}, Default: []string{"j", "down"}, Description: "Scroll down one line.", Where: "item"},
		{Name: "detail.scrollUp", View: "detail", Scopes: []string{"detail"}, Default: []string{"k", "up"}, Description: "Scroll up one line.", Where: "item"},
		{Name: "detail.pageDown", View: "detail", Scopes: []string{"detail"}, Default: []string{"pgdn"}, Description: "Page down.", Where: "item"},
		{Name: "detail.pageUp", View: "detail", Scopes: []string{"detail"}, Default: []string{"pgup"}, Description: "Page up.", Where: "item"},
		{Name: "detail.top", View: "detail", Scopes: []string{"detail"}, Default: []string{"home"}, Description: "Scroll to the top.", Where: "item"},
		{Name: "detail.bottom", View: "detail", Scopes: []string{"detail"}, Default: []string{"G", "end"}, Description: "Scroll to the end.", Where: "item"},
		{Name: "detail.nextMessage", View: "detail", Scopes: []string{"detail"}, Default: []string{"n"}, Description: "Next message.", Where: "item"},
		{Name: "detail.prevMessage", View: "detail", Scopes: []string{"detail"}, Default: []string{"p"}, Description: "Previous message.", Where: "item"},
		{Name: "detail.archive", View: "detail", Scopes: []string{"detail"}, Default: []string{"e", "y"}, Description: "Archive: mark done with outcome archived.", Where: "item"},
		{Name: "detail.snooze", View: "detail", Scopes: []string{"detail"}, Default: []string{"b"}, Description: "Snooze. In Snoozed, also offers unsnooze.", Where: "item"},
		{Name: "detail.openLink", View: "detail", Scopes: []string{"detail"}, Default: []string{"l"}, Description: "Open the only link, or pick one.", Where: "item"},
		{Name: "detail.copyLink", View: "detail", Scopes: []string{"detail"}, Default: []string{"L"}, Description: "Copy the link.", Where: "item"},
		{Name: "detail.copyMenu", View: "detail", Scopes: []string{"detail"}, Default: []string{"Y"}, Description: "Open the copy menu.", Where: "item"},
		{Name: "detail.plain", View: "detail", Scopes: []string{"detail"}, Default: []string{"V"}, Description: "Borderless, unwrapped view.", Where: "item"},
		{Name: "detail.avatar", View: "detail", Scopes: []string{"detail"}, Default: []string{"A"}, Description: "Open the avatar picker for that sender.", Where: "item"},
		{Name: "detail.owner", View: "detail", Scopes: []string{"detail"}, Default: []string{"o"}, Description: "Focus the owner's herdr tab.", Where: "item"},
		{Name: "detail.compose", View: "detail", Scopes: []string{"detail"}, Default: []string{"c"}, Description: "Compose a note.", Where: "item"},
		{Name: "detail.forward", View: "detail", Scopes: []string{"detail"}, Default: []string{"f"}, Description: "Forward the item.", Where: "item"},
		{Name: "detail.outcome", View: "detail", Scopes: []string{"detail"}, Default: []string{"d"}, Description: "Ask for an outcome, then mark done.", Where: "item"},
		{Name: "detail.reply", View: "detail", Scopes: []string{"detail"}, Default: []string{"0"}, Description: "Focus the pinned reply box.", Where: "item"},

		// Status view.
		{Name: "status.next", View: "status", Scopes: []string{"status"}, Default: []string{"j"}, Description: "Move to the next row.", Where: "status"},
		{Name: "status.prev", View: "status", Scopes: []string{"status"}, Default: []string{"k"}, Description: "Move to the previous row.", Where: "status"},
		{Name: "status.pageDown", View: "status", Scopes: []string{"status"}, Default: []string{"pgdn"}, Description: "Page down.", Where: "status"},
		{Name: "status.pageUp", View: "status", Scopes: []string{"status"}, Default: []string{"pgup"}, Description: "Page up.", Where: "status"},
		{Name: "status.top", View: "status", Scopes: []string{"status"}, Default: []string{"home"}, Description: "Jump to the top of the list.", Where: "status"},
		{Name: "status.bottom", View: "status", Scopes: []string{"status"}, Default: []string{"G", "end"}, Description: "Jump to the bottom of the list.", Where: "status"},
		{Name: "status.back", View: "status", Scopes: []string{"status"}, Default: []string{"u"}, Description: "Back to the list.", Where: "status"},
		{Name: "status.compose", View: "status", Scopes: []string{"status"}, Default: []string{"c"}, Description: "Compose a note to the selected agent.", Where: "status"},
		{Name: "status.clearFilter", View: "status", Scopes: []string{"status"}, Default: []string{"esc"}, Description: "Clear the agent filter.", Where: "status"},

		// Contacts view.
		{Name: "contacts.next", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"j"}, Description: "Move to the next contact.", Where: "contacts"},
		{Name: "contacts.prev", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"k"}, Description: "Move to the previous contact.", Where: "contacts"},
		{Name: "contacts.pageDown", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"pgdn"}, Description: "Page down.", Where: "contacts"},
		{Name: "contacts.pageUp", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"pgup"}, Description: "Page up.", Where: "contacts"},
		{Name: "contacts.top", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"home"}, Description: "Jump to the top of the list.", Where: "contacts"},
		{Name: "contacts.bottom", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"G", "end"}, Description: "Jump to the bottom of the list.", Where: "contacts"},
		{Name: "contacts.back", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"u"}, Description: "Back to the list.", Where: "contacts"},
		{Name: "contacts.filter", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"enter"}, Description: "Filter every folder to that agent.", Where: "contacts"},
		{Name: "contacts.clearFilter", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"esc"}, Description: "Clear the agent filter.", Where: "contacts"},
		{Name: "contacts.avatar", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"A"}, Description: "Open the avatar picker for that contact.", Where: "contacts"},
		{Name: "contacts.compose", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"c"}, Description: "Compose a note to the selected contact.", Where: "contacts"},
		{Name: "contacts.star", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"*"}, Description: "Star or unstar the contact.", Where: "contacts"},
		{Name: "contacts.hide", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"x"}, Description: "Hide this contact.", Where: "contacts"},
		{Name: "contacts.showHidden", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"H"}, Description: "Show or hide hidden contacts.", Where: "contacts"},
		{Name: "contacts.hideDead", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"X"}, Description: "Hide every dead contact.", Where: "contacts"},
		{Name: "contacts.release", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"R"}, Description: "Release a dead bus name.", Where: "contacts"},
		{Name: "contacts.alias", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"a"}, Description: "Alias this contact to a bus name.", Where: "contacts"},
		{Name: "contacts.details", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"i"}, Description: "Show or hide details on a narrow screen.", Where: "contacts"},
		{Name: "contacts.orgTree", View: "contacts", Scopes: []string{"contacts"}, Default: []string{"t"}, Description: "Toggle org tree view.", Where: "contacts"},

		// Compose navigation and recipient picker.
		{Name: "compose.send", View: "compose", Scopes: []string{"editor", "compose"}, Default: []string{"alt+enter", "ctrl+s"}, Description: "Send the draft from any focused composer.", Where: "every composer"},
		{Name: "compose.sendAndArchive", View: "compose", Scopes: []string{"editor", "compose"}, Default: []string{"alt+e"}, Description: "Send the reply and archive the thread.", Where: "reply box"},
		{Name: "compose.discard", View: "compose", Scopes: []string{"editor", "compose"}, Default: []string{"alt+d"}, Description: "Discard the reply draft.", Where: "reply box"},
		{Name: "compose.nextField", View: "compose", Scopes: []string{"editor", "compose", "picker"}, Default: []string{"tab"}, Description: "Move to the next compose field.", Where: "compose"},
		{Name: "compose.prevField", View: "compose", Scopes: []string{"editor", "compose", "picker"}, Default: []string{"shift+tab"}, Description: "Move to the previous compose field.", Where: "compose"},
		{Name: "compose.cancel", View: "compose", Scopes: []string{"editor", "compose", "picker"}, Default: []string{"esc"}, Description: "Leave the composer and keep the draft.", Where: "compose"},
		{Name: "compose.pickerNext", View: "compose", Scopes: []string{"picker"}, Default: []string{"down", "ctrl+n"}, Description: "Move down in the recipient list.", Where: "compose recipients"},
		{Name: "compose.pickerPrev", View: "compose", Scopes: []string{"picker"}, Default: []string{"up", "ctrl+p"}, Description: "Move up in the recipient list.", Where: "compose recipients"},
		{Name: "compose.pickerStar", View: "compose", Scopes: []string{"picker"}, Default: []string{"*"}, Description: "Star or unstar the recipient.", Where: "compose recipients"},

		// Editor text-box bindings.
		{Name: "edit.charForward", View: "editor", Scopes: []string{"editor"}, Default: []string{"right", "ctrl+f"}, Description: "Move cursor forward one character.", Where: "editor"},
		{Name: "edit.charBackward", View: "editor", Scopes: []string{"editor"}, Default: []string{"left", "ctrl+b"}, Description: "Move cursor backward one character.", Where: "editor"},
		{Name: "edit.lineNext", View: "editor", Scopes: []string{"editor"}, Default: []string{"down", "ctrl+n"}, Description: "Move cursor to the next line.", Where: "editor"},
		{Name: "edit.linePrev", View: "editor", Scopes: []string{"editor"}, Default: []string{"up", "ctrl+p"}, Description: "Move cursor to the previous line.", Where: "editor"},
		{Name: "edit.lineStart", View: "editor", Scopes: []string{"editor"}, Default: []string{"home", "ctrl+a"}, Description: "Move cursor to the start of the line.", Where: "editor"},
		{Name: "edit.lineEnd", View: "editor", Scopes: []string{"editor"}, Default: []string{"end", "ctrl+e"}, Description: "Move cursor to the end of the line.", Where: "editor"},
		{Name: "edit.deleteForward", View: "editor", Scopes: []string{"editor"}, Default: []string{"delete", "ctrl+d"}, Description: "Delete the character under the cursor.", Where: "editor"},
		{Name: "edit.deleteBackward", View: "editor", Scopes: []string{"editor"}, Default: []string{"backspace", "ctrl+h"}, Description: "Delete the character before the cursor.", Where: "editor"},
		{Name: "edit.killLine", View: "editor", Scopes: []string{"editor"}, Default: []string{"ctrl+k"}, Description: "Delete to the end of the line.", Where: "editor"},
		{Name: "edit.newline", View: "editor", Scopes: []string{"editor"}, Default: []string{"ctrl+j"}, Description: "Insert a newline.", Where: "editor"},
		{Name: "edit.openLine", View: "editor", Scopes: []string{"editor"}, Default: []string{"ctrl+o"}, Description: "Insert a newline and keep the cursor before it.", Where: "editor"},
		{Name: "edit.external", View: "editor", Scopes: []string{"editor"}, Default: []string{"ctrl+x ctrl+e", "ctrl+g"}, Description: "Edit the draft in an external editor.", Where: "editor"},
		{Name: "edit.fill", View: "editor", Scopes: []string{"editor"}, Default: []string{"alt+q"}, Description: "Fill the paragraph at the cursor.", Where: "editor"},
		{Name: "edit.pageUp", View: "editor", Scopes: []string{"editor"}, Default: []string{"pgup"}, Description: "Page up in the text box.", Where: "editor"},
		{Name: "edit.pageDown", View: "editor", Scopes: []string{"editor"}, Default: []string{"pgdn"}, Description: "Page down in the text box.", Where: "editor"},
	}
}

// KeyMap is the resolved set of inbox key bindings.
type KeyMap struct {
	catalog  []ActionDef
	bindings map[string][]KeySeq
	// scopeExact maps scope -> canonical single-stroke key -> action name.
	scopeExact map[string]map[string]string
	// scopePrefixes maps scope -> canonical prefix stroke -> list of (full seq, action).
	scopePrefixes map[string]map[string][]chordEntry
}

type chordEntry struct {
	seq    KeySeq
	action string
}

// DefaultKeyMap returns the default inbox keymap.
func DefaultKeyMap() *KeyMap {
	km, err := ResolveKeyMap(nil)
	if err != nil {
		panic(fmt.Sprintf("decide: invalid default keymap: %v", err))
	}
	return km
}

// ResolveKeyMap applies overrides from ui.keys onto the default keymap
// and validates action names, key syntax, per-view conflicts, and chord prefix clashes.
func ResolveKeyMap(overrides map[string][]string) (*KeyMap, error) {
	catalog := ActionCatalog()
	defs := make(map[string]ActionDef, len(catalog))
	for _, d := range catalog {
		defs[d.Name] = d
	}

	bindings, err := resolveBindings(catalog, defs, overrides)
	if err != nil {
		return nil, err
	}
	scopeExact, scopePrefixes, err := buildScopeIndex(catalog, bindings)
	if err != nil {
		return nil, err
	}
	if err := checkPrefixClashes(scopeExact, scopePrefixes); err != nil {
		return nil, err
	}
	return &KeyMap{
		catalog:       catalog,
		bindings:      bindings,
		scopeExact:    scopeExact,
		scopePrefixes: scopePrefixes,
	}, nil
}

func resolveBindings(catalog []ActionDef, defs map[string]ActionDef, overrides map[string][]string) (map[string][]KeySeq, error) {
	bindings := make(map[string][]KeySeq, len(catalog))
	for _, d := range catalog {
		seqs := make([]KeySeq, 0, len(d.Default))
		for _, raw := range d.Default {
			seq, err := ParseKeySeq(raw)
			if err != nil {
				return nil, fmt.Errorf("decide config ui.keys[%q]: invalid default key %q: %w", d.Name, raw, err)
			}
			seqs = append(seqs, seq)
		}
		bindings[d.Name] = seqs
	}
	overrideNames := make([]string, 0, len(overrides))
	for name := range overrides {
		overrideNames = append(overrideNames, name)
	}
	slices.Sort(overrideNames)
	for _, name := range overrideNames {
		if _, ok := defs[name]; !ok {
			return nil, fmt.Errorf("decide config ui.keys: unknown action %q", name)
		}
		rawList := overrides[name]
		seqs := make([]KeySeq, 0, len(rawList))
		seen := make(map[string]bool, len(rawList))
		for _, raw := range rawList {
			seq, err := ParseKeySeq(raw)
			if err != nil {
				return nil, fmt.Errorf("decide config ui.keys[%q]: invalid key %q: %w", name, raw, err)
			}
			canon := seq.String()
			if !seen[canon] {
				seen[canon] = true
				seqs = append(seqs, seq)
			}
		}
		bindings[name] = seqs
	}
	return bindings, nil
}

func buildScopeIndex(catalog []ActionDef, bindings map[string][]KeySeq) (map[string]map[string]string, map[string]map[string][]chordEntry, error) {
	scopeExact := make(map[string]map[string]string)
	scopePrefixes := make(map[string]map[string][]chordEntry)
	for _, d := range catalog {
		for _, seq := range bindings[d.Name] {
			canon := seq.String()
			for _, scope := range d.Scopes {
				if scopeExact[scope] == nil {
					scopeExact[scope] = make(map[string]string)
				}
				if scopePrefixes[scope] == nil {
					scopePrefixes[scope] = make(map[string][]chordEntry)
				}
				if prev, exists := scopeExact[scope][canon]; exists {
					return nil, nil, fmt.Errorf("decide config ui.keys: key %q in view %q is bound to both %q and %q", canon, scope, prev, d.Name)
				}
				scopeExact[scope][canon] = d.Name
				if len(seq) > 1 {
					prefix := seq[0].String()
					scopePrefixes[scope][prefix] = append(scopePrefixes[scope][prefix], chordEntry{seq: seq, action: d.Name})
				}
			}
		}
	}
	return scopeExact, scopePrefixes, nil
}

func checkPrefixClashes(scopeExact map[string]map[string]string, scopePrefixes map[string]map[string][]chordEntry) error {
	scopes := make([]string, 0, len(scopePrefixes))
	for scope := range scopePrefixes {
		scopes = append(scopes, scope)
	}
	slices.Sort(scopes)
	for _, scope := range scopes {
		prefixes := make([]string, 0, len(scopePrefixes[scope]))
		for p := range scopePrefixes[scope] {
			prefixes = append(prefixes, p)
		}
		slices.Sort(prefixes)
		for _, prefix := range prefixes {
			if soloAction, ok := scopeExact[scope][prefix]; ok {
				first := scopePrefixes[scope][prefix][0]
				return fmt.Errorf("decide config ui.keys: key %q in view %q is both bound to %q and a chord prefix for %q (%q)", prefix, scope, soloAction, first.action, first.seq.String())
			}
		}
	}
	return nil
}

// Bindings returns the key sequences bound to action.
func (km *KeyMap) Bindings(action string) []KeySeq {
	return km.bindings[action]
}

// PrimaryButtonString formats the primary key binding for action as a button
// hint, or "" when action has no bindings.
func (km *KeyMap) PrimaryButtonString(action string, ascii bool) string {
	seqs := km.bindings[action]
	if len(seqs) == 0 {
		return ""
	}
	return seqs[0].ButtonString(ascii)
}

// ConfigKeys returns the canonical key strings for action.
func (km *KeyMap) ConfigKeys(action string) []string {
	seqs := km.bindings[action]
	out := make([]string, len(seqs))
	for i, s := range seqs {
		out[i] = s.String()
	}
	return out
}

// Match reports the action bound to a single key stroke in scope.
func (km *KeyMap) Match(scope string, s KeyStroke) (string, bool) {
	act, ok := km.scopeExact[scope][s.String()]
	return act, ok
}

// IsPrefix reports whether s is a chord prefix in scope.
func (km *KeyMap) IsPrefix(scope string, s KeyStroke) bool {
	return len(km.scopePrefixes[scope][s.String()]) > 0
}

// MatchChord reports the action bound to a 2-stroke chord in scope.
func (km *KeyMap) MatchChord(scope string, prefix, next KeyStroke) (string, bool) {
	seq := KeySeq{prefix, next}.String()
	act, ok := km.scopeExact[scope][seq]
	return act, ok
}

// ChordEntries returns all chord entries in scope starting with prefix.
func (km *KeyMap) ChordEntries(scope string, prefix KeyStroke) []struct {
	Next   KeyStroke
	Action string
} {
	entries := km.scopePrefixes[scope][prefix.String()]
	out := make([]struct {
		Next   KeyStroke
		Action string
	}, 0, len(entries))
	for _, e := range entries {
		if len(e.seq) == 2 {
			out = append(out, struct {
				Next   KeyStroke
				Action string
			}{Next: e.seq[1], Action: e.action})
		}
	}
	return out
}

// FormatKeyMapText formats the active keymap as a human-readable table grouped by view.
func FormatKeyMapText(km *KeyMap) string {
	var b strings.Builder
	curView := ""
	maxAction := 0
	maxKeys := 0
	type row struct {
		view   string
		action string
		keys   string
		desc   string
	}
	rows := make([]row, 0, len(km.catalog))
	for _, d := range km.catalog {
		keys := "(unbound)"
		if cfgKeys := km.ConfigKeys(d.Name); len(cfgKeys) > 0 {
			keys = strings.Join(cfgKeys, ", ")
		}
		if len(d.Name) > maxAction {
			maxAction = len(d.Name)
		}
		if len(keys) > maxKeys {
			maxKeys = len(keys)
		}
		rows = append(rows, row{view: d.View, action: d.Name, keys: keys, desc: d.Description})
	}
	for _, r := range rows {
		if r.view != curView {
			if curView != "" {
				b.WriteByte('\n')
			}
			fmt.Fprintf(&b, "[%s]\n", r.view)
			curView = r.view
		}
		fmt.Fprintf(&b, "  %-*s  %-*s  %s\n", maxAction, r.action, maxKeys, r.keys, r.desc)
	}
	return b.String()
}

// FormatKeyMapJSON formats the active keymap as a JSON object suitable for config.json.
func FormatKeyMapJSON(km *KeyMap) ([]byte, error) {
	keys := make(map[string][]string, len(km.catalog))
	for _, d := range km.catalog {
		keys[d.Name] = km.ConfigKeys(d.Name)
	}
	doc := struct {
		UI struct {
			Keys map[string][]string `json:"keys"`
		} `json:"ui"`
	}{}
	doc.UI.Keys = keys
	data, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		return nil, fmt.Errorf("marshal keymap: %w", err)
	}
	return append(data, '\n'), nil
}

// FormatReadmeKeyTable generates the org-mode key table for README.org from km.
func FormatReadmeKeyTable(km *KeyMap) string {
	formatKeys := func(action string) string {
		seqs := km.Bindings(action)
		if len(seqs) == 0 {
			return "(unbound)"
		}
		parts := make([]string, len(seqs))
		for i, s := range seqs {
			parts[i] = "~" + s.HelpString() + "~"
		}
		return strings.Join(parts, " / ")
	}
	type tableRow struct {
		key    string
		where  string
		action string
	}
	rows := []tableRow{
		{formatKeys("quit"), "any", "Quit."},
		{formatKeys("help") + " / ~esc~", "any", "Show this inbox's shortcuts. " + formatKeys("help") + " or Esc closes them."},
		{formatKeys("list.next") + " / " + formatKeys("list.prev"), "list or item", "Move the row, or scroll one line in an item."},
		{formatKeys("detail.nextMessage") + " / " + formatKeys("detail.prevMessage"), "item", "Next or previous message."},
		{"~g~ then ~i~ ~z~ ~a~ ~d~ ~w~", "any", "Inbox, Snoozed, Answered, Done, Withdrawn."},
		{"~g~ then ~s~ / ~c~", "any", "Status or Contacts."},
		{formatKeys("goto.top") + " / " + formatKeys("list.bottom"), "list, status, contacts", "Top or bottom of the list."},
		{formatKeys("detail.bottom"), "item", "Scroll to the end."},
		{formatKeys("list.pageUp") + " / " + formatKeys("list.pageDown") + ", " + formatKeys("list.top") + " / " + formatKeys("list.bottom"), "list and item", "Page, or jump."},
		{formatKeys("list.open"), "list", "Open the row."},
		{formatKeys("detail.back"), "item, status, contacts", "Back to the list."},
		{formatKeys("list.compose"), "list, item, status, contacts", "Compose a note."},
		{formatKeys("list.archive"), "list, item", "Archive: mark done with outcome ~archived~."},
		{formatKeys("detail.outcome"), "item", "Ask for an outcome, then mark done."},
		{formatKeys("list.snooze"), "list, item", "Snooze. In Snoozed, " + formatKeys("list.snooze") + " also offers unsnooze."},
		{formatKeys("search"), "any", "Filter the list, or highlight an item. Empty Enter clears."},
		{formatKeys("list.clearFilter"), "list", "Clear the agent filter."},
		{"~1~-~9~", "item", "Answer with that option."},
		{formatKeys("detail.reply"), "item", "Focus the pinned reply box."},
		{formatKeys("detail.forward"), "item", "Forward the item."},
		{formatKeys("detail.openLink"), "item", "Open the only link, or pick one."},
		{formatKeys("detail.copyLink"), "item", "Copy the link."},
		{formatKeys("detail.copyMenu"), "item", "Open the copy menu."},
		{formatKeys("detail.plain"), "item", "Borderless, unwrapped view. " + formatKeys("detail.plain") + " again restores it."},
		{formatKeys("detail.avatar"), "item or contacts", "Open the avatar picker for that sender."},
		{formatKeys("detail.owner"), "item", "Focus the owner's herdr tab."},
		{formatKeys("mouse"), "any", "Toggle mouse capture."},
		{formatKeys("contacts.hide") + " / " + formatKeys("contacts.showHidden") + " / " + formatKeys("contacts.hideDead") + " / " + formatKeys("contacts.release") + " / " + formatKeys("contacts.alias"), "contacts", "Hide, show hidden, hide dead, release, alias."},
		{formatKeys("contacts.star"), "contacts, compose recipients", "Star or unstar the name. Starred names lead the list."},
		{formatKeys("contacts.filter"), "contacts", "Filter every folder to that agent."},
		{formatKeys("compose.nextField") + " / " + formatKeys("compose.prevField"), "compose", "Move among recipient, text, and Send."},
		{"~enter~", "compose", "Newline in the text. Send on the Send button."},
		{"~Ctrl+Enter~, " + formatKeys("compose.send"), "every composer", "Send the draft."},
		{formatKeys("compose.sendAndArchive"), "reply box", "Send the reply and archive the thread."},
		{formatKeys("compose.discard"), "reply box", "Discard the reply draft."},
		{formatKeys("edit.fill"), "compose", "Fill the paragraph at the cursor."},
		{formatKeys("edit.external"), "compose", "Edit the draft in an external editor."},
		{formatKeys("compose.pickerPrev") + " / " + formatKeys("compose.pickerNext"), "compose recipients", "Move in the filtered list."},
		{"~backspace~ / " + formatKeys("compose.cancel"), "compose recipients", "Widen the filter, then leave. The draft stays."},
		{"click / wheel", "list or item", "Open a row, pick an option, open a URL, or scroll."},
		{"Shift drag", "any", "Select text in the terminal."},
	}
	var b strings.Builder
	w1, w2, w3 := len("Key"), len("Where"), len("Action")
	for _, r := range rows {
		w1 = max(w1, len(r.key))
		w2 = max(w2, len(r.where))
		w3 = max(w3, len(r.action))
	}
	fmt.Fprintf(&b, "| %-*s | %-*s | %-*s |\n", w1, "Key", w2, "Where", w3, "Action")
	fmt.Fprintf(&b, "|-%s-+-%s-+-%s-|\n", strings.Repeat("-", w1), strings.Repeat("-", w2), strings.Repeat("-", w3))
	for _, r := range rows {
		fmt.Fprintf(&b, "| %-*s | %-*s | %-*s |\n", w1, r.key, w2, r.where, w3, r.action)
	}
	return b.String()
}
