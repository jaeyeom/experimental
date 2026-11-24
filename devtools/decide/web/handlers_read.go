package web

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"os"
	"slices"
	"sort"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

// maxQuery bounds the search text.
const maxQuery = 512

// chordTimeoutMs matches the terminal inbox's 1s chord timeout.
const chordTimeoutMs = 1000

func (s *Server) keyMap() (*decide.KeyMap, bool, error) {
	if s.KeyMap != nil {
		return s.KeyMap()
	}
	km, err := decide.ResolveKeyMap(s.Config.UI.Keys)
	if err != nil {
		return nil, false, err //nolint:wrapcheck // ResolveKeyMap names ui.keys
	}
	return km, s.Config.UI.ASCIIKeys, nil
}

type keymapAction struct {
	Name        string   `json:"name"`
	View        string   `json:"view"`
	Scopes      []string `json:"scopes"`
	Description string   `json:"description"`
	Where       string   `json:"where"`
	Keys        []string `json:"keys"`
	HelpKeys    []string `json:"helpKeys"`
	ButtonKeys  []string `json:"buttonKeys"`
}

func (s *Server) getKeymap(w http.ResponseWriter, _ *http.Request) {
	km, ascii, err := s.keyMap()
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	catalog := decide.ActionCatalog()
	actions := make([]keymapAction, 0, len(catalog))
	for _, d := range catalog {
		seqs := km.Bindings(d.Name)
		keys := make([]string, len(seqs))
		helpKeys := make([]string, len(seqs))
		buttonKeys := make([]string, len(seqs))
		for i, seq := range seqs {
			keys[i] = seq.String()
			helpKeys[i] = seq.HelpString()
			buttonKeys[i] = seq.ButtonString(ascii)
		}
		actions = append(actions, keymapAction{
			Name:        d.Name,
			View:        d.View,
			Scopes:      nonNil(d.Scopes),
			Description: d.Description,
			Where:       d.Where,
			Keys:        keys,
			HelpKeys:    helpKeys,
			ButtonKeys:  buttonKeys,
		})
	}
	writeJSON(w, http.StatusOK, map[string]any{
		"chordTimeoutMs": chordTimeoutMs,
		"asciiKeys":      ascii,
		"actions":        actions,
	})
}

func (s *Server) display() (decide.DisplaySettings, error) {
	if s.Display == nil {
		return decide.DisplaySettings{}, nil
	}
	return s.Display() //nolint:wrapcheck // the reader names its config
}

func (s *Server) orgEnabled() (enabled bool, spec string, err error) {
	if s.OrgConfig == nil {
		return false, "", nil
	}
	enabled, spec, err = s.OrgConfig()
	return enabled && spec != "", spec, err
}

func (s *Server) snapshot(w http.ResponseWriter, _ *http.Request) {
	led, err := decide.Open(s.Backend.Dir)
	if err != nil {
		fail(w, fmt.Errorf("open decide ledger: %w", err), http.StatusInternalServerError)
		return
	}
	seq, _, items, _, err := led.ChangesSince(0)
	if err != nil {
		fail(w, fmt.Errorf("decide changes: %w", err), http.StatusInternalServerError)
		return
	}
	if items == nil {
		items = []decide.Item{}
	}
	open, err := decide.Select(items, decide.StateOpen)
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	board, err := s.Backend.Status()
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	display, err := s.display()
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	draftSeq, drafts, err := s.drafts()
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	orgOn, _, err := s.orgEnabled()
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	st := s.state()
	st.mu.Lock()
	health := st.busHealth
	st.mu.Unlock()
	inbox := decide.InboxEntries(open, nil)
	if inbox == nil {
		inbox = []decide.InboxEntry{}
	}
	targets := s.Targets
	if targets == nil {
		targets = []decide.ConfigTarget{}
	}
	writeJSON(w, http.StatusOK, map[string]any{
		"seq":            seq,
		"items":          items,
		"inbox":          inbox,
		"unreadCount":    decide.UnreadCount(open),
		"status":         board,
		"drafts":         drafts,
		"draftSeq":       draftSeq,
		"display":        display,
		"targets":        targets,
		"agentbus":       s.Agentbus,
		"busHealth":      health,
		"orgTreeEnabled": orgOn,
		"serverTime":     s.now().UTC().Format(time.RFC3339Nano),
	})
}

func (s *Server) drafts() (int64, []decide.Draft, error) {
	ds, err := decide.OpenDraftStore(s.Backend.Dir)
	if err != nil {
		return 0, nil, fmt.Errorf("open decide draft store: %w", err)
	}
	if s.Backend.Now != nil {
		ds.SetClock(s.Backend.Now)
	}
	seq, drafts, err := ds.List()
	if err != nil {
		return 0, nil, fmt.Errorf("list decide drafts: %w", err)
	}
	if drafts == nil {
		drafts = []decide.Draft{}
	}
	return seq, drafts, nil
}

func (s *Server) getItem(w http.ResponseWriter, r *http.Request) {
	id, err := itemID(r)
	if err != nil {
		fail(w, err, 0)
		return
	}
	it, err := s.Backend.Get(id)
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	writeJSON(w, http.StatusOK, it)
}

func (s *Server) getStatus(w http.ResponseWriter, _ *http.Request) {
	board, err := s.Backend.Status()
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	writeJSON(w, http.StatusOK, board)
}

// contact is one row of the Contacts view.
type contact struct {
	Name     string `json:"name"`
	Bus      bool   `json:"bus"`
	Liveness string `json:"liveness,omitempty"`
	Handle   string `json:"handle,omitempty"`
	// When is the bus name's --when text; LivenessAge is the seconds since
	// agentbus observed Liveness; Last is the newest activity on its items.
	When        string         `json:"when,omitempty"`
	LivenessAge *int64         `json:"livenessAge,omitempty"`
	Last        *time.Time     `json:"last,omitempty"`
	Hidden      bool           `json:"hidden"`
	Favorite    bool           `json:"favorite"`
	Counts      map[string]int `json:"counts"`
	// Deliveries counts pending and failed deliveries to the contact.
	Deliveries map[string]int `json:"deliveries"`
}

func displayList(d decide.DisplaySettings, key string) []string {
	var out []string
	_ = json.Unmarshal(d.Keys[key], &out) // an unset or odd key is no names
	return out
}

func (s *Server) contacts(w http.ResponseWriter, r *http.Request) {
	items, err := s.Backend.List()
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	display, err := s.display()
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	var aliases map[string]string
	_ = json.Unmarshal(display.Keys["contactAliases"], &aliases)
	hidden := displayList(display, "hiddenContacts")
	favorites := displayList(display, "favoriteContacts")

	byName := map[string]*contact{}
	get := func(name string) *contact {
		if canon, ok := aliases[name]; ok && canon != "" {
			name = canon
		}
		c := byName[name]
		if c == nil {
			c = &contact{
				Name:       name,
				Hidden:     slices.Contains(hidden, name),
				Favorite:   slices.Contains(favorites, name),
				Counts:     map[string]int{"open": 0, "answered": 0, "done": 0, "withdrawn": 0},
				Deliveries: map[string]int{"pending": 0, "failed": 0},
			}
			byName[name] = c
		}
		return c
	}
	out := map[string]any{}
	if bus, berr := s.busContacts(r.Context()); berr != nil {
		out["busError"] = berr.Error()
	} else {
		for _, b := range bus {
			c := get(b.Name)
			c.Bus, c.Liveness, c.Handle, c.When, c.LivenessAge = true, b.Liveness, b.Handle, b.When, b.LivenessAge
		}
	}
	for i := range items {
		it := &items[i]
		if it.Owner != "" {
			count(get(it.Owner), it)
		}
		if it.Worker != "" {
			get(it.Worker)
		}
	}
	list := make([]*contact, 0, len(byName))
	for _, c := range byName {
		list = append(list, c)
	}
	sort.Slice(list, func(i, j int) bool { return list[i].Name < list[j].Name })
	out["contacts"] = list
	out["hiddenContacts"] = nonNil(hidden)
	out["favoriteContacts"] = nonNil(favorites)
	out["contactAliases"] = aliases
	writeJSON(w, http.StatusOK, out)
}

func nonNil(s []string) []string {
	if s == nil {
		return []string{}
	}
	return s
}

func count(c *contact, it *decide.Item) {
	if at := it.LastMessageAt(); c.Last == nil || at.After(*c.Last) {
		c.Last = &at
	}
	switch {
	case decide.InState(it, decide.StateOpen):
		c.Counts["open"]++
	case it.State == decide.StateAnswered:
		c.Counts["answered"]++
	case it.State == decide.StateDone:
		c.Counts["done"]++
	case it.State == decide.StateWithdrawn:
		c.Counts["withdrawn"]++
	}
	for _, d := range it.Delivery {
		switch {
		case d.Pending():
			c.Deliveries["pending"]++
		case d.State() == decide.OutcomeFailed:
			c.Deliveries["failed"]++
		}
	}
}

type busContact struct {
	Name        string `json:"name"`
	When        string `json:"when"`
	Liveness    string `json:"liveness"`
	Handle      string `json:"handle"`
	LivenessAge *int64 `json:"liveness_age"` //nolint:tagliatelle // the stable agentbus list --json field name
}

// busContacts is `agentbus list --json`. An unavailable bus is an error
// the caller degrades on, not a failed request.
func (s *Server) busContacts(ctx context.Context) ([]busContact, error) {
	if !s.Agentbus {
		return nil, nil
	}
	ctx, cancel := context.WithTimeout(ctx, s.Config.BusListTimeout())
	defer cancel()
	out, err := s.busRun()(ctx, []string{"list", "--json"})
	if err != nil {
		return nil, fmt.Errorf("agentbus list: %w", err)
	}
	var list []busContact
	if err := json.Unmarshal(out, &list); err != nil {
		return nil, fmt.Errorf("agentbus list: %w", err)
	}
	return list, nil
}

func (s *Server) orgTree(w http.ResponseWriter, r *http.Request) {
	enabled, spec, err := s.orgEnabled()
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	if !enabled {
		writeError(w, http.StatusNotFound, "the org tree is off; set ui.orgTree and ui.orgSpec")
		return
	}
	var mod time.Time
	if info, err := os.Stat(spec); err == nil {
		mod = info.ModTime()
	}
	st := s.state()
	st.mu.Lock()
	defer st.mu.Unlock()
	if st.orgTree == nil || st.orgSpec != spec || !st.orgMod.Equal(mod) {
		run := s.Orgctl
		if run == nil {
			run = tui.ExecOrgctl
		}
		ctx, cancel := context.WithTimeout(r.Context(), tui.OrgctlTimeout)
		defer cancel()
		out, err := run(ctx, []string{"tree", "--json", spec})
		if err == nil && !json.Valid(out) {
			err = fmt.Errorf("orgctl tree printed invalid JSON")
		}
		if err != nil {
			writeError(w, http.StatusBadGateway, err.Error())
			return
		}
		st.orgTree, st.orgSpec, st.orgMod = append(json.RawMessage(nil), out...), spec, mod
	}
	var modTime any
	if !mod.IsZero() {
		modTime = mod.UTC().Format(time.RFC3339Nano)
	}
	writeJSON(w, http.StatusOK, map[string]any{"tree": st.orgTree, "modTime": modTime})
}

// folderItems are the items shown in a folder, as the TUI selects them.
func folderItems(all []decide.Item, name string) ([]decide.Item, error) {
	for i, f := range tui.Folders {
		if !strings.EqualFold(f.Name, name) {
			continue
		}
		if i == 0 {
			open, err := decide.Select(all, decide.StateOpen)
			if err != nil {
				return nil, err //nolint:wrapcheck // Select names the state
			}
			entries := decide.InboxEntries(open, nil)
			out := make([]decide.Item, len(entries))
			for j := range entries {
				out[j] = entries[j].Item
			}
			return out, nil
		}
		out := []decide.Item{}
		for _, it := range all {
			if f.Snoozed == it.Snoozed() && (f.Snoozed || decide.InState(&it, f.State)) {
				out = append(out, it)
			}
		}
		decide.Sort(out)
		return out, nil
	}
	return nil, badRequest("unknown folder %q", name)
}

func searchText(it *decide.Item) string {
	fields := []string{it.ID, it.Owner, it.Question, it.Verbatim, it.DeliveryNotice}
	for _, m := range it.Messages {
		fields = append(fields, m.Text, m.Verbatim)
	}
	return strings.ToLower(strings.Join(fields, "\x00"))
}

// searchReq is a parsed search query.
type searchReq struct {
	words  []string
	class  string
	folder string
	log    bool
}

func parseSearch(q url.Values) (searchReq, error) {
	req := searchReq{words: strings.Fields(q.Get("q")), folder: q.Get("folder")}
	if len(q.Get("q")) > maxQuery {
		return req, badRequest("q is longer than %d bytes", maxQuery)
	}
	if raw := q.Get("class"); raw != "" {
		class, err := decide.ParseClass(raw)
		if err != nil {
			return req, badRequest("%v", err)
		}
		req.class = class
	}
	switch q.Get("mode") {
	case "":
	case "log":
		req.log = true
	default:
		return req, badRequest("unknown mode %q", q.Get("mode"))
	}
	if req.folder == "" {
		req.folder = "inbox"
	}
	return req, nil
}

func (sr searchReq) run(all []decide.Item) ([]decide.Item, error) {
	if sr.log {
		opts := decide.SearchOptions{Words: sr.words}
		if sr.class != "" {
			opts.Classes = []string{sr.class}
		}
		found, err := decide.SearchLog(all, opts)
		if err != nil {
			return nil, badRequest("%v", err)
		}
		return found, nil
	}
	in, err := folderItems(all, sr.folder)
	if err != nil {
		return nil, err
	}
	var found []decide.Item
	for i := range in {
		it := &in[i]
		if (sr.class == "" || it.EffectiveClass() == sr.class) && matchAll(searchText(it), sr.words) {
			found = append(found, *it)
		}
	}
	return found, nil
}

func (s *Server) search(w http.ResponseWriter, r *http.Request) {
	sr, err := parseSearch(r.URL.Query())
	if err != nil {
		fail(w, err, 0)
		return
	}
	all, err := s.Backend.List()
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	found, err := sr.run(all)
	if err != nil {
		fail(w, err, http.StatusInternalServerError)
		return
	}
	if found == nil {
		found = []decide.Item{}
	}
	writeJSON(w, http.StatusOK, map[string]any{"items": found})
}

func matchAll(text string, words []string) bool {
	for _, w := range words {
		if !strings.Contains(text, strings.ToLower(w)) {
			return false
		}
	}
	return true
}
