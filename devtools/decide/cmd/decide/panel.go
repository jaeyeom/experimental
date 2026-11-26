package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/decide"
)

// PanelListItem is one panel row printed by `decide panel ls --json`.
type PanelListItem struct {
	ID         string          `json:"id"`
	File       string          `json:"file,omitempty"`
	Title      string          `json:"title,omitempty"`
	Kind       string          `json:"kind,omitempty"`
	Data       json.RawMessage `json:"data,omitempty"`
	UpdatedAt  *time.Time      `json:"updatedAt,omitempty"`
	StaleAfter string          `json:"staleAfter,omitempty"`
	Source     string          `json:"source,omitempty"`
	Age        string          `json:"age,omitempty"`
	Stale      bool            `json:"stale"`
	Unreadable bool            `json:"unreadable,omitempty"`
	Error      string          `json:"error,omitempty"`
}

func cmdPanel(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		return fail(stderr, fmt.Errorf("decide panel requires set, rm, or ls"), 2)
	}
	switch args[0] {
	case "set":
		return cmdPanelSet(args[1:], stdout, stderr)
	case "rm":
		return cmdPanelRm(args[1:], stdout, stderr)
	case "ls":
		return cmdPanelLs(args[1:], stdout, stderr)
	default:
		return fail(stderr, fmt.Errorf("decide panel command %q is unknown", args[0]), 2)
	}
}

func cmdPanelSet(args []string, stdout, stderr io.Writer) int {
	args = hoistFlags(args, map[string]bool{
		"title": true, "kind": true, "data": true, "file": true, "stale-after": true, "source": true,
	})
	fs := flag.NewFlagSet("panel set", flag.ContinueOnError)
	fs.SetOutput(stderr)
	title := fs.String("title", "", "panel title")
	kind := fs.String("kind", "", "panel component kind")
	var dataFlag setString
	var fileFlag setString
	fs.Var(&dataFlag, "data", "panel data JSON")
	fs.Var(&fileFlag, "file", "file containing panel data JSON (or - for stdin)")
	staleAfter := fs.String("stale-after", "", "optional Go duration after which panel is stale")
	source := fs.String("source", "", "optional free-text writer name")
	jsonOut := fs.Bool("json", false, "print the saved panel as JSON")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	id, err := oneArg(fs.Args(), "panel id")
	if err != nil {
		return fail(stderr, err, 2)
	}
	if err := decide.ValidPanelID(id); err != nil {
		return fail(stderr, err, 2)
	}
	if dataFlag.set == fileFlag.set {
		return fail(stderr, fmt.Errorf("decide panel set requires exactly one of --data or --file"), 2)
	}
	raw, err := readPanelDataFlag(dataFlag, fileFlag)
	if err != nil {
		return fail(stderr, err, 2)
	}
	store, err := openPanels()
	if err != nil {
		return fail(stderr, err, 1)
	}
	saved, err := store.Set(decide.Panel{
		ID:         id,
		Title:      *title,
		Kind:       *kind,
		Data:       raw,
		StaleAfter: *staleAfter,
		Source:     *source,
	})
	if err != nil {
		return fail(stderr, err, 2)
	}
	if *jsonOut {
		return writeJSON(stdout, stderr, saved)
	}
	fmt.Fprintf(stdout, "%s %s %s\n", saved.ID, saved.Kind, saved.Title)
	return 0
}

func readPanelDataFlag(dataFlag, fileFlag setString) (json.RawMessage, error) {
	if dataFlag.set {
		return json.RawMessage(dataFlag.val), nil
	}
	if fileFlag.val == "-" {
		raw, err := io.ReadAll(textStdin())
		if err != nil {
			return nil, fmt.Errorf("read panel data: %w", err)
		}
		return json.RawMessage(raw), nil
	}
	raw, err := os.ReadFile(fileFlag.val) //nolint:gosec // operator-specified JSON file
	if err != nil {
		return nil, fmt.Errorf("read panel file: %w", err)
	}
	return json.RawMessage(raw), nil
}

func cmdPanelRm(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("panel rm", flag.ContinueOnError)
	fs.SetOutput(stderr)
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	id, err := oneArg(fs.Args(), "panel id")
	if err != nil {
		return fail(stderr, err, 2)
	}
	if err := decide.ValidPanelID(id); err != nil {
		return fail(stderr, err, 2)
	}
	store, err := openPanels()
	if err != nil {
		return fail(stderr, err, 1)
	}
	if err := store.Remove(id); err != nil {
		return fail(stderr, err, 1)
	}
	fmt.Fprintf(stdout, "removed %s\n", id)
	return 0
}

func cmdPanelLs(args []string, stdout, stderr io.Writer) int {
	fs := flag.NewFlagSet("panel ls", flag.ContinueOnError)
	fs.SetOutput(stderr)
	jsonOut := fs.Bool("json", false, "print panels as JSON")
	if code, stop := stopped(parseFlags(fs, args)); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide panel ls does not take arguments"), 2)
	}
	store, err := openPanels()
	if err != nil {
		return fail(stderr, err, 1)
	}
	now := time.Now()
	store.SetClock(func() time.Time { return now })
	entries, err := store.List()
	if err != nil {
		return fail(stderr, err, 1)
	}
	items := panelListItems(entries, now)
	if *jsonOut {
		return writeJSON(stdout, stderr, items)
	}
	for _, it := range items {
		if it.Unreadable {
			name := it.File
			if name == "" {
				name = it.ID
			}
			fmt.Fprintf(stdout, "panel unreadable: %s\n", name)
			continue
		}
		stale := ""
		if it.Stale {
			stale = "\tstale"
		}
		fmt.Fprintf(stdout, "%s\t%s\t%s\t%s%s\n", it.ID, it.Title, it.Kind, it.Age, stale)
	}
	return 0
}

func panelListItems(entries []decide.PanelEntry, now time.Time) []PanelListItem {
	items := make([]PanelListItem, 0, len(entries))
	for _, e := range entries {
		item := PanelListItem{
			ID:         e.ID,
			File:       e.File,
			Title:      e.Title,
			Kind:       e.Kind,
			Data:       e.Data,
			UpdatedAt:  e.UpdatedAt,
			StaleAfter: e.StaleAfter,
			Source:     e.Source,
			Stale:      e.Stale,
			Unreadable: e.Unreadable,
			Error:      e.Error,
		}
		if !e.Unreadable && e.UpdatedAt != nil {
			item.Age = decide.PanelAge(now.Sub(*e.UpdatedAt))
		}
		items = append(items, item)
	}
	return items
}

func openPanels() (*decide.PanelStore, error) {
	dir, err := decide.DefaultDir()
	if err != nil {
		return nil, fmt.Errorf("decide directory: %w", err)
	}
	store, err := decide.OpenPanels(dir)
	if err != nil {
		return nil, fmt.Errorf("open decide panels: %w", err)
	}
	return store, nil
}

func panelBadDetail(e decide.PanelEntry) string {
	name := e.File
	if name == "" {
		name = e.ID
	}
	if strings.TrimSpace(e.Error) == "" {
		return name
	}
	return fmt.Sprintf("%s (%s)", name, e.Error)
}
