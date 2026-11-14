package main

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"slices"

	"github.com/jaeyeom/experimental/devtools/decide"
	"github.com/jaeyeom/experimental/devtools/decide/inboxrpc"
	"github.com/jaeyeom/experimental/devtools/decide/tui"
)

// hubDisplay is a hub-mode inbox's display settings: the local config
// merged with the hub's display settings from each connect. fetch is
// inboxrpc.Client.Display.
type hubDisplay struct {
	local decide.ConfigFile
	fetch func() (decide.DisplaySettings, uint64)
	seen  uint64
}

// Display implements tui.DisplaySource. The first settings carry a
// note that names the hub's keys and the local overrides. Hub settings
// that do not parse leave the local ones and say why.
func (h *hubDisplay) Display() (tui.Display, bool) {
	settings, gen := h.fetch()
	if gen == 0 || gen == h.seen {
		return tui.Display{}, false
	}
	first := h.seen == 0
	h.seen = gen
	eff, err := decide.MergeDisplay(h.local, settings)
	if err != nil {
		d := displayOf(h.local.Config, nil)
		d.Note = err.Error()
		return d, true
	}
	d := displayOf(eff.Config, eff.Files)
	if first {
		d.Note = eff.Summary()
	}
	return d, true
}

func displayOf(cfg decide.Config, files map[string]decide.AvatarFile) tui.Display {
	return tui.Display{
		Targets:           inboxTargets(cfg),
		Agentbus:          cfg.Recipients.Agentbus,
		RecipientsTimeout: cfg.BusListTimeout(),
		Avatars:           cfg.Avatars,
		AvatarDefault:     cfg.AvatarDefault,
		AvatarMono:        cfg.AvatarMono,
		NoHyperlinks:      !cfg.HyperlinksOn(),
		AvatarFiles:       files,
		HiddenContacts:    append([]string(nil), cfg.HiddenContacts...),
	}
}

// cmdConfig is `decide config show [--effective]`. It runs here even in
// hub mode. Each line is a key, where it came from, and its value.
// --effective adds the hub's display settings for the keys this
// config does not set.
func cmdConfig(args []string, remote string, stdout, stderr io.Writer) int {
	if len(args) == 0 || args[0] != "show" {
		return fail(stderr, fmt.Errorf("usage: decide config show [--effective]"), 2)
	}
	fs := flag.NewFlagSet("config show", flag.ContinueOnError)
	fs.SetOutput(stderr)
	effective := fs.Bool("effective", false, "merge the hub's display settings")
	if code, stop := stopped(parseFlags(fs, args[1:])); stop {
		return code
	}
	if len(fs.Args()) != 0 {
		return fail(stderr, fmt.Errorf("decide config show takes no arguments"), 2)
	}
	local, err := decide.LoadConfigFile()
	if err != nil {
		return fail(stderr, err, 1)
	}
	var hub decide.DisplaySettings
	if *effective {
		if hub, err = fetchHubDisplay(remote); err != nil {
			return fail(stderr, err, 1)
		}
	}
	eff, err := decide.MergeDisplay(local, hub)
	if err != nil {
		return fail(stderr, err, 1)
	}
	keys := make([]string, 0, len(eff.Values))
	for k := range eff.Values {
		keys = append(keys, k)
	}
	slices.Sort(keys)
	for _, k := range keys {
		var value bytes.Buffer
		if err := json.Compact(&value, eff.Values[k]); err != nil {
			return fail(stderr, fmt.Errorf("decide config %s: %w", k, err), 1)
		}
		fmt.Fprintf(stdout, "%s\t%s\t%s\n", k, eff.Sources[k], value.String())
	}
	return 0
}

// fetchHubDisplay is the hub's display settings, or none when hub mode
// is off.
func fetchHubDisplay(remote string) (decide.DisplaySettings, error) {
	settings, err := hubConfig(remote)
	if err != nil || settings.hub.Target == "" {
		return decide.DisplaySettings{}, err
	}
	client := &inboxrpc.Client{Dial: inboxrpc.HubDialer(settings.hub), Label: settings.hub.Target}
	if err := client.Connect(); err != nil {
		return decide.DisplaySettings{}, err //nolint:wrapcheck // the client's error names the hub
	}
	defer client.Close()
	display, _ := client.Display()
	return display, nil
}
