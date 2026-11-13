package main

import (
	"context"
	"fmt"
	"io"
	"slices"
	"strings"

	"github.com/jaeyeom/experimental/devtools/slackwatch"
)

// listAgents returns names from `agentbus list`. Tests replace it.
var listAgents = slackwatch.AgentbusNames

// checkOwner accepts owner when it is a config target or an agentbus
// name. unchecked skips the check. When agentbus cannot be queried,
// the owner is accepted and a warning is written to stderr.
func checkOwner(cfg slackwatch.Config, owner string, unchecked bool, stderr io.Writer) error {
	if unchecked || owner == "" {
		return nil
	}
	names, err := listAgents(context.Background())
	if err != nil {
		fmt.Fprintf(stderr, "slackwatch: agentbus list failed (%v); accepting --owner %s\n", err, owner)
		return nil
	}
	known := knownOwners(cfg, names)
	if slices.Contains(known, owner) {
		return nil
	}
	list := "(none)"
	if len(known) > 0 {
		list = strings.Join(known, ", ")
	}
	return fmt.Errorf("unknown owner %q; known names: %s", owner, list)
}

func knownOwners(cfg slackwatch.Config, agents []string) []string {
	set := make(map[string]struct{}, len(cfg.Targets)+len(agents))
	for name := range cfg.Targets {
		if name != "" {
			set[name] = struct{}{}
		}
	}
	for _, name := range agents {
		if name != "" {
			set[name] = struct{}{}
		}
	}
	out := make([]string, 0, len(set))
	for name := range set {
		out = append(out, name)
	}
	slices.Sort(out)
	return out
}
