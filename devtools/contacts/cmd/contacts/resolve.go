package main

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/contacts"
)

// callerFlag registers --by and returns a function giving the caller:
// --by, else $CONTACTS_CALLER, else "unknown".
func (a *app) callerFlag(fs *flag.FlagSet) func() string {
	by := fs.String("by", "", "the calling agent (default $CONTACTS_CALLER)")
	return func() string {
		if *by != "" {
			return *by
		}
		if c := a.getenv("CONTACTS_CALLER"); c != "" {
			return c
		}
		return "unknown"
	}
}

// logAccess writes one access-log row; a failure is reported but does not
// change the command's result.
func (a *app) logAccess(s *contacts.Store, caller, command, personID, reason string) {
	if err := s.LogAccess(caller, command, personID, reason); err != nil {
		a.errorf("%v", err)
	}
}

func (a *app) printJSON(v any) {
	enc := json.NewEncoder(a.stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(v); err != nil {
		a.errorf("write json: %v", err)
	}
}

func (a *app) cmdResolve(args []string) int {
	fs := a.flags("resolve")
	asJSON := fs.Bool("json", false, "print JSON")
	reason := fs.String("reason", "", "why you are looking this person up")
	caller := a.callerFlag(fs)
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
		return exitUsage
	}
	s, _, ok := a.open()
	if !ok {
		return exitError
	}
	defer s.Close()
	p, err := s.Resolve(pos[0])
	a.logAccess(s, caller(), "resolve", p.ID, *reason)
	var amb *contacts.AmbiguousError
	switch {
	case errors.As(err, &amb):
		if *asJSON {
			a.printJSON(map[string]any{"candidates": amb.Candidates})
		} else {
			for _, c := range amb.Candidates {
				fmt.Fprintf(a.stdout, "%s\t%s\t%s\n", c.ID, c.Name, c.Team)
			}
		}
		return exitAmbiguous
	case errors.Is(err, contacts.ErrNotFound):
		if *asJSON {
			a.printJSON(map[string]any{"candidates": []contacts.Candidate{}})
		}
		a.errorf("no match for %q", pos[0])
		return exitUsage
	case err != nil:
		return a.fail(err)
	}
	d := s.Detail(p)
	if *asJSON {
		a.printJSON(d)
		return exitOK
	}
	a.printDetail(d)
	return exitOK
}

func (a *app) printDetail(d contacts.Detail) {
	line := func(label, value string) {
		if value != "" {
			fmt.Fprintf(a.stdout, "%-9s %s\n", label+":", value)
		}
	}
	line("id", d.ID)
	line("name", d.Name)
	line("aliases", strings.Join(d.Aliases, ", "))
	line("team", d.Team)
	line("role", d.Role)
	if d.TZ != "" {
		line("timezone", fmt.Sprintf("%s (now %s)", d.TZ, d.LocalTime))
	}
	line("channel", d.Channel)
	handles := make([]string, len(d.Handles))
	for i, h := range d.Handles {
		handles[i] = h.String()
	}
	line("handles", strings.Join(handles, ", "))
	line("notes", d.Notes)
}
