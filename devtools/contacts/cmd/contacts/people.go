package main

import (
	"errors"
	"flag"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/contacts"
)

// stringList is a repeatable string flag.
type stringList []string

func (l *stringList) String() string     { return strings.Join(*l, ",") }
func (l *stringList) Set(v string) error { *l = append(*l, v); return nil }

// personFlags are the person fields shared by add and edit.
type personFlags struct {
	aliases, handles        stringList
	team, role, tz, channel string
}

func (pf *personFlags) register(fs *flag.FlagSet) {
	fs.Var(&pf.aliases, "alias", "another name for the person (repeatable)")
	fs.Var(&pf.handles, "handle", "kind:value, kind one of chat, code, tracker, email, other:<label> (repeatable)")
	fs.StringVar(&pf.team, "team", "", "team")
	fs.StringVar(&pf.role, "role", "", "role")
	fs.StringVar(&pf.tz, "tz", "", "IANA timezone, such as Europe/London")
	fs.StringVar(&pf.channel, "channel", "", "preferred channel")
}

func parseHandles(values []string) ([]contacts.Handle, error) {
	var hs []contacts.Handle
	for _, v := range values {
		h, err := contacts.ParseHandle(v)
		if err != nil {
			return nil, fmt.Errorf("parse handle: %w", err)
		}
		hs = append(hs, h)
	}
	return hs, nil
}

// open loads the config and opens the store, printing any error.
func (a *app) open() (*contacts.Store, contacts.Config, bool) {
	cfg, err := a.config()
	if err != nil {
		a.errorf("%v", err)
		return nil, cfg, false
	}
	s, err := contacts.Open(cfg.DataDir, a.now)
	if err != nil {
		a.errorf("%v", err)
		return nil, cfg, false
	}
	return s, cfg, true
}

// fail prints err and returns its exit code: exitUsage for no such person,
// exitAmbiguous for several, exitError otherwise.
func (a *app) fail(err error) int {
	a.errorf("%v", err)
	var amb *contacts.AmbiguousError
	switch {
	case errors.Is(err, contacts.ErrNotFound):
		return exitUsage
	case errors.As(err, &amb):
		return exitAmbiguous
	}
	return exitError
}

// find resolves a <person> argument, printing any error. code is the exit
// code to return when ok is false.
func (a *app) find(s *contacts.Store, query string) (p contacts.Person, code int, ok bool) {
	p, err := s.Find(query)
	if errors.Is(err, contacts.ErrNotFound) {
		err = fmt.Errorf("%w: %q", err, query)
	}
	if err != nil {
		return p, a.fail(err), false
	}
	return p, exitOK, true
}

func (a *app) cmdAdd(args []string) int {
	fs := a.flags("add")
	caller := a.callerFlag(fs)
	var pf personFlags
	pf.register(fs)
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
		return exitUsage
	}
	handles, err := parseHandles(pf.handles)
	if err != nil {
		a.errorf("%v", err)
		return exitUsage
	}
	s, _, ok := a.open()
	if !ok {
		return exitError
	}
	defer s.Close()
	id, err := s.Add(contacts.Person{
		Name: pos[0], Aliases: pf.aliases, Handles: handles,
		Team: pf.team, Role: pf.role, TZ: pf.tz, Channel: pf.channel,
	})
	if err != nil {
		return a.fail(err)
	}
	a.logAccess(s, caller(), "add", id, "")
	fmt.Fprintln(a.stdout, id)
	return exitOK
}

func (a *app) cmdEdit(args []string) int {
	fs := a.flags("edit")
	caller := a.callerFlag(fs)
	var pf personFlags
	pf.register(fs)
	var removeAliases, removeHandles stringList
	fs.Var(&removeAliases, "remove-alias", "alias to remove (repeatable)")
	fs.Var(&removeHandles, "remove-handle", "kind:value to remove (repeatable)")
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
		return exitUsage
	}
	add, err := parseHandles(pf.handles)
	if err != nil {
		a.errorf("%v", err)
		return exitUsage
	}
	remove, err := parseHandles(removeHandles)
	if err != nil {
		a.errorf("%v", err)
		return exitUsage
	}
	patch := contacts.Patch{AddAliases: pf.aliases, RemoveAliases: removeAliases, AddHandles: add, RemoveHandles: remove}
	fs.Visit(func(f *flag.Flag) {
		switch f.Name {
		case "team":
			patch.Team = &pf.team
		case "role":
			patch.Role = &pf.role
		case "tz":
			patch.TZ = &pf.tz
		case "channel":
			patch.Channel = &pf.channel
		}
	})
	s, _, ok := a.open()
	if !ok {
		return exitError
	}
	defer s.Close()
	p, code, ok := a.find(s, pos[0])
	if !ok {
		return code
	}
	if err := s.Edit(p.ID, patch); err != nil {
		return a.fail(err)
	}
	a.logAccess(s, caller(), "edit", p.ID, "")
	fmt.Fprintln(a.stdout, p.ID)
	return exitOK
}

func (a *app) cmdMerge(args []string) int {
	fs := a.flags("merge")
	caller := a.callerFlag(fs)
	pos, ok := a.parse(fs, args, 2, 2)
	if !ok {
		return exitUsage
	}
	s, _, ok := a.open()
	if !ok {
		return exitError
	}
	defer s.Close()
	keep, code, ok := a.find(s, pos[0])
	if !ok {
		return code
	}
	drop, code, ok := a.find(s, pos[1])
	if !ok {
		return code
	}
	if err := s.Merge(keep.ID, drop.ID); err != nil {
		return a.fail(err)
	}
	a.logAccess(s, caller(), "merge", keep.ID, "")
	fmt.Fprintf(a.stdout, "merged %s into %s\n", drop.ID, keep.ID)
	return exitOK
}

func (a *app) cmdDelete(args []string) int {
	fs := a.flags("delete")
	caller := a.callerFlag(fs)
	yes := fs.Bool("yes", false, "confirm the deletion")
	pos, ok := a.parse(fs, args, 1, 1)
	if !ok {
		return exitUsage
	}
	if !*yes {
		a.errorf("delete removes the person, their events, commitments and notes file; pass --yes to confirm")
		return exitUsage
	}
	s, _, ok := a.open()
	if !ok {
		return exitError
	}
	defer s.Close()
	p, code, ok := a.find(s, pos[0])
	if !ok {
		return code
	}
	if err := s.Delete(p.ID); err != nil {
		return a.fail(err)
	}
	a.logAccess(s, caller(), "delete", p.ID, "")
	fmt.Fprintf(a.stdout, "deleted %s\n", p.ID)
	return exitOK
}
