package daemon

import (
	"errors"
	"fmt"
	"strings"

	"github.com/jaeyeom/experimental/devtools/agentbus/internal/eventlog"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/hibernate"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/lease"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/mailbox"
	"github.com/jaeyeom/experimental/devtools/agentbus/internal/memreport"
	"github.com/jaeyeom/experimental/devtools/pkg/hubcall"
)

// hibernateActor is the actor of the hibernate events.
const hibernateActor = "agentbus hibernate"

// hibernateOp handles the hibernate-* ops of `agentbus hibernate`,
// which reads the panes and stops the agents on its own machine: state
// reads what the checks need, begin writes the record before the stop,
// end marks how the stop went, and refused logs a refusal.
func (s *server) hibernateOp(req Request) Response {
	var args hibernate.Args
	if req.Hibernate != nil {
		args = *req.Hibernate
	}
	var (
		r   hibernate.Reply
		err error
	)
	switch req.Op {
	case "hibernate-state":
		r, err = s.hibernateState()
	case "hibernate-begin":
		r, err = s.hibernateBegin(req, args)
	case "hibernate-end":
		r, err = s.hibernateEnd(req, args)
	case "hibernate-refused":
		err = s.logHibernate(eventlog.Event{
			Event:  eventlog.EventHibernateRefused,
			Name:   s.agentName(args.Handle),
			Detail: "refused",
			Reason: strings.Join(args.Reasons, "; "),
		})
	default:
		return Response{Error: "unknown op"}
	}
	if err != nil {
		return Response{Error: err.Error()}
	}
	return Response{OK: true, Hibernate: &r}
}

func (s *server) hibernateState() (hibernate.Reply, error) {
	st, err := s.leases.Status(lease.Args{All: true})
	if err != nil {
		return hibernate.Reply{}, fmt.Errorf("read the lease table: %w", err)
	}
	return hibernate.Reply{
		Names:   s.reg.Names(),
		Seen:    s.reg.Seen(),
		Records: s.reg.Hibernations(),
		Leases:  st.Leases,
	}, nil
}

// hibernateBegin writes the record unless the agent holds or is queued
// for a lease: one it got since the caller checked.
func (s *server) hibernateBegin(req Request, args hibernate.Args) (hibernate.Reply, error) {
	if args.Record == nil || args.Record.Handle == "" {
		return hibernate.Reply{}, errors.New("hibernate-begin needs a record with a handle")
	}
	if err := s.ownAgent(req, args.Record.Handle); err != nil {
		return hibernate.Reply{}, err
	}
	st, err := s.leases.Status(lease.Args{All: true})
	if err != nil {
		return hibernate.Reply{}, fmt.Errorf("read the lease table: %w", err)
	}
	if reasons := hibernate.LeaseReasons(st.Leases, args.Record.Handle); len(reasons) > 0 {
		return hibernate.Reply{}, errors.New(strings.Join(reasons, "; "))
	}
	rec, err := s.reg.BeginHibernation(*args.Record)
	if err != nil {
		return hibernate.Reply{}, fmt.Errorf("write the hibernation record: %w", err)
	}
	return hibernate.Reply{Record: &rec}, nil
}

// hibernateEnd marks the record and logs agent_hibernated, or
// agent_hibernate_refused for a stop that failed.
func (s *server) hibernateEnd(req Request, args hibernate.Args) (hibernate.Reply, error) {
	if err := s.ownAgent(req, args.Handle); err != nil {
		return hibernate.Reply{}, err
	}
	reason := strings.Join(args.Reasons, "; ")
	rec, err := s.reg.EndHibernation(args.Handle, args.Status, reason)
	if err != nil {
		return hibernate.Reply{}, fmt.Errorf("mark the hibernation record: %w", err)
	}
	ev := eventlog.Event{
		Event:  eventlog.EventHibernated,
		Name:   s.agentName(args.Handle),
		Detail: memreport.HumanBytes(rec.RSSBytes) + " freed",
	}
	if rec.Status == hibernate.StatusStopFailed {
		ev = eventlog.Event{Event: eventlog.EventHibernateRefused, Name: ev.Name, Detail: rec.Status, Reason: reason}
	}
	if err := s.logHibernate(ev); err != nil {
		return hibernate.Reply{}, err
	}
	return hibernate.Reply{Record: &rec}, nil
}

func (s *server) logHibernate(ev eventlog.Event) error {
	ev.Actor = hibernateActor
	if err := s.log.Append(ev); err != nil {
		return fmt.Errorf("log %s: %w", ev.Event, err)
	}
	return nil
}

// wakeAgent relaunches a hibernated agent with no message to deliver.
// The daemon runs the relaunch through its own herdr adapter, which
// reaches an agent on another machine with `herdr --machine`, so in hub
// mode the hub wakes any machine's agent. Unlike hibernate, any caller
// may wake any agent: a message to it would wake it anyway.
//
// A Name with a Machine is a pane id there, and a pane with no
// hibernation record is not hibernated. A failed wake's State says why
// when the agent was not relaunched: WakeNotHibernated or WakeUnknown.
func (s *server) wakeAgent(req Request) Response {
	target := req.Name
	if req.Machine != "" {
		machine := req.Machine
		if caller, remote := s.remoteCaller(req.Env); remote && machine == hubcall.LocalMachine {
			machine = handleMachine(caller)
		}
		handle, ok := s.reg.HibernationOfPane(machine, req.Name)
		if !ok {
			return Response{Error: fmt.Sprintf("pane %s on %s is not hibernated", req.Name, machine), State: WakeNotHibernated}
		}
		target = handle
	}
	handle, err := s.box.Wake(s.ctx, target)
	switch {
	case errors.Is(err, mailbox.ErrUnknownTarget):
		return Response{Error: err.Error(), State: WakeUnknown}
	case errors.Is(err, mailbox.ErrNotHibernated):
		return Response{Error: err.Error(), Handle: handle, State: WakeNotHibernated}
	case err != nil:
		return Response{Error: err.Error(), Handle: handle}
	}
	return Response{OK: true, Name: req.Name, Handle: handle}
}

// The States of a wake that relaunched nothing.
const (
	WakeNotHibernated = "not-hibernated"
	WakeUnknown       = "unknown"
)

// agentName is handle's first name, else handle.
func (s *server) agentName(handle string) string {
	if names := s.reg.NamesOf(handle); len(names) > 0 {
		return names[0]
	}
	return handle
}

// ownAgent refuses a caller on another machine a hibernation record for
// an agent on a machine other than its own: hibernate stops agents on
// its own machine only. The user and callers on this machine may write
// any.
func (s *server) ownAgent(req Request, handle string) error {
	caller, remote := s.remoteCaller(req.Env)
	if !remote {
		return nil
	}
	machine := handleMachine(caller)
	if machine == "" || handleMachine(handle) != machine {
		return fmt.Errorf("%s is not on machine %s; %s may hibernate only its own machine's agents", handle, machine, caller)
	}
	return nil
}

// handleMachine is the machine part of <adapter>:<machine>/....
func handleMachine(handle string) string {
	_, rest, _ := strings.Cut(handle, ":")
	machine, _, _ := strings.Cut(rest, "/")
	return machine
}
