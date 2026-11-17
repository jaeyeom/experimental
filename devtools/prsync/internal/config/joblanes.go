package config

import (
	"fmt"
	"slices"
	"strings"
)

// QueueMachine is the agentbus job-queue name of a herdr machine: local
// or one of Machines. job_machines renames it; by default the name is
// the same.
func (c Config) QueueMachine(machine string) string {
	if name := c.JobMachines[machine]; name != "" {
		return name
	}
	return machine
}

// applyMachineKey applies a key that maps machines: machine_checkouts,
// job_machines, or job_lanes.
func applyMachineKey(cfg *Config, key, val string) error {
	switch key {
	case "machine_checkouts":
		return applyMachineCheckouts(cfg, val)
	case "job_machines":
		out, err := parseMachineValues(key, val, false)
		cfg.JobMachines = out
		return err
	case "job_lanes":
		return applyJobLanes(cfg, val)
	}
	return nil
}

// applyJobLanes reads comma-separated label=machine entries.
func applyJobLanes(cfg *Config, val string) error {
	out := make(map[string]string)
	for _, entry := range strings.Split(val, ",") {
		entry = strings.TrimSpace(entry)
		if entry == "" {
			continue
		}
		label, machine, ok := strings.Cut(entry, "=")
		label, machine = strings.TrimSpace(label), strings.TrimSpace(machine)
		if !ok || label == "" || machine == "" {
			return &KeyError{Key: "job_lanes", Reason: fmt.Sprintf("invalid entry %q; want label=machine", entry)}
		}
		out[label] = machine
	}
	cfg.JobLanes = out
	return nil
}

func validateJobs(c Config) error {
	known := func(m string) bool { return m == LocalMachine || slices.Contains(c.Machines, m) }
	for m := range c.JobMachines {
		if !known(m) {
			return &KeyError{Key: "job_machines", Reason: fmt.Sprintf("unknown machine %q; want local or one of machines", m)}
		}
	}
	for label, m := range c.JobLanes {
		if !known(m) {
			return &KeyError{Key: "job_lanes", Reason: fmt.Sprintf("unknown machine %q for %s; want local or one of machines", m, label)}
		}
	}
	return nil
}
