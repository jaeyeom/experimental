// Package jobs turns a scan into agentbus job specs, and talks to the
// agentbus job queue for dispatch --job.
package jobs

import (
	"fmt"
	"slices"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// KindConflict is the agent job a rebase becomes when GitHub cannot
// rebase the branch without a conflict.
const KindConflict = "conflict"

// Affinity values, most specific first. A job with none runs on any
// machine.
const (
	AffinityTab   = "tab"
	AffinityLane  = "lane"
	AffinityOwner = "owner"
)

// Executor values.
const (
	ExecutorCLI    = "cli"
	ExecutorTab    = "tab"
	ExecutorNewTab = "new-tab"
)

// MachineAny is the machine of a job with no affinity.
const MachineAny = "any"

// Spec is one job, in the terms of agentbus jobs enqueue.
type Spec struct {
	Kind     string   `json:"kind"`
	Subject  string   `json:"subject"`
	Head     string   `json:"head,omitempty"`
	Machine  string   `json:"machine"`
	Affinity string   `json:"affinity,omitempty"`
	Executor string   `json:"executor"`
	Items    []string `json:"items,omitempty"`
}

// Document is the scan --emit-jobs output.
type Document struct {
	Jobs []Spec `json:"jobs"`
}

// Build returns the jobs each open pull request in doc needs, in
// document order: merge-queue-fix, comment, ci-fix, then rebase. A
// draft (unless include_drafts) and a pull request whose tab could not
// be searched get none. tab_machine scope is ignored: one scan covers
// every machine's tabs.
func Build(doc scan.Document, owners Owners, cfg config.Config) Document {
	out := Document{Jobs: []Spec{}}
	for _, pr := range doc.PRs {
		if pr.State != "" && pr.State != "open" {
			continue
		}
		if pr.IsDraft && !cfg.IncludeDrafts {
			continue
		}
		if pr.Tab == nil && pr.TabUnknown != "" {
			continue
		}
		out.Jobs = append(out.Jobs, prJobs(pr, owners, cfg)...)
	}
	return out
}

func prJobs(pr scan.PR, owners Owners, cfg config.Config) []Spec {
	machine, affinity := Affinity(pr, owners, cfg)
	base := Spec{
		Subject:  Subject(pr.Repo, pr.Number),
		Head:     pr.HeadSHA,
		Machine:  machine,
		Affinity: affinity,
		Executor: AgentExecutor(affinity),
	}
	var out []Spec
	add := func(kind string, items []string) {
		s := base
		s.Kind = kind
		s.Items = items
		out = append(out, s)
	}
	if hasAction(pr, scan.ActionMergeQueueFix) {
		add(dispatch.KindMergeQueueFix, nil)
	}
	if pr.Unaddressed {
		ids := make([]string, 0, len(pr.BlockingComments))
		for _, c := range pr.BlockingComments {
			ids = append(ids, c.CommentID)
		}
		add(dispatch.KindComment, ids)
	}
	if pr.CIState == "failing" {
		add(dispatch.KindCIFix, nil)
	}
	if needsCatchUp(pr) {
		add(dispatch.KindRebase, nil)
		out[len(out)-1].Executor = ExecutorCLI
	}
	return out
}

// Affinity returns the machine a pull request's jobs need, most
// specific first: the machine of its agent tab, the lane of its first
// label in job_lanes (by name), the machine whose agent last worked it,
// or any machine. Machine names are job-queue names (job_machines).
func Affinity(pr scan.PR, owners Owners, cfg config.Config) (machine, affinity string) {
	if pr.Tab != nil && pr.Tab.PaneID != nil {
		return cfg.QueueMachine(TabMachine(pr.Tab)), AffinityTab
	}
	labels := slices.Clone(pr.Labels)
	slices.Sort(labels)
	for _, label := range labels {
		if m := cfg.JobLanes[label]; m != "" {
			return cfg.QueueMachine(m), AffinityLane
		}
	}
	if m := owners[Subject(pr.Repo, pr.Number)]; m != "" {
		return cfg.QueueMachine(m), AffinityOwner
	}
	return MachineAny, ""
}

// AgentExecutor is how an agent job runs: in its tab when it has tab
// affinity, otherwise in a new tab.
func AgentExecutor(affinity string) string {
	if affinity == AffinityTab {
		return ExecutorTab
	}
	return ExecutorNewTab
}

// TabMachine is the herdr machine of tab: local for the local server.
func TabMachine(tab *scan.Tab) string {
	if tab.Machine == "" {
		return config.LocalMachine
	}
	return tab.Machine
}

// Subject is a pull request's job subject, owner/repo#N.
func Subject(repo string, number int) string {
	return fmt.Sprintf("%s#%d", repo, number)
}

// needsCatchUp reports whether the branch is behind or conflicts with
// its base.
func needsCatchUp(pr scan.PR) bool {
	switch pr.MergeStateStatus {
	case "BEHIND", "DIRTY":
		return true
	}
	return pr.Mergeable == "CONFLICTING"
}

func hasAction(pr scan.PR, name string) bool {
	for _, a := range pr.Actions {
		if a.Name == name {
			return true
		}
	}
	return false
}
