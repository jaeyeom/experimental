package tick

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"time"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
)

// Job fields for the jobs layer (jaeyeom/experimental#876).
const (
	JobKindStartItem  = "start-item"
	AffinityLane      = "lane"
	AffinityOwner     = "owner"
	MachineAny        = "any"
	ExecutorNewTab    = "new-tab"
	CheckoutNone      = "none"
	outcomeEnqueued   = "enqueued"
	factoryBinaryName = "factory"
)

// Job is one start-item job spec, printed by tick --enqueue as one JSON
// line. It has no gate: gates come only from the jobs policy.
type Job struct {
	Kind    string `json:"kind"`
	Subject string `json:"subject"` // The item id.
	Source  string `json:"source"`  // kind:name.
	Title   string `json:"title"`
	// Dedupe is kind, source, and id: one live job per item.
	Dedupe string `json:"dedupe"`
	// Priority is the item's rank in this tick, 1 first.
	Priority int      `json:"priority"`
	Needs    JobNeeds `json:"needs"`
	// Command starts the item on the runner's machine.
	Command []string `json:"command"`
}

// JobNeeds is where a job may run.
type JobNeeds struct {
	// Machine is a herdr machine, this host for a local lane, or "any".
	Machine string `json:"machine"`
	// Affinity is "lane" for a lane label, "owner" for the machine whose
	// agent last worked the item, or empty for any machine.
	Affinity string `json:"affinity,omitempty"`
	Checkout string `json:"checkout"` // A path, or "none".
	Executor string `json:"executor"`
}

// Owner is the machine and tab of the latest claim on an item.
type Owner struct {
	Source    string    `json:"source"`
	ID        string    `json:"id"`
	Machine   string    `json:"machine"`
	Lane      string    `json:"lane,omitempty"`
	Tab       string    `json:"tab,omitempty"`
	Workspace string    `json:"workspace,omitempty"`
	State     string    `json:"state"`
	ClaimedAt time.Time `json:"claimed_at"` //nolint:tagliatelle // documented factory JSON output field
}

// Ownership is the owner a claim records. A claim on a remote lane that is
// still configured is owned by that lane's herdr machine; any other claim
// by the host that made it.
func Ownership(cfg *config.Config, c store.Claim) Owner {
	machine := c.Machine
	if lane, ok := cfg.LaneNamed(c.Lane); ok && lane.Remote() {
		machine = lane.Machine
	}
	return Owner{
		Source:    c.Source,
		ID:        c.ID,
		Machine:   machine,
		Lane:      c.Lane,
		Tab:       c.TabID,
		Workspace: c.WorkspaceID,
		State:     string(c.State),
		ClaimedAt: c.ClaimedAt.UTC(),
	}
}

// enqueue prints one start-item job per ready item, in rank order, instead
// of starting any. It claims nothing and opens no tab.
func (e *Engine) enqueue(ctx context.Context, at time.Time, ranked []candidate) error {
	if e.Jobs == nil {
		return errors.New("tick --enqueue: expected a job writer, found nil")
	}
	if len(ranked) == 0 {
		return e.finish(ctx, at, outcomeIdle, "no ready items")
	}
	enc := json.NewEncoder(e.Jobs)
	for i, cand := range ranked {
		job, err := e.jobFor(ctx, cand, i+1)
		if err != nil {
			return err
		}
		if err := enc.Encode(job); err != nil {
			return fmt.Errorf("tick --enqueue: write job: %w", err)
		}
	}
	return e.finish(ctx, at, outcomeEnqueued, fmt.Sprintf("enqueued %d start-item jobs", len(ranked)))
}

// jobFor builds an item's job. Affinity, most specific first: the item's
// lane label, then the machine whose agent last worked it, then any.
func (e *Engine) jobFor(ctx context.Context, cand candidate, priority int) (Job, error) {
	id := cand.item.ID
	job := Job{
		Kind:     JobKindStartItem,
		Subject:  id,
		Source:   cand.key,
		Title:    cand.item.Title,
		Dedupe:   JobKindStartItem + ":" + cand.key + ":" + id,
		Priority: priority,
		Needs:    JobNeeds{Machine: MachineAny, Checkout: orNone(cand.cfg.Cwd), Executor: ExecutorNewTab},
		Command:  []string{factoryBinaryName, "start", "--apply", id},
	}
	lane, labeled, err := e.Config.LabelLane(cand.item.Labels)
	if err != nil {
		return Job{}, fmt.Errorf("tick --enqueue %s: %w", id, err)
	}
	if labeled {
		job.Needs.Machine = e.Hostname
		if lane.Remote() {
			job.Needs.Machine = lane.Machine
		}
		job.Needs.Affinity = AffinityLane
		job.Needs.Checkout = orNone(lane.Checkout)
		job.Command = []string{factoryBinaryName, "start", "--apply", "--machine", lane.Name, id}
		return job, nil
	}
	claim, err := e.Store.LatestClaim(ctx, cand.key, id)
	switch {
	case errors.Is(err, source.ErrNotFound):
		return job, nil
	case err != nil:
		return Job{}, fmt.Errorf("tick --enqueue %s: %w", id, err)
	}
	if owner := Ownership(e.Config, claim); owner.Machine != "" {
		job.Needs.Machine = owner.Machine
		job.Needs.Affinity = AffinityOwner
	}
	return job, nil
}

func orNone(path string) string {
	if path == "" {
		return CheckoutNone
	}
	return path
}
