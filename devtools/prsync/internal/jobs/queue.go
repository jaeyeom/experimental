package jobs

import (
	"context"
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
)

const queueTimeout = 30 * time.Second

// Job is the part of an agentbus job that prsync reads.
type Job struct {
	ID      string   `json:"id"`
	Kind    string   `json:"kind"`
	Subject string   `json:"subject"`
	Head    string   `json:"head"`
	Needs   Needs    `json:"needs"`
	Items   []string `json:"items"`
	State   string   `json:"state"`
	// Saved and Answer are a resumed job's branch and the answer that
	// resumed it.
	Saved  string `json:"saved"`
	Answer string `json:"answer"`
}

// Needs is a job's machine, affinity, and executor.
type Needs struct {
	Machine  string `json:"machine"`
	Affinity string `json:"affinity"`
	Executor string `json:"executor"`
}

// Queue runs the agentbus jobs CLI.
type Queue struct {
	Exec executor.Executor
	Bin  string
}

// Show reads one job.
func (q Queue) Show(ctx context.Context, id string) (Job, error) {
	out, err := q.run(ctx, "show", id, "--json")
	if err != nil {
		return Job{}, err
	}
	var j Job
	if err := json.Unmarshal([]byte(out), &j); err != nil {
		return Job{}, fmt.Errorf("decode job %s: %w", id, err)
	}
	return j, nil
}

// Enqueue adds s and returns the job id, which is an existing live
// job's id when s is a duplicate.
func (q Queue) Enqueue(ctx context.Context, s Spec) (string, error) {
	out, err := q.run(ctx, append(s.EnqueueArgs(), "--json")...)
	if err != nil {
		return "", err
	}
	var j Job
	if err := json.Unmarshal([]byte(out), &j); err != nil {
		return "", fmt.Errorf("decode enqueued job: %w", err)
	}
	return j.ID, nil
}

// Done reports job id done. runner is the job's holder; empty settles
// an unknown or parked job.
func (q Queue) Done(ctx context.Context, id, runner, note string) error {
	_, err := q.run(ctx, settleArgs("done", id, runner, nil, note)...)
	return err
}

// Fail reports job id failed, with exit when it is set.
func (q Queue) Fail(ctx context.Context, id, runner string, exit *int, note string) error {
	_, err := q.run(ctx, settleArgs("fail", id, runner, exit, note)...)
	return err
}

// EnqueueArgs are the agentbus jobs enqueue arguments of s, after
// "jobs".
func (s Spec) EnqueueArgs() []string {
	args := []string{"enqueue", "--kind", s.Kind, "--subject", s.Subject}
	if s.Head != "" {
		args = append(args, "--head", s.Head)
	}
	args = append(args, "--machine", s.Machine)
	if s.Affinity != "" {
		args = append(args, "--affinity", s.Affinity)
	}
	args = append(args, "--executor", s.Executor)
	for _, item := range s.Items {
		args = append(args, "--item", item)
	}
	return args
}

func settleArgs(sub, id, runner string, exit *int, note string) []string {
	args := []string{sub, id}
	if runner != "" {
		args = append(args, "--runner", runner)
	}
	if exit != nil {
		args = append(args, "--exit", strconv.Itoa(*exit))
	}
	if note != "" {
		args = append(args, "--note", note)
	}
	return args
}

func (q Queue) run(ctx context.Context, args ...string) (string, error) {
	res, err := q.Exec.Execute(ctx, executor.ToolConfig{
		Command: q.Bin,
		Args:    append([]string{"jobs"}, args...),
		Timeout: queueTimeout,
	})
	if err != nil {
		return "", fmt.Errorf("run %s jobs %s: %w", q.Bin, args[0], err)
	}
	if res.ExitCode != 0 {
		msg := strings.TrimSpace(res.Stderr)
		if msg == "" {
			msg = strings.TrimSpace(res.Output)
		}
		return "", fmt.Errorf("%s jobs %s exited %d: %s", q.Bin, args[0], res.ExitCode, msg)
	}
	return res.Output, nil
}
