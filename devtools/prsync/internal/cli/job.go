package cli

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/jaeyeom/experimental/devtools/prsync/internal/config"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/dispatch"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/gh"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/jobs"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"
)

// Job results.
const (
	jobDone = "done"
	jobFail = "fail"
)

// jobResult is the dispatch --job output: how the job ended, as
// reported to the queue.
type jobResult struct {
	Job     string `json:"job"`
	Kind    string `json:"kind"`
	Subject string `json:"subject"`
	Result  string `json:"result"`
	Note    string `json:"note"`
	// Queued is the conflict job a conflicting rebase queued.
	Queued string `json:"queued,omitempty"`
}

// jobConflicts are the dispatch flags --job cannot take: the job says
// what to do.
var jobConflicts = []string{"pr", "all", "rebase", "merge", "ci-fix", "hint", "force", "stdin"}

func validateJobFlags(cmd *cobra.Command) error {
	if !cmd.Flags().Changed("job") {
		if cmd.Flags().Changed("runner") {
			return &ExitError{Code: ExitUsage, Err: errors.New("--runner requires --job")}
		}
		return nil
	}
	for _, name := range jobConflicts {
		if cmd.Flags().Changed(name) {
			return &ExitError{Code: ExitUsage, Err: fmt.Errorf("cannot combine --job and --%s", name)}
		}
	}
	return nil
}

// runJob runs queued job id live and reports it with agentbus jobs done
// or fail. runner is the job's holder (default $JOB_RUNNER). It exits 0
// once the report is in, whatever the job's result.
func runJob(ctx context.Context, stdout io.Writer, exec executor.Executor, cfg config.Config, id, runner string) error {
	if runner == "" {
		runner = os.Getenv("JOB_RUNNER")
	}
	cfg.DryRun = false
	logStartup(ctx, "dispatch", cfg)
	q := jobs.Queue{Exec: exec, Bin: "agentbus"}
	j, err := q.Show(ctx, id)
	if err != nil {
		return &ExitError{Code: ExitPrecondition, Err: err}
	}
	res := runJobKind(ctx, exec, cfg, q, j)
	var reportErr error
	if res.Result == jobDone {
		reportErr = q.Done(ctx, j.ID, runner, res.Note)
	} else {
		reportErr = q.Fail(ctx, j.ID, runner, nil, res.Note)
	}
	if err := writeJobJSON(stdout, res); err != nil {
		return err
	}
	if reportErr != nil {
		return &ExitError{Code: ExitPrecondition, Err: fmt.Errorf("report job %s: %w", j.ID, reportErr)}
	}
	return nil
}

func runJobKind(ctx context.Context, exec executor.Executor, cfg config.Config, q jobs.Queue, j jobs.Job) jobResult {
	res := jobResult{Job: j.ID, Kind: j.Kind, Subject: j.Subject}
	repo, number, err := dispatch.ParsePR(j.Subject)
	if err != nil {
		return failed(res, err.Error())
	}
	switch j.Kind {
	case dispatch.KindRebase:
		return rebaseJob(ctx, gh.NewClient(exec, cfg.GHBin), q, j, repo, number, res)
	case dispatch.KindComment, dispatch.KindCIFix, dispatch.KindMergeQueueFix, jobs.KindConflict:
		return promptJob(ctx, exec, cfg, j, res)
	}
	return failed(res, fmt.Sprintf("prsync runs no %q job", j.Kind))
}

// rebaseJob rebases the branch on GitHub, with no checkout. A conflict
// queues one conflict job for the PR's tab and ends the rebase job done.
func rebaseJob(ctx context.Context, client *gh.Client, q jobs.Queue, j jobs.Job, repo string, number int, res jobResult) jobResult {
	if j.Head == "" {
		return failed(res, "the job has no head")
	}
	head, err := client.RebaseBranch(ctx, repo, number, j.Head)
	switch {
	case errors.Is(err, gh.ErrConflict):
		spec := jobs.Spec{
			Kind:     jobs.KindConflict,
			Subject:  j.Subject,
			Head:     j.Head,
			Machine:  j.Needs.Machine,
			Affinity: j.Needs.Affinity,
			Executor: jobs.AgentExecutor(j.Needs.Affinity),
		}
		if spec.Machine == "" {
			spec.Machine = jobs.MachineAny
		}
		cid, qerr := q.Enqueue(ctx, spec)
		if qerr != nil {
			return failed(res, "rebase conflicts with the base; conflict job not queued: "+qerr.Error())
		}
		res.Result, res.Queued = jobDone, cid
		res.Note = "rebase conflicts with the base; queued conflict job " + cid
		return res
	case err != nil:
		return failed(res, "rebase on GitHub: "+err.Error())
	}
	res.Result = jobDone
	res.Note = "rebased on GitHub to " + head
	return res
}

// promptJob sends the job's prompt to the PR's agent as dispatch does,
// without dedupe: the queue already deduped the job.
func promptJob(ctx context.Context, exec executor.Executor, cfg config.Config, j jobs.Job, res jobResult) jobResult {
	var modes dispatchModes
	switch j.Kind {
	case dispatch.KindCIFix:
		modes.ciFix = true
	case jobs.KindConflict:
		modes.merge = cfg.DispatchCatchup == "merge" || dispatch.RebaseSuffixConflict(cfg.PromptSuffix) != ""
		modes.rebase = !modes.merge
	}
	out, err := dispatchPRs(ctx, exec, cfg, []string{j.Subject}, modes, "", true, false)
	if out == nil {
		return failed(res, err.Error())
	}
	for _, item := range out.Results {
		if jobs.Subject(item.Repo, item.Number) != j.Subject {
			continue
		}
		note := item.Action
		if item.Detail != "" {
			note += ": " + item.Detail
		}
		switch item.Action {
		case dispatch.ActionDispatched, dispatch.ActionDispatchedTimeout,
			dispatch.ActionDispatchedBlocked, dispatch.ActionOpenedTabAndDispatched:
			res.Result, res.Note = jobDone, note
			return res
		}
		return failed(res, note)
	}
	if err != nil {
		return failed(res, err.Error())
	}
	return failed(res, "dispatch has no result for "+j.Subject)
}

func failed(res jobResult, note string) jobResult {
	res.Result, res.Note = jobFail, strings.TrimSpace(note)
	return res
}

func writeJobJSON(w io.Writer, res jobResult) error {
	out, err := json.MarshalIndent(res, "", "  ")
	if err != nil {
		return fmt.Errorf("encode job result: %w", err)
	}
	if _, err := fmt.Fprintf(w, "%s\n", out); err != nil {
		return fmt.Errorf("write job result: %w", err)
	}
	return nil
}

// emitJobs writes the job specs of doc, after noting each agent tab's
// machine in the owners file beside state_file.
func emitJobs(w io.Writer, doc scan.Document, cfg config.Config) error {
	path := filepath.Join(filepath.Dir(cfg.StateFile), jobs.OwnersFile)
	owners, err := jobs.LoadOwners(path)
	if err != nil {
		return &ExitError{Code: ExitUsage, Err: err}
	}
	owners.Note(doc)
	if err := owners.Save(path); err != nil {
		return fmt.Errorf("emit jobs: %w", err)
	}
	out, err := json.MarshalIndent(jobs.Build(doc, owners, cfg), "", "  ")
	if err != nil {
		return fmt.Errorf("encode jobs: %w", err)
	}
	if _, err := fmt.Fprintf(w, "%s\n", out); err != nil {
		return fmt.Errorf("write jobs: %w", err)
	}
	return nil
}
