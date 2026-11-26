package cli

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
	"time"

	executor "github.com/jaeyeom/go-cmdexec"
	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gate"
	"github.com/jaeyeom/experimental/devtools/factory/internal/gh"
	"github.com/jaeyeom/experimental/devtools/factory/internal/prlog"
	"github.com/jaeyeom/experimental/devtools/factory/internal/remote"
	"github.com/jaeyeom/experimental/devtools/factory/internal/store"
	"github.com/jaeyeom/experimental/devtools/factory/internal/verify"
)

type verifyLoaded struct {
	id    string
	cfg   config.Source
	claim store.Claim
	found bool
	prs   []gh.PR
	lane  config.Lane // The claim's lane. Zero is the local checkout.
}

func newVerifyCmd(ctx context.Context) *cobra.Command {
	var noSync bool
	cmd := &cobra.Command{
		Use:           "verify <id>",
		Short:         "Check a worker pull request and checkout",
		SilenceErrors: true,
		SilenceUsage:  true,
		Args: func(_ *cobra.Command, args []string) error {
			if len(args) != 1 {
				return usageError{err: fmt.Errorf("verify: expected 1 argument, found %d", len(args))}
			}
			if args[0] == "" {
				return usageError{err: errors.New("verify: expected a non-empty id, found empty")}
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return runVerify(ctx, cmd, args[0], noSync)
		},
	}
	cmd.Flags().BoolVar(&noSync, "no-sync", false, "do not refresh a stale mirror")
	return cmd
}

func runVerify(ctx context.Context, cmd *cobra.Command, id string, noSync bool) (err error) {
	if ctx == nil {
		return errors.New("verify: expected a context, found nil")
	}
	cfg, err := loadConfig(cmd)
	if err != nil {
		return err
	}
	st, err := openDB(ctx, cmd)
	if err != nil {
		return err
	}
	defer func() {
		if cerr := st.Close(); err == nil && cerr != nil {
			err = fmt.Errorf("close store: %w", cerr)
		}
	}()
	loaded, err := loadVerify(ctx, cmd, cfg, st, id, noSync)
	if err != nil {
		return err
	}
	if err := recordSeenPRs(ctx, st, loaded); err != nil {
		return err
	}
	in, err := verifyInput(ctx, loaded)
	if err != nil {
		return err
	}
	fails, err := verify.Check(ctx, in)
	where := ""
	if loaded.lane.Remote() {
		where = fmt.Sprintf("checked on %s:%s", loaded.lane.Machine, loaded.cfg.Cwd)
	}
	return writeVerify(cmd.OutOrStdout(), loaded.id, where, fails, err)
}

func loadVerify(ctx context.Context, cmd *cobra.Command, cfg *config.Config, st *store.Store, id string, noSync bool) (verifyLoaded, error) {
	if err := refreshBeforeRead(ctx, cmd, cfg, st, "", noSync); err != nil {
		return verifyLoaded{}, err
	}
	it, err := resolveItem(ctx, st, id, "")
	if err != nil {
		return verifyLoaded{}, err
	}
	srcCfg, err := configFor(cfg, it.Source)
	if err != nil {
		return verifyLoaded{}, err
	}
	claim, found, err := latestClaim(ctx, st, it.Source, it.ID)
	if err != nil {
		return verifyLoaded{}, err
	}
	var lane config.Lane
	if found && claim.Lane != "" {
		srcCfg, lane, err = verifyLaneSource(cfg, srcCfg, it.ID, claim.Lane)
		if err != nil {
			return verifyLoaded{}, err
		}
	}
	var prs []gh.PR
	if verify.EffectiveOutputMode(found, claim, srcCfg) == config.OutputModePR {
		snap, err := loadSnapshot(ctx, st, srcCfg)
		if err != nil {
			return verifyLoaded{}, err
		}
		prs, err = showPRs(ctx, srcCfg.GHBin, snap.OpenPRs, it, srcCfg.PRTitlePattern, claim.PRs)
		if err != nil {
			return verifyLoaded{}, err
		}
		if err := prlog.Record(ctx, st, nowUTC(), it, observedPRs(prs)); err != nil {
			return verifyLoaded{}, fmt.Errorf("verify %s: %w", it.ID, err)
		}
	}
	return verifyLoaded{id: it.ID, cfg: srcCfg, claim: claim, found: found, prs: prs, lane: lane}, nil
}

// observedPRs drops pull requests rebuilt from a claim's prs_json. Those
// carry no state read from gh, so they are not an observation.
func observedPRs(prs []gh.PR) []gh.PR {
	out := make([]gh.PR, 0, len(prs))
	for _, pr := range prs {
		if !pr.CreatedAt.IsZero() {
			out = append(out, pr)
		}
	}
	return out
}

// verifyInput wires the checks. GitHub checks run locally. Checkout checks
// run locally, or on a remote lane's machine through its exec prefix after a
// reachability probe.
func verifyInput(ctx context.Context, loaded verifyLoaded) (verify.Input, error) {
	local := newCommandExec()
	if local == nil {
		return verify.Input{}, errors.New("verify: expected an executor, found nil")
	}
	_, pulls, err := externalClients(loaded.cfg.GHBin)
	if err != nil {
		return verify.Input{}, err
	}
	exec := local
	readFile := os.ReadFile
	if loaded.lane.Remote() {
		rexec, err := remote.New(local, loaded.lane.Exec)
		if err != nil {
			return verify.Input{}, fmt.Errorf("verify %s: lane %s: %w", loaded.id, loaded.lane.Name, err)
		}
		if err := probeMachine(ctx, rexec); err != nil {
			return verify.Input{}, fmt.Errorf("verify %s: machine %s unreachable: %w", loaded.id, loaded.lane.Machine, err)
		}
		exec = rexec
		readFile = remoteReadFile(ctx, rexec)
	}
	return verify.Input{
		ID:       loaded.id,
		Now:      time.Now().UTC(),
		Cfg:      loaded.cfg,
		Claim:    loaded.claim,
		HasClaim: loaded.found,
		PRs:      loaded.prs,
		View:     pulls.View,
		Status:   gate.GitStatus(exec),
		Stash:    gate.GitStashCount(exec),
		Base:     verify.GitBase(exec),
		Merge:    verify.GitMerge(exec),
		ReadFile: readFile,
	}, nil
}

// probeTimeout bounds the reachability probe on a remote lane's machine.
const probeTimeout = 30 * time.Second

// probeMachine runs `true` through the lane's exec prefix. A failure to run
// it, a timeout, or a non-zero exit (ssh exits 255 when it cannot connect)
// means the machine is unreachable.
func probeMachine(ctx context.Context, exec executor.Executor) error {
	res, err := exec.Execute(ctx, executor.ToolConfig{Command: "true", Timeout: probeTimeout})
	if err != nil {
		return err //nolint:wrapcheck // propagate context cancellation
	}
	if res == nil {
		return errors.New("expected an execution result, found nil")
	}
	if res.ExitCode != 0 {
		detail := strings.TrimSpace(res.Stderr)
		if detail == "" {
			detail = fmt.Sprintf("exit %d", res.ExitCode)
		}
		return errors.New(detail)
	}
	return nil
}

// remoteReadFile reads a file on the remote machine with cat, for the
// pull request template inside the lane's checkout.
func remoteReadFile(ctx context.Context, exec executor.Executor) func(string) ([]byte, error) {
	return func(path string) ([]byte, error) {
		res, err := exec.Execute(ctx, executor.ToolConfig{Command: "cat", Args: []string{"--", path}, Timeout: probeTimeout})
		if err != nil {
			return nil, fmt.Errorf("read %s: %w", path, err)
		}
		if res == nil || res.ExitCode != 0 {
			detail := "no result"
			if res != nil {
				detail = strings.TrimSpace(res.Stderr)
			}
			return nil, fmt.Errorf("read %s on the lane machine: %s", path, detail)
		}
		return []byte(res.Output), nil
	}
}

func recordSeenPRs(ctx context.Context, st *store.Store, loaded verifyLoaded) error {
	if !loaded.found || !loaded.claim.State.Active() || len(loaded.prs) == 0 {
		return nil
	}
	extra := make([]store.ClaimPR, 0, len(loaded.prs))
	for _, pr := range loaded.prs {
		extra = append(extra, store.ClaimPR{
			Repo: pr.Repo, Number: pr.Number, URL: pr.URL, Title: pr.Title, Head: pr.Head,
		})
	}
	merged := store.MergeClaimPRs(loaded.claim.PRs, extra)
	if store.SameClaimPRs(loaded.claim.PRs, merged) {
		return nil
	}
	claim := loaded.claim
	if err := st.FillClaim(ctx, claim.Source, claim.ID, claim.ClaimedAt, claim.TabID, claim.WorkspaceID, merged, time.Now().UTC()); err != nil {
		return fmt.Errorf("verify %s: record pull request: %w", loaded.id, err)
	}
	return nil
}

// writeVerify prints where the checkout was checked (a remote lane only),
// each failure, and ok when nothing failed.
func writeVerify(w io.Writer, id, where string, fails []string, err error) error {
	if where != "" {
		if _, werr := io.WriteString(w, where+"\n"); werr != nil {
			return fmt.Errorf("write verify: %w", werr)
		}
	}
	if werr := writeFailures(w, fails); werr != nil {
		return werr
	}
	if err != nil {
		return err
	}
	if len(fails) > 0 {
		return fmt.Errorf("verify %s: %d checks failed", id, len(fails))
	}
	if _, err := io.WriteString(w, "ok\n"); err != nil {
		return fmt.Errorf("write verify: %w", err)
	}
	return nil
}

func writeFailures(w io.Writer, fails []string) error {
	for _, line := range fails {
		if _, err := io.WriteString(w, line+"\n"); err != nil {
			return fmt.Errorf("write verify: %w", err)
		}
	}
	return nil
}

// verifyLaneSource points verify at the checkout of the claim's lane. A
// remote lane needs its exec prefix, which runs the checkout checks there.
func verifyLaneSource(cfg *config.Config, src config.Source, id, name string) (config.Source, config.Lane, error) {
	lane, err := claimLane(cfg, name)
	if err != nil {
		return config.Source{}, config.Lane{}, fmt.Errorf("verify %s: %w", id, err)
	}
	if lane.Remote() && len(lane.Exec) == 0 {
		return config.Source{}, config.Lane{}, fmt.Errorf("verify %s: lane %s on machine %s has no exec; set exec = [\"ssh\", \"HOST\"] on that [[lane]] so verify can check the checkout there", id, lane.Name, lane.Machine)
	}
	return config.LaneSource(src, lane), lane, nil
}
