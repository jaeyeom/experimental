package remotecall

import (
	"context"
	"time"
)

// herdrProbeTimeout bounds `herdr machine status`, which classifies a failure.
const herdrProbeTimeout = 2 * time.Second

// HerdrExec runs one herdr invocation. args do not include the binary.
// A non-nil error means the process did not start. A non-zero code is
// the command's own.
type HerdrExec func(ctx context.Context, args []string) (stdout, stderr string, code int, err error)

// OnHerdrMachine runs `--machine machine args...` through run under
// timeout. A failure that means the machine cannot be reached is a
// *UnreachableError, classified with probe; other results are run's, as
// for a local call.
func OnHerdrMachine(ctx context.Context, run HerdrExec, probe Probe, machine string, timeout time.Duration, args []string) (string, string, int, error) {
	callCtx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	stdout, stderr, code, err := run(callCtx, append([]string{"--machine", machine}, args...))
	timedOut := callCtx.Err() != nil && ctx.Err() == nil
	if err != nil && !timedOut {
		return stdout, stderr, code, err
	}
	res := Result{Stdout: stdout, Stderr: stderr, Code: code, TimedOut: timedOut}
	if ue := Classify(ctx, machine, res, probe); ue != nil {
		return stdout, stderr, code, ue
	}
	return stdout, stderr, code, nil
}

// HerdrProbe asks `herdr machine status`, which never prompts, through
// run whether machine's host answers.
func HerdrProbe(run HerdrExec, machine string) Probe {
	return func(ctx context.Context) bool {
		probeCtx, cancel := context.WithTimeout(ctx, herdrProbeTimeout)
		defer cancel()
		stdout, stderr, code, err := run(probeCtx, []string{"machine", "status", machine, "--json"})
		if err != nil || code != 0 || probeCtx.Err() != nil {
			return false
		}
		return HerdrMachineUp(stdout, stderr)
	}
}
