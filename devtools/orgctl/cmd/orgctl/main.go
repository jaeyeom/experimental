// Command orgctl checks an organization spec and diffs it against live state.
//
//	orgctl check [--approval-word WORD]... SPEC.json
//	orgctl plan [--machine NAME] [--role NAME] [--expiring 2d] SPEC.json
//
// --approval-word extends the default approval-word list. The spec's
// approvalWords field does the same. allowApprovalWords drops a default
// word and is printed as a warning.
//
// plan and drift are read-only. Exit 0 means no diff markers, 2 means
// changes, and 1 means an error. USER STEP lines are not changes.
package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/orgctl"
)

const usage = "usage: orgctl check [--approval-word WORD]... SPEC.json\n" +
	"       orgctl plan [--machine NAME] [--role NAME] [--expiring 2d] SPEC.json\n" +
	"       orgctl drift SPEC.json"

func main() {
	os.Exit(run(os.Args[1:], os.Stderr))
}

func run(args []string, stderr io.Writer) int {
	return runIO(args, os.Stdout, stderr, loadLive)
}

func runIO(args []string, stdout, stderr io.Writer, load func(orgctl.Spec) (orgctl.Live, error)) int {
	if len(args) == 0 {
		fmt.Fprintln(stderr, usage)
		return 2
	}
	switch args[0] {
	case "check":
		return runCheck(args, stderr)
	case "plan":
		return runPlan(args, stdout, stderr, load)
	case "drift":
		return runDrift(args, stdout, stderr, load)
	default:
		fmt.Fprintln(stderr, usage)
		return 2
	}
}

func runCheck(args []string, stderr io.Writer) int {
	fs := flag.NewFlagSet("check", flag.ContinueOnError)
	fs.SetOutput(stderr)
	var extra stringList
	fs.Var(&extra, "approval-word", "approval word added to the default list")
	if err := fs.Parse(args[1:]); err != nil {
		return 2
	}
	if fs.NArg() != 1 {
		fmt.Fprintln(stderr, "usage: orgctl check [--approval-word WORD]... SPEC.json")
		return 2
	}
	data, err := os.ReadFile(fs.Arg(0)) //nolint:gosec // G304: the operator named the spec file
	if err != nil {
		fmt.Fprintf(stderr, "read spec: %v\n", err)
		return 1
	}
	spec, err := orgctl.Decode(data)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	warnings, err := spec.CheckDecides([]string(extra))
	for _, warning := range warnings {
		fmt.Fprintln(stderr, warning)
	}
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	return 0
}

func runPlan(args []string, stdout, stderr io.Writer, load func(orgctl.Spec) (orgctl.Live, error)) int {
	fs := flag.NewFlagSet("plan", flag.ContinueOnError)
	fs.SetOutput(stderr)
	machine := fs.String("machine", "", "machine key to include")
	role := fs.String("role", "", "role name to include")
	expiring := fs.String("expiring", "2d", "how soon a cron counts as attention")
	if err := fs.Parse(args[1:]); err != nil {
		return 1
	}
	if fs.NArg() != 1 {
		fmt.Fprintln(stderr, "usage: orgctl plan [--machine NAME] [--role NAME] [--expiring 2d] SPEC.json")
		return 1
	}
	window, err := parseExpiring(*expiring)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	data, err := os.ReadFile(fs.Arg(0)) //nolint:gosec // G304: the operator named the spec file
	if err != nil {
		fmt.Fprintf(stderr, "read spec: %v\n", err)
		return 1
	}
	spec, err := orgctl.Decode(data)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if load == nil {
		load = loadLive
	}
	live, err := load(spec)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	lines := orgctl.Plan(spec, live, orgctl.PlanOptions{
		Machine:  *machine,
		Role:     *role,
		Expiring: window,
	})
	for _, line := range lines {
		fmt.Fprintln(stdout, line)
	}
	if orgctl.PlanChanged(lines) {
		return 2
	}
	return 0
}

func runDrift(args []string, stdout, stderr io.Writer, load func(orgctl.Spec) (orgctl.Live, error)) int {
	fs := flag.NewFlagSet("drift", flag.ContinueOnError)
	fs.SetOutput(stderr)
	if err := fs.Parse(args[1:]); err != nil {
		return 1
	}
	if fs.NArg() != 1 {
		fmt.Fprintln(stderr, "usage: orgctl drift SPEC.json")
		return 1
	}
	data, err := os.ReadFile(fs.Arg(0)) //nolint:gosec // G304: the operator named the spec file
	if err != nil {
		fmt.Fprintf(stderr, "read spec: %v\n", err)
		return 1
	}
	spec, err := orgctl.Decode(data)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if load == nil {
		load = loadLive
	}
	live, err := load(spec)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	lines := orgctl.Drift(spec, live)
	for _, line := range lines {
		fmt.Fprintln(stdout, line)
	}
	if orgctl.PlanChanged(lines) {
		return 2
	}
	return 0
}

func parseExpiring(value string) (time.Duration, error) {
	if days, ok := strings.CutSuffix(value, "d"); ok {
		n, err := strconv.Atoi(days)
		if err != nil || n < 0 {
			return 0, fmt.Errorf("bad expiring %q", value)
		}
		return time.Duration(n) * 24 * time.Hour, nil
	}
	window, err := time.ParseDuration(value)
	if err != nil {
		return 0, fmt.Errorf("bad expiring %q", value)
	}
	return window, nil
}

// stringList is a repeatable flag value.
type stringList []string

func (s *stringList) String() string { return strings.Join(*s, ",") }

func (s *stringList) Set(v string) error {
	*s = append(*s, v)
	return nil
}
