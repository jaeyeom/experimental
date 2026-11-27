// Command orgctl checks an organization spec and diffs it against live state.
//
//	orgctl check [--approval-word WORD]... SPEC.json
//	orgctl plan [--machine NAME] [--role NAME] [--expiring 2d] SPEC.json
//	orgctl tree [--root ROLE] [--depth N] [--live] [--json] [--ascii] SPEC.json
//	orgctl diff [--full] [--json] OLD.json NEW.json
//
// --approval-word extends the default approval-word list. The spec's
// approvalWords field does the same. allowApprovalWords drops a default
// word and is printed as a warning.
//
// plan and drift are read-only. Exit 0 means no diff markers, 2 means
// changes, and 1 means an error. USER STEP lines are not changes.
package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/orgctl"
)

const usage = "usage: orgctl check [--approval-word WORD]... SPEC.json\n" +
	"       orgctl plan [--machine NAME] [--role NAME] [--expiring 2d] [--only KIND]... [--json] [--cron-state-dir DIR] [--cron-stale 24h] [--agents-md-root DIR] SPEC.json\n" +
	"       orgctl drift [--agents-only] [--agents-md-root DIR] SPEC.json\n" +
	"       " + treeUsage + "\n" +
	"       " + diffUsage + "\n" +
	"       orgctl import [--out FILE] [--force] [--schema PATH]\n" +
	"       orgctl render agents-md --root DIR [--check] SPEC.json\n" +
	"       " + cronUsage

func main() {
	os.Exit(run(os.Args[1:], os.Stderr))
}

func run(args []string, stderr io.Writer) int {
	load := loadLive
	if len(args) > 0 && args[0] == "import" {
		load = func(orgctl.Spec) (orgctl.Live, error) { return loadImportLive() }
	}
	return runIO(args, os.Stdout, stderr, load)
}

func runIO(args []string, stdout, stderr io.Writer, load func(orgctl.Spec) (orgctl.Live, error)) int {
	return runIOWithInput(args, os.Stdin, stdout, stderr, load, time.Now)
}

func runIOWithInput(args []string, stdin io.Reader, stdout, stderr io.Writer, load func(orgctl.Spec) (orgctl.Live, error), now func() time.Time) int {
	if code, ok := newOrgctlTool().Handle(context.Background(), args, stdout, stderr); ok {
		return code
	}
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
	case "import":
		return runImport(args, stdout, stderr, load)
	case "render":
		return runRender(args, stdout, stderr)
	case "tree":
		return runTree(args, stdout, stderr, load)
	case "diff":
		return runDiff(args, stdout, stderr)
	case "cron":
		return runCron(args, stdin, stderr, now)
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
	warnings = append(warnings, spec.CheckScope()...)
	for _, warning := range warnings {
		fmt.Fprintln(stderr, warning)
	}
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if err := spec.CheckCwd(); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if err := spec.CheckPatterns(); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if err := spec.CheckRules(); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if err := spec.CheckClasses(); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if err := spec.CheckReports(); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	return 0
}

func runRender(args []string, stdout, stderr io.Writer) int {
	if len(args) < 2 || args[1] != "agents-md" {
		fmt.Fprintln(stderr, "usage: orgctl render agents-md --root DIR [--check] SPEC.json")
		return 2
	}
	fs := flag.NewFlagSet("render agents-md", flag.ContinueOnError)
	fs.SetOutput(stderr)
	root := fs.String("root", "", "output root directory")
	check := fs.Bool("check", false, "check whether rendered files are up to date")
	if err := fs.Parse(args[2:]); err != nil {
		return 2
	}
	if *root == "" || fs.NArg() != 1 {
		fmt.Fprintln(stderr, "usage: orgctl render agents-md --root DIR [--check] SPEC.json")
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
	diffs, err := orgctl.RenderAgentsMD(spec, *root, *check)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if *check && len(diffs) > 0 {
		for _, p := range diffs {
			fmt.Fprintln(stdout, p)
		}
		return 2
	}
	return 0
}

type planConfig struct {
	specPath     string
	machine      string
	role         string
	expiring     time.Duration
	only         []string
	asJSON       bool
	cronStateDir string
	cronStale    time.Duration
	agentsMDRoot string
}

func parsePlanArgs(args []string, stderr io.Writer) (planConfig, bool) {
	fs := flag.NewFlagSet("plan", flag.ContinueOnError)
	fs.SetOutput(stderr)
	machine := fs.String("machine", "", "machine key to include")
	role := fs.String("role", "", "role name to include")
	expiring := fs.String("expiring", "2d", "how soon a cron counts as attention")
	agentsMDRoot := fs.String("agents-md-root", "", "directory root where AGENTS.md files are rendered")
	asJSON := fs.Bool("json", false, "print plan items as a JSON list")
	cronStateDir := fs.String("cron-state-dir", "", "directory containing published session crons")
	cronStale := fs.String("cron-stale", "24h", "age after which a published cron file counts as stale")
	var only stringList
	fs.Var(&only, "only", "filter items by kind (repeatable)")
	if err := fs.Parse(args[1:]); err != nil {
		return planConfig{}, false
	}
	if fs.NArg() != 1 {
		fmt.Fprintln(stderr, "usage: orgctl plan [--machine NAME] [--role NAME] [--expiring 2d] [--only KIND]... [--json] [--cron-state-dir DIR] [--cron-stale 24h] [--agents-md-root DIR] SPEC.json")
		return planConfig{}, false
	}
	if err := orgctl.ValidatePlanKinds([]string(only)); err != nil {
		fmt.Fprintf(stderr, "orgctl plan: %v\n", err)
		return planConfig{}, false
	}
	window, err := parseExpiring(*expiring)
	if err != nil {
		fmt.Fprintln(stderr, err)
		return planConfig{}, false
	}
	staleWindow, err := parseExpiring(*cronStale)
	if err != nil {
		fmt.Fprintf(stderr, "bad cron-stale %q: %v\n", *cronStale, err)
		return planConfig{}, false
	}
	return planConfig{
		specPath:     fs.Arg(0),
		machine:      *machine,
		role:         *role,
		expiring:     window,
		only:         []string(only),
		asJSON:       *asJSON,
		cronStateDir: *cronStateDir,
		cronStale:    staleWindow,
		agentsMDRoot: *agentsMDRoot,
	}, true
}

func runPlan(args []string, stdout, stderr io.Writer, load func(orgctl.Spec) (orgctl.Live, error)) int {
	cfg, ok := parsePlanArgs(args, stderr)
	if !ok {
		return 1
	}
	data, err := os.ReadFile(cfg.specPath) //nolint:gosec // G304: the operator named the spec file
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
	if cfg.cronStateDir != "" {
		live.PublishedCrons = orgctl.ReadPublishedCrons(spec, live.Local, cfg.cronStateDir)
	}
	if root := firstNonEmpty(cfg.agentsMDRoot, spec.AgentsMDRoot); root != "" && live.AgentsMD == nil {
		agentsMD, err := orgctl.CompareAgentsMD(spec, root)
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		live.AgentsMD = agentsMD
	}
	items := orgctl.PlanItems(spec, live, orgctl.PlanOptions{
		Machine:   cfg.machine,
		Role:      cfg.role,
		Expiring:  cfg.expiring,
		Only:      cfg.only,
		CronStale: cfg.cronStale,
	})
	if err := writePlanOutput(stdout, items, cfg.asJSON); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if orgctl.PlanItemsChanged(items) {
		return 2
	}
	return 0
}

func writePlanOutput(stdout io.Writer, items []orgctl.PlanItem, asJSON bool) error {
	if !asJSON {
		for _, line := range orgctl.PlanLines(items) {
			fmt.Fprintln(stdout, line)
		}
		return nil
	}
	if items == nil {
		items = []orgctl.PlanItem{}
	}
	enc := json.NewEncoder(stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(items); err != nil {
		return fmt.Errorf("encode plan items: %w", err)
	}
	return nil
}

func runDrift(args []string, stdout, stderr io.Writer, load func(orgctl.Spec) (orgctl.Live, error)) int {
	fs := flag.NewFlagSet("drift", flag.ContinueOnError)
	fs.SetOutput(stderr)
	agentsOnly := fs.Bool("agents-only", false, "skip undeclared tabs that run no agent")
	agentsMDRoot := fs.String("agents-md-root", "", "directory root where AGENTS.md files are rendered")
	if err := fs.Parse(args[1:]); err != nil {
		return 1
	}
	if fs.NArg() != 1 {
		fmt.Fprintln(stderr, "usage: orgctl drift [--agents-only] [--agents-md-root DIR] SPEC.json")
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
	if root := firstNonEmpty(*agentsMDRoot, spec.AgentsMDRoot); root != "" && live.AgentsMD == nil {
		agentsMD, err := orgctl.CompareAgentsMD(spec, root)
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		live.AgentsMD = agentsMD
	}
	if root := firstNonEmpty(*agentsMDRoot, spec.AgentsMDRoot); root != "" && live.AgentsMD == nil {
		agentsMD, err := orgctl.CompareAgentsMD(spec, root)
		if err != nil {
			fmt.Fprintln(stderr, err)
			return 1
		}
		live.AgentsMD = agentsMD
	}
	lines := orgctl.DriftWith(spec, live, orgctl.DriftOptions{AgentsOnly: *agentsOnly})
	for _, line := range lines {
		fmt.Fprintln(stdout, line)
	}
	if orgctl.PlanChanged(lines) {
		return 2
	}
	return 0
}

func firstNonEmpty(a, b string) string {
	if a != "" {
		return a
	}
	return b
}

func runImport(args []string, stdout, stderr io.Writer, load func(orgctl.Spec) (orgctl.Live, error)) int {
	fs := flag.NewFlagSet("import", flag.ContinueOnError)
	fs.SetOutput(stderr)
	outPath := fs.String("out", "", "write the stub to this file instead of stdout")
	force := fs.Bool("force", false, "overwrite an existing --out file")
	schema := fs.String("schema", "", "Org.pkl the stub amends")
	if err := fs.Parse(args[1:]); err != nil {
		return 1
	}
	if fs.NArg() != 0 {
		fmt.Fprintln(stderr, "usage: orgctl import [--out FILE] [--force] [--schema PATH]")
		return 1
	}
	if load == nil {
		load = func(orgctl.Spec) (orgctl.Live, error) { return loadImportLive() }
	}
	live, err := load(orgctl.Spec{})
	if err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	amends := *schema
	if amends == "" {
		amends = findOrgPkl()
	}
	rendered := orgctl.RenderImport(live, amends)
	stub := rendered.Stub
	if !strings.HasSuffix(stub, "\n") {
		stub += "\n"
	}
	if *outPath == "" {
		fmt.Fprint(stdout, stub)
		return 0
	}
	if err := writeStub(*outPath, []byte(stub), *force); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	if err := writePromptFiles(filepath.Dir(*outPath), rendered.Files); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	return 0
}

func writeStub(path string, data []byte, force bool) error {
	flagSet := os.O_WRONLY | os.O_CREATE
	if force {
		flagSet |= os.O_TRUNC
	} else {
		flagSet |= os.O_EXCL
	}
	f, err := os.OpenFile(path, flagSet, 0o600) //nolint:gosec // G304: the operator named the output file
	if err != nil {
		if errors.Is(err, fs.ErrExist) {
			return fmt.Errorf("refusing to overwrite %s", path)
		}
		return fmt.Errorf("write stub: %w", err)
	}
	_, err = f.Write(data)
	cerr := f.Close()
	if err != nil {
		return fmt.Errorf("write stub: %w", err)
	}
	if cerr != nil {
		return fmt.Errorf("close stub: %w", cerr)
	}
	return nil
}

func writePromptFiles(dir string, files map[string]string) error {
	for rel, body := range files {
		path := filepath.Join(dir, rel)
		if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
			return fmt.Errorf("write prompt: %w", err)
		}
		if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
			return fmt.Errorf("write prompt: %w", err)
		}
	}
	return nil
}

func findOrgPkl() string {
	dir, err := os.Getwd()
	if err != nil {
		return "pkl/Org.pkl"
	}
	for {
		candidates := []string{
			filepath.Join(dir, "devtools", "orgctl", "pkl", "Org.pkl"),
			filepath.Join(dir, "pkl", "Org.pkl"),
		}
		for _, candidate := range candidates {
			if _, statErr := os.Stat(candidate); statErr == nil {
				abs, absErr := filepath.Abs(candidate)
				if absErr == nil {
					return abs
				}
				return candidate
			}
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "pkl/Org.pkl"
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
