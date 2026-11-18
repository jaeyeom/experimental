package orgctl

import (
	"bytes"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"slices"
	"testing"
)

func TestExampleJSONDecodes(t *testing.T) {
	stdout, stderr, code := evalPkl(t, filepath.Join(orgctlRoot(t), "examples/acme.pkl"))
	if code != 0 {
		t.Fatalf("pkl eval examples/acme.pkl exit %d\n%s", code, stderr)
	}
	spec, err := Decode(stdout)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if spec.Org != "Acme Desk" {
		t.Errorf("org = %q", spec.Org)
	}
	if len(spec.Machines) != 2 || spec.Machines["north"].Host != "north.example" || spec.Machines["south"].Timezone != "America/New_York" {
		t.Errorf("machines = %+v", spec.Machines)
	}
	if len(spec.Roles) != 3 {
		t.Fatalf("roles = %d, want 3", len(spec.Roles))
	}
	reviewClass, ok := spec.Classes["review"]
	if !ok || reviewClass.Description == "" || len(reviewClass.Rules) != 1 || reviewClass.Rules[0].ID != "cite-line" {
		t.Errorf("classes[review] = %+v", reviewClass)
	}
	lead := spec.Roles["coord/lead"]
	if lead.Class != "" {
		t.Errorf("coord/lead class = %q, want empty", lead.Class)
	}
	if lead.ReportsTo != nil {
		t.Errorf("coord/lead reportsTo = %q, want none", *lead.ReportsTo)
	}
	if !slices.Equal(lead.Rules, []string{"plain-words", "no-secrets"}) {
		t.Errorf("coord/lead rules = %#v", lead.Rules)
	}
	review := spec.Roles["review/desk"]
	if review.Class != "review" {
		t.Errorf("review/desk class = %q, want review", review.Class)
	}
	if review.ReportsTo == nil || *review.ReportsTo != "coord/lead" {
		t.Errorf("review/desk reportsTo = %v", review.ReportsTo)
	}
	watch := spec.Roles["watch/night"]
	if watch.When != "weekday mornings" {
		t.Errorf("watch/night when = %q", watch.When)
	}
	if len(watch.Schedules) != 1 || watch.Schedules[0].Cron != "0 9 * * 1-5" || !watch.Schedules[0].Durable {
		t.Errorf("watch/night schedules = %+v", watch.Schedules)
	}
	pool := spec.Pools["hands"]
	if pool.Max != 3 || pool.Starter != "coord/lead" || pool.Machine != "south" {
		t.Errorf("pool hands = %+v", pool)
	}
	if spec.Jobs == nil {
		t.Fatal("jobs missing")
	}
	if spec.Jobs.Policy != "~/.config/jobs/policy.json" || spec.Jobs.AnomalyRole != "coord/lead" {
		t.Errorf("jobs = %+v", spec.Jobs)
	}
	runner := spec.Jobs.Runners["jobs-runner"]
	if runner.Machine != "north" || !slices.Equal(runner.Kinds, []string{"rebase", "merge"}) {
		t.Errorf("runner jobs-runner = %+v", runner)
	}
}

func TestDecodeJobs(t *testing.T) {
	spec, err := Decode([]byte(`{"jobs":{"policy":"/p.json","anomalyRole":"a/one",` +
		`"runners":{"r1":{"machine":"one","kinds":["merge"]}}}}`))
	if err != nil {
		t.Fatal(err)
	}
	if spec.Jobs == nil || spec.Jobs.Policy != "/p.json" || spec.Jobs.AnomalyRole != "a/one" {
		t.Fatalf("jobs = %+v", spec.Jobs)
	}
	if r := spec.Jobs.Runners["r1"]; r.Machine != "one" || !slices.Equal(r.Kinds, []string{"merge"}) {
		t.Errorf("runner r1 = %+v", r)
	}
	none, err := Decode([]byte(`{"jobs":null}`))
	if err != nil || none.Jobs != nil {
		t.Errorf("null jobs = %+v, %v", none.Jobs, err)
	}
}

func TestConstraintFixturesFail(t *testing.T) {
	root := orgctlRoot(t)
	tests := []struct {
		name string
		file string
		want string
	}{
		{name: "duplicate tab", file: "pkl/testdata/duplicate-tab.pkl", want: "duplicate tab"},
		{name: "unknown reportsTo", file: "pkl/testdata/unknown-reports.pkl", want: "reportsTo"},
		{name: "pool schedules", file: "pkl/testdata/pool-schedules.pkl", want: "schedules"},
		{name: "unknown machine", file: "pkl/testdata/unknown-machine.pkl", want: "unknown machine"},
		{name: "unknown runner machine", file: "pkl/testdata/unknown-runner-machine.pkl", want: "runner names an unknown machine"},
		{name: "unknown anomaly role", file: "pkl/testdata/unknown-anomaly-role.pkl", want: "anomalyRole does not name a role"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, stderr, code := evalPkl(t, filepath.Join(root, tc.file))
			if code == 0 {
				t.Fatalf("pkl eval %s succeeded", tc.file)
			}
			if !bytes.Contains(stderr, []byte(tc.want)) {
				t.Errorf("stderr missing %q:\n%s", tc.want, stderr)
			}
		})
	}
}

func TestDecodeRejectsBadJSON(t *testing.T) {
	if _, err := Decode([]byte("{")); err == nil {
		t.Fatal("Decode accepted invalid JSON")
	}
}

func TestClassesAndRoleResolution(t *testing.T) {
	amends := filepath.Join(orgctlRoot(t), "pkl", "Org.pkl")
	src := `amends "` + amends + `"

org = "Fixture"

machines {
  ["one"] { host = "one.example"; herdrHandle = "one"; timezone = "UTC" }
}

rules {
  new { id = "org-rule"; text = "Org rule."; source = "handbook" }
}

classes {
  ["ops"] {
    description = "Operations roles."
    rules {
      new { id = "ops-rule"; text = "Ops rule."; source = "ops guide" }
    }
  }
  ["special"] {
    description = "Special roles."
  }
}

roles {
  ["ops/inbox"] {
    machine = "one"; workspace = "hq"; tab = "inbox"; cwd = "/work"; agent = "coder"
  }
  ["ops/override"] {
    machine = "one"; workspace = "hq"; tab = "override"; cwd = "/work"; agent = "coder"
    ` + "`class`" + ` = "special"
  }
  ["misc/helper"] {
    machine = "one"; workspace = "hq"; tab = "helper"; cwd = "/work"; agent = "coder"
  }
  ["solo"] {
    machine = "one"; workspace = "hq"; tab = "solo"; cwd = "/work"; agent = "coder"
  }
}
`
	path := filepath.Join(t.TempDir(), "org.pkl")
	if err := os.WriteFile(path, []byte(src), 0o600); err != nil {
		t.Fatal(err)
	}
	stdout, stderr, code := evalPkl(t, path)
	if code != 0 {
		t.Fatalf("pkl eval exit %d\n%s", code, stderr)
	}
	spec, err := Decode(stdout)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if got := spec.Roles["ops/inbox"].Class; got != "ops" {
		t.Errorf("ops/inbox class = %q, want ops", got)
	}
	if got := spec.Roles["ops/override"].Class; got != "special" {
		t.Errorf("ops/override class = %q, want special", got)
	}
	if got := spec.Roles["misc/helper"].Class; got != "" {
		t.Errorf("misc/helper class = %q, want empty", got)
	}
	if got := spec.Roles["solo"].Class; got != "" {
		t.Errorf("solo class = %q, want empty", got)
	}
	if err := spec.CheckRules(); err != nil {
		t.Errorf("CheckRules: %v", err)
	}
	if err := spec.CheckClasses(); err != nil {
		t.Errorf("CheckClasses: %v", err)
	}
}

func TestCheckRulesAndClasses(t *testing.T) {
	dupOrgClass := Spec{
		Rules: []Rule{{ID: "shared", Text: "a", Source: "s"}},
		Classes: map[string]Class{
			"ops": {Description: "ops", Rules: []Rule{{ID: "shared", Text: "b", Source: "s"}}},
		},
	}
	if err := dupOrgClass.CheckRules(); err == nil {
		t.Error("CheckRules accepted duplicate id across org and class rules")
	}

	dupTwoClasses := Spec{
		Classes: map[string]Class{
			"a": {Description: "a", Rules: []Rule{{ID: "r1", Text: "a", Source: "s"}}},
			"b": {Description: "b", Rules: []Rule{{ID: "r1", Text: "b", Source: "s"}}},
		},
	}
	if err := dupTwoClasses.CheckRules(); err == nil {
		t.Error("CheckRules accepted duplicate id across two classes")
	}

	unknownClass := Spec{
		Classes: map[string]Class{"ops": {Description: "ops"}},
		Roles:   map[string]Role{"ops/inbox": {Class: "missing"}},
	}
	if err := unknownClass.CheckClasses(); err == nil {
		t.Error("CheckClasses accepted unknown explicit class")
	}

	noClass := Spec{
		Classes: map[string]Class{"ops": {Description: "ops"}},
		Roles:   map[string]Role{"misc/helper": {}},
	}
	if err := noClass.CheckClasses(); err != nil {
		t.Errorf("CheckClasses rejected role without class: %v", err)
	}
}

func evalPkl(t *testing.T, path string) ([]byte, []byte, int) {
	t.Helper()
	cmd := exec.CommandContext(t.Context(), "pkl", "eval", "-f", "json", path) //nolint:gosec // G204: pkl eval of a repo fixture
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	err := cmd.Run()
	if err == nil {
		return stdout.Bytes(), stderr.Bytes(), 0
	}
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		return stdout.Bytes(), stderr.Bytes(), exitErr.ExitCode()
	}
	t.Fatalf("pkl: %v", err)
	return nil, nil, 1
}

func orgctlRoot(t *testing.T) string {
	t.Helper()
	var candidates []string
	if _, file, _, ok := runtime.Caller(0); ok {
		candidates = append(candidates, filepath.Dir(file))
	}
	candidates = append(candidates, ".")
	if srcdir := os.Getenv("TEST_SRCDIR"); srcdir != "" {
		ws := os.Getenv("TEST_WORKSPACE")
		if ws == "" {
			ws = "_main"
		}
		candidates = append(candidates, filepath.Join(srcdir, ws, "devtools/orgctl"))
	}
	for _, dir := range candidates {
		if _, err := os.Stat(filepath.Join(dir, "examples/acme.pkl")); err != nil {
			continue
		}
		abs, err := filepath.Abs(dir)
		if err != nil {
			t.Fatal(err)
		}
		return abs
	}
	t.Fatal("examples/acme.pkl not found")
	return ""
}
