package agentsmdsize

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/pkg/baseline"
)

// repoRoot returns the checkout root. Under Bazel the go.mod runfile is a
// symlink into the checkout, so resolving it finds the real tree.
func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", fmt.Errorf("getwd: %w", err)
	}
	for {
		mod := filepath.Join(dir, "go.mod")
		if _, err := os.Stat(mod); err == nil {
			resolved, err := filepath.EvalSymlinks(mod)
			if err != nil {
				return "", fmt.Errorf("resolve %s: %w", mod, err)
			}
			return filepath.Dir(resolved), nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", errors.New("go.mod not found")
		}
		dir = parent
	}
}

// TestRepoAgentsMDSize enforces AGENTS.md per-file and cascading byte size
// limits across the repository against .baselines/agents-md-size.json.
func TestRepoAgentsMDSize(t *testing.T) {
	root, err := repoRoot()
	if err != nil {
		t.Fatal(err)
	}
	bf, err := baseline.Load(filepath.Join(root, ".baselines", CheckerName+".json"))
	if err != nil {
		t.Fatalf("load baseline: %v", err)
	}
	report, err := CheckDir(root, Config{
		Baseline:       bf,
		StrictBaseline: true,
	})
	if err != nil {
		t.Fatalf("check dir: %v", err)
	}
	if report.FilesChecked == 0 {
		t.Fatal("no AGENTS.md files found")
	}
	for _, v := range report.Violations {
		t.Errorf("%s: %s %d bytes exceeds limit %d bytes (default %d); run agents-md-size --update-baseline",
			v.Path, v.Metric, v.Actual, v.Limit, v.Default)
	}
	for _, s := range report.StaleFindings {
		t.Errorf("stale baseline (%s): %s [%s] current=%d suggested=%d; run agents-md-size --tighten-baseline",
			s.Kind, s.Key, s.Metric, s.Current, s.Suggested)
	}
}
