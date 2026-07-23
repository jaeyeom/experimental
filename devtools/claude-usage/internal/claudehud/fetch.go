// Package claudehud is a TEMPORARY bridge to claude-hud's usage-api.js.
//
// It exists only so the MVP CLI can fetch live usage before the standalone
// credential resolver (#201) and Anthropic usage API client (#202) land.
// Do not expand this package; delete it when those issues ship.
package claudehud

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/claude-usage/internal/usage"
)

// ErrPluginNotFound means no claude-hud usage-api.js was found under the
// Claude config plugins cache.
var ErrPluginNotFound = errors.New("claude-hud usage-api.js not found")

// Fetcher loads usage by invoking Node against claude-hud's getUsage().
//
// TEMPORARY: replace with standalone credentials + HTTP client (issues #201, #202).
type Fetcher struct {
	// ConfigDir is CLAUDE_CONFIG_DIR (default: ~/.claude).
	ConfigDir string
	// NodeBin is the node executable (default: "node" from PATH).
	NodeBin string
	// HomeDir overrides os.UserHomeDir for tests.
	HomeDir string
}

// Fetch implements usage.Fetcher.
func (f *Fetcher) Fetch(ctx context.Context) (*usage.Data, error) {
	pluginDir, err := f.resolvePluginDir()
	if err != nil {
		return nil, err
	}
	usageAPI := filepath.Join(pluginDir, "dist", "usage-api.js")
	if _, err := os.Stat(usageAPI); err != nil {
		return nil, fmt.Errorf("%w under %s", ErrPluginNotFound, pluginDir)
	}

	nodeBin := f.NodeBin
	if nodeBin == "" {
		nodeBin = "node"
	}

	// Keep the access token inside the HUD module; only pass the module path.
	const script = `
const { getUsage } = await import(process.env.USAGE_API);
const u = await getUsage();
if (!u) {
  process.exit(2);
}
console.log(JSON.stringify(u));
`
	// nodeBin defaults to "node"; only overrideable for tests / PATH layout.
	// nosemgrep: go.lang.security.audit.dangerous-exec-command.dangerous-exec-command
	cmd := exec.CommandContext(ctx, nodeBin, "--input-type=module", "-e", script) //nolint:gosec // nodeBin is local tooling path, not user input
	cmd.Env = append(os.Environ(), "USAGE_API="+usageAPI)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) && exitErr.ExitCode() == 2 {
			return nil, usage.ErrNoData
		}
		msg := strings.TrimSpace(stderr.String())
		if msg == "" {
			msg = err.Error()
		}
		return nil, fmt.Errorf("claude-hud getUsage: %s", msg)
	}

	return parseUsageJSON(stdout.Bytes())
}

type hudJSON struct {
	PlanName        string   `json:"planName"`
	FiveHour        *float64 `json:"fiveHour"`
	SevenDay        *float64 `json:"sevenDay"`
	FiveHourResetAt *string  `json:"fiveHourResetAt"`
	SevenDayResetAt *string  `json:"sevenDayResetAt"`
}

func parseUsageJSON(raw []byte) (*usage.Data, error) {
	var h hudJSON
	if err := json.Unmarshal(raw, &h); err != nil {
		return nil, fmt.Errorf("parse usage json: %w", err)
	}
	d := &usage.Data{PlanName: h.PlanName}
	if h.FiveHour != nil {
		v := int(*h.FiveHour)
		d.FiveHour = &v
	}
	if h.SevenDay != nil {
		v := int(*h.SevenDay)
		d.SevenDay = &v
	}
	t5, err := parseOptionalTime(h.FiveHourResetAt)
	if err != nil {
		return nil, err
	}
	d.FiveHourResetAt = t5
	t7, err := parseOptionalTime(h.SevenDayResetAt)
	if err != nil {
		return nil, err
	}
	d.SevenDayResetAt = t7
	return d, nil
}

func parseOptionalTime(s *string) (*time.Time, error) {
	if s == nil || *s == "" {
		return nil, nil
	}
	t, err := time.Parse(time.RFC3339Nano, *s)
	if err != nil {
		t, err = time.Parse(time.RFC3339, *s)
		if err != nil {
			return nil, fmt.Errorf("parse time %q: %w", *s, err)
		}
	}
	return &t, nil
}

var semverDir = regexp.MustCompile(`^[0-9]+\.[0-9]+\.[0-9]+`)

type versionDir struct {
	major, minor, patch int
	path                string
}

func (f *Fetcher) resolvePluginDir() (string, error) {
	configDir, err := f.configDir()
	if err != nil {
		return "", err
	}
	cands, err := listClaudeHUDVersions(configDir)
	if err != nil {
		return "", err
	}
	if len(cands) == 0 {
		return "", fmt.Errorf("%w under %s/plugins/cache", ErrPluginNotFound, configDir)
	}
	sort.Slice(cands, func(i, j int) bool {
		return versionLess(cands[i], cands[j])
	})
	return cands[len(cands)-1].path, nil
}

func (f *Fetcher) configDir() (string, error) {
	if f.ConfigDir != "" {
		return f.ConfigDir, nil
	}
	if v := os.Getenv("CLAUDE_CONFIG_DIR"); v != "" {
		return v, nil
	}
	home := f.HomeDir
	if home == "" {
		var err error
		home, err = os.UserHomeDir()
		if err != nil {
			return "", fmt.Errorf("resolve home dir: %w", err)
		}
	}
	return filepath.Join(home, ".claude"), nil
}

func listClaudeHUDVersions(configDir string) ([]versionDir, error) {
	// Same layout the local script / statusline uses:
	// $CONFIG/plugins/cache/*/claude-hud/<version>/
	pattern := filepath.Join(configDir, "plugins", "cache", "*", "claude-hud", "*")
	matches, err := filepath.Glob(pattern)
	if err != nil {
		return nil, fmt.Errorf("glob claude-hud plugins: %w", err)
	}
	var cands []versionDir
	for _, m := range matches {
		vd, ok := parseVersionDir(m)
		if !ok {
			continue
		}
		if _, err := os.Stat(filepath.Join(m, "dist", "usage-api.js")); err != nil {
			continue
		}
		cands = append(cands, vd)
	}
	return cands, nil
}

func parseVersionDir(path string) (versionDir, bool) {
	base := filepath.Base(path)
	if !semverDir.MatchString(base) {
		return versionDir{}, false
	}
	parts := strings.SplitN(base, ".", 3)
	if len(parts) != 3 {
		return versionDir{}, false
	}
	patchStr := parts[2]
	if i := strings.IndexAny(patchStr, "-+"); i >= 0 {
		patchStr = patchStr[:i]
	}
	major, err1 := strconv.Atoi(parts[0])
	minor, err2 := strconv.Atoi(parts[1])
	patch, err3 := strconv.Atoi(patchStr)
	if err1 != nil || err2 != nil || err3 != nil {
		return versionDir{}, false
	}
	return versionDir{major: major, minor: minor, patch: patch, path: path}, true
}

func versionLess(a, b versionDir) bool {
	if a.major != b.major {
		return a.major < b.major
	}
	if a.minor != b.minor {
		return a.minor < b.minor
	}
	return a.patch < b.patch
}
