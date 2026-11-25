package approve

import (
	"errors"
	"fmt"
	"io"
	"os/exec"
	"runtime"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/github"
	"github.com/jaeyeom/experimental/devtools/pkg/ghcli"
)

// ErrInvalidPullRef is returned when a pull request reference is malformed.
var ErrInvalidPullRef = errors.New("invalid pull request reference")

// OpenOptions configures Open.
type OpenOptions struct {
	Ref   string
	Print bool
	// LoadRepos returns the configured owner/repo entries. It is called
	// only when Ref is a bare pull request number.
	LoadRepos func() ([]string, error)
	Out       io.Writer
	// GHBin overrides the gh executable name. Empty uses ghcli.Bin().
	GHBin string
	// GOOS overrides runtime.GOOS when selecting the fallback OS opener.
	GOOS string
	// LookPath overrides exec.LookPath for testing.
	LookPath func(file string) (string, error)
	// Run executes the chosen opener command. Nil runs exec.Command.
	Run func(name string, args ...string) error
}

// ConfiguredRepos returns the unique owner/repo entries from cfg.GitHub.
func ConfiguredRepos(cfg *config.Config) []string {
	if cfg == nil {
		return nil
	}
	seen := make(map[string]bool, len(cfg.GitHub.Repos))
	var repos []string
	for _, entry := range cfg.GitHub.Repos {
		entry = strings.TrimSpace(entry)
		if entry == "" {
			continue
		}
		full := entry
		if !strings.Contains(entry, "/") {
			if cfg.GitHub.Owner == "" {
				continue
			}
			full = cfg.GitHub.Owner + "/" + entry
		}
		if seen[full] {
			continue
		}
		seen[full] = true
		repos = append(repos, full)
	}
	return repos
}

// ParseOpenRef resolves a pull request reference given the configured
// owner/repo list. It accepts a bare number (using the single configured repo),
// owner/repo#N, or a GitHub pull request URL.
func ParseOpenRef(raw string, configuredRepos []string) (github.PullRef, error) {
	raw = strings.TrimSpace(raw)
	if raw == "" {
		return github.PullRef{}, fmt.Errorf("%w: empty reference", ErrInvalidPullRef)
	}
	if n, ok := parseBareNumber(raw); ok {
		return resolveBareNumber(n, configuredRepos)
	}
	ref, err := github.ParsePullRef(raw)
	if err != nil {
		return github.PullRef{}, fmt.Errorf("%w: %q", ErrInvalidPullRef, raw)
	}
	return ref, nil
}

func parseBareNumber(s string) (int, bool) {
	if s == "" {
		return 0, false
	}
	for i := 0; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return 0, false
		}
	}
	n, err := strconv.Atoi(s)
	if err != nil || n <= 0 {
		return 0, false
	}
	return n, true
}

func resolveBareNumber(n int, configuredRepos []string) (github.PullRef, error) {
	switch len(configuredRepos) {
	case 0:
		return github.PullRef{}, fmt.Errorf("pull request number %d requires a configured repository; pass owner/repo#%d or a pull request URL", n, n)
	case 1:
		owner, repo, ok := strings.Cut(configuredRepos[0], "/")
		if !ok || owner == "" || repo == "" || strings.Contains(repo, "/") {
			return github.PullRef{}, fmt.Errorf("configured repository %q is not owner/repo", configuredRepos[0])
		}
		return github.PullRef{
			Owner:  owner,
			Repo:   repo,
			Number: n,
			Raw:    fmt.Sprintf("%s/%s#%d", owner, repo, n),
		}, nil
	default:
		return github.PullRef{}, fmt.Errorf("pull request number %d is ambiguous across configured repos (%s); pass owner/repo#%d", n, strings.Join(configuredRepos, ", "), n)
	}
}

// Open resolves opts.Ref and either prints its URL or opens it in a browser.
func Open(opts OpenOptions) error {
	ref, err := resolveOpenRef(opts.Ref, opts.LoadRepos)
	if err != nil {
		return err
	}
	prURL := ref.URL()
	if opts.Print {
		if opts.Out == nil {
			return errors.New("open: expected an output writer")
		}
		if _, err := fmt.Fprintln(opts.Out, prURL); err != nil {
			return fmt.Errorf("write url: %w", err)
		}
		return nil
	}
	return openInBrowser(ref, prURL, opts)
}

func resolveOpenRef(raw string, loadRepos func() ([]string, error)) (github.PullRef, error) {
	raw = strings.TrimSpace(raw)
	var repos []string
	if _, ok := parseBareNumber(raw); ok && loadRepos != nil {
		var err error
		repos, err = loadRepos()
		if err != nil {
			return github.PullRef{}, err
		}
	}
	return ParseOpenRef(raw, repos)
}

func openInBrowser(ref github.PullRef, prURL string, opts OpenOptions) error {
	ghBin := opts.GHBin
	if ghBin == "" {
		ghBin = ghcli.Bin()
	}
	lookPath := opts.LookPath
	if lookPath == nil {
		lookPath = exec.LookPath
	}
	run := opts.Run
	if run == nil {
		run = runOpenerCommand
	}
	if _, err := lookPath(ghBin); err == nil {
		args := []string{"browse", strconv.Itoa(ref.Number), "-R", ref.Owner + "/" + ref.Repo}
		if err := run(ghBin, args...); err != nil {
			return fmt.Errorf("open %s with %s: %w", prURL, ghBin, err)
		}
		return nil
	}
	opener := osOpener(opts.GOOS)
	if err := run(opener, prURL); err != nil {
		return fmt.Errorf("open %s with %s: %w", prURL, opener, err)
	}
	return nil
}

func osOpener(goos string) string {
	if goos == "" {
		goos = runtime.GOOS
	}
	if goos == "darwin" {
		return "open"
	}
	return "xdg-open"
}

func runOpenerCommand(name string, args ...string) error {
	// name is either ghcli.Bin() ("gh" / $GH_BIN) or a fixed OS URL opener
	// ("open" / "xdg-open"), invoked directly without a shell using a validated
	// pull request ref or GitHub URL.
	// nosemgrep: dangerous-exec-command
	out, err := exec.Command(name, args...).CombinedOutput() //nolint:gosec // opener is gh or OS URL opener with validated PR ref arguments
	if err != nil {
		msg := strings.TrimSpace(string(out))
		if msg != "" {
			return fmt.Errorf("run %s: %w: %s", name, err, msg)
		}
		return fmt.Errorf("run %s: %w", name, err)
	}
	return nil
}
