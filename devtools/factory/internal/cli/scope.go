package cli

import (
	"fmt"

	"github.com/spf13/cobra"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

// processEnv is the environment list commands read the current directory
// from. Tests replace it.
var processEnv = config.OSEnv

// scopeFlags are --source and --all on a read-only list command.
type scopeFlags struct {
	only string
	all  bool
}

// addScopeFlags registers --all, and --source when the command does not
// already have it.
func addScopeFlags(cmd *cobra.Command, s *scopeFlags) {
	if cmd.Flags().Lookup("source") == nil {
		cmd.Flags().StringVar(&s.only, "source", "", "limit to this source key (kind:name, for example jira:acme)")
	}
	cmd.Flags().BoolVar(&s.all, "all", false, "show every source, not only the one whose cwd holds the current directory")
}

// check rejects --all together with --source.
func (s scopeFlags) check(name string) error {
	if s.all && s.only != "" {
		return usageError{err: fmt.Errorf("%s: expected --all or --source, found both", name)}
	}
	return nil
}

// resolve returns the source key a command limits itself to, or empty
// for every source. --source wins, --all is every source, and otherwise the
// current directory picks the source whose cwd holds it, with one stderr line
// saying so. Inside an unconfigured git repository (when --config was not
// explicitly passed), resolve returns an error naming the git root. Outside any
// git repository, resolve returns empty so every source is included.
func (s scopeFlags) resolve(cmd *cobra.Command, cfg *config.Config) (string, error) {
	if err := s.check(cmd.Name()); err != nil {
		return "", err
	}
	if s.only != "" || s.all {
		return s.only, nil
	}
	env := processEnv()
	dir, err := env.Getwd()
	if err != nil {
		return "", fmt.Errorf("%s: current directory: %w", cmd.Name(), err)
	}
	key, ok := cfg.SourceForDir(dir, env)
	if ok {
		verb := scopeVerb(cmd.Name())
		if _, err := fmt.Fprintf(cmd.ErrOrStderr(), "factory: %s %s (current directory); --all for every source\n", verb, key); err != nil {
			return "", fmt.Errorf("write scope: %w", err)
		}
		return key, nil
	}
	if !configFlagChanged(cmd) {
		root, inGit, err := config.GitRoot(dir, env)
		if err != nil {
			return "", fmt.Errorf("%s: %w", cmd.Name(), err)
		}
		if inGit {
			return "", fmt.Errorf("%s: git repository %s is not configured in %s; run `factory init` or pass --all or --source", cmd.Name(), root, cfg.Path)
		}
	}
	return "", nil
}

func scopeVerb(cmdName string) string {
	switch cmdName {
	case "sync", "gate", "tick", "run":
		return "scoping to"
	default:
		return "showing"
	}
}

func configFlagChanged(cmd *cobra.Command) bool {
	if cmd == nil || cmd.Root() == nil {
		return false
	}
	f := cmd.Root().PersistentFlags().Lookup("config")
	return f != nil && f.Changed
}
