package cli

import (
	"errors"
	"fmt"
	"net/http"

	executor "github.com/jaeyeom/go-cmdexec"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/github"
	"github.com/jaeyeom/experimental/devtools/factory/internal/source/jira"
)

// Deps are the clients the CLI injects into source backends.
type Deps struct {
	// HTTP is the client for HTTP backends such as Jira.
	HTTP *http.Client
	// Exec runs gh for GitHub sources.
	Exec executor.Executor
}

// newSource builds one backend. Tests replace it with a fake source.
var newSource = defaultSource

// buildSources returns backends keyed by kind:name, for example jira:acme
// or github:experimental.
func buildSources(cfg *config.Config, deps Deps) (map[string]source.Source, error) {
	if cfg == nil {
		return nil, errors.New("sources: expected a config, found nil")
	}
	out := make(map[string]source.Source, len(cfg.Sources))
	for i := range cfg.Sources {
		srcCfg := cfg.Sources[i]
		key, err := sourceKey(srcCfg.Kind, srcCfg.Name)
		if err != nil {
			return nil, err
		}
		if _, ok := out[key]; ok {
			return nil, fmt.Errorf("source %s: expected a unique key, found a duplicate", key)
		}
		built, err := newSource(&srcCfg, deps)
		if err != nil {
			return nil, err
		}
		if built == nil {
			return nil, fmt.Errorf("source %s: expected a source, found nil", key)
		}
		out[key] = built
	}
	return out, nil
}

func sourceKey(kind, name string) (string, error) {
	if kind == "" || name == "" {
		return "", fmt.Errorf("source: expected non-empty kind and name, found kind %q name %q", kind, name)
	}
	return kind + ":" + name, nil
}

func defaultSource(cfg *config.Source, deps Deps) (source.Source, error) {
	if cfg == nil {
		return nil, errors.New("source: expected a config source, found nil")
	}
	switch cfg.Kind {
	case "jira":
		if deps.HTTP == nil {
			return nil, fmt.Errorf("source %q: expected an HTTP client, found nil", cfg.Name)
		}
		return wiredSource(cfg, func(wired config.Source) source.Source {
			return jira.New(wired, deps.HTTP)
		})
	case "github":
		if deps.Exec == nil {
			return nil, fmt.Errorf("source %q: expected an executor, found nil", cfg.Name)
		}
		return wiredSource(cfg, func(wired config.Source) source.Source {
			return github.New(wired, deps.Exec, wired.GHBin)
		})
	default:
		return nil, fmt.Errorf("source %q: expected kind jira or github, found %q", cfg.Name, cfg.Kind)
	}
}

// sourceDeps returns the clients buildSources injects.
// GitHub runs gh through Exec. Jira uses HTTP. Either may be unused.
func sourceDeps() (Deps, error) {
	exec := newCommandExec()
	if exec == nil {
		return Deps{}, errors.New("sources: expected an executor, found nil")
	}
	return Deps{HTTP: &http.Client{}, Exec: exec}, nil
}

// wiredSource sets Name to kind:name before build.
// Jira and GitHub copy cfg.Name onto Item.Source. The store rejects an item
// whose Source is not the mirror key.
func wiredSource(cfg *config.Source, build func(config.Source) source.Source) (source.Source, error) {
	key, err := sourceKey(cfg.Kind, cfg.Name)
	if err != nil {
		return nil, err
	}
	wired := *cfg
	wired.Name = key
	return build(wired), nil
}
