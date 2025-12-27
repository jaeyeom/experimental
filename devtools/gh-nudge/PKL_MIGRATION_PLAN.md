# Pkl Migration Plan for gh-nudge

This document outlines the plan to migrate gh-nudge configuration from YAML to
Pkl using pkl-go with Bazel.

## Overview

**Current state:** YAML configuration with Go struct tags
**Target state:** Pkl schema with generated Go code via pkl-gen-go

**Note:** While `rules_pkl` is used for Pkl file management, it does not provide
Go code generation rules (only Java via `pkl_java_library`). Go code is generated
using `pkl-gen-go` from the `pkl-go` project.

**Tracking Progress:** When a phase is completed, mark it by appending
"(Completed)" to the phase header (e.g., `## Phase 1: ... (Completed)`).

## Benefits of Pkl

- **Type safety** - Errors caught at config load time
- **Defaults in schema** - No need to handle defaults in Go code
- **Environment variable support** - `read("env:VAR_NAME")`
- **Validation** - Add constraints like `reminder_threshold_hours: Int(this > 0)`
- **Imports/Composition** - Split configs across files

## Phase 1: Add rules_pkl to MODULE.bazel (Completed)

Add the following to `MODULE.bazel`:

```starlark
# Pkl for configuration
bazel_dep(name = "rules_pkl", version = "0.13.1")

pkl = use_extension("@rules_pkl//pkl:extensions.bzl", "pkl")
pkl.project(
    name = "pkl_project",
    pkl_project = "//:PklProject",
)
use_repo(pkl, "pkl_project", "pkl_project_deps")
```

## Phase 2: Create Pkl Project Files (Completed)

### PklProject (repo root)

Create `PklProject` at the repository root:

```pkl
amends "pkl:Project"

dependencies {
  ["pkl-go"] { uri = "package://pkg.pkl-lang.org/pkl-go/pkl.golang@0.12.1" }
}
```

Note: The `package` block is only needed if publishing the package. For local use
with rules_pkl, only the `dependencies` block is required.

### Generate PklProject.deps.json

Run the following to generate the dependency lock file:

```bash
pkl project resolve
```

## Phase 3: Define Pkl Schema (Completed)

Create the following file structure:

```
devtools/gh-nudge/
├── pkl/
│   ├── BUILD.bazel
│   ├── Config.pkl          # Schema definition
│   └── example.config.pkl  # Example config
```

### pkl/Config.pkl

```pkl
@go.Package { name = "github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config/pkl" }
module gh_nudge.Config

import "package://pkg.pkl-lang.org/pkl-go/pkl.golang@0.12.1#/go.pkl"

/// GitHub-related configuration
class GitHubConfig {
  /// Repository owner (organization or user). Defaults to authenticated user.
  owner: String?
  /// List of repository names to monitor
  repos: Listing<String>
}

/// Channel routing rule based on file patterns
class ChannelRoutingConfig {
  /// Regex pattern to match file paths
  pattern: String
  /// Slack channel to route matching PRs to
  channel: String
}

/// Slack-related configuration
class SlackConfig {
  /// Slack Bot OAuth token (starts with xoxb-)
  token: String
  /// Fallback channel if no routing patterns match
  default_channel: String
  /// GitHub username to Slack user ID mapping (for @mentions)
  user_id_mapping: Mapping<String, String>
  /// GitHub username to Slack DM channel ID mapping
  dm_channel_id_mapping: Mapping<String, String>
  /// Channel routing rules based on file patterns
  channel_routing: Listing<ChannelRoutingConfig>
}

/// General application settings
class SettingsConfig {
  /// Hours between reminder notifications for the same PR/reviewer
  reminder_threshold_hours: Int(this > 0) = 24
  /// Only send notifications during working hours
  working_hours_only: Boolean = false
  /// Template for reminder messages. Variables: {slack_id}, {title}, {hours}, {url}
  message_template: String = "Hey <@{slack_id}>, the PR '{title}' has been waiting for your review for {hours} hours."
  /// Send DMs to reviewers by default instead of channel messages
  dm_by_default: Boolean = false
}

/// Root configuration
github: GitHubConfig
slack: SlackConfig
settings: SettingsConfig = new {}
```

### pkl/example.config.pkl

```pkl
amends "Config.pkl"

github {
  owner = "organization-name"
  repos {
    "repo-name-1"
    "repo-name-2"
  }
}

slack {
  token = read("env:SLACK_TOKEN")
  default_channel = "#code-reviews"
  user_id_mapping {
    ["github-username-1"] = "U12345"
    ["github-username-2"] = "U67890"
  }
  dm_channel_id_mapping {
    ["github-username-1"] = "C12345"
    ["github-username-2"] = "C67890"
  }
  channel_routing {
    new {
      pattern = "frontend/.*\\.js$"
      channel = "#frontend"
    }
    new {
      pattern = "backend/.*\\.go$"
      channel = "#backend"
    }
    new {
      pattern = "docs/.*"
      channel = "#documentation"
    }
  }
}

settings {
  reminder_threshold_hours = 24
  working_hours_only = true
  dm_by_default = true
}
```

## Phase 4: BUILD.bazel for Pkl (Completed)

**Note:** `rules_pkl` does not provide Go code generation rules (only Java via
`pkl_java_library`). We use `pkl-gen-go` with a `genrule` instead.

### Install pkl-gen-go

First, install the `pkl-gen-go` tool:

```bash
go install github.com/apple/pkl-go/cmd/pkl-gen-go@latest
```

### devtools/gh-nudge/pkl/BUILD.bazel

```starlark
load("@rules_pkl//pkl:defs.bzl", "pkl_library")
load("@rules_go//go:def.bzl", "go_library")

pkl_library(
    name = "config_pkl",
    srcs = ["Config.pkl"],
    visibility = ["//visibility:public"],
)

# Generate Go code from Pkl schema using pkl-gen-go
genrule(
    name = "config_go_gen",
    srcs = ["Config.pkl"],
    outs = [
        "gen/Config.pkl.go",
        "gen/init.pkl.go",
    ],
    cmd = """
        mkdir -p $$(dirname $(location gen/Config.pkl.go))
        pkl-gen-go $(location Config.pkl) -o $$(dirname $(location gen/Config.pkl.go))
    """,
    visibility = ["//visibility:private"],
)

go_library(
    name = "config_go",
    srcs = [
        ":config_go_gen",
    ],
    importpath = "github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config/pkl",
    visibility = ["//visibility:public"],
    deps = [
        "@com_github_apple_pkl_go//pkl",
    ],
)
```

**Alternative:** If you prefer not to rely on `pkl-gen-go` being in PATH, you can
generate the Go files once and commit them to the repository:

```bash
# Generate Go files from Pkl schema
cd devtools/gh-nudge/pkl
pkl-gen-go Config.pkl -o gen/

# Commit the generated files
git add gen/
```

Then simplify the BUILD.bazel:

```starlark
load("@rules_go//go:def.bzl", "go_library")

go_library(
    name = "config_go",
    srcs = glob(["gen/*.go"]),
    importpath = "github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config/pkl",
    visibility = ["//visibility:public"],
    deps = [
        "@com_github_apple_pkl_go//pkl",
    ],
)
```

## Phase 5: Update go.mod (Completed)

Add pkl-go dependency:

```bash
go get github.com/apple/pkl-go@latest
```

## Phase 6: Update Config Package

### devtools/gh-nudge/internal/config/BUILD.bazel

Update dependencies to include generated Pkl types:

```starlark
go_library(
    name = "config",
    srcs = ["config.go"],
    importpath = "github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config",
    visibility = ["//devtools/gh-nudge:__subpackages__"],
    deps = [
        "//devtools/gh-nudge/internal/slack",
        "//devtools/gh-nudge/pkl:config_go",  # Generated Pkl types
        "@com_github_apple_pkl_go//pkl",
        "@in_gopkg_yaml_v3//:yaml_v3",  # Keep during transition
    ],
)
```

### devtools/gh-nudge/internal/config/config.go

Add Pkl loading support alongside YAML:

```go
package config

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/apple/pkl-go/pkl"
	"gopkg.in/yaml.v3"

	pklconfig "github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config/pkl"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/slack"
)

// LoadConfig loads the configuration from the specified file path.
// Supports both .pkl and .yaml files. If path is empty, it attempts
// to load from the default location (preferring .pkl over .yaml).
func LoadConfig(path string) (*Config, error) {
	if path == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return nil, fmt.Errorf("failed to get user home directory: %w", err)
		}
		basePath := filepath.Join(home, ".config", "gh-nudge", "config")

		// Prefer .pkl over .yaml
		if fileExists(basePath + ".pkl") {
			path = basePath + ".pkl"
		} else {
			path = basePath + ".yaml"
		}
	}

	if strings.HasSuffix(path, ".pkl") {
		return loadPklConfig(path)
	}
	return loadYamlConfig(path)
}

func fileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func loadPklConfig(path string) (*Config, error) {
	evaluator, err := pkl.NewEvaluator(context.Background(), pkl.PreconfiguredOptions)
	if err != nil {
		return nil, fmt.Errorf("failed to create pkl evaluator: %w", err)
	}
	defer evaluator.Close()

	var pklCfg pklconfig.Config
	if err := evaluator.EvaluateModule(context.Background(), pkl.FileSource(path), &pklCfg); err != nil {
		return nil, fmt.Errorf("failed to evaluate pkl config: %w", err)
	}

	return convertPklConfig(&pklCfg), nil
}

func convertPklConfig(pklCfg *pklconfig.Config) *Config {
	cfg := &Config{
		GitHub: GitHubConfig{
			Repos: pklCfg.Github.Repos,
		},
		Slack: SlackConfig{
			Token:              pklCfg.Slack.Token,
			DefaultChannel:     pklCfg.Slack.DefaultChannel,
			UserIDMapping:      slack.UserIDMapping(pklCfg.Slack.UserIdMapping),
			DMChannelIDMapping: slack.DMChannelIDMapping(pklCfg.Slack.DmChannelIdMapping),
		},
		Settings: SettingsConfig{
			ReminderThresholdHours: int(pklCfg.Settings.ReminderThresholdHours),
			WorkingHoursOnly:       pklCfg.Settings.WorkingHoursOnly,
			MessageTemplate:        pklCfg.Settings.MessageTemplate,
			DMByDefault:            pklCfg.Settings.DmByDefault,
		},
	}

	if pklCfg.Github.Owner != nil {
		cfg.GitHub.Owner = *pklCfg.Github.Owner
	}

	for _, route := range pklCfg.Slack.ChannelRouting {
		cfg.Slack.ChannelRouting = append(cfg.Slack.ChannelRouting, ChannelRoutingConfig{
			Pattern: route.Pattern,
			Channel: route.Channel,
		})
	}

	return cfg
}

func loadYamlConfig(path string) (*Config, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("failed to read config file: %w", err)
	}

	var cfg Config
	if err := yaml.Unmarshal(data, &cfg); err != nil {
		return nil, fmt.Errorf("failed to parse config file: %w", err)
	}

	// Set default values if not specified
	if cfg.Settings.ReminderThresholdHours == 0 {
		cfg.Settings.ReminderThresholdHours = 24
	}
	if cfg.Settings.MessageTemplate == "" {
		cfg.Settings.MessageTemplate = "Hey <@{slack_id}>, the PR '{title}' has been waiting for your review for {hours} hours."
	}

	return &cfg, nil
}
```

## Phase 7: Build and Test

### Option A: Using genrule (requires pkl-gen-go in PATH)

Ensure `pkl-gen-go` is installed and available in your PATH:

```bash
# Verify pkl-gen-go is available
which pkl-gen-go

# Build the generated Go code
bazel build //devtools/gh-nudge/pkl:config_go

# Build the entire project
bazel build //devtools/gh-nudge/...

# Run tests
bazel test //devtools/gh-nudge/...
```

### Option B: Pre-generate and commit (recommended for CI)

Generate the Go files once and commit them:

```bash
# Generate Go files
cd devtools/gh-nudge/pkl
pkl-gen-go Config.pkl -o gen/

# Verify generated files
ls gen/

# Build and test
bazel build //devtools/gh-nudge/...
bazel test //devtools/gh-nudge/...
```

## Phase 8: Update Documentation

Update `README.org` to document both configuration formats, recommending Pkl for
new users while noting YAML remains supported for backwards compatibility.

## Phase 9: Deprecation Timeline

1. **Initial release**: Support both YAML and Pkl (prefer Pkl)
2. **Future release**: Log deprecation warning when YAML config is loaded
3. **Final release**: Remove YAML support entirely

## References

- [Pkl Language](https://pkl-lang.org/)
- [rules_pkl](https://github.com/apple/rules_pkl)
- [pkl-go](https://github.com/apple/pkl-go)
