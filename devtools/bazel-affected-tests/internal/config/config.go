// Package config provides configuration file loading and pattern matching
// for bazel-affected-tests.
package config

import (
	"errors"
	"fmt"
	"io/fs"
	"os"

	"gopkg.in/yaml.v3"
)

const ConfigFileName = ".bazel-affected-tests.yaml"

// Config represents the configuration file structure.
type Config struct {
	Version int    `yaml:"version"`
	Rules   []Rule `yaml:"rules"`
}

// Rule maps glob patterns to Bazel targets.
type Rule struct {
	Patterns []string `yaml:"patterns"`
	Targets  []string `yaml:"targets"`
}

// LoadConfig loads the configuration from .bazel-affected-tests.yaml in the current directory.
// Returns nil, nil if the file does not exist.
// Returns nil, error if the file exists but cannot be parsed.
func LoadConfig() (*Config, error) {
	data, err := os.ReadFile(ConfigFileName)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return nil, nil
		}
		return nil, fmt.Errorf("failed to read config file: %w", err)
	}

	var config Config
	if err := yaml.Unmarshal(data, &config); err != nil {
		return nil, fmt.Errorf("failed to parse config file: %w", err)
	}

	return &config, nil
}

// MatchTargets returns all targets whose patterns match any of the given files.
func (c *Config) MatchTargets(files []string) []string {
	targetSet := make(map[string]bool)

	for _, rule := range c.Rules {
		matched := false
		for _, pattern := range rule.Patterns {
			for _, file := range files {
				if MatchPattern(pattern, file) {
					matched = true
					break
				}
			}
			if matched {
				break
			}
		}

		if matched {
			for _, target := range rule.Targets {
				targetSet[target] = true
			}
		}
	}

	// Convert set to slice
	targets := make([]string, 0, len(targetSet))
	for target := range targetSet {
		targets = append(targets, target)
	}

	return targets
}
