// Package diskwatch reports low free space and runs a configured command.
// It never deletes files.
package diskwatch

import (
	"errors"
	"fmt"
	"os"
	"time"

	"github.com/pelletier/go-toml/v2"
)

const (
	// DefaultCooldown is the wait after a trigger before the same mount
	// can trigger again, unless free space falls below urgent.
	DefaultCooldown = 6 * time.Hour
	// DefaultSummaryTop is how many usage rows a trigger lists.
	DefaultSummaryTop = 10
	// DefaultSummaryTimeout bounds the usage walk. A timeout yields a
	// partial summary and is not a failure.
	DefaultSummaryTimeout = 5 * time.Second
)

// Config is the user's diskwatch config.
type Config struct {
	Cooldown    time.Duration
	SummaryTop  int
	ReportPaths []string
	OnLow       []string
	Mounts      []Mount
}

// Mount is one filesystem to watch.
type Mount struct {
	Path     string
	Low      Amount
	High     Amount
	Urgent   *Amount
	Cooldown time.Duration
}

type fileConfig struct {
	Cooldown    string      `toml:"cooldown"`
	SummaryTop  int         `toml:"summary_top"`
	ReportPaths []string    `toml:"report_paths"`
	OnLow       []string    `toml:"on_low"`
	Mounts      []fileMount `toml:"mounts"`
}

type fileMount struct {
	Path     string `toml:"path"`
	Low      string `toml:"low"`
	High     string `toml:"high"`
	Urgent   string `toml:"urgent"`
	Cooldown string `toml:"cooldown"`
}

// ParseConfig parses TOML. low must be below high, and on_low must be a
// non-empty argv. Nothing in the defaults names a path.
func ParseConfig(data []byte) (Config, error) {
	var raw fileConfig
	if err := toml.Unmarshal(data, &raw); err != nil {
		return Config{}, fmt.Errorf("parse config: %w", err)
	}
	cfg := Config{
		Cooldown:    DefaultCooldown,
		SummaryTop:  raw.SummaryTop,
		ReportPaths: raw.ReportPaths,
		OnLow:       raw.OnLow,
	}
	if raw.Cooldown != "" {
		d, err := time.ParseDuration(raw.Cooldown)
		if err != nil {
			return Config{}, fmt.Errorf("cooldown: %w", err)
		}
		cfg.Cooldown = d
	}
	if cfg.Cooldown <= 0 {
		return Config{}, errors.New("cooldown must be positive")
	}
	if cfg.SummaryTop == 0 {
		cfg.SummaryTop = DefaultSummaryTop
	}
	if cfg.SummaryTop < 0 {
		return Config{}, errors.New("summary_top must be positive")
	}
	if err := validateArgv(cfg.OnLow); err != nil {
		return Config{}, err
	}
	if len(raw.Mounts) == 0 {
		return Config{}, errors.New("config has no mounts")
	}
	for _, mount := range raw.Mounts {
		parsed, err := parseMount(mount, cfg.Cooldown)
		if err != nil {
			return Config{}, err
		}
		cfg.Mounts = append(cfg.Mounts, parsed)
	}
	return cfg, nil
}

func validateArgv(argv []string) error {
	if len(argv) == 0 {
		return errors.New("on_low is empty")
	}
	for _, arg := range argv {
		if arg == "" {
			return errors.New("on_low has an empty argument")
		}
	}
	return nil
}

func parseMount(raw fileMount, cooldown time.Duration) (Mount, error) {
	if raw.Path == "" {
		return Mount{}, errors.New("mount path is empty")
	}
	low, err := ParseAmount(raw.Low)
	if err != nil {
		return Mount{}, fmt.Errorf("mount %s low: %w", raw.Path, err)
	}
	high, err := ParseAmount(raw.High)
	if err != nil {
		return Mount{}, fmt.Errorf("mount %s high: %w", raw.Path, err)
	}
	less, err := low.Less(high)
	if err != nil {
		return Mount{}, fmt.Errorf("mount %s: low and high must use the same unit", raw.Path)
	}
	if !less {
		return Mount{}, fmt.Errorf("mount %s: low must be below high", raw.Path)
	}
	mount := Mount{Path: raw.Path, Low: low, High: high, Cooldown: cooldown}
	if raw.Cooldown != "" {
		d, err := time.ParseDuration(raw.Cooldown)
		if err != nil {
			return Mount{}, fmt.Errorf("mount %s cooldown: %w", raw.Path, err)
		}
		if d <= 0 {
			return Mount{}, fmt.Errorf("mount %s: cooldown must be positive", raw.Path)
		}
		mount.Cooldown = d
	}
	if raw.Urgent == "" {
		return mount, nil
	}
	urgent, err := ParseAmount(raw.Urgent)
	if err != nil {
		return Mount{}, fmt.Errorf("mount %s urgent: %w", raw.Path, err)
	}
	urgentLess, err := urgent.Less(low)
	if err != nil {
		return Mount{}, fmt.Errorf("mount %s: urgent and low must use the same unit", raw.Path)
	}
	if !urgentLess {
		return Mount{}, fmt.Errorf("mount %s: urgent must be below low", raw.Path)
	}
	mount.Urgent = &urgent
	return mount, nil
}

// LoadConfig reads a TOML config file.
func LoadConfig(path string) (Config, error) {
	data, err := os.ReadFile(path) //nolint:gosec // G304: the user's own config path
	if err != nil {
		return Config{}, fmt.Errorf("read config: %w", err)
	}
	cfg, err := ParseConfig(data)
	if err != nil {
		return Config{}, err
	}
	return cfg, nil
}
