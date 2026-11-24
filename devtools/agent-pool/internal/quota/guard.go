// Package quota implements the consumer quota guard backed by claude-usage and
// quotapace, as well as the local SQLite donation audit ledger.
package quota

import (
	"context"
	"time"

	"github.com/jaeyeom/experimental/devtools/agent-pool/internal/protocol"
)

const (
	// DefaultProvider is the v1 quota usage provider.
	DefaultProvider = "claude-usage"
	// DefaultPollInterval is how long a usage reading is cached before re-polling.
	DefaultPollInterval = 5 * time.Minute
	// DefaultStaleAfter is the maximum age of a cached reading before it is stale.
	DefaultStaleAfter = 15 * time.Minute
	// DefaultMaxFiveHourPct is the default 5-hour usage cap percentage.
	DefaultMaxFiveHourPct = 75.0
	// DefaultMaxSevenDayPct is the default 7-day usage cap percentage.
	DefaultMaxSevenDayPct = 80.0
	// DefaultQuotapaceProfile is the default quotapace profile name.
	DefaultQuotapaceProfile = "agent-pool"
	// StateUnknown indicates that quota posture could not be determined.
	StateUnknown = "unknown"
)

// Config configures the quota guard.
type Config struct {
	Provider         string        `toml:"provider"`             // "claude-usage" (v1)
	PollInterval     time.Duration `toml:"poll_interval"`        // default 5m
	StaleAfter       time.Duration `toml:"stale_after"`          // default 15m
	MaxFiveHourPct   float64       `toml:"max_five_hour_pct"`    // default 75.0
	MaxSevenDayPct   float64       `toml:"max_seven_day_pct"`    // default 80.0
	QuotapaceProfile string        `toml:"quotapace_profile"`    // default "agent-pool"
	QuotapaceDir     string        `toml:"quotapace_dir"`        // optional override for quotapace data dir
	RequireGreenPace bool          `toml:"require_green_pace"`   // default true
	FailClosed       bool          `toml:"fail_closed_on_error"` // default true
}

// DefaultConfig returns a Config populated with default values.
func DefaultConfig() Config {
	return Config{
		Provider:         DefaultProvider,
		PollInterval:     DefaultPollInterval,
		StaleAfter:       DefaultStaleAfter,
		MaxFiveHourPct:   DefaultMaxFiveHourPct,
		MaxSevenDayPct:   DefaultMaxSevenDayPct,
		QuotapaceProfile: DefaultQuotapaceProfile,
		RequireGreenPace: true,
		FailClosed:       true,
	}
}

func normalizeConfig(cfg Config) Config {
	if cfg.Provider == "" {
		cfg.Provider = DefaultProvider
	}
	if cfg.PollInterval <= 0 {
		cfg.PollInterval = DefaultPollInterval
	}
	if cfg.StaleAfter <= 0 {
		cfg.StaleAfter = DefaultStaleAfter
	}
	if cfg.MaxFiveHourPct <= 0 {
		cfg.MaxFiveHourPct = DefaultMaxFiveHourPct
	}
	if cfg.MaxSevenDayPct <= 0 {
		cfg.MaxSevenDayPct = DefaultMaxSevenDayPct
	}
	if cfg.QuotapaceProfile == "" {
		cfg.QuotapaceProfile = DefaultQuotapaceProfile
	}
	return cfg
}

// Decision is the admission result returned by Guard.CanAccept.
type Decision struct {
	Allow  bool
	Reason string
	Health protocol.QuotaHealth
}

// Guard evaluates whether a consumer worker may accept new pooled jobs based
// on upstream quota readings and pace projections.
type Guard interface {
	CanAccept(ctx context.Context) Decision
	Refresh(ctx context.Context) (protocol.QuotaHealth, error)
}
