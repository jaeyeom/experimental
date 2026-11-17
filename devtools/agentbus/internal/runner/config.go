package runner

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"slices"
	"time"
)

// Defaults of the runner config.
const (
	DefaultPoll    = 10 * time.Second
	DefaultTimeout = time.Hour
)

// Kind is how this runner runs one job kind. The job's executor picks
// which of Command and Target is used.
type Kind struct {
	// Command is the argv of a cli job, or the argv that starts a
	// new-tab job's agent. See [Expand] for its placeholders.
	Command []string
	// Target is the agentbus name or handle a tab job's prompt goes to.
	Target string
	// Prompt is text put before the job line in a tab or new-tab
	// prompt.
	Prompt string
	// Parallel is how many jobs of the kind run at once. At least 1.
	Parallel int
	// Start puts the kind under the start pacing.
	Start bool
	// Timeout bounds a cli command, and how long the runner heartbeats
	// a job an agent works on.
	Timeout time.Duration
}

// Pacing spaces and caps the starts of start kinds. Zero turns a check
// off.
type Pacing struct {
	// MinSpacing is the least time between two starts on this runner.
	MinSpacing time.Duration
	// MaxInFlight caps the start jobs running in the queue, on every
	// machine.
	MaxInFlight int
	// LaneCap caps the start jobs this runner runs: its lane is its
	// machine.
	LaneCap int
	// MinFreeDisk is the least free space, in bytes, on DiskPath.
	MinFreeDisk uint64
	// DiskPath is where free space is measured. Empty is the home
	// directory.
	DiskPath string
}

// Config is the runner's local config file. Gates are not here: they
// come only from the user's jobs policy.
type Config struct {
	Poll  time.Duration
	Kinds map[string]Kind
	Start Pacing
}

// KindNames are the configured kinds, sorted.
func (c Config) KindNames() []string {
	names := make([]string, 0, len(c.Kinds))
	for name := range c.Kinds {
		names = append(names, name)
	}
	slices.Sort(names)
	return names
}

type kindFile struct {
	Command  []string `json:"command"`
	Target   string   `json:"target"`
	Prompt   string   `json:"prompt"`
	Parallel int      `json:"parallel"`
	Start    bool     `json:"start"`
	Timeout  string   `json:"timeout"`
}

type pacingFile struct {
	MinSpacing    string `json:"minSpacing"`
	MaxInFlight   int    `json:"maxInFlight"`
	LaneCap       int    `json:"laneCap"`
	MinFreeDiskMB uint64 `json:"minFreeDiskMb"`
	DiskPath      string `json:"diskPath"`
}

type configFile struct {
	Poll  string              `json:"poll"`
	Kinds map[string]kindFile `json:"kinds"`
	Start pacingFile          `json:"start"`
}

// ParseConfig reads the runner config JSON. Unknown keys are errors, so
// a gate, hook, or permission written here is refused, not ignored.
func ParseConfig(data []byte) (Config, error) {
	var f configFile
	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&f); err != nil {
		return Config{}, fmt.Errorf("decode runner config: %w", err)
	}
	cfg := Config{Poll: DefaultPoll, Kinds: map[string]Kind{}}
	var err error
	if cfg.Poll, err = duration("poll", f.Poll, DefaultPoll); err != nil {
		return Config{}, err
	}
	if len(f.Kinds) == 0 {
		return Config{}, fmt.Errorf("runner config: want at least one kind")
	}
	for name, kf := range f.Kinds {
		k := Kind{Command: kf.Command, Target: kf.Target, Prompt: kf.Prompt, Parallel: kf.Parallel, Start: kf.Start}
		if k.Parallel == 0 {
			k.Parallel = 1
		}
		if k.Parallel < 0 {
			return Config{}, fmt.Errorf("kind %s: parallel must be at least 1", name)
		}
		if len(k.Command) == 0 && k.Target == "" {
			return Config{}, fmt.Errorf("kind %s: want a command or a target", name)
		}
		if k.Timeout, err = duration("kind "+name+" timeout", kf.Timeout, DefaultTimeout); err != nil {
			return Config{}, err
		}
		cfg.Kinds[name] = k
	}
	p := f.Start
	if p.MaxInFlight < 0 || p.LaneCap < 0 {
		return Config{}, fmt.Errorf("start: maxInFlight and laneCap must not be negative")
	}
	cfg.Start = Pacing{MaxInFlight: p.MaxInFlight, LaneCap: p.LaneCap, MinFreeDisk: p.MinFreeDiskMB << 20, DiskPath: p.DiskPath}
	if cfg.Start.MinSpacing, err = duration("start minSpacing", p.MinSpacing, 0); err != nil {
		return Config{}, err
	}
	return cfg, nil
}

func duration(what, raw string, def time.Duration) (time.Duration, error) {
	if raw == "" {
		return def, nil
	}
	d, err := time.ParseDuration(raw)
	if err != nil || d <= 0 {
		return 0, fmt.Errorf("%s %q: want a positive duration such as \"5m\"", what, raw)
	}
	return d, nil
}

// LoadConfig reads the runner config at path. Unlike the jobs config,
// a missing file is an error: a runner with no kinds runs nothing.
func LoadConfig(path string) (Config, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return Config{}, fmt.Errorf("read runner config: %w", err)
	}
	cfg, err := ParseConfig(data)
	if err != nil {
		return Config{}, fmt.Errorf("%s: %w", path, err)
	}
	return cfg, nil
}
