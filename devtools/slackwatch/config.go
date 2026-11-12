// Package slackwatch tracks Slack posts made on a user's behalf and pushes
// replies to whoever owns each post.
//
// This package holds the config loader and the SQLite store shared by the
// server and the CLI.
package slackwatch

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"time"
)

// Default config values.
const (
	DefaultSweepInterval = 6 * time.Hour
	DefaultQuietExpiry   = 336 * time.Hour
	DefaultReopenWindow  = 720 * time.Hour
	DefaultExcerptLimit  = 300
	DefaultNotifyTimeout = 30 * time.Second
	DefaultUnownedGrace  = 10 * time.Second
)

// Config is ~/.config/slackwatch/config.json. Tokens are never stored here.
// TokenFile names a separate file that only the user can read.
type Config struct {
	NotifyTemplate   string            `json:"notifyTemplate"`
	LivenessTemplate string            `json:"livenessTemplate"`
	NotifyTimeout    Duration          `json:"notifyTimeout"`
	Targets          map[string]string `json:"targets"`
	FallbackTarget   string            `json:"fallbackTarget"`
	FooterPattern    Regexp            `json:"footerPattern"`
	NotifySelf       bool              `json:"notifySelf"`
	SweepInterval    Duration          `json:"sweepInterval"`
	QuietExpiry      Duration          `json:"quietExpiry"`
	ReopenWindow     Duration          `json:"reopenWindow"`
	UnownedGrace     Duration          `json:"unownedGrace"`
	ExcerptLimit     int               `json:"excerptLimit"`
	DBPath           string            `json:"dbPath"`
	TokenFile        string            `json:"tokenFile"`
}

// Target resolves name through the targets map. An unknown name is used
// as a raw address.
func (c Config) Target(name string) string {
	if t, ok := c.Targets[name]; ok {
		return t
	}
	return name
}

// Duration is a time.Duration written in JSON as a string such as "6h".
type Duration struct {
	time.Duration
}

// UnmarshalJSON parses a Go duration string.
func (d *Duration) UnmarshalJSON(b []byte) error {
	var s string
	if err := json.Unmarshal(b, &s); err != nil {
		return fmt.Errorf("duration must be a string such as \"6h\": %w", err)
	}
	v, err := time.ParseDuration(s)
	if err != nil {
		return fmt.Errorf("parse duration: %w", err)
	}
	d.Duration = v
	return nil
}

// MarshalJSON writes the duration as a string.
func (d Duration) MarshalJSON() ([]byte, error) {
	b, err := json.Marshal(d.String())
	if err != nil {
		return nil, fmt.Errorf("marshal duration: %w", err)
	}
	return b, nil
}

// Regexp is a regular expression written in JSON as a string. The zero
// value (nil Regexp) means no pattern.
type Regexp struct {
	*regexp.Regexp
}

// UnmarshalJSON compiles the pattern. An empty string means no pattern.
func (r *Regexp) UnmarshalJSON(b []byte) error {
	var s string
	if err := json.Unmarshal(b, &s); err != nil {
		return fmt.Errorf("pattern must be a string: %w", err)
	}
	if s == "" {
		r.Regexp = nil
		return nil
	}
	re, err := regexp.Compile(s)
	if err != nil {
		return fmt.Errorf("compile pattern: %w", err)
	}
	r.Regexp = re
	return nil
}

// MarshalJSON writes the pattern source.
func (r Regexp) MarshalJSON() ([]byte, error) {
	s := ""
	if r.Regexp != nil {
		s = r.String()
	}
	b, err := json.Marshal(s)
	if err != nil {
		return nil, fmt.Errorf("marshal pattern: %w", err)
	}
	return b, nil
}

// ConfigPath returns flagPath if set, else $SLACKWATCH_CONFIG, else
// ${XDG_CONFIG_HOME:-~/.config}/slackwatch/config.json.
func ConfigPath(flagPath string) string {
	if flagPath != "" {
		return flagPath
	}
	if v := os.Getenv("SLACKWATCH_CONFIG"); v != "" {
		return v
	}
	return filepath.Join(xdgDir("XDG_CONFIG_HOME", ".config"), "slackwatch", "config.json")
}

// DefaultDBPath returns ${XDG_STATE_HOME:-~/.local/state}/slackwatch/slackwatch.db.
func DefaultDBPath() string {
	return filepath.Join(xdgDir("XDG_STATE_HOME", filepath.Join(".local", "state")), "slackwatch", "slackwatch.db")
}

func xdgDir(env, homeRel string) string {
	if v := os.Getenv(env); v != "" {
		return v
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return homeRel
	}
	return filepath.Join(home, homeRel)
}

func defaultConfig() Config {
	return Config{
		SweepInterval: Duration{DefaultSweepInterval},
		QuietExpiry:   Duration{DefaultQuietExpiry},
		ReopenWindow:  Duration{DefaultReopenWindow},
		UnownedGrace:  Duration{DefaultUnownedGrace},
		ExcerptLimit:  DefaultExcerptLimit,
		NotifyTimeout: Duration{DefaultNotifyTimeout},
		DBPath:        DefaultDBPath(),
	}
}

// LoadConfig reads the config at path over the defaults. A missing file
// yields the defaults. Unknown keys are an error.
func LoadConfig(path string) (Config, error) {
	cfg := defaultConfig()
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return cfg, nil
	}
	if err != nil {
		return Config{}, fmt.Errorf("read slackwatch config: %w", err)
	}
	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&cfg); err != nil {
		return Config{}, fmt.Errorf("parse slackwatch config %s: %w", path, err)
	}
	if err := dec.Decode(&struct{}{}); !errors.Is(err, io.EOF) {
		return Config{}, fmt.Errorf("parse slackwatch config %s: trailing data after JSON object", path)
	}
	return cfg, nil
}

// Tokens are the Slack tokens in the token file, a JSON object such as
// {"appToken": "xapp-...", "userToken": "xoxp-..."}. AppToken is the
// app-level token for Socket Mode and UserToken the read-only user token.
type Tokens struct {
	AppToken  string `json:"appToken"`
	UserToken string `json:"userToken"`
}

// LoadTokens reads the token file. Errors never include its contents.
func LoadTokens(path string) (Tokens, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return Tokens{}, fmt.Errorf("read token file: %w", err)
	}
	var t Tokens
	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if dec.Decode(&t) != nil || t.AppToken == "" || t.UserToken == "" {
		return Tokens{}, fmt.Errorf(`token file %s must be a JSON object with only "appToken" and "userToken"`, path)
	}
	return t, nil
}

// CheckTokenFile returns an error unless path names a regular file that
// group and others cannot read. It never reads the file contents.
func CheckTokenFile(path string) error {
	if path == "" {
		return errors.New("tokenFile is not set in slackwatch config")
	}
	info, err := os.Stat(path)
	if err != nil {
		return fmt.Errorf("token file: %w", err)
	}
	if !info.Mode().IsRegular() {
		return fmt.Errorf("token file %s is not a regular file", path)
	}
	if perm := info.Mode().Perm(); perm&0o044 != 0 {
		return fmt.Errorf("token file %s has mode %04o; it must not be readable by group or others (chmod 600)", path, perm)
	}
	return nil
}
