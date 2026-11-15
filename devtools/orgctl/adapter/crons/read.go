// Package crons reads Claude Code durable scheduled jobs for a role.
// It opens <cwd>/.claude/scheduled_tasks.json read-only and never writes it.
// Jobs that exist only in a session are not in that file.
package crons

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"time"
)

// DefaultLifetime is how long a job stays listed after createdAt.
const DefaultLifetime = 7 * 24 * time.Hour

const promptLen = 80

// Config overrides the job lifetime. A non-positive Lifetime uses DefaultLifetime.
type Config struct {
	Lifetime time.Duration
}

// Schedule is one spec schedule to look up in the file.
type Schedule struct {
	ID      string
	Durable bool
}

// Job is one task from scheduled_tasks.json.
// Prompt is at most 80 characters. ExpiresAt is CreatedAt plus the lifetime.
type Job struct {
	ID                 string     `json:"id"`
	Cron               string     `json:"cron"`
	Prompt             string     `json:"prompt"`
	CreatedAt          time.Time  `json:"createdAt"`
	LastFiredAt        *time.Time `json:"lastFiredAt,omitempty"`
	CreatedBySessionID string     `json:"createdBySessionId,omitempty"`
	ExpiresAt          time.Time  `json:"expiresAt"`
}

// ScheduleStatus is how one spec schedule compares with the file.
type ScheduleStatus struct {
	ID     string
	Status string
}

const (
	// StatusPresent means the schedule id is in the file.
	StatusPresent = "present"
	// StatusMissing means a durable schedule is not in the file.
	StatusMissing = "missing"
	// StatusUnverifiable means a schedule that is not durable is absent,
	// or the file could not be read. Session-only jobs are not on disk.
	StatusUnverifiable = "unverifiable"
)

type rawTask struct {
	ID                 string   `json:"id"`
	Cron               string   `json:"cron"`
	Prompt             string   `json:"prompt"`
	CreatedAt          *float64 `json:"createdAt"`
	LastFiredAt        *float64 `json:"lastFiredAt"`
	CreatedBySessionID string   `json:"createdBySessionId"`
}

// Read reads cwd/.claude/scheduled_tasks.json.
// A missing file returns an empty job list.
// A malformed file returns a warning and no jobs.
// An absent schedule is unverifiable unless it is durable, which is missing.
func Read(cwd string, schedules []Schedule, cfg Config) ([]Job, []ScheduleStatus, []string) {
	path := filepath.Join(cwd, ".claude", "scheduled_tasks.json")
	data, err := openRead(path)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return []Job{}, classify(schedules, nil, true), nil
		}
		return []Job{}, classify(schedules, nil, false), []string{"scheduled tasks: not read: " + path}
	}
	jobs, warnings, ok := parse(data, lifetime(cfg))
	if !ok {
		return []Job{}, classify(schedules, nil, false), []string{"scheduled tasks: malformed file: " + path}
	}
	return jobs, classify(schedules, jobs, true), warnings
}

func lifetime(cfg Config) time.Duration {
	if cfg.Lifetime > 0 {
		return cfg.Lifetime
	}
	return DefaultLifetime
}

func openRead(path string) ([]byte, error) {
	f, err := os.Open(path) //nolint:gosec // G304: role cwd plus scheduled_tasks.json, read-only
	if err != nil {
		return nil, fmt.Errorf("open scheduled tasks: %w", err)
	}
	data, err := io.ReadAll(f)
	cerr := f.Close()
	if err != nil {
		return nil, fmt.Errorf("read scheduled tasks: %w", err)
	}
	if cerr != nil {
		return nil, fmt.Errorf("close scheduled tasks: %w", cerr)
	}
	return data, nil
}

func parse(data []byte, life time.Duration) ([]Job, []string, bool) {
	var doc struct {
		Tasks []rawTask `json:"tasks"`
	}
	if err := json.Unmarshal(data, &doc); err != nil {
		return nil, nil, false
	}
	jobs := make([]Job, 0, len(doc.Tasks))
	var warnings []string
	for _, task := range doc.Tasks {
		job, warning := convert(task, life)
		if warning != "" {
			warnings = append(warnings, warning)
			continue
		}
		jobs = append(jobs, job)
	}
	return jobs, warnings, true
}

func convert(task rawTask, life time.Duration) (Job, string) {
	if task.ID == "" || task.CreatedAt == nil {
		return Job{}, "scheduled tasks: malformed job"
	}
	created := time.UnixMilli(int64(*task.CreatedAt)).UTC()
	job := Job{
		ID:                 task.ID,
		Cron:               task.Cron,
		Prompt:             firstChars(task.Prompt, promptLen),
		CreatedAt:          created,
		CreatedBySessionID: task.CreatedBySessionID,
		ExpiresAt:          created.Add(life),
	}
	if task.LastFiredAt != nil {
		fired := time.UnixMilli(int64(*task.LastFiredAt)).UTC()
		job.LastFiredAt = &fired
	}
	return job, ""
}

func firstChars(s string, n int) string {
	runes := []rune(s)
	if len(runes) <= n {
		return s
	}
	return string(runes[:n])
}

func classify(schedules []Schedule, jobs []Job, listed bool) []ScheduleStatus {
	ids := make(map[string]struct{}, len(jobs))
	for _, job := range jobs {
		ids[job.ID] = struct{}{}
	}
	out := make([]ScheduleStatus, 0, len(schedules))
	for _, schedule := range schedules {
		_, found := ids[schedule.ID]
		out = append(out, ScheduleStatus{
			ID:     schedule.ID,
			Status: statusOf(found, listed, schedule.Durable),
		})
	}
	return out
}

func statusOf(found, listed, durable bool) string {
	if found {
		return StatusPresent
	}
	if listed && durable {
		return StatusMissing
	}
	return StatusUnverifiable
}
