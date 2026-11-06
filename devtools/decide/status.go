package decide

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/google/renameio/v2"
	"golang.org/x/sys/unix"
)

const (
	// StatusWorking is the default agent state.
	StatusWorking = "working"
	// StatusIdle means the agent is waiting.
	StatusIdle = "idle"
	// StatusPaused means the agent stopped on purpose.
	StatusPaused = "paused"
	// StatusBlocked means the agent needs a person.
	StatusBlocked = "blocked"

	statusFile = "status.json"
	statusLock = "status.lock"
)

// AgentStatus is one agent's published row.
type AgentStatus struct {
	Name      string    `json:"name"`
	State     string    `json:"state"`
	Text      string    `json:"text"`
	UpdatedAt time.Time `json:"updatedAt"`
}

type statusFileData struct {
	Agents []AgentStatus `json:"agents"`
}

// Status is the agent and resource board in one decide state directory.
type Status struct {
	dir string
	now func() time.Time
}

// OpenStatus creates dir if needed and returns the status board stored there.
func OpenStatus(dir string) (*Status, error) {
	if dir == "" {
		return nil, fmt.Errorf("decide directory is empty")
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, fmt.Errorf("create decide directory: %w", err)
	}
	return &Status{dir: dir, now: time.Now}, nil
}

// SetAgent writes one agent's row. An empty state becomes working for a new
// name and keeps the stored state for an existing name.
func (s *Status) SetAgent(name, state, text string) (AgentStatus, error) {
	name = strings.TrimSpace(name)
	if name == "" {
		return AgentStatus{}, fmt.Errorf("decide status agent name is required")
	}
	if strings.TrimSpace(text) == "" {
		return AgentStatus{}, fmt.Errorf("decide status text is required")
	}
	if state != "" && !validAgentState(state) {
		return AgentStatus{}, fmt.Errorf("decide status state %q is invalid", state)
	}
	var saved AgentStatus
	err := s.locked(true, func(data *statusFileData) error {
		idx := agentIndex(data.Agents, name)
		if state == "" {
			if idx >= 0 {
				state = data.Agents[idx].State
			} else {
				state = StatusWorking
			}
		}
		saved = AgentStatus{
			Name:      name,
			State:     state,
			Text:      text,
			UpdatedAt: s.clock().UTC(),
		}
		if idx >= 0 {
			data.Agents[idx] = saved
			return nil
		}
		data.Agents = append(data.Agents, saved)
		sortAgents(data.Agents)
		return nil
	})
	if err != nil {
		return AgentStatus{}, err
	}
	return saved, nil
}

// ClearAgent removes one agent. Other rows stay.
func (s *Status) ClearAgent(name string) error {
	name = strings.TrimSpace(name)
	if name == "" {
		return fmt.Errorf("decide status agent name is required")
	}
	return s.locked(true, func(data *statusFileData) error {
		idx := agentIndex(data.Agents, name)
		if idx < 0 {
			return fmt.Errorf("decide status agent %q is not set", name)
		}
		data.Agents = append(data.Agents[:idx], data.Agents[idx+1:]...)
		return nil
	})
}

// Agents returns the stored agent rows, sorted by name.
func (s *Status) Agents() ([]AgentStatus, error) {
	var out []AgentStatus
	err := s.locked(false, func(data *statusFileData) error {
		out = append([]AgentStatus(nil), data.Agents...)
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

func validAgentState(state string) bool {
	switch state {
	case StatusWorking, StatusIdle, StatusPaused, StatusBlocked:
		return true
	default:
		return false
	}
}

func agentIndex(agents []AgentStatus, name string) int {
	for i := range agents {
		if agents[i].Name == name {
			return i
		}
	}
	return -1
}

func sortAgents(agents []AgentStatus) {
	sort.Slice(agents, func(i, j int) bool {
		return agents[i].Name < agents[j].Name
	})
}

func (s *Status) clock() time.Time {
	if s.now == nil {
		return time.Now()
	}
	return s.now()
}

func (s *Status) locked(write bool, fn func(*statusFileData) error) (err error) {
	f, err := s.acquire()
	if err != nil {
		return err
	}
	defer func() {
		relErr := release(f)
		if err == nil {
			err = relErr
		}
	}()
	data, err := loadStatus(filepath.Join(s.dir, statusFile))
	if err != nil {
		return err
	}
	if err = fn(data); err != nil {
		return err
	}
	if !write {
		return nil
	}
	return saveStatus(s.dir, data)
}

func (s *Status) acquire() (*os.File, error) {
	path := filepath.Join(s.dir, statusLock)
	//nolint:gosec // operator-local state file
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o600)
	if err != nil {
		return nil, fmt.Errorf("open decide status lock: %w", err)
	}
	if err := unix.Flock(int(f.Fd()), unix.LOCK_EX); err != nil { //nolint:gosec // G115: a file descriptor fits in int
		_ = f.Close()
		return nil, fmt.Errorf("lock decide status: %w", err)
	}
	return f, nil
}

func loadStatus(path string) (*statusFileData, error) {
	raw, err := os.ReadFile(path) //nolint:gosec // operator-local state file
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return &statusFileData{}, nil
		}
		return nil, fmt.Errorf("read decide status: %w", err)
	}
	var data statusFileData
	if err := json.Unmarshal(raw, &data); err != nil {
		return nil, fmt.Errorf("decode decide status: %w", err)
	}
	if data.Agents == nil {
		data.Agents = []AgentStatus{}
	}
	return &data, nil
}

func saveStatus(dir string, data *statusFileData) error {
	if data.Agents == nil {
		data.Agents = []AgentStatus{}
	}
	raw, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		return fmt.Errorf("encode decide status: %w", err)
	}
	raw = append(raw, '\n')
	if err := renameio.WriteFile(filepath.Join(dir, statusFile), raw, 0o600); err != nil {
		return fmt.Errorf("write decide status: %w", err)
	}
	return nil
}
