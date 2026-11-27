package orgctl

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// DefaultCronStale is how old a published cron file may be before plan marks it stale.
const DefaultCronStale = 24 * time.Hour

// PublishedCron is one live cron entry published by a running session.
type PublishedCron struct {
	ID       string    `json:"id"`
	Schedule string    `json:"schedule"`
	Prompt   string    `json:"prompt,omitempty"`
	Created  time.Time `json:"created,omitempty"`
	Expiry   time.Time `json:"expiry,omitempty"`
}

// PublishedCrons is the per-role cron state published by a session.
type PublishedCrons struct {
	Role        string          `json:"role"`
	PublishedAt time.Time       `json:"publishedAt"`
	Crons       []PublishedCron `json:"crons"`
	// ReadError records a malformed or unreadable published file so plan
	// can report unverifiable instead of failing the entire run.
	ReadError string `json:"-"`
}

// ValidateRoleName rejects role names that are empty or could traverse paths.
func ValidateRoleName(role string) error {
	if role == "" {
		return errors.New("role name must not be empty")
	}
	if strings.HasPrefix(role, "/") || strings.HasSuffix(role, "/") {
		return fmt.Errorf("invalid role name %q: must not start or end with '/'", role)
	}
	for i := 0; i < len(role); i++ {
		b := role[i]
		if b < 0x20 || b == 0x7f || b == '\\' {
			return fmt.Errorf("invalid role name %q: contains forbidden character", role)
		}
	}
	for _, seg := range strings.Split(role, "/") {
		if seg == "" || seg == "." || seg == ".." {
			return fmt.Errorf("invalid role name %q: contains invalid path segment %q", role, seg)
		}
	}
	return nil
}

// DefaultCronStateDir returns $XDG_STATE_HOME/orgctl/crons, falling back
// to ~/.local/state/orgctl/crons when XDG_STATE_HOME is unset.
func DefaultCronStateDir() (string, error) {
	if xdg := os.Getenv("XDG_STATE_HOME"); xdg != "" {
		base, err := ExpandPath(xdg)
		if err != nil {
			return "", err
		}
		return filepath.Join(base, "orgctl", "crons"), nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("home directory: %w", err)
	}
	return filepath.Join(home, ".local", "state", "orgctl", "crons"), nil
}

// PublishedCronPath resolves <dir>/<role>.json safely, using DefaultCronStateDir
// when dir is empty.
func PublishedCronPath(dir, role string) (string, error) {
	if err := ValidateRoleName(role); err != nil {
		return "", err
	}
	base := dir
	if base == "" {
		var err error
		base, err = DefaultCronStateDir()
		if err != nil {
			return "", err
		}
	} else {
		var err error
		base, err = ExpandPath(base)
		if err != nil {
			return "", err
		}
	}
	absBase, err := filepath.Abs(base)
	if err != nil {
		return "", fmt.Errorf("resolve cron state dir: %w", err)
	}
	target := filepath.Join(absBase, filepath.FromSlash(role)+".json")
	rel, err := filepath.Rel(absBase, target)
	if err != nil || rel == "." || rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		return "", fmt.Errorf("invalid role name %q: escapes cron state dir", role)
	}
	return target, nil
}

type rawPublishedCron struct {
	ID        string    `json:"id"`
	Schedule  string    `json:"schedule"`
	Cron      string    `json:"cron"`
	Prompt    string    `json:"prompt"`
	Created   *flexTime `json:"created"`
	CreatedAt *flexTime `json:"createdAt"`
	Expiry    *flexTime `json:"expiry"`
	ExpiresAt *flexTime `json:"expiresAt"`
}

type flexTime struct {
	t time.Time
}

func (f *flexTime) UnmarshalJSON(data []byte) error {
	trimmed := bytes.TrimSpace(data)
	if len(trimmed) == 0 || bytes.Equal(trimmed, []byte("null")) {
		return nil
	}
	if trimmed[0] == '"' {
		var t time.Time
		if err := json.Unmarshal(trimmed, &t); err != nil {
			return fmt.Errorf("parse timestamp: %w", err)
		}
		f.t = t.UTC()
		return nil
	}
	var millis int64
	if err := json.Unmarshal(trimmed, &millis); err != nil {
		return fmt.Errorf("parse epoch timestamp: %w", err)
	}
	f.t = time.UnixMilli(millis).UTC()
	return nil
}

// ParsePublishedCrons decodes a published cron list from either a JSON array
// or an object with a "crons" (or "tasks") array.
func ParsePublishedCrons(data []byte) ([]PublishedCron, error) {
	raw, err := unmarshalRawPublishedCrons(data)
	if err != nil {
		return nil, err
	}
	out := make([]PublishedCron, 0, len(raw))
	for i, item := range raw {
		cron, err := normalizePublishedCron(i, item)
		if err != nil {
			return nil, err
		}
		out = append(out, cron)
	}
	return out, nil
}

func unmarshalRawPublishedCrons(data []byte) ([]rawPublishedCron, error) {
	trimmed := bytes.TrimSpace(data)
	if len(trimmed) == 0 {
		return nil, errors.New("parse published crons: empty input")
	}
	switch trimmed[0] {
	case '[':
		var raw []rawPublishedCron
		if err := json.Unmarshal(trimmed, &raw); err != nil {
			return nil, fmt.Errorf("parse published crons: %w", err)
		}
		return raw, nil
	case '{':
		var doc struct {
			Crons []rawPublishedCron `json:"crons"`
			Tasks []rawPublishedCron `json:"tasks"`
		}
		if err := json.Unmarshal(trimmed, &doc); err != nil {
			return nil, fmt.Errorf("parse published crons: %w", err)
		}
		switch {
		case doc.Crons != nil:
			return doc.Crons, nil
		case doc.Tasks != nil:
			return doc.Tasks, nil
		default:
			return nil, errors.New("parse published crons: expected \"crons\" array")
		}
	default:
		return nil, errors.New("parse published crons: expected JSON array or object")
	}
}

func normalizePublishedCron(index int, item rawPublishedCron) (PublishedCron, error) {
	sched := firstSet(item.Schedule, item.Cron)
	if item.ID == "" {
		return PublishedCron{}, fmt.Errorf("published cron[%d]: missing id", index)
	}
	if sched == "" {
		return PublishedCron{}, fmt.Errorf("published cron[%d] (%s): missing schedule", index, item.ID)
	}
	return PublishedCron{
		ID:       item.ID,
		Schedule: sched,
		Prompt:   item.Prompt,
		Created:  pickFlexTime(item.Created, item.CreatedAt),
		Expiry:   pickFlexTime(item.Expiry, item.ExpiresAt),
	}, nil
}

func firstSet(a, b string) string {
	if a != "" {
		return a
	}
	return b
}

func pickFlexTime(a, b *flexTime) time.Time {
	if a != nil {
		return a.t
	}
	if b != nil {
		return b.t
	}
	return time.Time{}
}

// PublishCrons writes a role's live cron list atomically to <dir>/<role>.json.
func PublishCrons(dir, role string, crons []PublishedCron, now time.Time) (string, error) {
	path, err := PublishedCronPath(dir, role)
	if err != nil {
		return "", err
	}
	if now.IsZero() {
		now = time.Now().UTC()
	} else {
		now = now.UTC()
	}
	if crons == nil {
		crons = []PublishedCron{}
	}
	doc := PublishedCrons{
		Role:        role,
		PublishedAt: now,
		Crons:       crons,
	}
	body, err := json.MarshalIndent(doc, "", "  ")
	if err != nil {
		return "", fmt.Errorf("marshal published crons: %w", err)
	}
	body = append(body, '\n')
	if err := writeAtomic(path, body, 0o700); err != nil {
		return "", err
	}
	return path, nil
}

// ReadPublishedCrons reads published session cron files for each role on local
// (or all roles if local is empty). Missing files are omitted from the returned map.
func ReadPublishedCrons(spec Spec, local, dir string) map[string]PublishedCrons {
	out := map[string]PublishedCrons{}
	for _, name := range mapKeys(spec.Roles) {
		role := spec.Roles[name]
		if local != "" && role.Machine != local {
			continue
		}
		path, err := PublishedCronPath(dir, name)
		if err != nil {
			out[name] = PublishedCrons{Role: name, ReadError: err.Error()}
			continue
		}
		info, err := os.Stat(path)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				continue
			}
			out[name] = PublishedCrons{Role: name, ReadError: fmt.Sprintf("stat published crons: %v", err)}
			continue
		}
		data, err := os.ReadFile(path) //nolint:gosec // G304: validated role path under cron state dir
		if err != nil {
			out[name] = PublishedCrons{Role: name, ReadError: fmt.Sprintf("read published crons: %v", err)}
			continue
		}
		var doc PublishedCrons
		if err := json.Unmarshal(data, &doc); err != nil {
			out[name] = PublishedCrons{Role: name, ReadError: fmt.Sprintf("parse published crons: %v", err)}
			continue
		}
		if doc.Role == "" {
			doc.Role = name
		}
		if doc.PublishedAt.IsZero() {
			doc.PublishedAt = info.ModTime().UTC()
		}
		if doc.Crons == nil {
			doc.Crons = []PublishedCron{}
		}
		out[name] = doc
	}
	return out
}
