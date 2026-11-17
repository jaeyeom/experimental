package jobs

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"slices"
	"strings"

	"github.com/google/renameio/v2"
	"github.com/jaeyeom/experimental/devtools/prsync/internal/scan"
)

// OwnersFile sits next to state_file. It keeps, for each pull request,
// the machine whose agent tab last worked it, so its jobs keep that
// machine after the tab is gone.
const OwnersFile = "job-owners.json"

// Owners maps owner/repo#N to a herdr machine (local or one of
// machines).
type Owners map[string]string

// LoadOwners reads path. A missing file is empty.
func LoadOwners(path string) (Owners, error) {
	raw, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return Owners{}, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read job owners: %w", err)
	}
	out := Owners{}
	if err := json.Unmarshal(raw, &out); err != nil {
		return nil, fmt.Errorf("decode job owners %s: %w", path, err)
	}
	return out, nil
}

// Save writes o to path atomically.
func (o Owners) Save(path string) error {
	raw, err := json.MarshalIndent(o, "", "  ")
	if err != nil {
		return fmt.Errorf("encode job owners: %w", err)
	}
	if err := renameio.WriteFile(path, append(raw, '\n'), 0o600); err != nil {
		return fmt.Errorf("write job owners: %w", err)
	}
	return nil
}

// Note records the machine of each agent tab in doc, and forgets pull
// requests of doc's repositories that are no longer open in it.
func (o Owners) Note(doc scan.Document) {
	open := make(map[string]bool, len(doc.PRs))
	for _, pr := range doc.PRs {
		key := Subject(pr.Repo, pr.Number)
		open[key] = pr.State == "" || pr.State == "open"
		if pr.Tab != nil && pr.Tab.PaneID != nil {
			o[key] = TabMachine(pr.Tab)
		}
	}
	for key := range o {
		repo, _, ok := strings.Cut(key, "#")
		if ok && slices.Contains(doc.Repos, repo) && !open[key] {
			delete(o, key)
		}
	}
}
