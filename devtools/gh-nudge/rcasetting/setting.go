// Package rcasetting reads the gh-merge rca_marker setting.
package rcasetting

import (
	"errors"
	"fmt"
	"os"
	"sync"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config"
)

// Source loads one gh-merge config and returns its rca_marker.
// An empty Path uses the default gh-nudge config file.
type Source struct {
	Path string

	once   sync.Once
	cfg    *config.Config
	err    error
	absent bool
}

// For returns the rca_marker for owner/repo.
// A missing config file, or no home directory for the default path, returns an empty marker.
func (s *Source) For(owner, repo string) (string, error) {
	if s == nil {
		return "", nil
	}
	s.once.Do(s.load)
	if s.absent {
		return "", nil
	}
	if s.err != nil {
		return "", fmt.Errorf("gh-merge config: %w", s.err)
	}
	return s.cfg.ForRepository(owner, repo).Settings.RCAMarker, nil
}

func (s *Source) load() {
	if s.Path == "" {
		if _, err := os.UserHomeDir(); err != nil {
			s.err = err
			s.absent = true
			return
		}
	}
	cfg, err := config.LoadConfig(s.Path)
	if errors.Is(err, os.ErrNotExist) {
		s.err = err
		s.absent = true
		return
	}
	s.cfg = cfg
	s.err = err
}
