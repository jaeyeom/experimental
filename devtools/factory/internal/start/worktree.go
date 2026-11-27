package start

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/jaeyeom/experimental/devtools/factory/internal/config"
)

func (s *Starter) effectiveSpawner() string {
	if s.Cfg.Worker.Spawner != "" {
		return s.Cfg.Worker.Spawner
	}
	return config.SpawnerAgentbus
}

func (s *Starter) effectiveAgent() string {
	if s.Cfg.Worker.Agent != "" {
		return s.Cfg.Worker.Agent
	}
	return s.Defaults.AgentKind
}

func (s *Starter) effectiveDefaultArgs() []string {
	if s.Cfg.Worker.AgentArgs != nil {
		return s.Cfg.Worker.AgentArgs
	}
	return s.Defaults.AgentArgs
}

func (s *Starter) effectiveWorktree() bool {
	return s.Cfg.Worker.Worktree
}

func (s *Starter) effectiveOutputMode() string {
	if s.Cfg.Output.Mode != "" {
		return s.Cfg.Output.Mode
	}
	return config.OutputModePR
}

// WorktreeSpec returns the default worktree directory (slot-1) and branch name for id.
func WorktreeSpec(cwd, id string) (dir, branch string) {
	return WorktreeSlotSpec(cwd, id, 1)
}

// WorktreeSlotSpec returns the isolated worktree directory for a 1-based slot
// and the issue-specific branch name for id.
func WorktreeSlotSpec(cwd, id string, slot int) (dir, branch string) {
	if slot < 1 {
		slot = 1
	}
	clean := filepath.Clean(cwd)
	slug := sanitizeWorktreeName(id)
	dir = filepath.Join(filepath.Dir(clean), "."+filepath.Base(clean)+"-worktrees", fmt.Sprintf("slot-%d", slot))
	branch = "factory/" + slug
	return dir, branch
}

func (s *Starter) allocateWorktree(ctx context.Context, id string) (dir, branch string, err error) {
	claimed := make(map[string]struct{})
	if s.Store != nil {
		claims, err := s.Store.ActiveClaims(ctx)
		if err != nil {
			return "", "", err //nolint:wrapcheck // propagate context cancellation
		}
		for _, c := range claims {
			if c.Worktree != "" {
				claimed[filepath.Clean(c.Worktree)] = struct{}{}
			}
		}
	}
	for slot := 1; ; slot++ {
		dir, branch = WorktreeSlotSpec(s.Cfg.Cwd, id, slot)
		if _, ok := claimed[filepath.Clean(dir)]; ok {
			continue
		}
		_, statErr := os.Lstat(dir)
		if errors.Is(statErr, os.ErrNotExist) {
			return dir, branch, nil
		}
		if statErr != nil {
			return "", "", fmt.Errorf("check worktree slot %s: %w", dir, statErr)
		}
	}
}

func sanitizeWorktreeName(id string) string {
	var b strings.Builder
	b.Grow(len(id))
	for _, r := range id {
		switch {
		case (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') || r == '-' || r == '_' || r == '.':
			b.WriteRune(r)
		default:
			b.WriteByte('-')
		}
	}
	out := strings.Trim(b.String(), "-.")
	if out == "" {
		return "worker"
	}
	return out
}

func (s *Starter) prepareWorktree(ctx context.Context, id string) (string, error) {
	if !s.effectiveWorktree() || s.Machine != "" {
		return "", nil
	}
	if s.WorktreeAdd == nil {
		return "", errors.New("start: expected a git worktree add function, found nil")
	}
	dir, branch, err := s.allocateWorktree(ctx, id)
	if err != nil {
		return "", err
	}
	if err := s.WorktreeAdd(ctx, s.Cfg.Cwd, dir, branch, s.Cfg.Base); err != nil {
		return "", err
	}
	return dir, nil
}

func (s *Starter) cleanupWorktree(ctx context.Context, dir string) error {
	if dir == "" || s.WorktreeRemove == nil {
		return nil
	}
	return s.WorktreeRemove(ctx, s.Cfg.Cwd, dir)
}
