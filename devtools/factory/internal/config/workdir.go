package config

import (
	"path/filepath"
	"strings"
)

// SourceForDir returns the key (kind:name) of the source whose cwd contains
// dir: dir is that cwd or below it. Paths are compared after resolving
// symbolic links through env.EvalSymlinks, when set. When cwds are nested,
// the deepest one wins. ok is false when no cwd contains dir, or when two
// sources share the deepest cwd, since then neither is the current one.
func (c *Config) SourceForDir(dir string, env Env) (key string, ok bool) {
	if c == nil || dir == "" {
		return "", false
	}
	dir = realPath(dir, env)
	best, depth, tied := "", -1, false
	for i := range c.Sources {
		src := &c.Sources[i]
		if src.Cwd == "" {
			continue
		}
		cwd := realPath(src.Cwd, env)
		if !within(dir, cwd) {
			continue
		}
		switch d := len(cwd); {
		case d > depth:
			best, depth, tied = src.Kind+":"+src.Name, d, false
		case d == depth:
			tied = true
		}
	}
	if depth < 0 || tied {
		return "", false
	}
	return best, true
}

// SourceCoveringDir returns the key (kind:name) of a source whose cwd contains
// dir (dir is that cwd or below it). Unlike SourceForDir, ties still return the
// deepest matching source so callers can detect any existing coverage.
func (c *Config) SourceCoveringDir(dir string, env Env) (key string, ok bool) {
	if c == nil || dir == "" {
		return "", false
	}
	dir = realPath(dir, env)
	best, depth := "", -1
	for i := range c.Sources {
		src := &c.Sources[i]
		if src.Cwd == "" {
			continue
		}
		cwd := realPath(src.Cwd, env)
		if !within(dir, cwd) {
			continue
		}
		if d := len(cwd); d > depth {
			best, depth = src.Kind+":"+src.Name, d
		}
	}
	if depth < 0 {
		return "", false
	}
	return best, true
}

// realPath cleans p and resolves its symbolic links when env can. When p
// does not exist, its deepest existing ancestor is resolved and the rest is
// kept, so a missing subdirectory still matches a symlinked checkout.
func realPath(p string, env Env) string {
	p = filepath.Clean(p)
	if env.EvalSymlinks == nil {
		return p
	}
	rest := ""
	for dir := p; ; {
		if resolved, err := env.EvalSymlinks(dir); err == nil {
			return filepath.Join(resolved, rest)
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return p
		}
		rest = filepath.Join(filepath.Base(dir), rest)
		dir = parent
	}
}

// within reports whether dir is root or below it. Both are clean.
func within(dir, root string) bool {
	rel, err := filepath.Rel(root, dir)
	if err != nil {
		return false
	}
	return rel == "." || (rel != ".." && !strings.HasPrefix(rel, ".."+string(filepath.Separator)))
}
