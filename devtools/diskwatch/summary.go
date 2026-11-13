package diskwatch

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/bmatcuk/doublestar/v4"
)

// Entry is one reported path and the bytes counted under it.
type Entry struct {
	Path  string `json:"path"`
	Bytes int64  `json:"bytes"`
}

// Summary is a best-effort usage report. Partial means the walk stopped
// early. That is not a failure.
type Summary struct {
	Entries []Entry `json:"entries"`
	Partial bool    `json:"partial"`
}

// Text is the summary as lines of byte count and path, largest first.
func (s Summary) Text() string {
	var b strings.Builder
	for _, entry := range s.Entries {
		fmt.Fprintf(&b, "%d\t%s\n", entry.Bytes, entry.Path)
	}
	if s.Partial {
		b.WriteString("partial\n")
	}
	return b.String()
}

type nodeInfo struct {
	isDir bool
	size  int64
	dev   uint64
}

type lister interface {
	lstat(path string) (nodeInfo, error)
	names(path string) ([]string, error)
}

type osLister struct{}

func (osLister) names(path string) ([]string, error) {
	entries, err := os.ReadDir(path)
	if err != nil {
		return nil, fmt.Errorf("read dir: %w", err)
	}
	names := make([]string, len(entries))
	for i, entry := range entries {
		names[i] = entry.Name()
	}
	return names, nil
}

// summarize measures each glob match. The walk stops at deadline, does not
// follow symlinks, and does not enter a different filesystem. Results are
// largest first and limited to top.
func summarize(ctx context.Context, patterns []string, home string, top int, deadline time.Time, now func() time.Time, fs lister) Summary {
	if fs == nil {
		fs = osLister{}
	}
	if now == nil {
		now = time.Now
	}
	var summary Summary
	for _, pattern := range patterns {
		if ctx.Err() != nil || now().After(deadline) {
			summary.Partial = true
			break
		}
		for _, match := range expandPattern(pattern, home, fs) {
			info, err := fs.lstat(match)
			if err != nil {
				continue
			}
			bytes, partial := walkPath(ctx, fs, match, info, deadline, now)
			summary.Entries = append(summary.Entries, Entry{Path: match, Bytes: bytes})
			if partial {
				summary.Partial = true
			}
		}
	}
	sort.Slice(summary.Entries, func(i, j int) bool {
		if summary.Entries[i].Bytes == summary.Entries[j].Bytes {
			return summary.Entries[i].Path < summary.Entries[j].Path
		}
		return summary.Entries[i].Bytes > summary.Entries[j].Bytes
	})
	if top > 0 && len(summary.Entries) > top {
		summary.Entries = summary.Entries[:top]
	}
	return summary
}

func expandPattern(pattern, home string, fs lister) []string {
	expanded := expandHome(pattern, home)
	if !strings.ContainsAny(expanded, "*?[") {
		if _, err := fs.lstat(expanded); err == nil {
			return []string{expanded}
		}
		return nil
	}
	matches, err := doublestar.FilepathGlob(expanded)
	if err != nil {
		return nil
	}
	return matches
}

func expandHome(path, home string) string {
	if home == "" {
		return path
	}
	if path == "~" {
		return home
	}
	if strings.HasPrefix(path, "~/") {
		return filepath.Join(home, path[2:])
	}
	return path
}

func walkPath(ctx context.Context, fs lister, path string, info nodeInfo, deadline time.Time, now func() time.Time) (int64, bool) {
	partial := false
	if !info.isDir {
		return info.size, false
	}
	return walkDir(ctx, fs, path, info.dev, deadline, now, &partial), partial
}

func walkDir(ctx context.Context, fs lister, path string, rootDev uint64, deadline time.Time, now func() time.Time, partial *bool) int64 {
	names, err := fs.names(path)
	if err != nil {
		*partial = true
		return 0
	}
	var sum int64
	for _, name := range names {
		if ctx.Err() != nil || now().After(deadline) {
			*partial = true
			return sum
		}
		child := filepath.Join(path, name)
		info, err := fs.lstat(child)
		if err != nil {
			continue
		}
		if info.dev != rootDev {
			continue
		}
		if info.isDir {
			sum += walkDir(ctx, fs, child, rootDev, deadline, now, partial)
			continue
		}
		sum += info.size
	}
	return sum
}
