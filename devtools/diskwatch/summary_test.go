package diskwatch

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"
)

type fakeFS struct {
	nodes     map[string]nodeInfo
	kids      map[string][]string
	stats     int
	nameCalls []string
}

func (f *fakeFS) lstat(path string) (nodeInfo, error) {
	f.stats++
	info, ok := f.nodes[path]
	if !ok {
		return nodeInfo{}, osErrNotExist
	}
	return info, nil
}

func (f *fakeFS) names(path string) ([]string, error) {
	f.nameCalls = append(f.nameCalls, path)
	return f.kids[path], nil
}

type osErr string

func (e osErr) Error() string { return string(e) }

const osErrNotExist osErr = "missing"

func TestSummarySortsAndLimits(t *testing.T) {
	t.Parallel()
	fs := &fakeFS{
		nodes: map[string]nodeInfo{
			"/a": {size: 10, dev: 1},
			"/b": {size: 50, dev: 1},
			"/c": {size: 20, dev: 1},
		},
	}
	got := summarize(context.Background(), []string{"/a", "/b", "/c"}, "", 2, time.Now().Add(time.Hour), time.Now, fs)
	if got.Partial || len(got.Entries) != 2 || got.Entries[0].Path != "/b" || got.Entries[1].Path != "/c" {
		t.Fatalf("summary = %+v", got)
	}
}

func TestSummaryDoesNotCrossFilesystems(t *testing.T) {
	t.Parallel()
	fs := &fakeFS{
		nodes: map[string]nodeInfo{
			"/tree":           {isDir: true, dev: 1},
			"/tree/keep":      {size: 5, dev: 1},
			"/tree/other":     {isDir: true, dev: 2},
			"/tree/other/big": {size: 1000, dev: 2},
		},
		kids: map[string][]string{
			"/tree":       {"keep", "other"},
			"/tree/other": {"big"},
		},
	}
	got := summarize(context.Background(), []string{"/tree"}, "", 10, time.Now().Add(time.Hour), time.Now, fs)
	if len(got.Entries) != 1 || got.Entries[0].Bytes != 5 {
		t.Fatalf("summary = %+v", got)
	}
	for _, path := range fs.nameCalls {
		if path == "/tree/other" {
			t.Fatal("walk entered another filesystem")
		}
	}
}

func TestSummaryHonorsDeadline(t *testing.T) {
	t.Parallel()
	fs := &fakeFS{
		nodes: map[string]nodeInfo{
			"/tree":   {isDir: true, dev: 1},
			"/tree/a": {size: 1, dev: 1},
			"/tree/b": {size: 100, dev: 1},
		},
		kids: map[string][]string{"/tree": {"a", "b"}},
	}
	deadline := time.Date(2026, 11, 13, 0, 0, 0, 0, time.UTC)
	calls := 0
	now := func() time.Time {
		calls++
		if calls > 1 {
			return deadline.Add(time.Second)
		}
		return deadline.Add(-time.Second)
	}
	got := summarize(context.Background(), []string{"/tree"}, "", 10, deadline, now, fs)
	if !got.Partial {
		t.Fatal("expected a partial summary")
	}
	if len(got.Entries) != 1 || got.Entries[0].Bytes != 0 {
		t.Fatalf("stopped too late: %+v", got)
	}
}

func TestSummaryGlobOnDisk(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "one"), []byte("abc"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "two"), []byte("z"), 0o600); err != nil {
		t.Fatal(err)
	}
	got := summarize(context.Background(), []string{filepath.Join(dir, "*")}, "", 10, time.Now().Add(time.Hour), time.Now, nil)
	if len(got.Entries) != 2 || got.Entries[0].Bytes != 3 {
		t.Fatalf("summary = %+v", got)
	}
}
