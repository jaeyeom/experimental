package procinfo

import (
	"context"
	"errors"
	"testing"
)

func TestMatchNameIsExactOrAGlobOnNameOrArgv0(t *testing.T) {
	bazel := Process{Name: "java", Argv: []string{"/usr/local/bin/bazel", "build"}}
	if !MatchName([]string{"bazel"}, bazel) {
		t.Fatal("argv[0] base bazel did not match")
	}
	if MatchName([]string{"baz"}, bazel) {
		t.Fatal("a part of the name matched")
	}
	if !MatchName([]string{"ja*"}, bazel) {
		t.Fatal("glob on the name did not match")
	}
}

func TestInside(t *testing.T) {
	for _, tt := range []struct {
		path, dir string
		want      bool
	}{
		{"/src/repo", "/src/repo", true},
		{"/src/repo/sub/", "/src/repo", true},
		{"/src/repo2", "/src/repo", false},
		{"/src", "/src/repo", false},
		{"", "/src/repo", false},
		{"/src/repo", "", false},
	} {
		if got := Inside(tt.path, tt.dir); got != tt.want {
			t.Errorf("Inside(%q, %q) = %v, want %v", tt.path, tt.dir, got, tt.want)
		}
	}
}

func TestBuildToolsFindsAToolWhoseCwdIsInsideDir(t *testing.T) {
	procs := []Process{
		{PID: 10, Name: "make", Argv: []string{"make", "check"}},
		{PID: 11, Name: "bazel", Argv: []string{"bazel", "--workspace_directory=/x", "server"}},
		{PID: 12, Name: "go", Argv: []string{"go", "test"}},
		{PID: 13, Name: "vim", Argv: []string{"vim"}},
		{PID: 14, Name: "go", Argv: []string{"go", "build"}},
	}
	cwds := map[int]string{10: "/elsewhere", 11: "/src/repo", 12: "/src/repo/pkg", 13: "/src/repo", 14: "/src/repo"}
	cwd := func(_ context.Context, pid int) (string, error) {
		if c, ok := cwds[pid]; ok {
			return c, nil
		}
		return "", errors.New("no cwd")
	}
	got := BuildTools(context.Background(), procs, DefaultBuildTools, DefaultBuildToolIgnore, map[int]struct{}{14: {}}, "/src/repo", cwd)
	if len(got) != 1 || got[0].PID != 12 || got[0].Cwd != "/src/repo/pkg" {
		t.Fatalf("BuildTools = %+v, want only pid 12 with its cwd", got)
	}
}

func TestAncestors(t *testing.T) {
	procs := []Process{{PID: 1, PPID: 0}, {PID: 5, PPID: 1}, {PID: 9, PPID: 5}, {PID: 7, PPID: 1}}
	got := Ancestors(procs, 9)
	for _, pid := range []int{9, 5, 1} {
		if _, ok := got[pid]; !ok {
			t.Fatalf("Ancestors(9) = %v, missing %d", got, pid)
		}
	}
	if _, ok := got[7]; ok {
		t.Fatalf("Ancestors(9) = %v, has 7", got)
	}
}

func TestParseLsofCwd(t *testing.T) {
	if got := ParseLsofCwd("p4242\nfcwd\nn/src/repo dir\n"); got != "/src/repo dir" {
		t.Fatalf("ParseLsofCwd() = %q", got)
	}
	if got := ParseLsofCwd("p4242\n"); got != "" {
		t.Fatalf("ParseLsofCwd() = %q, want empty", got)
	}
}
