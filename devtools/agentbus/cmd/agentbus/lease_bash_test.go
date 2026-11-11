package main

import (
	"reflect"
	"testing"
)

func TestGitChangeDirs(t *testing.T) {
	const cwd, home = "/start", "/home/u"
	for _, tt := range []struct {
		cmd  string
		want []string
	}{
		// Read-only git and other programs pass.
		{"git status", nil},
		{"git log --oneline -5", nil},
		{"git diff HEAD~1 && git show HEAD", nil},
		{"git fetch origin; git rev-parse HEAD", nil},
		{"git stash list", nil},
		{"git stash show -p", nil},
		{"git tag", nil},
		{"git tag -l 'v*'", nil},
		{"git branch -a", nil},
		{"git branch -vv --list", nil},
		{"git worktree list", nil},
		{"echo git commit", nil},
		{"ls; make test", nil},
		{"", nil},
		// State-changing git, in the starting directory.
		{"git commit -m x", []string{"/start"}},
		{"git add -A && git commit -m 'a && b'", []string{"/start"}},
		{"git checkout main", []string{"/start"}},
		{"git switch -c topic", []string{"/start"}},
		{"git merge topic", []string{"/start"}},
		{"git rebase main", []string{"/start"}},
		{"git reset --hard", []string{"/start"}},
		{"git stash", []string{"/start"}},
		{"git stash pop", []string{"/start"}},
		{"git pull --rebase", []string{"/start"}},
		{"git cherry-pick abc", []string{"/start"}},
		{"git revert abc", []string{"/start"}},
		{"git am < p.patch", []string{"/start"}},
		{"git apply p.patch", []string{"/start"}},
		{"git clean -fdx", []string{"/start"}},
		{"git restore a.go", []string{"/start"}},
		{"git rm a.go", []string{"/start"}},
		{"git mv a.go b.go", []string{"/start"}},
		{"git tag v1", []string{"/start"}},
		{"git branch -d old", []string{"/start"}},
		{"git branch -D old", []string{"/start"}},
		{"git branch -m new", []string{"/start"}},
		{"git branch --delete old", []string{"/start"}},
		{"git worktree add ../wt", []string{"/start"}},
		// Global options before the subcommand.
		{"git -c user.name=x commit -m y", []string{"/start"}},
		{"git --no-pager -c a=b status", nil},
		{"/usr/bin/git commit", []string{"/start"}},
		// Prefixes.
		{"GIT_EDITOR=true git rebase --continue", []string{"/start"}},
		{"env GIT_EDITOR=true git commit", []string{"/start"}},
		{"env -u FOO git commit", []string{"/start"}},
		{"command git commit", []string{"/start"}},
		{"nohup git pull &", []string{"/start"}},
		{"time git commit", []string{"/start"}},
		// Separators.
		{"make && git commit -m x", []string{"/start"}},
		{"false || git reset --hard", []string{"/start"}},
		{"git log | head; git commit", []string{"/start"}},
		{"echo $(git commit -m x)", []string{"/start"}},
		{"(git commit -m x)", []string{"/start"}},
		{"echo hi\ngit commit -m x", []string{"/start"}},
		{"bash -c 'git commit -m x'", []string{"/start"}},
		// cd and git -C move where git runs; the starting directory
		// still counts, since a cd in a subshell or pipe does not last.
		{"cd /repo && git rebase main", []string{"/repo", "/start"}},
		{"cd /repo; git status", nil},
		{"cd sub && git commit", []string{"/start/sub", "/start"}},
		{"cd ~/src/repo && git commit", []string{"/home/u/src/repo", "/start"}},
		{"cd && git commit", []string{"/home/u", "/start"}},
		{"cd /a && cd b && git commit", []string{"/a/b", "/start"}},
		{"cd $REPO && git commit", []string{"/start"}},
		{"pushd /repo && git commit", []string{"/repo", "/start"}},
		{"git -C /repo reset --hard", []string{"/repo"}},
		{"git -C sub commit", []string{"/start/sub"}},
		{"git -C /repo -C sub commit", []string{"/repo/sub"}},
		{"git -C /repo log", nil},
		{"cd /a && git -C sub commit", []string{"/a/sub", "/start/sub"}},
		{"git --work-tree=/repo checkout .", []string{"/repo"}},
		{"git --git-dir /repo/.git reset", []string{"/repo/.git"}},
	} {
		got := gitChangeDirs(tt.cmd, cwd, home)
		if !reflect.DeepEqual(got, tt.want) {
			t.Errorf("gitChangeDirs(%q) = %q, want %q", tt.cmd, got, tt.want)
		}
	}
}
