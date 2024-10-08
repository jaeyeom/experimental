package main

import (
	"os"
	"os/exec"
	"path/filepath"

	"github.com/jaeyeom/sugo/errors/must"
)

func GenericRule(name string, cmd *exec.Cmd, opts ...RuleOpts) *Rule {
	return ApplyOpts(&Rule{
		Name: name,
		Build: func() error {
			return Run(cmd)
		},
	}, opts)
}

func PackageInstallRule(name string) *Rule {
	return &Rule{
		Name: name,
		Check: func() bool {
			cmd := exec.Command(name)
			return cmd.Err == nil
		},
		Build: pkgMans.Install(name),
	}
}

func DirRule(name string, dir string) *Rule {
	if name == "" {
		name = dir
	}
	return &Rule{
		Name:  name,
		Check: CheckDir(dir),
		Build: func() error {
			return os.MkdirAll(dir, 0755)
		},
	}
}

func ParentDirRule(name string, dir string) *Rule {
	return DirRule(name, filepath.Dir(dir))
}

func GitCloneRule(name string, repo string, targetDir string) *Rule {
	if name == "" {
		name = targetDir
	}
	return &Rule{
		Name:  name,
		Check: CheckDir(targetDir),
		Build: func() error {
			cmd := exec.Command("git", "clone", repo, targetDir)
			return Run(cmd)
		},
		Deps: []*Rule{
			Ref("git"),
			ParentDirRule("", targetDir),
		},
	}
}

func GoPath() string {
	if os.Getenv("GOPATH") != "" {
		return os.Getenv("GOPATH")
	}
	return filepath.Join(must.String(os.UserHomeDir()), "go")
}

func GoInstallRule(name string, pkg string, opts ...RuleOpts) *Rule {
	r := &Rule{
		Name:  name,
		Check: CheckDir(filepath.Join(GoPath(), "bin", name)),
		Build: func() error {
			cmd := exec.Command("go", "install", pkg)
			return Run(cmd)
		},
		Deps: []*Rule{
			Ref("go"),
		},
	}
	for _, opt := range opts {
		opt(r)
	}
	return r
}
