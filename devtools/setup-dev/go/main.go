package main

import (
	"log"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/jaeyeom/sugo/errors/must"
)

var configDir = must.String(os.UserConfigDir())

var emacsDir = filepath.Join(configDir, "emacs")

var pkgMans = NewPackageManagers()

var ruleGraph = RuleGraph{
	Rules: map[string]*Rule{
		"all": &Rule{
			Name: "all",
			Deps: []*Rule{Ref("spacemacs")},
		},
		"config-dir": DirRule("config-dir", configDir),
		"git":        PackageInstallRule("git"),
		"python":     PackageInstallRule("python"),
		"python3":    Ref("python"),
		"go":         PackageInstallRule("go"),
		"emacs": &Rule{
			Name:  "emacs",
			Check: CheckEmacsVersion("29.4"),
			Build: pkgMans.Install("emacs"),
		},
		"git-clone-spacemacs": GitCloneRule(
			"git-clone-spacemacs",
			"https://github.com/syl20bnr/spacemacs",
			emacsDir,
		),
		"spacemacs": &Rule{
			Name: "spacemacs",
			Deps: []*Rule{
				Ref("emacs"),
				Ref("git-clone-spacemacs"),
			},
		},
		"godoc": GoInstallRule(
			"godoc",
			"golang.org/x/tools/cmd/godoc@latest",
		),
		"goimports": GoInstallRule(
			"goimports",
			"golang.org/x/tools/cmd/goimports@latest",
		),
		"gorename": GoInstallRule(
			"gorename",
			"golang.org/x/tools/cmd/gorename@latest",
		),
		"guru": GoInstallRule(
			"guru",
			"golang.org/x/tools/cmd/guru@latest",
		),
		"gotests": GoInstallRule(
			"gotests",
			"github.com/cweill/gotests/gotest@latest",
		),
		"fillstruct": GoInstallRule(
			"fillstruct",
			"github.com/davidrjenni/reftools/cmd/fillstruct@latest",
		),
		"gomodifytags": GoInstallRule(
			"gomodifytags",
			"github.com/fatih/gomodifytags@latest",
		),
		"godoctor": GoInstallRule(
			"godoctor",
			"github.com/godoctor/godoctor@latest",
		),
		"gopkgs": GoInstallRule(
			"gopkgs",
			"github.com/uudashr/gopkgs/v2/cmd/gopkgs@latest",
		),
		"impl": GoInstallRule(
			"impl",
			"github.com/josharian/impl@latest",
		),
		"godef": GoInstallRule(
			"godef",
			"github.com/rogpeppe/godef@latest",
		),
		"image2ascii": GoInstallRule(
			"image2ascii",
			"github.com/qeesung/image2ascii@latest",
		),
		"protoc-gen-go": GoInstallRule(
			"protoc-gen-go",
			"google.golang.org/protobuf/cmd/protoc-gen-go@latest",
			Dep(Ref("protoc")),
		),
		"protoc-gen-go-grpc": GoInstallRule(
			"protoc-gen-go-grpc",
			"google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest",
			Dep(Ref("protoc")),
		),
		"spacemacs-go": &Rule{
			Name: "spacemacs-go",
			Deps: []*Rule{
				Ref("spacemacs"),
				Ref("go"),
				Ref("godoc"),
				Ref("goimports"),
				Ref("gorename"),
				Ref("guru"),
				Ref("gotests"),
				Ref("fillstruct"),
				Ref("gomodifytags"),
				Ref("godoctor"),
				Ref("gopkgs"),
				Ref("impl"),
				Ref("godef"),
			},
		},
		"hello": &Rule{
			Name: "hello",
			Build: func() error {
				cmd := exec.Command("echo", "Hello, world!")
				return Run(cmd)
			},
		},
	},
}

func main() {
	// First upgrade the packages.
	err := pkgMans.UpdateAll()
	if err != nil {
		log.Fatal(err)
	}
	args := os.Args[1:]
	for _, arg := range args {
		if err := ruleGraph.Ensure(Ref(arg)); err != nil {
			log.Fatal(err)
		}
	}
}
