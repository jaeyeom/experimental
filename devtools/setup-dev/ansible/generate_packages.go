// Binary generate_packages generates Ansible yaml files for the package
// installations.
package main

import (
	"os"
	"text/template"
)

var packagesTemplate = `---
- name: Ensure {{.Command}} is present
  hosts: all
  tasks:
    - name: Ensure {{.Command}} is present on non-Termux systems
      package:
        name: {{.DebianPkgName}}
        state: present
      when: ansible_env.TERMUX_VERSION is not defined
      become: yes

    - name: Ensure {{.Command}} is present on Termux
      shell: command -v {{.Command}} || pkg install -y {{.TermuxPkgName}}
      when: ansible_env.TERMUX_VERSION is defined
`

type PackageData struct {
	Command       string
	debianPkgName string
	termuxPkgName string
}

func (p PackageData) DebianPkgName() string {
	if p.debianPkgName != "" {
		return p.debianPkgName
	}
	return p.Command
}

func (p PackageData) TermuxPkgName() string {
	if p.termuxPkgName != "" {
		return p.termuxPkgName
	}
	return p.Command
}

var packages = []PackageData{
	{Command: "curl"},
	{Command: "emacs"},
	{Command: "git"},
	{Command: "go", debianPkgName: "golang-go", termuxPkgName: "golang"},
	{Command: "jq"},
	{Command: "ssh", debianPkgName: "openssh-client", termuxPkgName: "openssh"},
}

type GoInstall struct {
	Command string
	PkgPath string
}

var goInstallTemplate = `---
- import_playbook: "go.yml"

- name: Ensure {{.Command}} is present
  hosts: all
  tasks:
    - name: Ensure {{.Command}} is present
      command: go install {{.PkgPath}}
`

var gopkgs = []GoInstall{
	{"godoc", "golang.org/x/tools/cmd/godoc@latest"},
	{"goimports", "golang.org/x/tools/cmd/goimports@latest"},
	{"gorename", "golang.org/x/tools/cmd/gorename@latest"},
	{"guru", "golang.org/x/tools/cmd/guru@latest"},
	{"gotests", "github.com/cweill/gotests/gotest@latest"},
	{"fillstruct", "github.com/davidrjenni/reftools/cmd/fillstruct@latest"},
	{"gomodifytags", "github.com/fatih/gomodifytags@latest"},
	{"godoctor", "github.com/godoctor/godoctor@latest"},
	{"gopkgs", "github.com/uudashr/gopkgs/v2/cmd/gopkgs@latest"},
	{"impl", "github.com/josharian/impl@latest"},
	{"godef", "github.com/rogpeppe/godef@latest"},
	{"image2ascii", "github.com/qeesung/image2ascii@latest"},
	{"protoc-gen-go", "google.golang.org/protobuf/cmd/protoc-gen-go@latest"},
	{"protoc-gen-go-grpc", "google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest"},
}

func main() {
	pkgTmpl, err := template.New("packages").Parse(packagesTemplate)
	if err != nil {
		panic(err)
	}

	for _, pkg := range packages {
		f, err := os.Create(pkg.Command + ".yml")
		if err != nil {
			panic(err)
		}
		defer f.Close()
		err = pkgTmpl.Execute(f, pkg)
		if err != nil {
			panic(err)
		}
	}
	goInstallTmpl, err := template.New("go_install").Parse(goInstallTemplate)
	if err != nil {
		panic(err)
	}
	for _, pkg := range gopkgs {

		f, err := os.Create(pkg.Command + ".yml")
		if err != nil {
			panic(err)
		}
		defer f.Close()
		err = goInstallTmpl.Execute(f, pkg)
		if err != nil {
			panic(err)
		}
	}
}
