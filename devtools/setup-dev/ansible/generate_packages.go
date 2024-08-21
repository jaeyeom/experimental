// Binary generate_packages generates Ansible yaml files for the package
// installations.
package main

import (
	"os"
	"strings"
	"text/template"
)

var packagesTemplate = `---
- name: Ensure {{.Command}} is present
  hosts: all
  tasks:
    - name: Include guard for {{.Command}} playbook
      block:
        - name: Stop early if the {{.Command}} playbook is already included
          meta: end_play
          when: {{.Command}}_playbook_imported is defined
        - name: Ensure the {{.Command}} playbook is not included
          set_fact:
            {{.Command}}_playbook_imported: true
          when: {{.Command}}_playbook_imported is not defined

    - name: Ensure {{.Command}} is present on non-Termux systems
      package:
        name: {{.DebianPkgName}}
        state: present
      when: ansible_env.TERMUX_VERSION is not defined
      become: yes

    - name: Ensure {{.Command}} is present on Termux
      block:
        - name: Check if {{.Command}} is installed
          shell: command -v {{.Command}}
          changed_when: False
      rescue:
        - name: Install {{.Command}} on Termux
          command: pkg install -y {{.TermuxPkgName}}
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
	{Command: "jq"},
	{Command: "keychain"},
	{Command: "ssh", debianPkgName: "openssh-client", termuxPkgName: "openssh"},
	{Command: "sshpass"},
}

type GoInstall struct {
	Command string
	PkgPath string
}

func (g GoInstall) CommandID() string {
	// Replace dash to underscore.
	return strings.ReplaceAll(g.Command, "-", "_")
}

var goInstallTemplate = `---
- import_playbook: setup-user-go-bin-directory.yml

- name: Ensure {{.Command}} is present
  hosts: all
  tasks:
    - name: Include guard for {{.Command}} playbook
      block:
        - name: Stop early if the {{.Command}} playbook is already included
          meta: end_play
          when: {{.Command}}_playbook_imported is defined
        - name: Ensure the {{.Command}} playbook is not included
          set_fact:
            {{.Command}}_playbook_imported: true
          when: {{.Command}}_playbook_imported is not defined

    - name: Check if {{.Command}} is installed
      shell: go version -m $(command -v {{.Command}}) | grep '^\s*mod\s'
      register: {{.CommandID}}_installed
      ignore_errors: yes
      changed_when: False

    - name: Extract {{.Command}} version
      block:
        - name: Set {{.Command}} facts
          set_fact:
            {{.CommandID}}_module_path: "{{"{{"}} {{.CommandID}}_installed.stdout.split()[1] {{"}}"}}"
            {{.CommandID}}_module_version: "{{"{{"}} {{.CommandID}}_installed.stdout.split()[2] {{"}}"}}"
        - name: Determine the latest {{.Command}} version
          command: go list -m -f "{{"{{"}} '{{"{{"}}' {{"}}"}}.Version {{"{{"}} '{{"}}"}}' {{"}}"}}" "{{"{{"}} {{.CommandID}}_module_path {{"}}"}}@latest"
          register: {{.CommandID}}_latest
          ignore_errors: yes
          changed_when: False

        - name: Debug module path and version
          debug:
            msg: "{{"{{"}} {{.CommandID}}_module_path {{"}}"}} {{"{{"}} {{.CommandID}}_module_version {{"}}"}} => {{"{{"}} {{.CommandID}}_latest.stdout {{"}}"}}"
      rescue:
        - name: Clear {{.Command}} facts
          set_fact:
            {{.CommandID}}_module_path: ""
            {{.CommandID}}_module_version: ""

    - name: Upgrade {{.Command}}
      command: go install {{.PkgPath}}
      when: {{.CommandID}}_module_version is not defined or {{.CommandID}}_module_version == "" or {{.CommandID}}_module_version != {{.CommandID}}_latest.stdout
`

var gopkgs = []GoInstall{
	{"godoc", "golang.org/x/tools/cmd/godoc@latest"},
	{"goimports", "golang.org/x/tools/cmd/goimports@latest"},
	{"gorename", "golang.org/x/tools/cmd/gorename@latest"},
	{"guru", "golang.org/x/tools/cmd/guru@latest"},
	{"gotests", "github.com/cweill/gotests/...@latest"},
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
