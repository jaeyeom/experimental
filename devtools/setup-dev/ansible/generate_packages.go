// Binary generate_packages generates Ansible yaml files for the package
// installations.
package main

import (
	"os"
	"strings"
	"text/template"
)

var packagesTemplate = `---
{{- range .Imports }}
- import_playbook: {{.}}.yml
{{- end }}
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
      when: ansible_env.TERMUX_VERSION is defined{{.Suffix}}
`

type PackageData struct {
	Command       string
	debianPkgName string
	termuxPkgName string
	Imports       []string
	Suffix        string
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
	{Command: "ag", debianPkgName: "silversearcher-ag", termuxPkgName: "silversearcher-ag"},
	{Command: "buf"},
	{Command: "curl"},
	{Command: "dart"},
	{Command: "emacs"},
	{Command: "gh"},
	{Command: "git"},
	{Command: "grep"},
	{Command: "grpcio", debianPkgName: "python3-grpcio", termuxPkgName: "python-grpcio"},
	{Command: "htop"},
	{Command: "jq"},
	{Command: "keychain"},
	{
		Command:       "locate",
		debianPkgName: "mlocate",
		termuxPkgName: "mlocate",
		Suffix: `

    - name: Ensure locate DB is up-to-date
      command: updatedb
      become: "{{ 'no' if ansible_env.TERMUX_VERSION is defined else 'yes' }}"`,
	},
	{Command: "kotlinc", debianPkgName: "kotlin", termuxPkgName: "kotlin"},
	{Command: "man"},
	{Command: "mono", debianPkgName: "mono-devel", termuxPkgName: "mono"},
	{Command: "notmuch", debianPkgName: "notmuch", termuxPkgName: "notmuch", Imports: []string{"python3-notmuch2"}},
	{Command: "python3-notmuch2", debianPkgName: "python3-notmuch2", termuxPkgName: "notmuch"},
	{Command: "protoc", debianPkgName: "protobuf-compiler", termuxPkgName: "protobuf"},
	{Command: "rg", debianPkgName: "ripgrep", termuxPkgName: "ripgrep"},
	{Command: "sed"},
	{Command: "ssh", debianPkgName: "openssh-client", termuxPkgName: "openssh"},
	{Command: "sshpass"},
	{Command: "tmux"},
	{Command: "unzip"},
	{Command: "zip"},
}

type GoInstall struct {
	Command string
	PkgPath string
	Imports []string
}

func (g GoInstall) CommandID() string {
	// Replace dash to underscore.
	return strings.ReplaceAll(g.Command, "-", "_")
}

var goInstallTemplate = `---
- import_playbook: setup-user-go-bin-directory.yml
{{- range .Imports }}
- import_playbook: {{.}}.yml
{{- end }}

- name: Ensure {{.Command}} is present
  hosts: all
  tasks:
    - name: Include guard for {{.Command}} playbook
      block:
        - name: Stop early if the {{.Command}} playbook is already included
          meta: end_play
          when: {{.CommandID}}_playbook_imported is defined
        - name: Ensure the {{.Command}} playbook is not included
          set_fact:
            {{.CommandID}}_playbook_imported: true
          when: {{.CommandID}}_playbook_imported is not defined

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
	{"fillstruct", "github.com/davidrjenni/reftools/cmd/fillstruct@latest", nil},
	{"godef", "github.com/rogpeppe/godef@latest", nil},
	{"godoc", "golang.org/x/tools/cmd/godoc@latest", nil},
	{"godoctor", "github.com/godoctor/godoctor@latest", nil},
	{"goimports", "golang.org/x/tools/cmd/goimports@latest", nil},
	{"gomodifytags", "github.com/fatih/gomodifytags@latest", nil},
	{"gopkgs", "github.com/uudashr/gopkgs/v2/cmd/gopkgs@latest", nil},
	{"gopls", "golang.org/x/tools/gopls@latest", nil},
	{"gorename", "golang.org/x/tools/cmd/gorename@latest", nil},
	{"gotests", "github.com/cweill/gotests/...@latest", nil},
	{"guru", "golang.org/x/tools/cmd/guru@latest", nil},
	{"image2ascii", "github.com/qeesung/image2ascii@latest", nil},
	{"impl", "github.com/josharian/impl@latest", nil},
	{
		"protoc-gen-go",
		"google.golang.org/protobuf/cmd/protoc-gen-go@latest",
		[]string{"protoc"},
	},
	{
		"protoc-gen-go-grpc",
		"google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest",
		[]string{"protoc"},
	},
}

type PipInstall struct {
	Command string
	pkgName string
	Imports []string
}

func (p PipInstall) CommandID() string {
	// Replace dash to underscore.
	return strings.ReplaceAll(p.Command, "-", "_")
}

func (p PipInstall) PkgName() string {
	if p.pkgName != "" {
		return p.pkgName
	}

	return p.Command
}

var pipInstallTemplate = `---
{{- range .Imports }}
- import_playbook: {{.}}.yml
{{- end }}

- name: Ensure {{.Command}} is present
  hosts: all
  tasks:
    - name: Include guard for {{.Command}} playbook
      block:
        - name: Stop early if the {{.Command}} playbook is already included
          meta: end_play
          when: {{.CommandID}}_playbook_imported is defined
        - name: Ensure the {{.Command}} playbook is not included
          set_fact:
            {{.CommandID}}_playbook_imported: true
          when: {{.CommandID}}_playbook_imported is not defined

    - name: Ensure if {{.Command}} is installed
      ansible.builtin.pip:
        name: {{.PkgName}}
        state: latest
`

var pipPkgs = []PipInstall{
	{Command: "protovalidate"},
}

func generatePackages() {
	pkgTmpl, err := template.New("packages").Parse(packagesTemplate)
	if err != nil {
		panic(err)
	}

	for _, pkg := range packages {
		outf, err := os.Create(pkg.Command + ".yml")
		if err != nil {
			panic(err)
		}
		defer outf.Close()

		err = pkgTmpl.Execute(outf, pkg)
		if err != nil {
			panic(err)
		}
	}
}

func generateGoInstall() {
	goInstallTmpl, err := template.New("go_install").Parse(goInstallTemplate)
	if err != nil {
		panic(err)
	}

	for _, pkg := range gopkgs {
		outf, err := os.Create(pkg.Command + ".yml")
		if err != nil {
			panic(err)
		}
		defer outf.Close()

		err = goInstallTmpl.Execute(outf, pkg)
		if err != nil {
			panic(err)
		}
	}
}

func generatePipInstall() {
	pipInstallTmpl, err := template.New("pip_install").Parse(pipInstallTemplate)
	if err != nil {
		panic(err)
	}

	for _, pkg := range pipPkgs {
		outf, err := os.Create(pkg.Command + ".yml")
		if err != nil {
			panic(err)
		}
		defer outf.Close()

		err = pipInstallTmpl.Execute(outf, pkg)
		if err != nil {
			panic(err)
		}
	}
}

func main() {
	generatePackages()
	generateGoInstall()
	generatePipInstall()
}
