// Binary generate_packages generates Ansible yaml files for the package
// installations.
package main

import (
	"os"
	"strings"
	"text/template"
)

type PackageData struct {
	command       string
	debianPkgName string
	UbuntuPPA     string
	termuxPkgName string
	brewPkgName   string
	brewTap       string
	brewOptions   []string
	Imports       []string
	Suffix        string
}

func (p PackageData) Command() string {
	return p.command
}

func (g PackageData) CommandID() string {
	// Replace dash to underscore.
	return strings.ReplaceAll(g.command, "-", "_")
}

func (p PackageData) DebianPkgName() string {
	if p.debianPkgName != "" {
		return p.debianPkgName
	}
	return p.command
}

func (p PackageData) TermuxPkgName() string {
	if p.termuxPkgName != "" {
		return p.termuxPkgName
	}
	return p.command
}

func (p PackageData) BrewPkgName() string {
	if p.brewPkgName != "" {
		return p.brewPkgName
	}
	return p.command
}

func (p PackageData) BrewTap() string {
	return p.brewTap
}

func (p PackageData) BrewOptions() []string {
	return p.brewOptions
}

type GoInstall struct {
	command string
	PkgPath string
	Imports []string
}

func (g GoInstall) Command() string {
	return g.command
}

func (g GoInstall) CommandID() string {
	// Replace dash to underscore.
	return strings.ReplaceAll(g.command, "-", "_")
}

type PipInstall struct {
	command string
	pkgName string
	Imports []string
}

func (p PipInstall) Command() string {
	return p.command
}

func (p PipInstall) CommandID() string {
	// Replace dash to underscore.
	return strings.ReplaceAll(p.command, "-", "_")
}

func (p PipInstall) PkgName() string {
	if p.pkgName != "" {
		return p.pkgName
	}

	return p.command
}

type CargoInstall struct {
	command string
	pkgName string
	Imports []string
}

func (c CargoInstall) Command() string {
	return c.command
}

func (c CargoInstall) CommandID() string {
	// Replace dash to underscore.
	return strings.ReplaceAll(c.command, "-", "_")
}

func (c CargoInstall) PkgName() string {
	if c.pkgName != "" {
		return c.pkgName
	}
	return c.command
}

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
          when: {{.CommandID}}_playbook_imported is defined
        - name: Ensure the {{.Command}} playbook is not included
          set_fact:
            {{.CommandID}}_playbook_imported: true
          when: {{.CommandID}}_playbook_imported is not defined
{{ if .UbuntuPPA }}
    - name: Ensure {{.Command}} PPA is present in Ubuntu
      apt_repository:
        repo: "{{.UbuntuPPA}}"
        state: present
        update_cache: yes
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin" and ansible_facts['distribution'] == "Ubuntu"
      become: yes

    - name: Ensure bookworm-backports is added to sources.list.d
      ansible.builtin.apt_repository:
        repo: "deb http://deb.debian.org/debian bookworm-backports main contrib non-free non-free-firmware"
        state: present
        update_cache: yes
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin" and ansible_facts['distribution'] == "Debian" and ansible_facts['distribution_major_version'] == "12"
      become: yes
{{ end }}{{ if .BrewTap }}
    - name: Tap {{.BrewTap}} for {{.Command}}
      community.general.homebrew_tap:
        name: {{.BrewTap}}
        state: present
      when: ansible_facts['os_family'] == "Darwin"
{{ end }}
    - name: Ensure {{.Command}} is present on MacOS
      community.general.homebrew:
        name: {{.BrewPkgName}}
        state: present{{ if .BrewOptions }}
        install_options:{{ range .BrewOptions }}
          - {{ . }}{{ end }}{{ end }}
      when: ansible_facts['os_family'] == "Darwin"

    - name: Ensure {{.Command}} is present on non-Termux, non-MacOS systems
      package:
        name: {{.DebianPkgName}}
        state: present
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin"
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

var cargoInstallTemplate = `---
- import_playbook: setup-cargo.yml
{{- if ne .Command "cargo-install-update" }}
- import_playbook: cargo-install-update.yml
{{- end }}
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
      shell: command -v {{.Command}}
      register: {{.CommandID}}_installed
      ignore_errors: yes
      changed_when: False

    - name: Install {{.Command}} using Cargo
      command: cargo install {{.PkgName}}
      when: {{.CommandID}}_installed.rc != 0

    - name: Update {{.Command}} to latest version
      command: cargo install-update {{.PkgName}}
      register: {{.CommandID}}_update_result
      changed_when: "{{.CommandID}}_update_result.stdout is search('Overall updated [1-9]')"
      when: {{.CommandID}}_installed.rc == 0
`

type Commander interface {
	Command() string
}

var packages = []PackageData{
	{command: "ag", debianPkgName: "silversearcher-ag", termuxPkgName: "silversearcher-ag", brewPkgName: "the_silver_searcher"},
	{command: "buf"},
	{command: "clang-format"},
	{command: "curl"},
	{command: "dart"},
	{command: "emacs", UbuntuPPA: "ppa:ubuntuhandbook1/emacs", brewPkgName: "emacs-plus", brewTap: "d12frosted/emacs-plus", brewOptions: []string{"with-native-comp", "with-dbus", "with-imagemagick"}},
	{command: "gh"},
	{command: "git"},
	{command: "gpg", brewPkgName: "gnupg"},
	{command: "gpg-agent", Imports: []string{"gpg"}, brewPkgName: "gnupg"},
	{command: "grep"},
	{command: "grpcio", debianPkgName: "python3-grpcio", termuxPkgName: "python-grpcio", brewPkgName: "python-grpcio"},
	{command: "htop"},
	{command: "jq"},
	{command: "keychain"},
	{
		command:       "locate",
		debianPkgName: "mlocate",
		termuxPkgName: "mlocate",
		brewPkgName:   "findutils",
		Suffix: `

    - name: Ensure locate DB is up-to-date
      command: updatedb
      become: "{{ 'no' if ansible_env.TERMUX_VERSION is defined else 'yes' }}"`,
	},
	{command: "kotlinc", debianPkgName: "kotlin", termuxPkgName: "kotlin", brewPkgName: "kotlin"},
	{command: "man", brewPkgName: "man-db"},
	{command: "mono", debianPkgName: "mono-devel", termuxPkgName: "mono"},
	{command: "notmuch", debianPkgName: "notmuch", termuxPkgName: "notmuch", Imports: []string{"python3-notmuch2"}},
	{command: "pandoc"},
	{command: "pass"},
	{command: "protoc", debianPkgName: "protobuf-compiler", termuxPkgName: "protobuf", brewPkgName: "protobuf"},
	{command: "psql", debianPkgName: "postgresql-client", termuxPkgName: "postgresql", brewPkgName: "postgresql"},
	{command: "python3-notmuch2", debianPkgName: "python3-notmuch2", termuxPkgName: "notmuch", brewPkgName: "notmuch"},
	{command: "rg", debianPkgName: "ripgrep", termuxPkgName: "ripgrep", brewPkgName: "ripgrep"},
	{command: "sed", brewPkgName: "gsed"},
	{command: "ssh", debianPkgName: "openssh-client", termuxPkgName: "openssh", brewPkgName: "openssh"},
	{command: "sshpass"},
	{command: "tlmgr", debianPkgName: "texlive-lang-korean", termuxPkgName: "texlive-installer", brewPkgName: "mactex"},
	{command: "tmux"},
	{command: "unzip"},
	{command: "zip"},
}

var gopkgs = []GoInstall{
	{"buildifier", "github.com/bazelbuild/buildtools/buildifier@latest", nil},
	{"buildozer", "github.com/bazelbuild/buildtools/buildozer@latest", nil},
	{"fillstruct", "github.com/davidrjenni/reftools/cmd/fillstruct@latest", nil},
	{"godef", "github.com/rogpeppe/godef@latest", nil},
	{"godoc", "golang.org/x/tools/cmd/godoc@latest", nil},
	{"godoctor", "github.com/godoctor/godoctor@latest", nil},
	{"gofumpt", "mvdan.cc/gofumpt@latest", nil},
	{"goimports", "golang.org/x/tools/cmd/goimports@latest", nil},
	{"gomodifytags", "github.com/fatih/gomodifytags@latest", nil},
	{"gopkgs", "github.com/uudashr/gopkgs/v2/cmd/gopkgs@latest", nil},
	{"gopls", "golang.org/x/tools/gopls@latest", nil},
	{"gorename", "golang.org/x/tools/cmd/gorename@latest", nil},
	{"gotests", "github.com/cweill/gotests/...@latest", nil},
	{"grpcui", "github.com/fullstorydev/grpcui/cmd/grpcui@latest", nil},
	{"guru", "golang.org/x/tools/cmd/guru@latest", nil},
	{"hugo", "github.com/gohugoio/hugo@latest", nil},
	{"image2ascii", "github.com/qeesung/image2ascii@latest", nil},
	{"impl", "github.com/josharian/impl@latest", nil},
	{"jira", "github.com/ankitpokhrel/jira-cli/cmd/jira@latest", nil},
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
	{
		"protolint",
		"github.com/yoheimuta/protolint/cmd/protolint@latest",
		nil,
	},
}

var pipPkgs = []PipInstall{
	{command: "protovalidate"},
}

var cargoPkgs = []CargoInstall{
	{command: "cargo-add", pkgName: "cargo-edit"},
	{command: "cargo-install-update", pkgName: "cargo-update"},
	{command: "cargo-outdated"},
	{command: "emacs-lsp-booster"},
}

func generatePackages[T Commander](tmpl *template.Template, pkgs []T) {
	for _, pkg := range pkgs {
		outf, err := os.Create(pkg.Command() + ".yml")
		if err != nil {
			panic(err)
		}
		defer outf.Close()

		err = tmpl.Execute(outf, pkg)
		if err != nil {
			panic(err)
		}
	}
}

func main() {
	tmpl := template.Must(template.New("packages").Parse(packagesTemplate))
	template.Must(tmpl.New("goinstall").Parse(goInstallTemplate))
	template.Must(tmpl.New("pipinstall").Parse(pipInstallTemplate))
	template.Must(tmpl.New("cargoinstall").Parse(cargoInstallTemplate))

	generatePackages(tmpl.Lookup("packages"), packages)
	generatePackages(tmpl.Lookup("goinstall"), gopkgs)
	generatePackages(tmpl.Lookup("pipinstall"), pipPkgs)
	generatePackages(tmpl.Lookup("cargoinstall"), cargoPkgs)
}
