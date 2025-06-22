// Binary generate_packages generates Ansible yaml files for the package
// installations.
//
// This package provides a unified system for generating Ansible playbooks that
// install development tools across different platforms (macOS, Linux, Termux).
// The core design uses platform-specific installation methods that implement
// the InstallMethod interface.
//
// Key exported types:
//
// PackageData represents a traditional system package with platform-specific
// package names and setup requirements.
//
// InstallMethod is the core interface that all installation methods implement.
// It defines how to render installation tasks and manage dependencies.
//
// PlatformSpecificTool represents a tool that can be installed using different
// methods on different platforms (e.g., brew on macOS, pkg on Termux).
//
// Installation method implementations:
//   - PackageInstallMethod: System package managers (apt, yum, etc.)
//   - BrewInstallMethod: Homebrew packages with taps and options
//   - TermuxPkgInstallMethod: Termux pkg command
//   - PipInstallMethod: Python pip packages
//   - GoInstallMethod: Go packages with version checking
//   - CargoInstallMethod: Rust cargo packages with updates
//   - NpmInstallMethod: Node.js npm packages
//   - UbuntuPkgInstallMethod: Ubuntu-specific packages with PPA support
//   - DebianPkgInstallMethod: Debian-specific packages with backports
//
// Commander interface is implemented by all tool types to provide a unified
// way to get command names for file generation.
package main

import (
	"bufio"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"text/template"
)

// PackageData represents a traditional system package that can be installed
// via platform-specific package managers (apt, yum, pkg, brew, etc.).
// It supports different package names per platform and additional setup
// requirements like PPAs, taps, and installation options.
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

// InstallMethod defines the interface that all installation methods must implement.
// It provides a unified way to handle different installation approaches across platforms.
type InstallMethod interface {
	GetMethodType() string
	GetImports() []string
	RenderInstallTask(command string) string
	RenderSetupTasks(command string) string // For PPA, taps, backports, etc.
}

// PackageInstallMethod handles installation via system package managers
// like apt, yum, dnf, etc. on Linux distributions.
type PackageInstallMethod struct {
	Name string
}

func (p PackageInstallMethod) GetMethodType() string {
	return "package"
}

func (p PackageInstallMethod) GetImports() []string {
	return nil
}

func (p PackageInstallMethod) RenderSetupTasks(command string) string {
	return ""
}

func (p PackageInstallMethod) RenderInstallTask(command string) string {
	return `        - name: Install ` + command + ` on non-Termux, non-MacOS systems
          package:
            name: ` + p.Name + `
            state: present
          become: yes`
}

// BrewInstallMethod handles installation via Homebrew on macOS.
// Supports taps for third-party repositories and installation options.
type BrewInstallMethod struct {
	Name    string
	Tap     string
	Options []string
}

func (b BrewInstallMethod) GetMethodType() string {
	return "brew"
}

func (b BrewInstallMethod) GetImports() []string {
	return nil
}

func (b BrewInstallMethod) RenderSetupTasks(command string) string {
	if b.Tap == "" {
		return ""
	}
	return `    - name: Tap ` + b.Tap + ` for ` + command + `
      community.general.homebrew_tap:
        name: ` + b.Tap + `
        state: present
      when: ansible_facts['os_family'] == "Darwin"

`
}

func (b BrewInstallMethod) RenderInstallTask(command string) string {
	task := `      block:
        - name: Check if ` + command + ` is installed
          shell: command -v ` + command + `
          changed_when: False
      rescue:
        - name: Install ` + command + ` on MacOS
          community.general.homebrew:
            name: ` + b.Name + `
            state: present`
	if len(b.Options) > 0 {
		task += `
            install_options:`
		for _, opt := range b.Options {
			task += `
              - ` + opt
		}
	}
	return task
}

// TermuxPkgInstallMethod handles installation via the pkg command on Termux.
type TermuxPkgInstallMethod struct {
	Name string
}

func (t TermuxPkgInstallMethod) GetMethodType() string {
	return "termux-pkg"
}

func (t TermuxPkgInstallMethod) GetImports() []string {
	return nil
}

func (t TermuxPkgInstallMethod) RenderSetupTasks(command string) string {
	return ""
}

func (t TermuxPkgInstallMethod) RenderInstallTask(command string) string {
	return `      block:
        - name: Check if ` + command + ` is installed
          shell: command -v ` + command + `
          changed_when: False
      rescue:
        - name: Install ` + command + ` on Termux
          command: pkg install -y ` + t.Name
}

// PipInstallMethod handles installation via Python pip.
type PipInstallMethod struct {
	Name string
}

func (p PipInstallMethod) GetMethodType() string {
	return "pip"
}

func (p PipInstallMethod) GetImports() []string {
	return nil
}

func (p PipInstallMethod) RenderSetupTasks(command string) string {
	return ""
}

func (p PipInstallMethod) RenderInstallTask(command string) string {
	return `    - name: Ensure if ` + command + ` is installed
      ansible.builtin.pip:
        name: ` + p.Name + `
        state: latest`
}

// GoInstallMethod handles installation via 'go install' command.
// Includes version checking and upgrade logic for Go modules.
type GoInstallMethod struct {
	PkgPath string
}

func (g GoInstallMethod) GetMethodType() string {
	return "go"
}

func (g GoInstallMethod) GetImports() []string {
	return []string{"setup-user-go-bin-directory"}
}

func (g GoInstallMethod) RenderSetupTasks(command string) string {
	return ""
}

func (g GoInstallMethod) RenderInstallTask(command string) string {
	commandID := strings.ReplaceAll(command, "-", "_")
	return `    - name: Check if ` + command + ` is installed
      shell: go version -m $(command -v ` + command + `) | grep '^\s*mod\s'
      register: ` + commandID + `_installed
      ignore_errors: yes
      changed_when: False

    - name: Extract ` + command + ` version
      block:
        - name: Set ` + command + ` facts
          set_fact:
            ` + commandID + `_module_path: "{{ ` + commandID + `_installed.stdout.split()[1] }}"
            ` + commandID + `_module_version: "{{ ` + commandID + `_installed.stdout.split()[2] }}"
        - name: Determine the latest ` + command + ` version
          command: go list -m -f "{{ '{{' }}.Version {{ '}}' }}" "{{ ` + commandID + `_module_path }}@latest"
          register: ` + commandID + `_latest
          ignore_errors: yes
          changed_when: False

        - name: Debug module path and version
          debug:
            msg: "{{ ` + commandID + `_module_path }} {{ ` + commandID + `_module_version }} => {{ ` + commandID + `_latest.stdout }}"
      rescue:
        - name: Clear ` + command + ` facts
          set_fact:
            ` + commandID + `_module_path: ""
            ` + commandID + `_module_version: ""

    - name: Upgrade ` + command + `
      command: go install ` + g.PkgPath + `
      when: ` + commandID + `_module_version is not defined or ` + commandID + `_module_version == "" or ` + commandID + `_module_version != ` + commandID + `_latest.stdout`
}

// CargoInstallMethod handles installation via Rust cargo command.
// Includes update logic using cargo-install-update.
type CargoInstallMethod struct {
	Name string
}

func (c CargoInstallMethod) GetMethodType() string {
	return "cargo"
}

func (c CargoInstallMethod) GetImports() []string {
	return []string{"setup-cargo", "cargo-install-update"}
}

func (c CargoInstallMethod) RenderSetupTasks(command string) string {
	return ""
}

func (c CargoInstallMethod) RenderInstallTask(command string) string {
	commandID := strings.ReplaceAll(command, "-", "_")
	task := `    - name: Check if ` + command + ` is installed
      shell: command -v ` + command + `
      register: ` + commandID + `_installed
      ignore_errors: yes
      changed_when: False

    - name: Install ` + command + ` using Cargo
      command: cargo install ` + c.Name + `
      when: ` + commandID + `_installed.rc != 0

    - name: Update ` + command + ` to latest version
      command: cargo install-update ` + c.Name + `
      register: ` + commandID + `_update_result
      changed_when: "` + commandID + `_update_result.stdout is search('Overall updated [1-9]')"
      when: ` + commandID + `_installed.rc == 0`

	return task
}

// NpmInstallMethod handles installation via Node.js npm command.
type NpmInstallMethod struct {
	Name string
}

func (n NpmInstallMethod) GetMethodType() string {
	return "npm"
}

func (n NpmInstallMethod) GetImports() []string {
	return []string{"npm"}
}

func (n NpmInstallMethod) RenderSetupTasks(command string) string {
	return ""
}

func (n NpmInstallMethod) RenderInstallTask(command string) string {
	commandID := strings.ReplaceAll(command, "-", "_")
	return `    - name: Check if ` + command + ` is installed
      shell: command -v ` + command + `
      register: ` + commandID + `_installed
      ignore_errors: yes
      changed_when: False

    - name: Install ` + command + ` using npm
      command: npm install -g ` + n.Name + `
      when: ` + commandID + `_installed.rc != 0`
}

// UbuntuPkgInstallMethod handles Ubuntu-specific package installation
// with PPA (Personal Package Archive) support.
type UbuntuPkgInstallMethod struct {
	Name string
	PPA  string
}

func (u UbuntuPkgInstallMethod) GetMethodType() string {
	return "ubuntu"
}

func (u UbuntuPkgInstallMethod) GetImports() []string {
	return nil
}

func (u UbuntuPkgInstallMethod) RenderSetupTasks(command string) string {
	if u.PPA == "" {
		return ""
	}
	return `    - name: Ensure ` + command + ` PPA is present in Ubuntu
      apt_repository:
        repo: "` + u.PPA + `"
        state: present
        update_cache: yes
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin" and ansible_facts['distribution'] == "Ubuntu"
      become: yes

`
}

func (u UbuntuPkgInstallMethod) RenderInstallTask(command string) string {
	return `      block:
        - name: Check if ` + command + ` is installed
          shell: command -v ` + command + `
          changed_when: False
      rescue:
        - name: Install ` + command + ` on non-Termux, non-MacOS systems
          package:
            name: ` + u.Name + `
            state: present
          become: yes`
}

// DebianPkgInstallMethod handles Debian-specific package installation
// with bookworm-backports repository support.
type DebianPkgInstallMethod struct {
	Name string
}

func (d DebianPkgInstallMethod) GetMethodType() string {
	return "debian"
}

func (d DebianPkgInstallMethod) GetImports() []string {
	return nil
}

func (d DebianPkgInstallMethod) RenderSetupTasks(command string) string {
	return `    - name: Ensure bookworm-backports is added to sources.list.d
      ansible.builtin.apt_repository:
        repo: "deb http://deb.debian.org/debian bookworm-backports main contrib non-free non-free-firmware"
        state: present
        update_cache: yes
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin" and ansible_facts['distribution'] == "Debian" and ansible_facts['distribution_major_version'] == "12"
      become: yes

`
}

func (d DebianPkgInstallMethod) RenderInstallTask(command string) string {
	return `      block:
        - name: Check if ` + command + ` is installed
          shell: command -v ` + command + `
          changed_when: False
      rescue:
        - name: Install ` + command + ` on non-Termux, non-MacOS systems
          package:
            name: ` + d.Name + `
            state: present
          become: yes`
}

// PlatformSpecificTool represents a development tool that can be installed
// using different methods on different platforms. This is the unified approach
// that replaced separate install type arrays (GoInstall, PipInstall, etc.).
type PlatformSpecificTool struct {
	command   string
	platforms map[string]InstallMethod
	Imports   []string
}

func (p PlatformSpecificTool) Command() string {
	return p.command
}

func (p PlatformSpecificTool) CommandID() string {
	// Replace dash to underscore.
	return strings.ReplaceAll(p.command, "-", "_")
}

func (p PlatformSpecificTool) GetPlatforms() map[string]InstallMethod {
	return p.platforms
}

func (p PlatformSpecificTool) GetAllImports() []string {
	importsMap := make(map[string]bool)
	var importsOrder []string

	// Add explicit imports first, maintaining order
	for _, imp := range p.Imports {
		if !importsMap[imp] && imp != p.command {
			importsMap[imp] = true
			importsOrder = append(importsOrder, imp)
		}
	}

	// Add imports from platform methods, maintaining their order
	for _, method := range p.platforms {
		for _, imp := range method.GetImports() {
			if !importsMap[imp] && imp != p.command {
				importsMap[imp] = true
				importsOrder = append(importsOrder, imp)
			}
		}
	}

	return importsOrder
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
      block:
        - name: Check if {{.Command}} is installed
          shell: command -v {{.Command}}
          changed_when: False
      rescue:
        - name: Install {{.Command}} on MacOS
          community.general.homebrew:
            name: {{.BrewPkgName}}
            state: present{{ if .BrewOptions }}
            install_options:{{ range .BrewOptions }}
              - {{ . }}{{ end }}{{ end }}
      when: ansible_facts['os_family'] == "Darwin"

    - name: Ensure {{.Command}} is present on non-Termux, non-MacOS systems
      block:
        - name: Check if {{.Command}} is installed
          shell: command -v {{.Command}}
          changed_when: False
      rescue:
        - name: Install {{.Command}} on non-Termux, non-MacOS systems
          package:
            name: {{.DebianPkgName}}
            state: present
          become: yes
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin"

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

var platformSpecificTemplate = `---
{{- range .GetAllImports }}
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
{{- $platforms := .GetPlatforms }}
{{- if index $platforms "darwin" }}
{{- $method := index $platforms "darwin" }}

    - name: Ensure {{.Command}} is present on MacOS
{{$method.RenderInstallTask .Command}}
      when: ansible_facts['os_family'] == "Darwin"
{{- end }}
{{- if index $platforms "termux" }}
{{- $method := index $platforms "termux" }}

    - name: Ensure {{.Command}} is present on Termux
{{$method.RenderInstallTask .Command}}
      when: ansible_env.TERMUX_VERSION is defined
{{- end }}
{{- if index $platforms "all" }}
{{- $method := index $platforms "all" }}

{{$method.RenderInstallTask .Command}}
{{- else }}
{{- if index $platforms "debian-like" }}
{{- $method := index $platforms "debian-like" }}
{{- if eq $method.GetMethodType "pip" }}

{{$method.RenderInstallTask .Command}}
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin"
{{- else }}

    - name: Install {{.Command}} via {{$method.GetMethodType}} on Debian/Ubuntu
{{$method.RenderInstallTask .Command}}
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin"
{{- end }}
{{- else }}
{{- if index $platforms "debian" }}
{{- $method := index $platforms "debian" }}

    - name: Install {{.Command}} via {{$method.GetMethodType}} on Debian
{{$method.RenderInstallTask .Command}}
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin" and ansible_facts['distribution'] == "Debian"
{{- end }}
{{- if index $platforms "ubuntu" }}
{{- $method := index $platforms "ubuntu" }}

    - name: Install {{.Command}} via {{$method.GetMethodType}} on Ubuntu
{{$method.RenderInstallTask .Command}}
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin" and ansible_facts['distribution'] == "Ubuntu"
{{- end }}
{{- end }}
{{- end }}
`

// Commander interface is implemented by all tool types to provide
// a unified way to get command names for file generation.
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
	{command: "npm", debianPkgName: "npm", termuxPkgName: "nodejs", brewPkgName: "node"},
	{command: "pandoc"},
	{command: "pass"},
	{command: "poetry", debianPkgName: "python3-poetry", termuxPkgName: "python-poetry"},
	{command: "protoc", debianPkgName: "protobuf-compiler", termuxPkgName: "protobuf", brewPkgName: "protobuf"},
	{command: "psql", debianPkgName: "postgresql-client", termuxPkgName: "postgresql", brewPkgName: "postgresql"},
	{command: "python3-notmuch2", debianPkgName: "python3-notmuch2", termuxPkgName: "notmuch", brewPkgName: "notmuch"},
	{command: "rg", debianPkgName: "ripgrep", termuxPkgName: "ripgrep", brewPkgName: "ripgrep"},
	{command: "sed", brewPkgName: "gsed"},
	{command: "ssh", debianPkgName: "openssh-client", termuxPkgName: "openssh", brewPkgName: "openssh"},
	{command: "sshpass"},
	{command: "starship"},
	{command: "tlmgr", debianPkgName: "texlive-lang-korean", termuxPkgName: "texlive-installer", brewPkgName: "mactex"},
	{command: "tmux"},
	{command: "unzip"},
	{command: "which"},
	{command: "zip"},
	{command: "zoxide"},
}

var platformSpecificTools = []PlatformSpecificTool{
	{
		command: "ruff",
		platforms: map[string]InstallMethod{
			"termux":      TermuxPkgInstallMethod{Name: "ruff"},
			"darwin":      BrewInstallMethod{Name: "ruff"},
			"debian-like": PipInstallMethod{Name: "ruff"},
		},
		Imports: nil,
	},
	{
		command: "buildifier",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/bazelbuild/buildtools/buildifier@latest"},
		},
		Imports: nil,
	},
	{
		command: "buildozer",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/bazelbuild/buildtools/buildozer@latest"},
		},
		Imports: nil,
	},
	{
		command: "fillstruct",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/davidrjenni/reftools/cmd/fillstruct@latest"},
		},
		Imports: nil,
	},
	{
		command: "godef",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/rogpeppe/godef@latest"},
		},
		Imports: nil,
	},
	{
		command: "godoc",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "golang.org/x/tools/cmd/godoc@latest"},
		},
		Imports: nil,
	},
	{
		command: "godoctor",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/godoctor/godoctor@latest"},
		},
		Imports: nil,
	},
	{
		command: "gofumpt",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "mvdan.cc/gofumpt@latest"},
		},
		Imports: nil,
	},
	{
		command: "goimports",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "golang.org/x/tools/cmd/goimports@latest"},
		},
		Imports: nil,
	},
	{
		command: "gomodifytags",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/fatih/gomodifytags@latest"},
		},
		Imports: nil,
	},
	{
		command: "gopkgs",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/uudashr/gopkgs/v2/cmd/gopkgs@latest"},
		},
		Imports: nil,
	},
	{
		command: "gopls",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "golang.org/x/tools/gopls@latest"},
		},
		Imports: nil,
	},
	{
		command: "gorename",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "golang.org/x/tools/cmd/gorename@latest"},
		},
		Imports: nil,
	},
	{
		command: "gotests",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/cweill/gotests/...@latest"},
		},
		Imports: nil,
	},
	{
		command: "grpcui",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/fullstorydev/grpcui/cmd/grpcui@latest"},
		},
		Imports: nil,
	},
	{
		command: "guru",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "golang.org/x/tools/cmd/guru@latest"},
		},
		Imports: nil,
	},
	{
		command: "hugo",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/gohugoio/hugo@latest"},
		},
		Imports: nil,
	},
	{
		command: "image2ascii",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/qeesung/image2ascii@latest"},
		},
		Imports: nil,
	},
	{
		command: "impl",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/josharian/impl@latest"},
		},
		Imports: nil,
	},
	{
		command: "jira",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/ankitpokhrel/jira-cli/cmd/jira@latest"},
		},
		Imports: nil,
	},
	{
		command: "protoc-gen-go",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "google.golang.org/protobuf/cmd/protoc-gen-go@latest"},
		},
		Imports: []string{"protoc"},
	},
	{
		command: "protoc-gen-go-grpc",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest"},
		},
		Imports: []string{"protoc"},
	},
	{
		command: "protolint",
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: "github.com/yoheimuta/protolint/cmd/protolint@latest"},
		},
		Imports: nil,
	},
	{
		command: "cargo-add",
		platforms: map[string]InstallMethod{
			"all": CargoInstallMethod{Name: "cargo-edit"},
		},
		Imports: nil,
	},
	{
		command: "cargo-install-update",
		platforms: map[string]InstallMethod{
			"all": CargoInstallMethod{Name: "cargo-update"},
		},
		Imports: nil,
	},
	{
		command: "cargo-outdated",
		platforms: map[string]InstallMethod{
			"all": CargoInstallMethod{Name: "cargo-outdated"},
		},
		Imports: nil,
	},
	{
		command: "emacs-lsp-booster",
		platforms: map[string]InstallMethod{
			"all": CargoInstallMethod{Name: "emacs-lsp-booster"},
		},
		Imports: nil,
	},
	{
		command: "protovalidate",
		platforms: map[string]InstallMethod{
			"all": PipInstallMethod{Name: "protovalidate"},
		},
		Imports: nil,
	},
	{
		command: "claude",
		platforms: map[string]InstallMethod{
			"all": NpmInstallMethod{Name: "@anthropic-ai/claude-code"},
		},
		Imports: nil,
	},
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

func getAllYmlFiles() ([]string, error) {
	var ymlFiles []string
	err := filepath.WalkDir(".", func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() && strings.HasSuffix(path, ".yml") && !strings.Contains(path, "/") {
			ymlFiles = append(ymlFiles, strings.TrimSuffix(path, ".yml"))
		}
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("walk directory: %w", err)
	}
	sort.Strings(ymlFiles)
	return ymlFiles, nil
}

func getGeneratedRuleNames() []string {
	var generated []string

	for _, pkg := range packages {
		generated = append(generated, pkg.Command())
	}

	for _, pkg := range platformSpecificTools {
		generated = append(generated, pkg.Command())
	}

	sort.Strings(generated)
	return generated
}

func getManualPlaybooks() ([]string, error) {
	allYml, err := getAllYmlFiles()
	if err != nil {
		return nil, err
	}

	generated := getGeneratedRuleNames()
	generatedMap := make(map[string]bool)
	for _, name := range generated {
		generatedMap[name] = true
	}

	var manual []string
	for _, yml := range allYml {
		if !generatedMap[yml] {
			manual = append(manual, yml+".yml")
		}
	}

	return manual, nil
}

func updateReadmeManualPlaybooks() error {
	readmePath := "README.org"

	file, err := os.Open(readmePath)
	if err != nil {
		return fmt.Errorf("failed to open README.org: %w", err)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	inManualSection := false

	for scanner.Scan() {
		line := scanner.Text()
		if strings.Contains(line, "* Manual Playbooks") {
			inManualSection = true
			lines = append(lines, line)
			lines = append(lines, "These files are playbooks not generated from =generate_packages.go=:")
			lines = append(lines, "")

			manualPlaybooks, err := getManualPlaybooks()
			if err != nil {
				return fmt.Errorf("failed to get manual playbooks: %w", err)
			}

			for _, playbook := range manualPlaybooks {
				lines = append(lines, "- "+playbook)
			}

			for scanner.Scan() {
				nextLine := scanner.Text()
				if strings.HasPrefix(nextLine, "* ") && nextLine != "* Manual Playbooks" {
					lines = append(lines, nextLine)
					break
				}
			}
		} else if !inManualSection {
			lines = append(lines, line)
		}
	}

	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		return fmt.Errorf("error reading README.org: %w", err)
	}

	output := strings.Join(lines, "\n") + "\n"
	err = os.WriteFile(readmePath, []byte(output), 0o600)
	if err != nil {
		return fmt.Errorf("failed to write README.org: %w", err)
	}

	return nil
}

func main() {
	tmpl := template.Must(template.New("packages").Parse(packagesTemplate))
	template.Must(tmpl.New("platformspecific").Parse(platformSpecificTemplate))

	generatePackages(tmpl.Lookup("packages"), packages)
	generatePackages(tmpl.Lookup("platformspecific"), platformSpecificTools)

	err := updateReadmeManualPlaybooks()
	if err != nil {
		panic(err)
	}
}
