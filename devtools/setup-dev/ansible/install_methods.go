package main

import "strings"

// indent adds the specified number of spaces to the beginning of each line
// in the given string, useful for adjusting YAML indentation levels.
func indent(s string, spaces int) string {
	prefix := strings.Repeat(" ", spaces)
	lines := strings.Split(s, "\n")
	for i, line := range lines {
		if line != "" {
			lines[i] = prefix + line
		}
	}
	return strings.Join(lines, "\n")
}

// stripBlockAndIndent removes a leading task name and "block:" lines if present,
// then indents the remaining content. This is useful for RenderBlockInstallTask
// implementations where the template already provides the "block:" keyword.
func stripBlockAndIndent(s string, spaces int) string {
	lines := strings.Split(s, "\n")
	startIdx := 0

	// Check if first line is a task name (starts with "    - name:")
	if len(lines) > 0 && strings.HasPrefix(strings.TrimSpace(lines[0]), "- name:") {
		startIdx = 1
	}

	// Check if next line contains just whitespace + "block:"
	if startIdx < len(lines) && strings.TrimSpace(lines[startIdx]) == "block:" {
		startIdx++
	}

	// Rejoin and indent the remaining lines
	remaining := strings.Join(lines[startIdx:], "\n")
	return indent(remaining, spaces)
}

// PackageInstallMethod handles installation via system package managers
// like apt, yum, dnf, etc. on Linux distributions.
type PackageInstallMethod struct {
	Name string
}

func (p PackageInstallMethod) GetMethodType() string {
	return "package"
}

func (p PackageInstallMethod) GetImports() []Import {
	return nil
}

func (p PackageInstallMethod) RenderSetupTasks(_ string) string {
	return ""
}

func (p PackageInstallMethod) RenderInstallTask(command string) string {
	return `        - name: Install ` + command + ` on non-Termux, non-MacOS systems
          package:
            name: ` + p.Name + `
            state: present
          become: yes`
}

func (p PackageInstallMethod) RenderBlockInstallTask(command string) string {
	return p.RenderInstallTask(command)
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

func (b BrewInstallMethod) GetImports() []Import {
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

func (b BrewInstallMethod) RenderBlockInstallTask(command string) string {
	return stripBlockAndIndent(b.RenderInstallTask(command), 0)
}

// TermuxPkgInstallMethod handles installation via the pkg command on Termux.
type TermuxPkgInstallMethod struct {
	Name string
}

func (t TermuxPkgInstallMethod) GetMethodType() string {
	return "termux-pkg"
}

func (t TermuxPkgInstallMethod) GetImports() []Import {
	return nil
}

func (t TermuxPkgInstallMethod) RenderSetupTasks(_ string) string {
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

func (t TermuxPkgInstallMethod) RenderBlockInstallTask(command string) string {
	return stripBlockAndIndent(t.RenderInstallTask(command), 0)
}

// PipInstallMethod handles installation via Python pip.
type PipInstallMethod struct {
	Name string
}

func (p PipInstallMethod) GetMethodType() string {
	return "pip"
}

func (p PipInstallMethod) GetImports() []Import {
	return nil
}

func (p PipInstallMethod) RenderSetupTasks(_ string) string {
	return ""
}

func (p PipInstallMethod) RenderInstallTask(command string) string {
	return `    - name: Ensure if ` + command + ` is installed
      ansible.builtin.pip:
        name: ` + p.Name + `
        state: latest`
}

func (p PipInstallMethod) RenderBlockInstallTask(command string) string {
	return indent(p.RenderInstallTask(command), 4)
}

// GoInstallMethod handles installation via 'go install' command.
// Includes version checking and upgrade logic for Go modules.
type GoInstallMethod struct {
	PkgPath string
}

func (g GoInstallMethod) GetMethodType() string {
	return "go"
}

func (g GoInstallMethod) GetImports() []Import {
	return []Import{{Playbook: "setup-user-go-bin-directory"}}
}

func (g GoInstallMethod) RenderSetupTasks(_ string) string {
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

func (g GoInstallMethod) RenderBlockInstallTask(command string) string {
	return g.RenderInstallTask(command)
}

// CargoInstallMethod handles installation via Rust cargo command.
// Includes update logic using cargo-install-update.
type CargoInstallMethod struct {
	Name string
}

func (c CargoInstallMethod) GetMethodType() string {
	return "cargo"
}

func (c CargoInstallMethod) GetImports() []Import {
	return []Import{
		{Playbook: "setup-cargo"},
		{Playbook: "cargo-install-update"},
	}
}

func (c CargoInstallMethod) RenderSetupTasks(_ string) string {
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

func (c CargoInstallMethod) RenderBlockInstallTask(command string) string {
	return indent(c.RenderInstallTask(command), 4)
}

// NpmInstallMethod handles installation via Node.js npm command.
type NpmInstallMethod struct {
	Name string
}

func (n NpmInstallMethod) GetMethodType() string {
	return "npm"
}

func (n NpmInstallMethod) GetImports() []Import {
	return []Import{{Playbook: "setup-npm"}}
}

func (n NpmInstallMethod) RenderSetupTasks(_ string) string {
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

func (n NpmInstallMethod) RenderBlockInstallTask(command string) string {
	return indent(n.RenderInstallTask(command), 4)
}

// NvmInstallMethod handles installation via npm through nvm.
// This provides an alternative to NpmInstallMethod for packages that need
// newer Node.js versions or better separation between system and development environments.
type NvmInstallMethod struct {
	Name string
}

func (n NvmInstallMethod) GetMethodType() string {
	return "nvm"
}

func (n NvmInstallMethod) GetImports() []Import {
	return []Import{{Playbook: "setup-nvm"}}
}

func (n NvmInstallMethod) RenderSetupTasks(_ string) string {
	return ""
}

func (n NvmInstallMethod) RenderInstallTask(command string) string {
	commandID := strings.ReplaceAll(command, "-", "_")
	return `    - name: Check if ` + command + ` is installed
      shell: command -v ` + command + `
      register: ` + commandID + `_installed
      ignore_errors: yes
      changed_when: False

    - name: Install ` + command + ` using npm through nvm
      command: nvm exec default npm install -g ` + n.Name + `
      when: ` + commandID + `_installed.rc != 0`
}

func (n NvmInstallMethod) RenderBlockInstallTask(command string) string {
	return indent(n.RenderInstallTask(command), 4)
}

// UvInstallMethod handles installation via uv tool command.
type UvInstallMethod struct {
	Name string
}

func (u UvInstallMethod) GetMethodType() string {
	return "uv"
}

func (u UvInstallMethod) GetImports() []Import {
	return []Import{{Playbook: "uv"}}
}

func (u UvInstallMethod) RenderSetupTasks(_ string) string {
	return ""
}

func (u UvInstallMethod) RenderInstallTask(command string) string {
	return `    - name: Ensure ` + command + ` is installed with uv
      shell: uv tool install ` + u.Name + `
      args:
        creates: ~/.local/bin/` + command
}

func (u UvInstallMethod) RenderBlockInstallTask(command string) string {
	return indent(u.RenderInstallTask(command), 4)
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

func (u UbuntuPkgInstallMethod) GetImports() []Import {
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

func (u UbuntuPkgInstallMethod) RenderBlockInstallTask(command string) string {
	return u.RenderInstallTask(command)
}

// DebianPkgInstallMethod handles Debian-specific package installation
// with bookworm-backports repository support.
type DebianPkgInstallMethod struct {
	Name string
}

func (d DebianPkgInstallMethod) GetMethodType() string {
	return "debian"
}

func (d DebianPkgInstallMethod) GetImports() []Import {
	return nil
}

func (d DebianPkgInstallMethod) RenderSetupTasks(_ string) string {
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

func (d DebianPkgInstallMethod) RenderBlockInstallTask(command string) string {
	return d.RenderInstallTask(command)
}

// ShellInstallMethod handles installation via shell commands with version checking.
type ShellInstallMethod struct {
	// InstallCommand is the shell command to install or update the tool.
	// Can use Ansible template variables, particularly:
	//   - {{ <command>_latest_release.json.<LatestVersionPath> }} for the version fetched from LatestVersionURL
	// Example: curl -o- https://example.com/{{ nvm_latest_release.json.tag_name }}/install.sh | bash
	InstallCommand string

	// VersionCommand is the shell command to check the installed version (e.g., "tool --version").
	// Leave empty to skip version checking and always run InstallCommand if tool is not found.
	VersionCommand string

	// VersionRegex is the regex pattern to extract the version number from VersionCommand output.
	// Use capturing group to extract version (e.g., "([0-9.]+)" or "version ([0-9.]+)").
	// Required if VersionCommand is specified.
	VersionRegex string

	// LatestVersionURL is the URL to fetch the latest version information (typically GitHub API).
	// Example: "https://api.github.com/repos/owner/repo/releases/latest"
	// Leave empty to skip version checking and always run InstallCommand if tool is not found.
	LatestVersionURL string

	// LatestVersionPath is the JSON path to extract the version from LatestVersionURL response.
	// Example: "tag_name" for GitHub releases (accesses .tag_name in the JSON response)
	// Required if LatestVersionURL is specified.
	LatestVersionPath string
}

func (s ShellInstallMethod) GetMethodType() string {
	return "shell"
}

func (s ShellInstallMethod) GetImports() []Import {
	return nil
}

func (s ShellInstallMethod) RenderSetupTasks(_ string) string {
	return ""
}

func (s ShellInstallMethod) RenderInstallTask(command string) string {
	commandID := strings.ReplaceAll(command, "-", "_")

	if s.VersionCommand != "" && s.LatestVersionURL != "" {
		return `    - name: Ensure ` + command + ` is present
      block:
        - name: Check if ` + command + ` is installed
          shell: command -v ` + command + `
          register: ` + commandID + `_command_check
          failed_when: false
          changed_when: False

        - name: Get installed ` + command + ` version
          command: ` + s.VersionCommand + `
          register: ` + commandID + `_version_output
          failed_when: false
          changed_when: False
          when: ` + commandID + `_command_check.rc == 0

        - name: Parse installed ` + command + ` version
          set_fact:
            ` + commandID + `_installed_version: "{{ (` + commandID + `_version_output.stdout | regex_search('` + s.VersionRegex + `', '\\1')) | default(['0.0.0']) | first }}"
          when: ` + commandID + `_command_check.rc == 0

        - name: Set default version when ` + command + ` is not installed
          set_fact:
            ` + commandID + `_installed_version: "0.0.0"
          when: ` + commandID + `_command_check.rc != 0

        - name: Get latest available ` + command + ` version from GitHub
          uri:
            url: ` + s.LatestVersionURL + `
            return_content: yes
          register: ` + commandID + `_latest_release

        - name: Parse latest ` + command + ` version from GitHub response
          set_fact:
            ` + commandID + `_latest_version: "{{ ` + commandID + `_latest_release.json.` + s.LatestVersionPath + ` | regex_replace('^v', '') }}"

        - name: Install/update ` + command + ` if outdated
          shell: ` + s.InstallCommand + `
          when: ` + commandID + `_installed_version != ` + commandID + `_latest_version`
	}

	return `    - name: Ensure ` + command + ` is present
      block:
        - name: Check if ` + command + ` is installed
          shell: command -v ` + command + `
          register: ` + commandID + `_installed
          failed_when: false
          changed_when: False

        - name: Install ` + command + `
          shell: ` + s.InstallCommand + `
          when: ` + commandID + `_installed.rc != 0`
}

func (s ShellInstallMethod) RenderBlockInstallTask(command string) string {
	return stripBlockAndIndent(s.RenderInstallTask(command), 0)
}
