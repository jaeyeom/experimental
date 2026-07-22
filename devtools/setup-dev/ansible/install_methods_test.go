package main

import (
	"reflect"
	"strings"
	"testing"
)

func assertContains(t *testing.T, got string, wants []string) {
	t.Helper()
	for _, want := range wants {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q\nfull output:\n%s", want, got)
		}
	}
}

func assertNotContains(t *testing.T, got string, unwanteds []string) {
	t.Helper()
	for _, unwanted := range unwanteds {
		if strings.Contains(got, unwanted) {
			t.Errorf("unexpectedly contains %q\nfull output:\n%s", unwanted, got)
		}
	}
}

func TestIndent(t *testing.T) {
	tests := []struct {
		name   string
		input  string
		spaces int
		want   string
	}{
		{
			name:   "single line",
			input:  "hello",
			spaces: 4,
			want:   "    hello",
		},
		{
			name:   "multiline",
			input:  "a\nb\nc",
			spaces: 2,
			want:   "  a\n  b\n  c",
		},
		{
			name:   "empty lines stay empty",
			input:  "a\n\nb",
			spaces: 2,
			want:   "  a\n\n  b",
		},
		{
			name:   "zero spaces",
			input:  "x",
			spaces: 0,
			want:   "x",
		},
		{
			name:   "empty string",
			input:  "",
			spaces: 4,
			want:   "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := indent(tt.input, tt.spaces); got != tt.want {
				t.Errorf("indent() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestStripBlockAndIndent(t *testing.T) {
	tests := []struct {
		name   string
		input  string
		spaces int
		want   string
	}{
		{
			name: "strips name and block",
			input: `    - name: Ensure foo is present
      block:
        - name: Check if foo is installed
          shell: command -v foo`,
			spaces: 0,
			want: `        - name: Check if foo is installed
          shell: command -v foo`,
		},
		{
			name: "block only without name",
			input: `      block:
        - name: Install foo
          package:
            name: foo`,
			spaces: 0,
			want: `        - name: Install foo
          package:
            name: foo`,
		},
		{
			// A leading "- name:" line is always stripped (template already
			// provides the task name). Content without that header is kept.
			name:   "no name or block header indents content",
			input:  "        shell: echo hi",
			spaces: 2,
			want:   "          shell: echo hi",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := stripBlockAndIndent(tt.input, tt.spaces); got != tt.want {
				t.Errorf("stripBlockAndIndent() =\n%q\nwant:\n%q", got, tt.want)
			}
		})
	}
}

func TestPackageInstallMethod(t *testing.T) {
	method := PackageInstallMethod{Name: "silversearcher-ag"}
	if got := method.GetMethodType(); got != "package" {
		t.Errorf("GetMethodType() = %q, want package", got)
	}
	if imports := method.GetImports(); imports != nil {
		t.Errorf("GetImports() = %v, want nil", imports)
	}
	if setup := method.RenderSetupTasks("ag"); setup != "" {
		t.Errorf("RenderSetupTasks() = %q, want empty", setup)
	}

	got := method.RenderInstallTask("ag")
	assertContains(t, got, []string{
		"command -v ag",
		"name: silversearcher-ag",
		"become: yes",
		"package:",
	})

	block := method.RenderBlockInstallTask("ag")
	assertContains(t, block, []string{"name: silversearcher-ag", "command -v ag"})
	if strings.HasPrefix(strings.TrimSpace(block), "block:") {
		t.Errorf("RenderBlockInstallTask should strip leading block:\n%s", block)
	}
}

func TestBrewInstallMethod(t *testing.T) {
	t.Run("without tap or options", func(t *testing.T) {
		method := BrewInstallMethod{Name: "ripgrep"}
		if got := method.GetMethodType(); got != "brew" {
			t.Errorf("GetMethodType() = %q, want brew", got)
		}
		if setup := method.RenderSetupTasks("rg"); setup != "" {
			t.Errorf("RenderSetupTasks() = %q, want empty", setup)
		}
		got := method.RenderInstallTask("rg")
		assertContains(t, got, []string{
			"command -v rg",
			"community.general.homebrew:",
			"name: ripgrep",
			"state: present",
		})
		assertNotContains(t, got, []string{"install_options:"})
	})

	t.Run("with tap and options", func(t *testing.T) {
		method := BrewInstallMethod{
			Name:    "emacs-plus",
			Tap:     "d12frosted/emacs-plus",
			Options: []string{"with-native-comp", "with-dbus"},
		}
		setup := method.RenderSetupTasks("emacs")
		assertContains(t, setup, []string{
			"Tap d12frosted/emacs-plus for emacs",
			"community.general.homebrew_tap:",
			"name: d12frosted/emacs-plus",
			`when: ansible_facts['os_family'] == "Darwin"`,
		})

		got := method.RenderInstallTask("emacs")
		assertContains(t, got, []string{
			"name: emacs-plus",
			"install_options:",
			"- with-native-comp",
			"- with-dbus",
		})
	})
}

func TestBrewCaskInstallMethod(t *testing.T) {
	method := BrewCaskInstallMethod{Name: "visual-studio-code"}
	if got := method.GetMethodType(); got != "brew-cask" {
		t.Errorf("GetMethodType() = %q, want brew-cask", got)
	}
	got := method.RenderInstallTask("code")
	assertContains(t, got, []string{
		"community.general.homebrew_cask:",
		"name: visual-studio-code",
		"Install code on MacOS via cask",
	})
	block := method.RenderBlockInstallTask("code")
	assertContains(t, block, []string{"name: visual-studio-code"})
}

func TestTermuxPkgInstallMethod(t *testing.T) {
	method := TermuxPkgInstallMethod{Name: "git"}
	if got := method.GetMethodType(); got != "termux-pkg" {
		t.Errorf("GetMethodType() = %q, want termux-pkg", got)
	}
	got := method.RenderInstallTask("git")
	assertContains(t, got, []string{
		"Install git on Termux",
		"pkg install -y git",
		"command -v git",
	})
}

func TestPipInstallMethod(t *testing.T) {
	method := PipInstallMethod{Name: "yt-dlp"}
	if got := method.GetMethodType(); got != "pip" {
		t.Errorf("GetMethodType() = %q, want pip", got)
	}
	got := method.RenderInstallTask("yt-dlp")
	assertContains(t, got, []string{
		"ansible.builtin.pip:",
		"name: yt-dlp",
		"state: latest",
	})
	// RenderBlockInstallTask should indent the task.
	block := method.RenderBlockInstallTask("yt-dlp")
	if !strings.HasPrefix(block, "    ") {
		t.Errorf("RenderBlockInstallTask should be indented:\n%s", block)
	}
}

func TestGoInstallMethodRenderInstallTask(t *testing.T) {
	method := GoInstallMethod{PkgPath: "example.com/mytool@latest"}
	got := method.RenderInstallTask("my-tool")

	// Hyphens in the command name must become underscores in Ansible vars.
	assertContains(t, got, []string{
		"my_tool_installed",
		"my_tool_module_version",
		"my_tool_latest",
		"my_tool_build_go",
		"my_tool_upgrade",
	})

	// Build Go version is extracted from the binary metadata.
	assertContains(t, got, []string{
		"go version -m $(command -v my-tool)",
		"my_tool_build_go",
	})

	// Upgrade when module is missing/outdated OR built with a different toolchain.
	assertContains(t, got, []string{
		"my_tool_module_version is not defined",
		"my_tool_module_version == \"\"",
		"my_tool_module_version != my_tool_latest.stdout",
		"my_tool_build_go.stdout | default('') != go_toolchain_version",
	})

	// Install uses the configured package path.
	assertContains(t, got, []string{"go install example.com/mytool@latest"})

	if got := method.GetMethodType(); got != "go" {
		t.Errorf("GetMethodType() = %q, want go", got)
	}
	if setup := method.RenderSetupTasks("my-tool"); setup != "" {
		t.Errorf("RenderSetupTasks() = %q, want empty", setup)
	}
	block := method.RenderBlockInstallTask("my-tool")
	if !strings.HasPrefix(block, "    ") {
		t.Errorf("RenderBlockInstallTask should indent:\n%s", block)
	}
}

func TestGoInstallMethodGetImports(t *testing.T) {
	method := GoInstallMethod{PkgPath: "example.com/mytool@latest"}
	imports := method.GetImports()
	if len(imports) != 1 || imports[0].Playbook != "setup-user-go-bin-directory" {
		t.Errorf("GetImports() = %v, want setup-user-go-bin-directory", imports)
	}
}

func TestRustupComponentMethod(t *testing.T) {
	method := RustupComponentMethod{Name: "rustfmt"}
	if got := method.GetMethodType(); got != "rustup-component" {
		t.Errorf("GetMethodType() = %q, want rustup-component", got)
	}
	imports := method.GetImports()
	if len(imports) != 1 || imports[0].Playbook != "setup-cargo" {
		t.Errorf("GetImports() = %v, want setup-cargo", imports)
	}
	got := method.RenderInstallTask("rustfmt")
	assertContains(t, got, []string{
		"rustup component add rustfmt",
		"rustfmt_installed",
		".cargo/bin",
	})
}

func TestCargoInstallMethod(t *testing.T) {
	method := CargoInstallMethod{Name: "ripgrep"}
	if got := method.GetMethodType(); got != "cargo" {
		t.Errorf("GetMethodType() = %q, want cargo", got)
	}
	imports := method.GetImports()
	wantImports := []Import{
		{Playbook: "setup-cargo"},
		{Playbook: "cargo-install-update"},
	}
	if !reflect.DeepEqual(imports, wantImports) {
		t.Errorf("GetImports() = %v, want %v", imports, wantImports)
	}

	got := method.RenderInstallTask("rg")
	assertContains(t, got, []string{
		"rg_installed",
		"cargo install ripgrep",
		"cargo install-update ripgrep",
		"rg_update_result",
		"CARGO_BUILD_JOBS",
	})
}

func TestNpmInstallMethod(t *testing.T) {
	t.Run("without install args", func(t *testing.T) {
		method := NpmInstallMethod{Name: "prettier"}
		if got := method.GetMethodType(); got != "npm" {
			t.Errorf("GetMethodType() = %q, want npm", got)
		}
		imports := method.GetImports()
		if len(imports) != 1 || imports[0].Playbook != "setup-npm" {
			t.Errorf("GetImports() = %v, want setup-npm", imports)
		}
		got := method.RenderInstallTask("prettier")
		assertContains(t, got, []string{
			"npm install -g prettier",
			"prettier_installed",
		})
	})

	t.Run("with install args", func(t *testing.T) {
		method := NpmInstallMethod{Name: "some-pkg", InstallArgs: []string{"--legacy-peer-deps", "--force"}}
		got := method.RenderInstallTask("some-pkg")
		assertContains(t, got, []string{
			"npm install -g some-pkg --legacy-peer-deps --force",
		})
	})
}

func TestNvmInstallMethod(t *testing.T) {
	method := NvmInstallMethod{Name: "@biomejs/biome"}
	if got := method.GetMethodType(); got != "nvm" {
		t.Errorf("GetMethodType() = %q, want nvm", got)
	}
	imports := method.GetImports()
	if len(imports) != 1 || imports[0].Playbook != "setup-nvm" {
		t.Errorf("GetImports() = %v, want setup-nvm", imports)
	}
	got := method.RenderInstallTask("biome")
	assertContains(t, got, []string{
		"detected_nvm_dir",
		"nvm use --delete-prefix default --silent",
		"npm install -g @biomejs/biome",
		"biome_installed",
	})
}

func TestUvInstallMethod(t *testing.T) {
	method := UvInstallMethod{Name: "ruff"}
	if got := method.GetMethodType(); got != "uv" {
		t.Errorf("GetMethodType() = %q, want uv", got)
	}
	imports := method.GetImports()
	if len(imports) != 1 || imports[0].Playbook != "uv" {
		t.Errorf("GetImports() = %v, want uv", imports)
	}
	got := method.RenderInstallTask("ruff")
	assertContains(t, got, []string{
		"uv tool install ruff",
		"creates: ~/.local/bin/ruff",
	})
}

func TestUbuntuPkgInstallMethod(t *testing.T) {
	t.Run("without PPA", func(t *testing.T) {
		method := UbuntuPkgInstallMethod{Name: "curl"}
		if got := method.GetMethodType(); got != "ubuntu" {
			t.Errorf("GetMethodType() = %q, want ubuntu", got)
		}
		if setup := method.RenderSetupTasks("curl"); setup != "" {
			t.Errorf("RenderSetupTasks() = %q, want empty", setup)
		}
		got := method.RenderInstallTask("curl")
		assertContains(t, got, []string{"name: curl", "package:"})
	})

	t.Run("with PPA", func(t *testing.T) {
		method := UbuntuPkgInstallMethod{Name: "emacs", PPA: "ppa:ubuntuhandbook1/emacs"}
		setup := method.RenderSetupTasks("emacs")
		assertContains(t, setup, []string{
			`repo: "ppa:ubuntuhandbook1/emacs"`,
			"apt_repository:",
			`ansible_facts['distribution'] == "Ubuntu"`,
			"become: yes",
		})
	})
}

func TestDebianPkgInstallMethod(t *testing.T) {
	method := DebianPkgInstallMethod{Name: "emacs"}
	if got := method.GetMethodType(); got != "debian" {
		t.Errorf("GetMethodType() = %q, want debian", got)
	}
	setup := method.RenderSetupTasks("emacs")
	assertContains(t, setup, []string{
		"deb822_repository:",
		"backports",
		`ansible_facts['distribution'] == "Debian"`,
	})
	got := method.RenderInstallTask("emacs")
	assertContains(t, got, []string{"name: emacs", "package:"})
}

func TestShellInstallMethodSimple(t *testing.T) {
	method := ShellInstallMethod{
		InstallCommand: "curl -fsSL https://example.com/install.sh | bash",
	}
	if got := method.GetMethodType(); got != "shell" {
		t.Errorf("GetMethodType() = %q, want shell", got)
	}
	if imports := method.GetImports(); imports != nil {
		t.Errorf("GetImports() = %v, want nil", imports)
	}

	got := method.RenderInstallTask("my-tool")
	assertContains(t, got, []string{
		"my_tool_installed",
		"command -v my-tool",
		"curl -fsSL https://example.com/install.sh | bash",
		"when: my_tool_installed.rc != 0",
	})
	// Simple path should not do version checks.
	assertNotContains(t, got, []string{
		"_installed_version",
		"Get latest available",
	})
}

func TestShellInstallMethodWithVersionCheck(t *testing.T) {
	method := ShellInstallMethod{
		InstallCommand:    "curl -fsSL https://example.com/install.sh | bash",
		VersionCommand:    "my-tool --version",
		VersionRegex:      `([0-9.]+)`,
		LatestVersionURL:  "https://api.github.com/repos/owner/repo/releases/latest",
		LatestVersionPath: "tag_name",
	}

	got := method.RenderInstallTask("my-tool")
	assertContains(t, got, []string{
		"my_tool_command_check",
		"my-tool --version",
		"my_tool_installed_version",
		"my_tool_latest_version",
		"github-release-info.yml",
		"github_release_owner: owner",
		"github_release_repo: repo",
		"when: my_tool_installed_version != my_tool_latest_version",
		`regex_search('([0-9.]+)'`,
		"tag_name",
	})
}

func TestShellInstallMethodBecomeAndEnvironment(t *testing.T) {
	method := ShellInstallMethod{
		InstallCommand: "installer.sh",
		Become:         true,
		Environment: map[string]string{
			"FOO": "bar",
			"BAZ": "qux",
		},
	}

	if got := method.renderBecome(); got != "\n          become: yes" {
		t.Errorf("renderBecome() = %q", got)
	}
	env := method.renderEnvironment()
	// Keys must be sorted.
	assertContains(t, env, []string{
		"environment:",
		"BAZ: qux",
		"FOO: bar",
	})
	if strings.Index(env, "BAZ") > strings.Index(env, "FOO") {
		t.Errorf("environment keys should be sorted alphabetically:\n%s", env)
	}

	got := method.RenderInstallTask("nix")
	assertContains(t, got, []string{
		"become: yes",
		"environment:",
		"FOO: bar",
		"BAZ: qux",
		"installer.sh",
	})
}

func TestShellInstallMethodNoBecomeNoEnv(t *testing.T) {
	method := ShellInstallMethod{InstallCommand: "echo hi"}
	if got := method.renderBecome(); got != "" {
		t.Errorf("renderBecome() = %q, want empty", got)
	}
	if got := method.renderEnvironment(); got != "" {
		t.Errorf("renderEnvironment() = %q, want empty", got)
	}
}

func TestRenderGitHubReleaseInfoTasks(t *testing.T) {
	t.Run("cached github latest release", func(t *testing.T) {
		got := renderGitHubReleaseInfoTasks(
			"tool",
			"https://api.github.com/repos/cli/cli/releases/latest",
			"tool_latest_release",
		)
		assertContains(t, got, []string{
			"include_tasks: tasks/github-release-info.yml",
			"github_release_name: tool",
			"github_release_owner: cli",
			"github_release_repo: cli",
			"github_release_register: tool_latest_release",
		})
		assertNotContains(t, got, []string{"uri:", "Rate limit exceeded"})
	})

	t.Run("non-cacheable url uses uri module", func(t *testing.T) {
		got := renderGitHubReleaseInfoTasks(
			"tool",
			"https://example.com/version.json",
			"tool_latest_release",
		)
		assertContains(t, got, []string{
			"uri:",
			"url: https://example.com/version.json",
			"register: tool_latest_release",
			"Rate limit exceeded",
			"status_code: [200, 403]",
		})
		assertNotContains(t, got, []string{"github-release-info.yml"})
	})
}

func TestGhExtensionInstallMethod(t *testing.T) {
	method := GhExtensionInstallMethod{Repo: "jaeyeom/gh-repox"}
	if got := method.GetMethodType(); got != "gh-extension" {
		t.Errorf("GetMethodType() = %q, want gh-extension", got)
	}
	imports := method.GetImports()
	if len(imports) != 1 || imports[0].Playbook != "gh" {
		t.Errorf("GetImports() = %v, want gh", imports)
	}
	if got := method.extensionName(); got != "gh-repox" {
		t.Errorf("extensionName() = %q, want gh-repox", got)
	}

	got := method.RenderInstallTask("gh-repox")
	assertContains(t, got, []string{
		"gh_repox_installed",
		"gh extension list | grep -q 'gh-repox'",
		"gh extension install jaeyeom/gh-repox",
		"gh extension upgrade gh-repox",
		"when: gh_repox_installed.rc != 0",
		"when: gh_repox_installed.rc == 0",
	})
}

func TestAptRepoInstallMethod(t *testing.T) {
	method := AptRepoInstallMethod{
		Name:           "gcloud-cli",
		GPGKeyURL:      "https://packages.cloud.google.com/apt/doc/apt-key.gpg",
		GPGKeyPath:     "/etc/apt/trusted.gpg.d/google.gpg",
		RepoURL:        "https://packages.cloud.google.com/apt",
		RepoComponents: "main",
		Codename:       "cloud-sdk",
		Arch:           "amd64",
	}
	if got := method.GetMethodType(); got != "apt-repo" {
		t.Errorf("GetMethodType() = %q, want apt-repo", got)
	}

	setup := method.RenderSetupTasks("gcloud")
	assertContains(t, setup, []string{
		"gcloud_gpg_key",
		"https://packages.cloud.google.com/apt/doc/apt-key.gpg",
		"/etc/apt/trusted.gpg.d/google.gpg",
		"uris: \"https://packages.cloud.google.com/apt\"",
		"suites: \"cloud-sdk\"",
		"architectures: amd64",
		"- main",
		"deb822_repository:",
	})

	// Default when uses WhenDebianLike.
	assertContains(t, setup, []string{WhenDebianLike})

	got := method.RenderInstallTask("gcloud")
	assertContains(t, got, []string{
		"ansible.builtin.apt:",
		"name: gcloud-cli",
		"update_cache: yes",
	})

	block := method.RenderBlockInstallTask("gcloud")
	if strings.HasPrefix(strings.TrimSpace(block), "block:") {
		t.Errorf("RenderBlockInstallTask should strip leading block:\n%s", block)
	}
}

func TestAptRepoInstallMethodCustomWhenAndAutoCodename(t *testing.T) {
	method := AptRepoInstallMethod{
		Name:           "emacs",
		GPGKeyURL:      "https://example.com/key.gpg",
		GPGKeyPath:     "/etc/apt/trusted.gpg.d/emacs.gpg",
		RepoURL:        "https://example.com/apt",
		RepoComponents: "main contrib",
		// Codename empty → auto-detect
		When: WhenUbuntu,
	}
	setup := method.RenderSetupTasks("emacs")
	assertContains(t, setup, []string{
		WhenUbuntu,
		"{{ ansible_facts['distribution_release'] }}",
		"- main",
		"- contrib",
	})
	// Custom When should replace the default debian-like condition.
	// The Ubuntu when is more specific; ensure we don't only use WhenDebianLike alone as the only condition.
	if !strings.Contains(setup, WhenUbuntu) {
		t.Error("expected custom When condition")
	}
}

func TestInstallMethodInterfaceCompliance(t *testing.T) {
	// Compile-time style check that all concrete methods implement InstallMethod.
	methods := []InstallMethod{
		PackageInstallMethod{Name: "x"},
		BrewInstallMethod{Name: "x"},
		BrewCaskInstallMethod{Name: "x"},
		TermuxPkgInstallMethod{Name: "x"},
		PipInstallMethod{Name: "x"},
		GoInstallMethod{PkgPath: "x"},
		RustupComponentMethod{Name: "x"},
		CargoInstallMethod{Name: "x"},
		NpmInstallMethod{Name: "x"},
		NvmInstallMethod{Name: "x"},
		UvInstallMethod{Name: "x"},
		UbuntuPkgInstallMethod{Name: "x"},
		DebianPkgInstallMethod{Name: "x"},
		ShellInstallMethod{InstallCommand: "x"},
		GhExtensionInstallMethod{Repo: "a/b"},
		AptRepoInstallMethod{Name: "x", GPGKeyURL: "u", GPGKeyPath: "p", RepoURL: "r", RepoComponents: "main"},
	}
	for _, m := range methods {
		if m.GetMethodType() == "" {
			t.Errorf("%T GetMethodType() returned empty", m)
		}
		// Rendering should not panic for minimal configs.
		_ = m.RenderSetupTasks("cmd")
		_ = m.RenderInstallTask("cmd")
		_ = m.RenderBlockInstallTask("cmd")
		_ = m.GetImports()
	}
}
