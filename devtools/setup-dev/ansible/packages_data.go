package main

var packages = []PackageData{
	{command: "7z", debianPkgName: "p7zip-full", termuxPkgName: "p7zip", brewPkgName: "p7zip"},
	{command: "ag", debianPkgName: "silversearcher-ag", termuxPkgName: "silversearcher-ag", brewPkgName: "the_silver_searcher"},
	{command: "buf"},
	{command: "clang-format"},
	{command: "cmake"},
	{command: "curl"},
	{command: "dart"},
	{command: "emacs", UbuntuPPA: "ppa:ubuntuhandbook1/emacs", brewPkgName: "emacs-plus", brewTap: "d12frosted/emacs-plus", brewOptions: []string{"with-native-comp", "with-dbus", "with-imagemagick"}},
	{command: "ffmpegthumbnailer"},
	{command: "fzf"},
	{command: "gh"},
	{command: "git"},
	{command: "gpg", brewPkgName: "gnupg"},
	{command: "gpg-agent", Imports: []Import{{Playbook: "gpg"}}, brewPkgName: "gnupg"},
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

    - name: Ensure locate DB is up-to-date on non-macOS systems
      command: updatedb
      become: "{{ 'no' if ansible_env.TERMUX_VERSION is defined else 'yes' }}"
      when: ansible_facts['os_family'] != "Darwin"

    - name: Note about locate DB on macOS
      debug:
        msg: "On macOS, run 'sudo /usr/libexec/locate.updatedb' manually to update the locate database"
      when: ansible_facts['os_family'] == "Darwin"`,
	},
	{command: "kotlinc", debianPkgName: "kotlin", termuxPkgName: "kotlin", brewPkgName: "kotlin"},
	{command: "libtool"},
	{command: "libvterm", debianPkgName: "libvterm-dev", termuxPkgName: "libvterm", brewPkgName: "libvterm"},
	{command: "man", brewPkgName: "man-db"},
	{command: "mono", debianPkgName: "mono-devel", termuxPkgName: "mono"},
	{command: "notmuch", debianPkgName: "notmuch", termuxPkgName: "notmuch", Imports: []Import{{Playbook: "python3-notmuch2"}}},
	{command: "npm", debianPkgName: "npm", termuxPkgName: "nodejs", brewPkgName: "node"},
	{command: "pandoc"},
	{command: "pass"},
	{command: "perl"},
	{command: "poetry", debianPkgName: "python3-poetry", termuxPkgName: "python-poetry"},
	{command: "protoc", debianPkgName: "protobuf-compiler", termuxPkgName: "protobuf", brewPkgName: "protobuf"},
	{command: "psql", debianPkgName: "postgresql-client", termuxPkgName: "postgresql", brewPkgName: "postgresql"},
	{command: "python3-notmuch2", debianPkgName: "python3-notmuch2", termuxPkgName: "notmuch", brewPkgName: "notmuch"},
	{command: "rg", debianPkgName: "ripgrep", termuxPkgName: "ripgrep", brewPkgName: "ripgrep"},
	{command: "sed", brewPkgName: "gsed"},
	{command: "ssh", debianPkgName: "openssh-client", termuxPkgName: "openssh", brewPkgName: "openssh"},
	{command: "sshpass"},
	{command: "tlmgr", debianPkgName: "texlive-lang-korean", termuxPkgName: "texlive-installer", brewPkgName: "mactex"},
	{command: "tmux"},
	{command: "udocker"},
	{command: "unzip"},
	{command: "vipsthumbnail", debianPkgName: "libvips-tools", termuxPkgName: "libvips", brewPkgName: "vips"},
	{command: "which"},
	{command: "zip"},
	{command: "zoxide"},
}

var platformSpecificTools = []PlatformSpecificTool{
	GoTool("github.com/jaeyeom/experimental/devtools/bazel-affected-tests/cmd/bazel-affected-tests@latest"),
	GoTool("github.com/bazelbuild/buildtools/buildifier@latest"),
	GoTool("github.com/bazelbuild/buildtools/buildozer@latest"),
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
		command: "claude",
		platforms: map[string]InstallMethod{
			"all": NpmInstallMethod{Name: "@anthropic-ai/claude-code"},
		},
		Imports: nil,
	},
	{
		command: "claudelytics",
		platforms: map[string]InstallMethod{
			"all": CargoInstallMethod{Name: "claudelytics"},
		},
		Imports: nil,
	},
	{
		command:   "docker",
		platforms: nil, // No installation tasks - only conditional imports
		Imports: []Import{
			{Playbook: "setup-docker-lima", When: "ansible_facts['os_family'] == \"Darwin\""},
			{Playbook: "setup-docker-wrapper-udocker", When: "ansible_env.TERMUX_VERSION is defined"},
			{Playbook: "setup-docker-ce", When: "ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != \"Darwin\""},
		},
	},
	{
		command: "emacs-lsp-booster",
		platforms: map[string]InstallMethod{
			"all": CargoInstallMethod{Name: "emacs-lsp-booster"},
		},
		Imports: nil,
	},
	{
		command: "fd",
		platforms: map[string]InstallMethod{
			"debian-like": CargoInstallMethod{Name: "fd-find"},
			"termux":      TermuxPkgInstallMethod{Name: "fd"},
			"darwin":      BrewInstallMethod{Name: "fd"},
		},
	},
	GoTool("github.com/davidrjenni/reftools/cmd/fillstruct@latest"),
	GoTool("github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-codeowners@latest"),
	GoTool("github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-merge@latest"),
	GoTool("github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-nudge@latest"),
	GoTool("github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-pr-review@latest"),
	GoTool("github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-slack@latest"),
	GoTool("github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-storage@latest"),
	GoTool("github.com/rogpeppe/godef@latest"),
	GoTool("golang.org/x/tools/cmd/godoc@latest"),
	GoTool("github.com/godoctor/godoctor@latest"),
	GoTool("mvdan.cc/gofumpt@latest"),
	GoTool("golang.org/x/tools/cmd/goimports@latest"),
	GoTool("github.com/fatih/gomodifytags@latest"),
	GoTool("github.com/uudashr/gopkgs/v2/cmd/gopkgs@latest"),
	GoTool("golang.org/x/tools/gopls@latest"),
	GoTool("golang.org/x/tools/cmd/gorename@latest"),
	GoTool("github.com/cweill/gotests/gotests@latest"),
	GoTool("github.com/fullstorydev/grpcui/cmd/grpcui@latest"),
	GoTool("golang.org/x/tools/cmd/guru@latest"),
	GoTool("github.com/gohugoio/hugo@latest"),
	GoTool("github.com/qeesung/image2ascii@latest"),
	GoTool("github.com/josharian/impl@latest"),
	GoTool("github.com/ankitpokhrel/jira-cli/cmd/jira@latest"),
	GoTool("github.com/jaeyeom/godernize/oserrors/cmd/oserrorsgodernize@latest"),
	GoTool("google.golang.org/protobuf/cmd/protoc-gen-go@latest", Import{Playbook: "protoc"}),
	GoTool("google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest", Import{Playbook: "protoc"}),
	GoTool("github.com/yoheimuta/protolint/cmd/protolint@latest"),
	{
		command: "protovalidate",
		platforms: map[string]InstallMethod{
			"all": PipInstallMethod{Name: "protovalidate"},
		},
		Imports: nil,
	},
	GoTool("github.com/jaeyeom/experimental/devtools/repo-sync/cmd/repo-sync@latest"),
	{
		command: "ruff",
		platforms: map[string]InstallMethod{
			"termux":      TermuxPkgInstallMethod{Name: "ruff"},
			"darwin":      BrewInstallMethod{Name: "ruff"},
			"debian-like": UvInstallMethod{Name: "ruff"},
		},
		Imports: nil,
	},
	{
		command: "starship",
		platforms: map[string]InstallMethod{
			"darwin": BrewInstallMethod{Name: "starship"},
			"termux": TermuxPkgInstallMethod{Name: "starship"},
			"debian-like": ShellInstallMethod{
				InstallCommand:    "curl -sS https://starship.rs/install.sh | sh -s -- -y",
				VersionCommand:    "starship --version",
				VersionRegex:      "starship ([0-9.]+)",
				LatestVersionURL:  "https://api.github.com/repos/starship/starship/releases/latest",
				LatestVersionPath: "tag_name",
			},
		},
		Imports: nil,
	},
	{
		command: "uv",
		platforms: map[string]InstallMethod{
			"darwin": BrewInstallMethod{Name: "uv"},
			"termux": TermuxPkgInstallMethod{Name: "uv"},
			"debian-like": ShellInstallMethod{
				InstallCommand: "curl -LsSf https://astral.sh/uv/install.sh | sh",
			},
		},
		Imports: nil,
	},
}
