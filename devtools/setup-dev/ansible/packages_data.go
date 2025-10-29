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
      when: ` + WhenNotDarwin + `

    - name: Note about locate DB on macOS
      debug:
        msg: "On macOS, run 'sudo /usr/libexec/locate.updatedb' manually to update the locate database"
      when: ` + WhenDarwin,
	},
	{command: "kotlinc", debianPkgName: "kotlin", termuxPkgName: "kotlin", brewPkgName: "kotlin"},
	{command: "libtool"},
	{command: "libvterm", debianPkgName: "libvterm-dev", termuxPkgName: "libvterm", brewPkgName: "libvterm"},
	{command: "make"},
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
	{command: "w3m"},
	{command: "wget"},
	{command: "which"},
	{command: "zip"},
	{command: "zoxide"},
}

var platformSpecificTools = []PlatformSpecificTool{
	GoTool("bazel-affected-tests", "github.com/jaeyeom/experimental/devtools/bazel-affected-tests/cmd/bazel-affected-tests@latest"),
	GoTool("buildifier", "github.com/bazelbuild/buildtools/buildifier@latest"),
	GoTool("buildozer", "github.com/bazelbuild/buildtools/buildozer@latest"),
	{
		command: "cargo-add",
		platforms: map[PlatformName]InstallMethod{
			PlatformAll: CargoInstallMethod{Name: "cargo-edit"},
		},
		Imports: nil,
	},
	{
		command: "cargo-install-update",
		platforms: map[PlatformName]InstallMethod{
			PlatformAll: CargoInstallMethod{Name: "cargo-update"},
		},
		Imports: nil,
	},
	{
		command: "cargo-outdated",
		platforms: map[PlatformName]InstallMethod{
			PlatformAll: CargoInstallMethod{Name: "cargo-outdated"},
		},
		Imports: nil,
	},
	{
		command: "claude",
		platforms: map[PlatformName]InstallMethod{
			PlatformAll: NpmInstallMethod{Name: "@anthropic-ai/claude-code"},
		},
		Imports: nil,
	},
	{
		command: "claudelytics",
		platforms: map[PlatformName]InstallMethod{
			PlatformAll: CargoInstallMethod{Name: "claudelytics"},
		},
		Imports: nil,
	},
	{
		command: "copier",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "copier"},
			PlatformTermux:     UvInstallMethod{Name: "copier"},
			PlatformDebianLike: UvInstallMethod{Name: "copier"},
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
		platforms: map[PlatformName]InstallMethod{
			PlatformAll: CargoInstallMethod{Name: "emacs-lsp-booster"},
		},
		Imports: nil,
	},
	{
		command: "fd",
		platforms: map[PlatformName]InstallMethod{
			PlatformDebianLike: CargoInstallMethod{Name: "fd-find"},
			PlatformTermux:     TermuxPkgInstallMethod{Name: "fd"},
			PlatformDarwin:     BrewInstallMethod{Name: "fd"},
		},
	},
	GoTool("fillstruct", "github.com/davidrjenni/reftools/cmd/fillstruct@latest"),
	{
		command: "gemini",
		platforms: map[PlatformName]InstallMethod{
			PlatformDebianLike: NpmInstallMethod{Name: "@google/gemini-cli"},
			PlatformTermux:     NpmInstallMethod{Name: "@google/gemini-cli"},
			PlatformDarwin:     BrewInstallMethod{Name: "gemini-cli"},
		},
		Imports: nil,
	},
	GoTool("gh-codeowners", "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-codeowners@latest"),
	GoTool("gh-merge", "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-merge@latest"),
	GoTool("gh-nudge", "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-nudge@latest"),
	GoTool("gh-pr-review", "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-pr-review@latest"),
	GoTool("gh-slack", "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-slack@latest"),
	GoTool("gh-storage", "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-storage@latest"),
	GoTool("godef", "github.com/rogpeppe/godef@latest"),
	GoTool("godoc", "golang.org/x/tools/cmd/godoc@latest"),
	GoTool("godoctor", "github.com/godoctor/godoctor@latest"),
	GoTool("gofumpt", "mvdan.cc/gofumpt@latest"),
	GoTool("goimports", "golang.org/x/tools/cmd/goimports@latest"),
	GoTool("gomodifytags", "github.com/fatih/gomodifytags@latest"),
	GoTool("gopkgs", "github.com/uudashr/gopkgs/v2/cmd/gopkgs@latest"),
	GoTool("gopls", "golang.org/x/tools/gopls@latest"),
	GoTool("gorename", "golang.org/x/tools/cmd/gorename@latest"),
	GoTool("gotests", "github.com/cweill/gotests/gotests@latest"),
	GoTool("grpcui", "github.com/fullstorydev/grpcui/cmd/grpcui@latest"),
	GoTool("guru", "golang.org/x/tools/cmd/guru@latest"),
	GoTool("hugo", "github.com/gohugoio/hugo@latest"),
	GoTool("image2ascii", "github.com/qeesung/image2ascii@latest"),
	GoTool("impl", "github.com/josharian/impl@latest"),
	GoTool("jira", "github.com/ankitpokhrel/jira-cli/cmd/jira@latest"),
	{
		command: "lcov",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "lcov"},
			PlatformDebianLike: PackageInstallMethod{Name: "lcov"},
			// termux: Skipped due to GNU Make 4.4.1 bugs with lcov's Makefile
		},
		Imports: nil,
	},
	GoTool("oserrorsgodernize", "github.com/jaeyeom/godernize/oserrors/cmd/oserrorsgodernize@latest"),
	GoTool("protoc-gen-go", "google.golang.org/protobuf/cmd/protoc-gen-go@latest", Import{Playbook: "protoc"}),
	GoTool("protoc-gen-go-grpc", "google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest", Import{Playbook: "protoc"}),
	GoTool("protolint", "github.com/yoheimuta/protolint/cmd/protolint@latest"),
	{
		command: "protovalidate",
		platforms: map[PlatformName]InstallMethod{
			PlatformAll: PipInstallMethod{Name: "protovalidate"},
		},
		Imports: nil,
	},
	GoTool("repo-sync", "github.com/jaeyeom/experimental/devtools/repo-sync/cmd/repo-sync@latest"),
	{
		command: "ruff",
		platforms: map[PlatformName]InstallMethod{
			PlatformTermux:     TermuxPkgInstallMethod{Name: "ruff"},
			PlatformDarwin:     BrewInstallMethod{Name: "ruff"},
			PlatformDebianLike: UvInstallMethod{Name: "ruff"},
		},
		Imports: nil,
	},
	{
		command: "semgrep",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "semgrep"},
			PlatformTermux:     PipInstallMethod{Name: "semgrep"},
			PlatformDebianLike: PipInstallMethod{Name: "semgrep"},
		},
		Imports: nil,
	},
	{
		command: "starship",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: BrewInstallMethod{Name: "starship"},
			PlatformTermux: TermuxPkgInstallMethod{Name: "starship"},
			PlatformDebianLike: ShellInstallMethod{
				InstallCommand:    "curl -sS https://starship.rs/install.sh | sh -s -- -y --bin-dir ~/.cargo/bin",
				VersionCommand:    "starship --version",
				VersionRegex:      "starship ([0-9.]+)",
				LatestVersionURL:  "https://api.github.com/repos/starship/starship/releases/latest",
				LatestVersionPath: "tag_name",
			},
		},
		Imports: []Import{{Playbook: "rustc"}, {Playbook: "curl"}},
	},
	GoTool("task", "github.com/go-task/task/v3/cmd/task@latest"),
	{
		command: "uv",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: BrewInstallMethod{Name: "uv"},
			PlatformTermux: TermuxPkgInstallMethod{Name: "uv"},
			PlatformDebianLike: ShellInstallMethod{
				InstallCommand: "curl -LsSf https://astral.sh/uv/install.sh | sh",
			},
		},
		Imports: nil,
	},
}
