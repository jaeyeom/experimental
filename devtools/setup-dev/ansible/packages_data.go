package main

var packages = []PackageData{
	{command: "7z", debianPkgName: "p7zip-full", termuxPkgName: "p7zip", brewPkgName: "p7zip"},
	{command: "ag", debianPkgName: "silversearcher-ag", termuxPkgName: "silversearcher-ag", brewPkgName: "the_silver_searcher"},
	{command: "clang-format"},
	{command: "cmake"},
	{command: "curl"},
	{command: "dart", debianPkgName: "dart", termuxPkgName: "dart", brewPkgName: "dart-sdk"},
	{command: "direnv"},
	{command: "emacs", UbuntuPPA: "ppa:ubuntuhandbook1/emacs", brewPkgName: "emacs-plus", brewTap: "d12frosted/emacs-plus", brewOptions: []string{"with-dbus", "with-imagemagick"}},
	{command: "ffmpegthumbnailer"},
	{command: "fzf"},
	{command: "gh"},
	{command: "git"},
	{command: "go", debianPkgName: "golang", termuxPkgName: "golang", brewPkgName: "go"},
	{command: "gpg", brewPkgName: "gnupg"},
	{command: "gpg-agent", Imports: []Import{{Playbook: "gpg"}}, brewPkgName: "gnupg"},
	{command: "grep"},
	{command: "htop"},
	{command: "jq"},
	{command: "keychain"},
	{command: "kotlinc", debianPkgName: "kotlin", termuxPkgName: "kotlin", brewPkgName: "kotlin"},
	{command: "libssl-dev", checkCommand: "pkg-config --exists libssl", termuxPkgName: "openssl", brewPkgName: "openssl"},
	{command: "libtool", debianPkgName: "libtool-bin"},
	{command: "libvterm", checkCommand: "pkg-config --exists vterm", debianPkgName: "libvterm-dev", termuxPkgName: "libvterm", brewPkgName: "libvterm"},
	{
		command:       "locate",
		debianPkgName: "plocate",
		termuxPkgName: "mlocate",
		brewPkgName:   "findutils",
		Suffix: `

    - name: Ensure locate DB is up-to-date on non-macOS systems
      command: updatedb
      become: "{{ 'no' if ansible_facts['env']['TERMUX_VERSION'] is defined else 'yes' }}"
      ignore_errors: true
      when: ` + WhenNotDarwin + `

    - name: Note about locate DB on macOS
      debug:
        msg: "On macOS, run 'sudo /usr/libexec/locate.updatedb' manually to update the locate database"
      when: ` + WhenDarwin,
	},
	{command: "make"},
	{command: "man", brewPkgName: "man-db"},
	{command: "mono", debianPkgName: "mono-devel", termuxPkgName: "mono"},
	{command: "notmuch", debianPkgName: "notmuch", termuxPkgName: "notmuch", Imports: []Import{{Playbook: "python3-notmuch2"}}},
	{command: "npm", debianPkgName: "npm", termuxPkgName: "nodejs", brewPkgName: "node"},
	{command: "openssl", debianPkgName: "openssl", termuxPkgName: "openssl-tool", brewPkgName: "openssl"},
	{command: "pandoc"},
	{command: "pass"},
	{command: "perl"},
	{command: "pkg-config"},
	{command: "poetry", debianPkgName: "python3-poetry", termuxPkgName: "python-poetry"},
	{command: "protoc", debianPkgName: "protobuf-compiler", termuxPkgName: "protobuf", brewPkgName: "protobuf"},
	{command: "psql", debianPkgName: "postgresql-client", termuxPkgName: "postgresql", brewPkgName: "postgresql"},
	{command: "python3-notmuch2", checkCommand: "python3 -c \"import notmuch2\"", debianPkgName: "python3-notmuch2", termuxPkgName: "notmuch", brewPkgName: "notmuch"},
	{command: "rg", debianPkgName: "ripgrep", termuxPkgName: "ripgrep", brewPkgName: "ripgrep"},
	{command: "sed", brewPkgName: "gsed"},
	{command: "shellcheck"},
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
	{
		command: "act",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "act"},
			PlatformDebianLike: GoInstallMethod{PkgPath: "github.com/nektos/act@latest"},
			PlatformTermux:     GoInstallMethod{PkgPath: "github.com/nektos/act@latest"},
		},
		Imports: []Import{{Playbook: "gh"}},
	},
	{
		command: "actionlint",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "actionlint"},
			PlatformDebianLike: GoInstallMethod{PkgPath: "github.com/rhysd/actionlint/cmd/actionlint@latest"},
			PlatformTermux:     GoInstallMethod{PkgPath: "github.com/rhysd/actionlint/cmd/actionlint@latest"},
		},
		Imports: []Import{{Playbook: "gh"}},
	},
	GoTool("bazel-affected-tests", "github.com/jaeyeom/experimental/devtools/bazel-affected-tests/cmd/bazel-affected-tests@latest"),
	{
		command: "biome",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "biome"},
			PlatformDebianLike: NvmInstallMethod{Name: "@biomejs/biome"},
			PlatformTermux:     NpmInstallMethod{Name: "@biomejs/biome"},
		},
		Imports: nil,
	},
	GoTool("buf", "github.com/bufbuild/buf/cmd/buf@latest"),
	GoTool("buildifier", "github.com/bazelbuild/buildtools/buildifier@latest"),
	GoTool("buildozer", "github.com/bazelbuild/buildtools/buildozer@latest"),
	{
		command: "cargo",
		platforms: map[PlatformName]InstallMethod{
			PlatformTermux: TermuxPkgInstallMethod{Name: "rust"},
		},
	},
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
		Imports: []Import{
			{Playbook: "pkg-config", When: WhenNotDarwin},
			{Playbook: "libssl-dev", When: WhenNotDarwin},
		},
	},
	{
		command: "cargo-outdated",
		platforms: map[PlatformName]InstallMethod{
			PlatformAll: CargoInstallMethod{Name: "cargo-outdated"},
		},
		Imports: nil,
	},
	{
		command: "check-jsonschema",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "check-jsonschema"},
			PlatformTermux:     UvInstallMethod{Name: "check-jsonschema"},
			PlatformDebianLike: UvInstallMethod{Name: "check-jsonschema"},
		},
		Imports: []Import{{Playbook: "uv", When: WhenNotDarwin}},
	},
	{
		command: "claude",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: ShellInstallMethod{
				InstallCommand: "curl -fsSL https://claude.ai/install.sh | bash",
			},
			PlatformDebianLike: ShellInstallMethod{
				InstallCommand: "curl -fsSL https://claude.ai/install.sh | bash",
			},
			PlatformTermux: NpmInstallMethod{Name: "@anthropic-ai/claude-code"},
		},
		Imports: []Import{
			{Playbook: "curl", When: WhenDarwin},
			{Playbook: "setup-npm", When: WhenTermux},
			{Playbook: "setup-nvm", When: WhenDebianLike},
		},
	},
	{
		command: "claudelytics",
		platforms: map[PlatformName]InstallMethod{
			PlatformAll: CargoInstallMethod{Name: "claudelytics"},
		},
		Imports: nil,
	},
	{
		command: "cloudflared",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: BrewInstallMethod{Name: "cloudflared"},
			PlatformDebianLike: AptRepoInstallMethod{
				Name:           "cloudflared",
				GPGKeyURL:      "https://pkg.cloudflare.com/cloudflare-main.gpg",
				GPGKeyPath:     "/usr/share/keyrings/cloudflare-main.gpg",
				RepoURL:        "https://pkg.cloudflare.com/cloudflared",
				RepoComponents: "main",
				Codename:       "any",
			},
		},
	},
	{
		command: "codex",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     NpmInstallMethod{Name: "@openai/codex"},
			PlatformDebianLike: NvmInstallMethod{Name: "@openai/codex"},
			PlatformTermux: ShellInstallMethod{
				InstallCommand: `
# Dependencies (rust, git, make) are ensured by imported playbooks

# Fail on any error
set -e

# Clone and build
# Verified repository: https://github.com/openai/codex (contains codex-rs)
git clone https://github.com/openai/codex $TMPDIR/codex
cd $TMPDIR/codex/codex-rs
cargo build --release
cp target/release/codex {{ user_bin_directory }}/
rm -rf $TMPDIR/codex
`,
				VersionCommand: "codex --version",
				VersionRegex:   "([0-9.]+)",
			},
		},
		Imports: []Import{
			{Playbook: "setup-npm", When: WhenNotTermux},
			{Playbook: "setup-cargo", When: WhenTermux},
			{Playbook: "setup-git", When: WhenTermux},
			{Playbook: "make", When: WhenTermux},
			{Playbook: "setup-user-bin-directory", When: WhenTermux},
		},
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
		command: "delta",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: BrewInstallMethod{Name: "git-delta"},
			PlatformDebian: DebianPkgInstallMethod{Name: "git-delta"},
			PlatformUbuntu: UbuntuPkgInstallMethod{Name: "git-delta"},
			PlatformTermux: TermuxPkgInstallMethod{Name: "git-delta"},
		},
	},
	{
		command: "detekt",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: BrewInstallMethod{Name: "detekt"},
			PlatformDebianLike: ShellInstallMethod{
				InstallCommand: `
version="{{ detekt_latest_release.json.tag_name | regex_replace('^v', '') }}"
curl -sSLO "https://github.com/detekt/detekt/releases/download/v${version}/detekt-cli-${version}.zip"
unzip -o "detekt-cli-${version}.zip"
rm "detekt-cli-${version}.zip"
rm -rf {{ user_bin_directory }}/../lib/detekt
mkdir -p {{ user_bin_directory }}/../lib
mv "detekt-cli-${version}" {{ user_bin_directory }}/../lib/detekt
ln -sf {{ user_bin_directory }}/../lib/detekt/bin/detekt-cli {{ user_bin_directory }}/detekt
`,
				VersionCommand:    "detekt --version",
				VersionRegex:      "([0-9.]+)",
				LatestVersionURL:  "https://api.github.com/repos/detekt/detekt/releases/latest",
				LatestVersionPath: "tag_name",
			},
		},
		Imports: []Import{
			{Playbook: "curl", When: WhenDebianLike},
			{Playbook: "unzip", When: WhenDebianLike},
			{Playbook: "setup-user-bin-directory", When: WhenDebianLike},
		},
	},
	{
		command:   "docker",
		platforms: nil, // No installation tasks - only conditional imports
		Imports: []Import{
			{Playbook: "setup-docker-lima", When: "ansible_facts['os_family'] == \"Darwin\""},
			{Playbook: "setup-docker-wrapper-udocker", When: "ansible_facts['env']['TERMUX_VERSION'] is defined"},
			{Playbook: "setup-docker-ce", When: "ansible_facts['env']['TERMUX_VERSION'] is not defined and ansible_facts['os_family'] != \"Darwin\""},
		},
	},
	{
		command: "e",
		platforms: map[PlatformName]InstallMethod{
			PlatformDebianLike: PackageInstallMethod{Name: "e-wrapper"},
			PlatformDarwin: ShellInstallMethod{
				InstallCommand: `curl -fsSL https://raw.githubusercontent.com/kilobyte/e/master/e -o {{ user_bin_directory }}/e && chmod 0700 {{ user_bin_directory }}/e`,
			},
			PlatformTermux: ShellInstallMethod{
				InstallCommand: `curl -fsSL https://raw.githubusercontent.com/kilobyte/e/master/e -o {{ user_bin_directory }}/e && chmod 0700 {{ user_bin_directory }}/e`,
			},
		},
		Imports: []Import{
			{Playbook: "setup-user-bin-directory", When: WhenNotDebianLike},
			{Playbook: "curl", When: WhenNotDebianLike},
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
		command: "eslint",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "eslint"},
			PlatformDebianLike: NvmInstallMethod{Name: "eslint"},
			PlatformTermux:     NpmInstallMethod{Name: "eslint"},
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
		command: "gcloud",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: BrewInstallMethod{Name: "google-cloud-sdk"},
			PlatformDebianLike: AptRepoInstallMethod{
				Name:           "google-cloud-cli",
				GPGKeyURL:      "https://packages.cloud.google.com/apt/doc/apt-key.gpg",
				GPGKeyPath:     "/etc/apt/trusted.gpg.d/google-cloud.gpg",
				RepoURL:        "https://packages.cloud.google.com/apt",
				RepoComponents: "main",
				Codename:       "cloud-sdk",
			},
			// Termux: Not officially supported by Google
		},
		Imports: nil,
	},
	{
		command: "gdformat",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     UvInstallMethod{Name: "gdtoolkit"},
			PlatformTermux:     UvInstallMethod{Name: "gdtoolkit"},
			PlatformDebianLike: UvInstallMethod{Name: "gdtoolkit"},
		},
		Imports: []Import{{Playbook: "uv"}},
	},
	{
		command: "gemini",
		platforms: map[PlatformName]InstallMethod{
			PlatformDebianLike: NvmInstallMethod{Name: "@google/gemini-cli"},
			PlatformTermux:     NpmInstallMethod{Name: "@google/gemini-cli"},
			PlatformDarwin:     BrewInstallMethod{Name: "gemini-cli"},
		},
		Imports: nil,
	},
	GoTool("gh-codeowners", "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-codeowners@latest", Import{Playbook: "gh"}),
	GoTool("gh-merge", "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-merge@latest", Import{Playbook: "gh"}),
	GoTool("gh-nudge", "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-nudge@latest", Import{Playbook: "gh"}),
	GoTool("gh-pr-review", "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-pr-review@latest", Import{Playbook: "gh"}),
	GoTool("gh-slack", "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-slack@latest", Import{Playbook: "gh"}),
	GoTool("gh-storage", "github.com/jaeyeom/experimental/devtools/gh-nudge/cmd/gh-storage@latest", Import{Playbook: "gh"}),
	GoTool("gherun", "github.com/jaeyeom/experimental/devtools/gherun/cmd/gherun@latest", Import{Playbook: "gh"}),
	{
		command: "githooks-cli",
		platforms: map[PlatformName]InstallMethod{
			PlatformAll: ShellInstallMethod{
				InstallCommand: `
if git hooks --version >/dev/null 2>&1; then
    echo "Githooks already installed, skipping."
else
    {{ playbook_dir }}/verified-run exec https://raw.githubusercontent.com/gabyx/githooks/main/scripts/install.sh -- -- --non-interactive
fi`,
			},
		},
		Imports: []Import{{Playbook: "git"}, {Playbook: "curl"}},
	},
	GoTool("godef", "github.com/rogpeppe/godef@latest"),
	GoTool("godoc", "golang.org/x/tools/cmd/godoc@latest"),
	GoTool("godoctor", "github.com/godoctor/godoctor@latest"),
	{
		command: "godot",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: BrewCaskInstallMethod{Name: "godot"},
		},
		Imports: nil,
	},
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
		command: "ktlint",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: BrewInstallMethod{Name: "ktlint"},
			PlatformDebianLike: ShellInstallMethod{
				InstallCommand:    `curl -sSL https://github.com/pinterest/ktlint/releases/download/{{ ktlint_latest_release.json.tag_name }}/ktlint -o {{ user_bin_directory }}/ktlint && chmod a+x {{ user_bin_directory }}/ktlint`,
				VersionCommand:    "ktlint --version",
				VersionRegex:      "([0-9.]+)",
				LatestVersionURL:  "https://api.github.com/repos/pinterest/ktlint/releases/latest",
				LatestVersionPath: "tag_name",
			},
			PlatformTermux: ShellInstallMethod{
				InstallCommand:    `curl -sSL https://github.com/pinterest/ktlint/releases/download/{{ ktlint_latest_release.json.tag_name }}/ktlint -o {{ user_bin_directory }}/ktlint && chmod a+x {{ user_bin_directory }}/ktlint`,
				VersionCommand:    "ktlint --version",
				VersionRegex:      "([0-9.]+)",
				LatestVersionURL:  "https://api.github.com/repos/pinterest/ktlint/releases/latest",
				LatestVersionPath: "tag_name",
			},
		},
		Imports: []Import{
			{Playbook: "curl", When: WhenNotDarwin},
			{Playbook: "setup-user-bin-directory", When: WhenNotDarwin},
		},
	},
	{
		command: "lcov",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "lcov"},
			PlatformDebianLike: PackageInstallMethod{Name: "lcov"},
			// termux: Skipped due to GNU Make 4.4.1 bugs with lcov's Makefile
		},
		Imports: nil,
	},
	{
		command: "mypy",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "mypy"},
			PlatformDebianLike: UvInstallMethod{Name: "mypy"},
			PlatformTermux:     UvInstallMethod{Name: "mypy"},
		},
		Imports: nil,
	},
	{
		command: "nvm",
		platforms: map[PlatformName]InstallMethod{
			PlatformAll: ShellInstallMethod{
				InstallCommand:    `curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/{{ nvm_latest_release.json.tag_name }}/install.sh | bash`,
				VersionCommand:    "nvm --version",
				VersionRegex:      "([0-9.]+)",
				LatestVersionURL:  "https://api.github.com/repos/nvm-sh/nvm/releases/latest",
				LatestVersionPath: "tag_name",
			},
		},
		Imports: []Import{{Playbook: "curl"}},
	},
	GoTool("org-lint", "github.com/jaeyeom/experimental/devtools/linters/cmd/org-lint@latest", Import{Playbook: "emacs"}),
	GoTool("oserrorsgodernize", "github.com/jaeyeom/godernize/oserrors/cmd/oserrorsgodernize@latest"),
	{
		command: "pkl",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: BrewInstallMethod{Name: "pkl"},
			PlatformDebianLike: ShellInstallMethod{
				InstallCommand:    `arch=$(uname -m) && os=$(uname -s | tr '[:upper:]' '[:lower:]') && if [ "$arch" = "x86_64" ]; then arch="amd64"; elif [ "$arch" = "arm64" ]; then arch="aarch64"; fi && curl -L -o ~/.local/bin/pkl "https://github.com/apple/pkl/releases/download/{{ pkl_latest_release.json.tag_name }}/pkl-${os}-${arch}" && chmod +x ~/.local/bin/pkl`,
				VersionCommand:    "pkl --version",
				VersionRegex:      "Pkl ([0-9.]+)",
				LatestVersionURL:  "https://api.github.com/repos/apple/pkl/releases/latest",
				LatestVersionPath: "tag_name",
			},
		},
		Imports: []Import{
			{Playbook: "setup-user-bin-directory", When: WhenDebianLike},
			{Playbook: "curl", When: WhenDebianLike},
		},
	},
	GoTool("pkl-gen-go", "github.com/apple/pkl-go/cmd/pkl-gen-go@latest", Import{Playbook: "pkl"}),
	{
		command: "pnpm",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "pnpm"},
			PlatformDebianLike: NvmInstallMethod{Name: "pnpm"},
			PlatformTermux:     NpmInstallMethod{Name: "pnpm"},
		},
		Imports: nil,
	},
	{
		command: "prettier",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "prettier"},
			PlatformDebianLike: NvmInstallMethod{Name: "prettier"},
			PlatformTermux:     NpmInstallMethod{Name: "prettier"},
		},
		Imports: nil,
	},
	GoTool("protoc-gen-go", "google.golang.org/protobuf/cmd/protoc-gen-go@latest", Import{Playbook: "protoc"}),
	GoTool("protoc-gen-go-grpc", "google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest", Import{Playbook: "protoc"}),
	GoTool("protolint", "github.com/yoheimuta/protolint/cmd/protolint@latest"),
	{
		command: "pylsp",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "python-lsp-server"},
			PlatformTermux:     UvInstallMethod{Name: "python-lsp-server"},
			PlatformDebianLike: UvInstallMethod{Name: "python-lsp-server"},
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
		command: "rust-analyzer",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     RustupComponentMethod{Name: "rust-analyzer"},
			PlatformDebianLike: RustupComponentMethod{Name: "rust-analyzer"},
			PlatformTermux:     TermuxPkgInstallMethod{Name: "rust-analyzer"},
		},
		Imports: nil,
	},
	{
		command: "rustup",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "rustup"},
			PlatformDebianLike: PackageInstallMethod{Name: "rustup"},
		},
	},
	{
		command: "semgrep",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "semgrep"},
			PlatformDebianLike: UvInstallMethod{Name: "semgrep"},
			// Termux can't install semgrep reliably.
		},
		Imports: nil,
	},
	GoTool("shfmt", "mvdan.cc/sh/v3/cmd/shfmt@latest"),
	{
		command: "sqlfluff",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "sqlfluff"},
			PlatformDebianLike: UvInstallMethod{Name: "sqlfluff"},
			PlatformTermux:     PipInstallMethod{Name: "sqlfluff"},
		},
		Imports: nil,
	},
	{
		command: "starship",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: BrewInstallMethod{Name: "starship"},
			PlatformTermux: TermuxPkgInstallMethod{Name: "starship"},
			PlatformDebianLike: ShellInstallMethod{
				InstallCommand:    "{{ playbook_dir }}/verified-run exec https://starship.rs/install.sh -- -y --bin-dir ~/.cargo/bin",
				VersionCommand:    "starship --version",
				VersionRegex:      "starship ([0-9.]+)",
				LatestVersionURL:  "https://api.github.com/repos/starship/starship/releases/latest",
				LatestVersionPath: "tag_name",
			},
		},
		Imports: []Import{{Playbook: "setup-rust"}, {Playbook: "curl"}},
	},
	{
		command: "stylelint",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "stylelint"},
			PlatformDebianLike: NvmInstallMethod{Name: "stylelint"},
			PlatformTermux:     NpmInstallMethod{Name: "stylelint"},
		},
		Imports: nil,
	},
	{
		command: "swiftformat",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: BrewInstallMethod{Name: "swiftformat"},
		},
	},
	{
		command: "swiftlint",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: BrewInstallMethod{Name: "swiftlint"},
		},
	},
	GoTool("task", "github.com/go-task/task/v3/cmd/task@latest"),
	{
		command: "tsc",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "typescript"},
			PlatformDebianLike: NvmInstallMethod{Name: "typescript"},
			PlatformTermux:     NpmInstallMethod{Name: "typescript"},
		},
		Imports: nil,
	},
	{
		command: "turbo",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "turbo"},
			PlatformDebianLike: NvmInstallMethod{Name: "turbo"},
			PlatformTermux:     NpmInstallMethod{Name: "turbo"},
		},
		Imports: nil,
	},
	{
		command: "ucm",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin: BrewInstallMethod{Name: "unison-language", Tap: "unisonweb/unison"},
			PlatformDebianLike: AptRepoInstallMethod{
				Name:           "unisonweb",
				GPGKeyURL:      "https://debian.unison-lang.org/public.gpg",
				GPGKeyPath:     "/etc/apt/trusted.gpg.d/unison-computing.gpg",
				RepoURL:        "https://debian.unison-lang.org/",
				RepoComponents: "main",
				Codename:       "trixie",
				Arch:           "amd64",
			},
		},
		Imports: nil,
	},
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
	{
		command: "wrangler",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     NpmInstallMethod{Name: "wrangler"},
			PlatformDebianLike: NvmInstallMethod{Name: "wrangler"},
			PlatformTermux:     NpmInstallMethod{Name: "wrangler"},
		},
		Imports: nil,
	},
	GoTool("yamlfmt", "github.com/google/yamlfmt/cmd/yamlfmt@latest"),
	{
		command: "yamllint",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "yamllint"},
			PlatformDebianLike: PackageInstallMethod{Name: "yamllint"},
			PlatformTermux:     PipInstallMethod{Name: "yamllint"},
		},
	},
}
