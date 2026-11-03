package main

import (
	"bytes"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"runtime"
	"sort"
	"strings"
	"testing"

	// Renders the Ansible YAML template under test, not HTML.
	// nosemgrep: import-text-template
	"text/template"
)

func TestPackagesSorted(t *testing.T) {
	if !sort.SliceIsSorted(packages, func(i, j int) bool {
		return packages[i].command < packages[j].command
	}) {
		t.Errorf("packages slice is not sorted by command name")
		for i := 1; i < len(packages); i++ {
			if packages[i].command < packages[i-1].command {
				t.Errorf("Package '%s' is out of order (comes after '%s')", packages[i].command, packages[i-1].command)
			}
		}
	}
}

func TestNoNpmOnDebianLike(t *testing.T) {
	for _, tool := range platformSpecificTools {
		method, ok := tool.platforms[PlatformDebianLike]
		if !ok {
			continue
		}
		if _, isNpm := method.(NpmInstallMethod); isNpm {
			t.Errorf("platform-specific tool %q uses NpmInstallMethod on PlatformDebianLike; use NvmInstallMethod instead", tool.command)
		}
	}
}

func TestGitHubReleaseCacheKey(t *testing.T) {
	tests := []struct {
		name       string
		releaseURL string
		want       string
	}{
		{
			name:       "GitHub latest release URL",
			releaseURL: "https://api.github.com/repos/owner/repo/releases/latest",
			want:       "owner__repo",
		},
		{
			name:       "non-GitHub URL",
			releaseURL: "https://example.com/repos/owner/repo/releases/latest",
			want:       "",
		},
		{
			name:       "GitHub non-latest release URL",
			releaseURL: "https://api.github.com/repos/owner/repo/releases/tags/v1.0.0",
			want:       "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := githubReleaseCacheKey(tt.releaseURL); got != tt.want {
				t.Errorf("githubReleaseCacheKey(%q) = %q, want %q", tt.releaseURL, got, tt.want)
			}
		})
	}
}

func TestHashicorpAptRepoName(t *testing.T) {
	var repoNames []string
	for _, tool := range platformSpecificTools {
		if tool.command != "terraform" && tool.command != "terraform-ls" {
			continue
		}
		method, ok := tool.platforms[PlatformDebianLike].(AptRepoInstallMethod)
		if !ok {
			t.Fatalf("%s debian method is %T, want AptRepoInstallMethod", tool.command, tool.platforms[PlatformDebianLike])
		}
		if method.RepoName != "hashicorp" {
			t.Errorf("%s RepoName = %q, want hashicorp", tool.command, method.RepoName)
		}
		if method.Name != tool.command {
			t.Errorf("%s apt package Name = %q, want %q", tool.command, method.Name, tool.command)
		}
		repoNames = append(repoNames, method.RepoName)
	}
	if len(repoNames) != 2 {
		t.Fatalf("found %d HashiCorp apt tools, want 2", len(repoNames))
	}
	if repoNames[0] != repoNames[1] {
		t.Errorf("terraform and terraform-ls RepoName differ: %q vs %q", repoNames[0], repoNames[1])
	}
}

func TestPlatformSpecificToolsSorted(t *testing.T) {
	if !sort.SliceIsSorted(platformSpecificTools, func(i, j int) bool {
		return platformSpecificTools[i].command < platformSpecificTools[j].command
	}) {
		t.Errorf("platformSpecificTools slice is not sorted by command name")
		for i := 1; i < len(platformSpecificTools); i++ {
			if platformSpecificTools[i].command < platformSpecificTools[i-1].command {
				t.Errorf("Tool '%s' is out of order (comes after '%s')", platformSpecificTools[i].command, platformSpecificTools[i-1].command)
			}
		}
	}
}

func TestGetAllImports(t *testing.T) {
	tests := []struct {
		name string
		tool PlatformSpecificTool
		want []Import
	}{
		{
			name: "all-platform method produces unconditional imports",
			tool: PlatformSpecificTool{
				command: "mytool",
				platforms: map[PlatformName]InstallMethod{
					PlatformAll: GoInstallMethod{PkgPath: "example.com/mytool@latest"},
				},
			},
			want: []Import{
				{Playbook: "setup-user-go-bin-directory"},
			},
		},
		{
			name: "per-platform methods produce conditional imports",
			tool: PlatformSpecificTool{
				command: "prettier",
				platforms: map[PlatformName]InstallMethod{
					PlatformDarwin:     BrewInstallMethod{Name: "prettier"},
					PlatformTermux:     NpmInstallMethod{Name: "prettier"},
					PlatformDebianLike: NvmInstallMethod{Name: "prettier"},
				},
			},
			want: []Import{
				{Playbook: "setup-npm", When: WhenTermux},
				{Playbook: "setup-nvm", When: WhenDebianLike},
			},
		},
		{
			name: "explicit unconditional import stays unconditional",
			tool: PlatformSpecificTool{
				command: "biome",
				platforms: map[PlatformName]InstallMethod{
					PlatformDarwin:     BrewInstallMethod{Name: "biome"},
					PlatformTermux:     NpmInstallMethod{Name: "@biomejs/biome"},
					PlatformDebianLike: NvmInstallMethod{Name: "@biomejs/biome"},
				},
				Imports: []Import{{Playbook: "setup-npm"}},
			},
			want: []Import{
				{Playbook: "setup-npm"},
				{Playbook: "setup-nvm", When: WhenDebianLike},
			},
		},
		{
			name: "explicit conditional imports preserved",
			tool: PlatformSpecificTool{
				command: "claude",
				platforms: map[PlatformName]InstallMethod{
					PlatformDarwin:     ShellInstallMethod{InstallCommand: "curl | bash"},
					PlatformTermux:     NpmInstallMethod{Name: "@anthropic-ai/claude-code"},
					PlatformDebianLike: ShellInstallMethod{InstallCommand: "curl | bash"},
				},
				Imports: []Import{
					{Playbook: "curl", When: WhenDarwin},
					{Playbook: "setup-npm", When: WhenTermux},
					{Playbook: "setup-nvm", When: WhenDebianLike},
				},
			},
			want: []Import{
				{Playbook: "curl", When: WhenDarwin},
				{Playbook: "setup-npm", When: WhenTermux},
				{Playbook: "setup-nvm", When: WhenDebianLike},
			},
		},
		{
			name: "same import from multiple platforms combines with or",
			tool: PlatformSpecificTool{
				command: "act",
				platforms: map[PlatformName]InstallMethod{
					PlatformDarwin:     BrewInstallMethod{Name: "act"},
					PlatformTermux:     GoInstallMethod{PkgPath: "github.com/nektos/act@latest"},
					PlatformDebianLike: GoInstallMethod{PkgPath: "github.com/nektos/act@latest"},
				},
				Imports: []Import{{Playbook: "gh"}},
			},
			want: []Import{
				{Playbook: "gh"},
				{Playbook: "setup-user-go-bin-directory", When: "(" + WhenTermux + ") or (" + WhenDebianLike + ")"},
			},
		},
		{
			name: "explicit conditional import not widened by methods",
			tool: PlatformSpecificTool{
				command: "check-jsonschema",
				platforms: map[PlatformName]InstallMethod{
					PlatformDarwin:     BrewInstallMethod{Name: "check-jsonschema"},
					PlatformTermux:     UvInstallMethod{Name: "check-jsonschema"},
					PlatformDebianLike: UvInstallMethod{Name: "check-jsonschema"},
				},
				Imports: []Import{{Playbook: "uv", When: WhenNotDarwin}},
			},
			want: []Import{
				{Playbook: "uv", When: WhenNotDarwin},
			},
		},
		{
			name: "self-import is excluded",
			tool: PlatformSpecificTool{
				command: "setup-npm",
				platforms: map[PlatformName]InstallMethod{
					PlatformAll: NpmInstallMethod{Name: "setup-npm"},
				},
			},
			want: nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.tool.GetAllImports()
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("GetAllImports() mismatch:\n  got:  %s\n  want: %s", formatImports(got), formatImports(tt.want))
			}
		})
	}
}

func TestHasConditionalImports(t *testing.T) {
	tests := []struct {
		name string
		tool PlatformSpecificTool
		want bool
	}{
		{
			name: "no imports",
			tool: PlatformSpecificTool{
				command: "mytool",
				platforms: map[PlatformName]InstallMethod{
					PlatformDarwin: BrewInstallMethod{Name: "mytool"},
				},
			},
			want: false,
		},
		{
			name: "only unconditional imports",
			tool: PlatformSpecificTool{
				command: "mytool",
				platforms: map[PlatformName]InstallMethod{
					PlatformAll: GoInstallMethod{PkgPath: "example.com/mytool@latest"},
				},
			},
			want: false,
		},
		{
			name: "has conditional imports",
			tool: PlatformSpecificTool{
				command: "prettier",
				platforms: map[PlatformName]InstallMethod{
					PlatformDarwin:     BrewInstallMethod{Name: "prettier"},
					PlatformTermux:     NpmInstallMethod{Name: "prettier"},
					PlatformDebianLike: NvmInstallMethod{Name: "prettier"},
				},
			},
			want: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.tool.HasConditionalImports(); got != tt.want {
				t.Errorf("HasConditionalImports() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestBuildCommandAptInfo(t *testing.T) {
	info := buildCommandAptInfo()
	if len(info) == 0 {
		t.Fatal("expected at least one entry in apt info map")
	}

	// Spot-check: "emacs" should have an AptRepo entry (Ubuntu PPA with modern GPG key).
	emacsInfo, ok := info["emacs"]
	if !ok {
		t.Fatal("expected 'emacs' in apt info map")
	}
	if emacsInfo.PackageName != "emacs" {
		t.Errorf("emacs PackageName = %q, want %q", emacsInfo.PackageName, "emacs")
	}
	if emacsInfo.SourceType != AptSourceAptRepo {
		t.Errorf("emacs SourceType = %d, want AptSourceAptRepo (%d)", emacsInfo.SourceType, AptSourceAptRepo)
	}
	if emacsInfo.AptRepo == nil {
		t.Fatal("emacs AptRepo should not be nil")
	}

	// Spot-check: "gcloud" should have an AptRepo entry.
	gcloudInfo, ok := info["gcloud"]
	if !ok {
		t.Fatal("expected 'gcloud' in apt info map")
	}
	if gcloudInfo.SourceType != AptSourceAptRepo {
		t.Errorf("gcloud SourceType = %d, want AptSourceAptRepo (%d)", gcloudInfo.SourceType, AptSourceAptRepo)
	}
	if gcloudInfo.AptRepo == nil {
		t.Fatal("gcloud AptRepo should not be nil")
	}

	// Spot-check: "curl" should have SourceType None.
	curlInfo, ok := info["curl"]
	if !ok {
		t.Fatal("expected 'curl' in apt info map")
	}
	if curlInfo.SourceType != AptSourceNone {
		t.Errorf("curl SourceType = %d, want AptSourceNone (%d)", curlInfo.SourceType, AptSourceNone)
	}

	// Spot-check: Go-only tools should NOT appear.
	for _, goTool := range []string{"gopls", "shfmt", "hugo"} {
		if _, ok := info[goTool]; ok {
			t.Errorf("go-installed tool %q should not appear in apt info map", goTool)
		}
	}

	// Verify every entry has a non-empty PackageName.
	for cmd, entry := range info {
		if entry.PackageName == "" {
			t.Errorf("entry for %q has empty PackageName", cmd)
		}
	}
}

func sliceContains(s []string, v string) bool {
	for _, item := range s {
		if item == v {
			return true
		}
	}
	return false
}

func TestDiscoverProfiles(t *testing.T) {
	profiles, err := discoverProfiles()
	if err != nil {
		t.Fatalf("discoverProfiles() error: %v", err)
	}
	if len(profiles) == 0 {
		t.Fatal("expected at least one profile")
	}
	// Verify sorted.
	if !sort.StringsAreSorted(profiles) {
		t.Error("profiles should be sorted")
	}
	// Spot-check known profiles.
	for _, name := range []string{"emacs-daily", "minimal"} {
		if !sliceContains(profiles, name) {
			t.Errorf("expected profile %q not found", name)
		}
	}
}

func TestGetProfileAptPackages(t *testing.T) {
	pkgs, err := getProfileAptPackages("minimal")
	if err != nil {
		t.Fatalf("getProfileAptPackages(minimal) error: %v", err)
	}
	// Minimal profile should produce some packages (from setup-devtools, etc.).
	if len(pkgs) == 0 {
		t.Fatal("expected at least one package for minimal profile")
	}
	// Verify sorted by PackageName.
	if !sort.SliceIsSorted(pkgs, func(i, j int) bool {
		return pkgs[i].PackageName < pkgs[j].PackageName
	}) {
		t.Error("profile packages should be sorted by PackageName")
	}
	// Verify no duplicates.
	for i := 1; i < len(pkgs); i++ {
		if pkgs[i].PackageName == pkgs[i-1].PackageName {
			t.Errorf("duplicate package: %s", pkgs[i].PackageName)
		}
	}
}

func TestProfileAptPackagesSubsetOfAll(t *testing.T) {
	info := buildCommandAptInfo()
	allSet := make(map[string]bool)
	for _, entry := range info {
		allSet[entry.PackageName] = true
	}

	profiles, err := discoverProfiles()
	if err != nil {
		t.Fatalf("discoverProfiles() error: %v", err)
	}
	for _, profile := range profiles {
		pkgs, err := getProfileAptPackages(profile)
		if err != nil {
			t.Errorf("getProfileAptPackages(%q) error: %v", profile, err)
			continue
		}
		for _, p := range pkgs {
			if !allSet[p.PackageName] {
				t.Errorf("profile %q package %q is not in the global apt packages list", profile, p.PackageName)
			}
		}
	}
}

func TestGetPostImports(t *testing.T) {
	tests := []struct {
		name     string
		tool     PlatformSpecificTool
		wantPre  []Import
		wantPost []Import
	}{
		{
			name: "post-install import excluded from GetAllImports",
			tool: PlatformSpecificTool{
				command: "mytool",
				platforms: map[PlatformName]InstallMethod{
					PlatformTermux: NpmInstallMethod{Name: "mytool"},
				},
				Imports: []Import{
					{Playbook: "setup-npm", When: WhenTermux},
					{Playbook: "setup-mytool-wrapper", When: WhenTermux, PostInstall: true},
				},
			},
			wantPre:  []Import{{Playbook: "setup-npm", When: WhenTermux}},
			wantPost: []Import{{Playbook: "setup-mytool-wrapper", When: WhenTermux}},
		},
		{
			name: "no post-install imports returns nil",
			tool: PlatformSpecificTool{
				command: "bar",
				platforms: map[PlatformName]InstallMethod{
					PlatformAll: GoInstallMethod{PkgPath: "example.com/bar@latest"},
				},
				Imports: []Import{
					{Playbook: "dep"},
				},
			},
			wantPre:  []Import{{Playbook: "dep"}, {Playbook: "setup-user-go-bin-directory"}},
			wantPost: nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotPre := tt.tool.GetAllImports()
			if !reflect.DeepEqual(gotPre, tt.wantPre) {
				t.Errorf("GetAllImports() mismatch:\n  got:  %s\n  want: %s", formatImports(gotPre), formatImports(tt.wantPre))
			}
			gotPost := tt.tool.GetPostImports()
			if !reflect.DeepEqual(gotPost, tt.wantPost) {
				t.Errorf("GetPostImports() mismatch:\n  got:  %s\n  want: %s", formatImports(gotPost), formatImports(tt.wantPost))
			}
		})
	}
}

func findPlatformSpecificTool(t *testing.T, command string) PlatformSpecificTool {
	t.Helper()
	for _, candidate := range platformSpecificTools {
		if candidate.command == command {
			return candidate
		}
	}
	t.Fatalf("%s is not registered in platformSpecificTools", command)
	return PlatformSpecificTool{}
}

func TestGrokTermuxWrapper(t *testing.T) {
	tool := findPlatformSpecificTool(t, "grok")
	got := tool.GetPostImports()
	want := []Import{{Playbook: "setup-grok-termux", When: WhenTermux}}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("GetPostImports() = %s, want %s", formatImports(got), formatImports(want))
	}
	for _, imp := range tool.GetAllImports() {
		if imp.Playbook == "setup-grok-termux" {
			t.Errorf("setup-grok-termux should be post-install only, found in GetAllImports: %s", formatImports(tool.GetAllImports()))
		}
	}
}

func TestAgyCLI(t *testing.T) {
	tool := findPlatformSpecificTool(t, "agy")
	method, ok := tool.platforms[PlatformAll].(ShellInstallMethod)
	if !ok {
		t.Fatalf("agy method = %T, want ShellInstallMethod", tool.platforms[PlatformAll])
	}
	wantInstall := "{{ playbook_dir }}/verified-run exec https://antigravity.google/cli/install.sh"
	if method.InstallCommand != wantInstall {
		t.Errorf("agy InstallCommand = %q, want %q", method.InstallCommand, wantInstall)
	}
	wantPATH := `"{{ user_bin_directory }}:{{ ansible_facts['env']['PATH'] }}"`
	if got := method.Environment["PATH"]; got != wantPATH {
		t.Errorf("agy PATH = %q, want %q", got, wantPATH)
	}
	wantImports := []Import{
		{Playbook: "curl"},
		{Playbook: "setup-user-bin-directory"},
	}
	if !reflect.DeepEqual(tool.Imports, wantImports) {
		t.Errorf("agy Imports = %s, want %s", formatImports(tool.Imports), formatImports(wantImports))
	}
}

func TestAsideCLI(t *testing.T) {
	tool := findPlatformSpecificTool(t, "aside")
	if _, ok := tool.platforms[PlatformTermux]; ok {
		t.Error("aside should omit Termux; official installer is linux/macOS only")
	}
	wantInstall := "{{ playbook_dir }}/verified-run exec https://releases.aside.com/install.sh"
	wantPATH := `"{{ user_bin_directory }}:{{ ansible_facts['env']['PATH'] }}"`
	for _, platform := range []PlatformName{PlatformDarwin, PlatformDebianLike} {
		method, ok := tool.platforms[platform].(ShellInstallMethod)
		if !ok {
			t.Errorf("aside %s method = %T, want ShellInstallMethod", platform, tool.platforms[platform])
			continue
		}
		if method.InstallCommand != wantInstall {
			t.Errorf("aside %s InstallCommand = %q, want %q", platform, method.InstallCommand, wantInstall)
		}
		if got := method.Environment["PATH"]; got != wantPATH {
			t.Errorf("aside %s PATH = %q, want %q", platform, got, wantPATH)
		}
	}
	wantImports := []Import{
		{Playbook: "curl"},
		{Playbook: "setup-user-bin-directory"},
		{Playbook: "libatomic1", When: WhenDebianLike},
	}
	if !reflect.DeepEqual(tool.Imports, wantImports) {
		t.Errorf("aside Imports = %s, want %s", formatImports(tool.Imports), formatImports(wantImports))
	}
}

func TestGhPlaybookInstallsGhStackExtension(t *testing.T) {
	var pkg PackageData
	found := false
	for _, candidate := range packages {
		if candidate.command == "gh" {
			pkg = candidate
			found = true
			break
		}
	}
	if !found {
		t.Fatal("gh is not registered in packages")
	}

	tmpl := template.Must(template.New("packages").Parse(packagesTemplate))
	var buf bytes.Buffer
	if err := tmpl.Execute(&buf, pkg); err != nil {
		t.Fatalf("render gh playbook: %v", err)
	}
	got := buf.String()
	for _, want := range []string{
		"gh extension install github/gh-stack",
		"gh extension upgrade gh-stack",
		"gh extension list | grep -q 'gh-stack'",
		"when: " + WhenNotTermux,
	} {
		if !strings.Contains(got, want) {
			t.Errorf("gh playbook missing %q", want)
		}
	}
	// The extension tasks must sit inside the non-Termux block, after gh is installed.
	installGh := strings.Index(got, "name: Install gh on Termux")
	extension := strings.Index(got, "gh extension install github/gh-stack")
	if installGh < 0 || extension < 0 || extension < installGh {
		t.Errorf("gh-stack install should follow the gh package install tasks")
	}
}

func TestGhosttyCaskSkipsExistingApp(t *testing.T) {
	tool := findPlatformSpecificTool(t, "ghostty")
	if len(tool.platforms) != 1 {
		t.Errorf("ghostty platforms = %d, want darwin only", len(tool.platforms))
	}
	method, ok := tool.platforms[PlatformDarwin].(BrewCaskInstallMethod)
	if !ok {
		t.Fatalf("ghostty darwin method = %T, want BrewCaskInstallMethod", tool.platforms[PlatformDarwin])
	}
	if method.Name != "ghostty" {
		t.Errorf("ghostty cask name = %q, want ghostty", method.Name)
	}
	for _, probe := range []string{
		"/Applications/Ghostty.app",
		"~/Applications/Ghostty.app",
		"command -v ghostty",
	} {
		if !strings.Contains(method.CheckCommand, probe) {
			t.Errorf("ghostty CheckCommand = %q, want it to include %q", method.CheckCommand, probe)
		}
	}
	task := method.RenderInstallTask("ghostty")
	checkAt := strings.Index(task, "Check if ghostty is installed")
	installAt := strings.Index(task, "community.general.homebrew_cask:")
	if checkAt < 0 || installAt < 0 || checkAt > installAt {
		t.Errorf("ghostty install task must probe for an existing app before brew, got:\n%s", task)
	}
	if !strings.Contains(task, "state: present") {
		t.Errorf("ghostty install task must use state present, got:\n%s", task)
	}
}

func TestLibatomic1Package(t *testing.T) {
	var pkg PackageData
	found := false
	for _, candidate := range packages {
		if candidate.command == "libatomic1" {
			pkg = candidate
			found = true
			break
		}
	}
	if !found {
		t.Fatal("libatomic1 is not registered in packages")
	}
	if pkg.DebianPkgName() != "libatomic1" {
		t.Errorf("libatomic1 DebianPkgName = %q, want libatomic1", pkg.DebianPkgName())
	}
	if !strings.Contains(pkg.CheckCommand(), "libatomic") {
		t.Errorf("libatomic1 CheckCommand = %q, want a libatomic probe", pkg.CheckCommand())
	}
}

func TestPrsyncGoTool(t *testing.T) {
	tool := findPlatformSpecificTool(t, "prsync")
	method, ok := tool.platforms[PlatformAll].(GoInstallMethod)
	if !ok {
		t.Fatalf("prsync PlatformAll method = %T, want GoInstallMethod", tool.platforms[PlatformAll])
	}
	wantPath := "github.com/jaeyeom/experimental/devtools/prsync/cmd/prsync@latest"
	if method.PkgPath != wantPath {
		t.Errorf("prsync PkgPath = %q, want %q", method.PkgPath, wantPath)
	}
	// herdr has no Termux build. prsync still installs there and warns when
	// herdr is absent; the import itself must not run on Termux.
	wantImports := []Import{{Playbook: "gh"}, {Playbook: "herdr", When: WhenNotTermux}}
	if !reflect.DeepEqual(tool.Imports, wantImports) {
		t.Errorf("prsync Imports = %s, want %s", formatImports(tool.Imports), formatImports(wantImports))
	}
	wantAll := []Import{
		{Playbook: "gh"},
		{Playbook: "herdr", When: WhenNotTermux},
		{Playbook: "setup-user-go-bin-directory"},
	}
	if got := tool.GetAllImports(); !reflect.DeepEqual(got, wantAll) {
		t.Errorf("prsync GetAllImports() = %s, want %s", formatImports(got), formatImports(wantAll))
	}
}

func TestTflintReleaseArchive(t *testing.T) {
	tool := findPlatformSpecificTool(t, "tflint")
	if _, ok := tool.platforms[PlatformTermux]; ok {
		t.Error("tflint should omit Termux; upstream publishes no Android archive")
	}
	if _, ok := tool.platforms[PlatformDarwin].(BrewInstallMethod); ok {
		t.Error("tflint darwin method must not be the removed homebrew-core formula")
	}

	wantImports := []Import{
		{Playbook: "curl", When: WhenNotTermux},
		{Playbook: "unzip", When: WhenNotTermux},
		{Playbook: "setup-user-bin-directory", When: WhenNotTermux},
	}
	if !reflect.DeepEqual(tool.Imports, wantImports) {
		t.Errorf("tflint Imports = %s, want %s", formatImports(tool.Imports), formatImports(wantImports))
	}

	for _, platform := range []PlatformName{PlatformDarwin, PlatformDebianLike} {
		method, ok := tool.platforms[platform].(ShellInstallMethod)
		if !ok {
			t.Errorf("tflint %s method = %T, want ShellInstallMethod", platform, tool.platforms[platform])
			continue
		}
		cmd := method.InstallCommand
		if strings.Contains(cmd, "install_linux.sh") || strings.Contains(cmd, "| bash") || strings.Contains(cmd, "| sh") {
			t.Errorf("tflint %s InstallCommand still runs a remote shell script: %q", platform, cmd)
		}
		if !strings.Contains(cmd, "curl -fsSL") {
			t.Errorf("tflint %s InstallCommand must fail on HTTP errors, got %q", platform, cmd)
		}
		if !strings.Contains(cmd, "{{ user_bin_directory }}") {
			t.Errorf("tflint %s InstallCommand must install into user_bin_directory", platform)
		}
		checksum := strings.Index(cmd, "checksums.txt")
		unzip := strings.Index(cmd, "unzip")
		if checksum < 0 || unzip < 0 || checksum > unzip {
			t.Errorf("tflint %s InstallCommand must check checksums.txt before unzip", platform)
		}
		if got := method.Environment["PATH"]; got != `"{{ user_bin_directory }}:{{ ansible_facts['env']['PATH'] }}"` {
			t.Errorf("tflint %s PATH = %q", platform, got)
		}
	}

	debian, debianOK := tool.platforms[PlatformDebianLike].(ShellInstallMethod)
	if debianOK {
		for _, asset := range []string{"tflint_linux_amd64.zip", "tflint_linux_arm64.zip", "uname -m"} {
			if !strings.Contains(debian.InstallCommand, asset) {
				t.Errorf("tflint debian InstallCommand missing %q", asset)
			}
		}
	}
	darwin, darwinOK := tool.platforms[PlatformDarwin].(ShellInstallMethod)
	if darwinOK {
		for _, asset := range []string{"tflint_darwin_amd64.zip", "tflint_darwin_arm64.zip"} {
			if !strings.Contains(darwin.InstallCommand, asset) {
				t.Errorf("tflint darwin InstallCommand missing %q", asset)
			}
		}
	}
}

func TestHerdrOmitsTermux(t *testing.T) {
	tool := findPlatformSpecificTool(t, "herdr")
	if _, ok := tool.platforms[PlatformTermux]; ok {
		t.Error("herdr should omit Termux; upstream ships Linux and macOS builds only")
	}
	if _, ok := tool.platforms[PlatformAll]; ok {
		t.Error("herdr should not use PlatformAll; that would install it on Termux")
	}
}

func TestEmacsDarwinApplicationsAlias(t *testing.T) {
	tool := findPlatformSpecificTool(t, "emacs")
	method, ok := tool.platforms[PlatformDarwin].(BrewInstallMethod)
	if !ok {
		t.Fatalf("emacs darwin method = %T, want BrewInstallMethod", tool.platforms[PlatformDarwin])
	}
	if method.Name != "emacs-plus" {
		t.Errorf("emacs brew name = %q, want emacs-plus", method.Name)
	}
	if strings.Contains(method.Name, "@") {
		t.Errorf("emacs brew name must stay unversioned, got %q", method.Name)
	}
	if method.Tap != "d12frosted/emacs-plus" {
		t.Errorf("emacs tap = %q", method.Tap)
	}

	suffix := tool.Suffix
	for _, want := range []string{
		"{{ playbook_dir }}/emacs_app_link.sh",
		"- symlink",
		"become: yes",
		WhenDarwin,
		`emacs_app_alias.rc not in [0, 2]`,
		`emacs_app_alias.rc | default(0) == 2`,
	} {
		if !strings.Contains(suffix, want) {
			t.Errorf("emacs suffix missing %q", want)
		}
	}
	script, err := os.ReadFile(filepath.Join(ansiblePlaybookDir(t), "emacs_app_link.sh"))
	if err != nil {
		t.Fatal(err)
	}
	scriptText := string(script)
	for _, want := range []string{
		"brew --prefix emacs-plus",
		"EMACS_APP_LINK_DIR:-/Applications",
		"make alias file to posix file",
		"POSIX path of ((original item of (POSIX file",
		"as alias)) as alias",
		`with properties {name:\"Emacs.app\"}`,
		`ln -s "$src" "$dest"`,
	} {
		if !strings.Contains(scriptText, want) {
			t.Errorf("emacs_app_link.sh missing %q", want)
		}
	}
	if strings.Contains(scriptText, "emacs-plus@") || strings.Contains(suffix, "emacs-plus@") {
		t.Error("emacs link must not hardcode an emacs-plus version")
	}
	if strings.Contains(scriptText, "rm -rf") || strings.Contains(suffix, "ansible.builtin.copy") {
		t.Error("emacs link must not copy or recursively remove Emacs.app")
	}
	userTask, adminTask, ok := strings.Cut(suffix, "Expose emacs-plus Emacs.app in /Applications as admin")
	if !ok {
		t.Fatal("emacs suffix missing admin fallback task")
	}
	if strings.Contains(userTask, "become:") {
		t.Error("user alias task must not become")
	}
	if !strings.Contains(adminTask, "become: yes") {
		t.Error("admin fallback task must become")
	}

	tmpl := template.Must(template.New("platformspecific").Parse(platformSpecificTemplate))
	var buf bytes.Buffer
	if err := tmpl.Execute(&buf, tool); err != nil {
		t.Fatalf("render emacs playbook: %v", err)
	}
	got := buf.String()
	installAt := strings.Index(got, "Install emacs on MacOS")
	aliasAt := strings.Index(got, "Expose emacs-plus Emacs.app in /Applications")
	if installAt < 0 || aliasAt < 0 || aliasAt < installAt {
		t.Errorf("Emacs.app alias task should follow the Darwin emacs-plus install")
	}
}

func TestEmacsAppLinkScript(t *testing.T) {
	if runtime.GOOS != "darwin" {
		t.Skip("emacs app link script uses BSD stat")
	}

	t.Run("shellcheck", func(t *testing.T) {
		if _, err := exec.LookPath("shellcheck"); err != nil {
			t.Skip("shellcheck not installed")
		}
		cmd := exec.Command("shellcheck", "-s", "sh", filepath.Join(ansiblePlaybookDir(t), "emacs_app_link.sh")) //nolint:gosec // shellcheck the repo script under test
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("shellcheck: %v\n%s", err, out)
		}
	})

	root := t.TempDir()
	prefix := filepath.Join(root, "opt", "emacs-plus")
	src := filepath.Join(prefix, "Emacs.app")
	if err := os.MkdirAll(src, 0o755); err != nil {
		t.Fatal(err)
	}
	destDir := filepath.Join(root, "Applications")
	if err := os.MkdirAll(destDir, 0o755); err != nil {
		t.Fatal(err)
	}
	dest := filepath.Join(destDir, "Emacs.app")

	t.Run("creates finder alias", func(t *testing.T) {
		if err := os.RemoveAll(dest); err != nil {
			t.Fatal(err)
		}
		log := filepath.Join(t.TempDir(), "osascript.log")
		stdout, code := runEmacsAppLink(t, emacsAppLinkEnv{
			mode: "alias", brewPrefix: prefix, destDir: destDir, log: log,
		})
		if code != 0 || stdout != "created" {
			t.Fatalf("code=%d stdout=%q, want created", code, stdout)
		}
		info, err := os.Lstat(dest)
		if err != nil {
			t.Fatal(err)
		}
		if info.Mode()&os.ModeSymlink != 0 {
			t.Fatal("Finder alias path is a symlink")
		}
		if !osascriptLogContains(t, log, "make alias file") {
			t.Fatal("osascript was not asked to make an alias")
		}
		if err := os.Remove(dest); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("symlink fallback when finder fails", func(t *testing.T) {
		if err := os.RemoveAll(dest); err != nil {
			t.Fatal(err)
		}
		stdout, code := runEmacsAppLink(t, emacsAppLinkEnv{
			mode: "alias", brewPrefix: prefix, destDir: destDir, createFail: true,
		})
		if code != 0 || stdout != "created" {
			t.Fatalf("code=%d stdout=%q, want created symlink", code, stdout)
		}
		got, err := os.Readlink(dest)
		if err != nil {
			t.Fatal(err)
		}
		if got != src {
			t.Errorf("symlink = %q, want %q", got, src)
		}
		if err := os.Remove(dest); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("skips symlink that already points at the prefix", func(t *testing.T) {
		if err := os.RemoveAll(dest); err != nil {
			t.Fatal(err)
		}
		if err := os.Symlink(src, dest); err != nil {
			t.Fatal(err)
		}
		log := filepath.Join(t.TempDir(), "osascript.log")
		stdout, code := runEmacsAppLink(t, emacsAppLinkEnv{
			mode: "alias", brewPrefix: prefix, destDir: destDir, log: log,
		})
		if code != 0 || stdout != "unchanged" {
			t.Fatalf("code=%d stdout=%q, want unchanged", code, stdout)
		}
		if osascriptLogContains(t, log, "make alias file") {
			t.Fatal("replaced a symlink that already points at the prefix")
		}
		if err := os.Remove(dest); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("skips alias whose target resolves to the current prefix", func(t *testing.T) {
		if err := os.RemoveAll(dest); err != nil {
			t.Fatal(err)
		}
		cellar := filepath.Join(root, "Cellar", "Emacs.app")
		if err := os.MkdirAll(cellar, 0o755); err != nil {
			t.Fatal(err)
		}
		linkedPrefix := filepath.Join(root, "linked-prefix")
		if err := os.MkdirAll(linkedPrefix, 0o755); err != nil {
			t.Fatal(err)
		}
		linkedSrc := filepath.Join(linkedPrefix, "Emacs.app")
		if err := os.Symlink(cellar, linkedSrc); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(dest, []byte("alias"), 0o600); err != nil {
			t.Fatal(err)
		}
		stdout, code := runEmacsAppLink(t, emacsAppLinkEnv{
			mode: "alias", brewPrefix: linkedPrefix, destDir: destDir, aliasTarget: cellar + "/",
		})
		if code != 0 || stdout != "unchanged" {
			t.Fatalf("code=%d stdout=%q, want unchanged", code, stdout)
		}
		if err := os.Remove(dest); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("refuses a real bundle", func(t *testing.T) {
		if err := os.RemoveAll(dest); err != nil {
			t.Fatal(err)
		}
		if err := os.MkdirAll(filepath.Join(dest, "Contents"), 0o755); err != nil {
			t.Fatal(err)
		}
		marker := filepath.Join(dest, "Contents", "keep")
		if err := os.WriteFile(marker, []byte("x"), 0o600); err != nil {
			t.Fatal(err)
		}
		_, code := runEmacsAppLink(t, emacsAppLinkEnv{
			mode: "alias", brewPrefix: prefix, destDir: destDir,
		})
		if code != 1 {
			t.Fatalf("code=%d, want 1", code)
		}
		if _, err := os.Stat(marker); err != nil {
			t.Fatalf("real bundle was removed: %v", err)
		}
		if err := os.RemoveAll(dest); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("refuses a file that is not an alias", func(t *testing.T) {
		if err := os.RemoveAll(dest); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(dest, []byte("not-an-alias"), 0o600); err != nil {
			t.Fatal(err)
		}
		_, code := runEmacsAppLink(t, emacsAppLinkEnv{
			mode: "alias", brewPrefix: prefix, destDir: destDir,
		})
		if code != 1 {
			t.Fatalf("code=%d, want 1", code)
		}
		body, err := os.ReadFile(dest)
		if err != nil || string(body) != "not-an-alias" {
			t.Fatalf("file changed: %q err=%v", body, err)
		}
		if err := os.Remove(dest); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("permission denied asks for admin", func(t *testing.T) {
		locked := filepath.Join(t.TempDir(), "Applications")
		if err := os.MkdirAll(locked, 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.Chmod(locked, 0o555); err != nil {
			t.Fatal(err)
		}
		t.Cleanup(func() { _ = os.Chmod(locked, 0o755) })
		_, code := runEmacsAppLink(t, emacsAppLinkEnv{
			mode: "alias", brewPrefix: prefix, destDir: locked, createFail: true,
		})
		if code != 2 {
			t.Fatalf("code=%d, want 2", code)
		}
		if _, err := os.Lstat(filepath.Join(locked, "Emacs.app")); !errors.Is(err, fs.ErrNotExist) {
			t.Fatalf("created Emacs.app without write permission: %v", err)
		}
	})

	t.Run("skips when the prefix has no Emacs.app", func(t *testing.T) {
		if err := os.RemoveAll(dest); err != nil {
			t.Fatal(err)
		}
		stdout, code := runEmacsAppLink(t, emacsAppLinkEnv{
			mode: "alias", brewPrefix: t.TempDir(), destDir: destDir,
		})
		if code != 0 || stdout != "unchanged" {
			t.Fatalf("code=%d stdout=%q, want unchanged", code, stdout)
		}
		if _, err := os.Lstat(dest); !errors.Is(err, fs.ErrNotExist) {
			t.Fatalf("created Emacs.app without a source bundle: %v", err)
		}
	})

	t.Run("skips when emacs-plus is not installed", func(t *testing.T) {
		if err := os.RemoveAll(dest); err != nil {
			t.Fatal(err)
		}
		stdout, code := runEmacsAppLink(t, emacsAppLinkEnv{
			mode: "alias", brewPrefix: prefix, destDir: destDir, failPrefix: true,
		})
		if code != 0 || stdout != "unchanged" {
			t.Fatalf("code=%d stdout=%q, want unchanged", code, stdout)
		}
		if _, err := os.Lstat(dest); !errors.Is(err, fs.ErrNotExist) {
			t.Fatalf("created Emacs.app without emacs-plus: %v", err)
		}
	})

	t.Run("admin mode creates a symlink without finder", func(t *testing.T) {
		if err := os.RemoveAll(dest); err != nil {
			t.Fatal(err)
		}
		log := filepath.Join(t.TempDir(), "osascript.log")
		stdout, code := runEmacsAppLink(t, emacsAppLinkEnv{
			mode: "symlink", brewPrefix: prefix, destDir: destDir, log: log,
		})
		if code != 0 || stdout != "created" {
			t.Fatalf("code=%d stdout=%q, want created", code, stdout)
		}
		got, err := os.Readlink(dest)
		if err != nil {
			t.Fatal(err)
		}
		if got != src {
			t.Errorf("symlink = %q, want %q", got, src)
		}
		if osascriptLogContains(t, log, "make alias file") {
			t.Fatal("admin symlink mode called osascript")
		}
		if err := os.Remove(dest); err != nil {
			t.Fatal(err)
		}
	})
}

type emacsAppLinkEnv struct {
	mode        string
	brewPrefix  string
	destDir     string
	log         string
	aliasTarget string
	createFail  bool
	failPrefix  bool
}

func runEmacsAppLink(t *testing.T, env emacsAppLinkEnv) (string, int) {
	t.Helper()
	work := t.TempDir()
	bin := filepath.Join(work, "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatal(err)
	}
	brew := "#!/bin/sh\n" +
		"if [ \"$BREW_FAIL\" = 1 ]; then\n" +
		"  exit 1\n" +
		"fi\n" +
		"if [ \"$1\" = \"--prefix\" ] && [ \"$2\" = \"emacs-plus\" ]; then\n" +
		"  printf '%s\\n' \"$BREW_PREFIX\"\n" +
		"  exit 0\n" +
		"fi\n" +
		"printf '%s\\n' \"unexpected brew args: $*\" >&2\n" +
		"exit 1\n"
	writeExecutable(t, filepath.Join(bin, "brew"), brew)
	createFail := "0"
	if env.createFail {
		createFail = "1"
	}
	brewFail := "0"
	if env.failPrefix {
		brewFail = "1"
	}
	osascript := "#!/bin/sh\n" +
		"printf '%s\\n' \"$*\" >> \"$OSASCRIPT_LOG\"\n" +
		"script=$2\n" +
		"case \"$script\" in\n" +
		"  *\"make alias file\"*)\n" +
		"    if [ \"$OSASCRIPT_CREATE_FAIL\" = 1 ]; then exit 1; fi\n" +
		"    : > \"$LINK_DEST/Emacs.app\"\n" +
		"    exit 0\n" +
		"    ;;\n" +
		"  *\"original item\"*)\n" +
		"    if [ -n \"${OSASCRIPT_TARGET:-}\" ]; then\n" +
		"      printf '%s\\n' \"$OSASCRIPT_TARGET\"\n" +
		"      exit 0\n" +
		"    fi\n" +
		"    exit 1\n" +
		"    ;;\n" +
		"esac\n" +
		"exit 1\n"
	writeExecutable(t, filepath.Join(bin, "osascript"), osascript)
	if env.log == "" {
		env.log = filepath.Join(work, "osascript.log")
	}
	args := []string{filepath.Join(ansiblePlaybookDir(t), "emacs_app_link.sh")}
	if env.mode == "symlink" {
		args = append(args, "symlink")
	}
	cmd := exec.Command("/bin/sh", args...) //nolint:gosec // run the repo script under test with a temp PATH
	cmd.Env = []string{
		"PATH=" + bin + ":/bin:/usr/bin",
		"EMACS_APP_LINK_DIR=" + env.destDir,
		"BREW_PREFIX=" + env.brewPrefix,
		"LINK_DEST=" + env.destDir,
		"OSASCRIPT_LOG=" + env.log,
		"OSASCRIPT_TARGET=" + env.aliasTarget,
		"OSASCRIPT_CREATE_FAIL=" + createFail,
		"BREW_FAIL=" + brewFail,
		"HOME=" + work,
		"TMPDIR=" + work,
	}
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	code := 0
	if err != nil {
		var exitErr *exec.ExitError
		if !errors.As(err, &exitErr) {
			t.Fatalf("run link script: %v\n%s", err, stderr.String())
		}
		code = exitErr.ExitCode()
	}
	if stderr.Len() > 0 {
		t.Logf("stderr: %s", stderr.String())
	}
	return strings.TrimSpace(string(out)), code
}

func writeExecutable(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(path, 0o755); err != nil {
		t.Fatal(err)
	}
}

func osascriptLogContains(t *testing.T, path, needle string) bool {
	t.Helper()
	body, err := os.ReadFile(path)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return false
		}
		t.Fatal(err)
	}
	return strings.Contains(string(body), needle)
}

func formatImports(imports []Import) string {
	if len(imports) == 0 {
		return "nil"
	}
	s := "["
	for i, imp := range imports {
		if i > 0 {
			s += ", "
		}
		if imp.When != "" {
			s += fmt.Sprintf("{%q when: %q}", imp.Playbook, imp.When)
		} else {
			s += fmt.Sprintf("{%q}", imp.Playbook)
		}
	}
	return s + "]"
}
