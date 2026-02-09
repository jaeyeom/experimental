package main

import (
	"fmt"
	"reflect"
	"sort"
	"testing"
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

func TestGetDebianAptPackages(t *testing.T) {
	pkgs := getDebianAptPackages()
	if len(pkgs) == 0 {
		t.Fatal("expected at least one apt package")
	}
	// Verify sorted.
	if !sort.StringsAreSorted(pkgs) {
		t.Error("packages should be sorted")
	}
	// Verify no duplicates.
	for i := 1; i < len(pkgs); i++ {
		if pkgs[i] == pkgs[i-1] {
			t.Errorf("duplicate package: %s", pkgs[i])
		}
	}
	// Spot-check known packages from the packages slice.
	for _, name := range []string{"curl", "git", "jq", "ripgrep"} {
		if !sliceContains(pkgs, name) {
			t.Errorf("expected package %q not found", name)
		}
	}
	// Spot-check known packages from platformSpecificTools (apt-based).
	for _, name := range []string{"lcov"} {
		if !sliceContains(pkgs, name) {
			t.Errorf("expected platform-specific apt package %q not found", name)
		}
	}
	// Verify non-apt tools are excluded (Go-installed tools should not appear).
	for _, name := range []string{"gopls", "shfmt", "hugo"} {
		if sliceContains(pkgs, name) {
			t.Errorf("go-installed tool %q should not appear in apt packages", name)
		}
	}
}

func TestBuildCommandAptInfo(t *testing.T) {
	info := buildCommandAptInfo()
	if len(info) == 0 {
		t.Fatal("expected at least one entry in apt info map")
	}

	// Spot-check: "emacs" should have a PPA entry.
	emacsInfo, ok := info["emacs"]
	if !ok {
		t.Fatal("expected 'emacs' in apt info map")
	}
	if emacsInfo.PackageName != "emacs" {
		t.Errorf("emacs PackageName = %q, want %q", emacsInfo.PackageName, "emacs")
	}
	if emacsInfo.SourceType != AptSourcePPA {
		t.Errorf("emacs SourceType = %d, want AptSourcePPA (%d)", emacsInfo.SourceType, AptSourcePPA)
	}
	if emacsInfo.PPA != "ppa:ubuntuhandbook1/emacs" {
		t.Errorf("emacs PPA = %q, want %q", emacsInfo.PPA, "ppa:ubuntuhandbook1/emacs")
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

func TestBuildCommandAptInfoConsistentWithGetDebianAptPackages(t *testing.T) {
	// Verify the refactored getDebianAptPackages produces the same set
	// of package names as iterating buildCommandAptInfo directly.
	info := buildCommandAptInfo()
	fromMap := make(map[string]bool)
	for _, entry := range info {
		fromMap[entry.PackageName] = true
	}

	pkgs := getDebianAptPackages()
	fromFunc := make(map[string]bool)
	for _, p := range pkgs {
		fromFunc[p] = true
	}

	for name := range fromMap {
		if !fromFunc[name] {
			t.Errorf("package %q in buildCommandAptInfo but missing from getDebianAptPackages", name)
		}
	}
	for name := range fromFunc {
		if !fromMap[name] {
			t.Errorf("package %q in getDebianAptPackages but missing from buildCommandAptInfo", name)
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
	allPkgs := getDebianAptPackages()
	allSet := make(map[string]bool)
	for _, p := range allPkgs {
		allSet[p] = true
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
