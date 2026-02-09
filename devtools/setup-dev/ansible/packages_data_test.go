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

func sliceContains(s []string, v string) bool {
	for _, item := range s {
		if item == v {
			return true
		}
	}
	return false
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
