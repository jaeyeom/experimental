package main

import (
	"reflect"
	"testing"
)

func TestPackageDataDefaults(t *testing.T) {
	pkg := PackageData{command: "curl"}
	if got := pkg.Command(); got != "curl" {
		t.Errorf("Command() = %q, want curl", got)
	}
	if got := pkg.CheckCommand(); got != "command -v curl" {
		t.Errorf("CheckCommand() = %q, want default command -v", got)
	}
	if got := pkg.DebianPkgName(); got != "curl" {
		t.Errorf("DebianPkgName() = %q, want curl", got)
	}
	if got := pkg.TermuxPkgName(); got != "curl" {
		t.Errorf("TermuxPkgName() = %q, want curl", got)
	}
	if got := pkg.BrewPkgName(); got != "curl" {
		t.Errorf("BrewPkgName() = %q, want curl", got)
	}
	if got := pkg.BrewTap(); got != "" {
		t.Errorf("BrewTap() = %q, want empty", got)
	}
	if got := pkg.BrewOptions(); got != nil {
		t.Errorf("BrewOptions() = %v, want nil", got)
	}
	if got := pkg.CommandID(); got != "curl" {
		t.Errorf("CommandID() = %q, want curl", got)
	}
}

func TestPackageDataOverrides(t *testing.T) {
	pkg := PackageData{
		command:       "ag",
		checkCommand:  "which ag",
		debianPkgName: "silversearcher-ag",
		termuxPkgName: "silversearcher-ag",
		brewPkgName:   "the_silver_searcher",
		brewTap:       "homebrew/core",
		brewOptions:   []string{"with-pcre"},
	}
	if got := pkg.CheckCommand(); got != "which ag" {
		t.Errorf("CheckCommand() = %q", got)
	}
	if got := pkg.DebianPkgName(); got != "silversearcher-ag" {
		t.Errorf("DebianPkgName() = %q", got)
	}
	if got := pkg.TermuxPkgName(); got != "silversearcher-ag" {
		t.Errorf("TermuxPkgName() = %q", got)
	}
	if got := pkg.BrewPkgName(); got != "the_silver_searcher" {
		t.Errorf("BrewPkgName() = %q", got)
	}
	if got := pkg.BrewTap(); got != "homebrew/core" {
		t.Errorf("BrewTap() = %q", got)
	}
	if !reflect.DeepEqual(pkg.BrewOptions(), []string{"with-pcre"}) {
		t.Errorf("BrewOptions() = %v", pkg.BrewOptions())
	}
}

func TestCommandIDHyphenAndLeadingDigit(t *testing.T) {
	tests := []struct {
		command string
		want    string
	}{
		{command: "my-tool", want: "my_tool"},
		{command: "7z", want: "cmd_7z"},
		{command: "plain", want: "plain"},
	}
	for _, tt := range tests {
		t.Run(tt.command, func(t *testing.T) {
			pkg := PackageData{command: tt.command}
			if got := pkg.CommandID(); got != tt.want {
				t.Errorf("PackageData.CommandID() = %q, want %q", got, tt.want)
			}
			tool := PlatformSpecificTool{command: tt.command}
			if got := tool.CommandID(); got != tt.want {
				t.Errorf("PlatformSpecificTool.CommandID() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestPlatformSpecificToolHasPlatform(t *testing.T) {
	tool := PlatformSpecificTool{
		command: "prettier",
		platforms: map[PlatformName]InstallMethod{
			PlatformDarwin:     BrewInstallMethod{Name: "prettier"},
			PlatformTermux:     NpmInstallMethod{Name: "prettier"},
			PlatformDebianLike: NvmInstallMethod{Name: "prettier"},
		},
	}

	if !tool.HasDarwin() || tool.DarwinMethod() == nil {
		t.Error("expected Darwin method")
	}
	if !tool.HasTermux() || tool.TermuxMethod() == nil {
		t.Error("expected Termux method")
	}
	if !tool.HasDebianLike() || tool.DebianLikeMethod() == nil {
		t.Error("expected DebianLike method")
	}
	if tool.HasAll() || tool.AllMethod() != nil {
		t.Error("did not expect All method")
	}
	if tool.HasDebian() || tool.DebianMethod() != nil {
		t.Error("did not expect Debian method")
	}
	if tool.HasUbuntu() || tool.UbuntuMethod() != nil {
		t.Error("did not expect Ubuntu method")
	}
	if tool.Command() != "prettier" {
		t.Errorf("Command() = %q", tool.Command())
	}
}

func TestCombineWhenConditions(t *testing.T) {
	tests := []struct {
		name  string
		whens []string
		want  string
	}{
		{
			name:  "single condition unchanged",
			whens: []string{WhenDarwin},
			want:  WhenDarwin,
		},
		{
			name:  "two conditions joined with or",
			whens: []string{WhenTermux, WhenDebianLike},
			want:  "(" + WhenTermux + ") or (" + WhenDebianLike + ")",
		},
		{
			name:  "duplicates removed",
			whens: []string{WhenDarwin, WhenDarwin, WhenTermux},
			want:  "(" + WhenDarwin + ") or (" + WhenTermux + ")",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := combineWhenConditions(tt.whens); got != tt.want {
				t.Errorf("combineWhenConditions() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestGoTool(t *testing.T) {
	tool := GoTool("gopls", "golang.org/x/tools/gopls@latest", Import{Playbook: "go"})
	if tool.Command() != "gopls" {
		t.Errorf("Command() = %q", tool.Command())
	}
	if !tool.HasAll() {
		t.Error("GoTool should use PlatformAll")
	}
	method, ok := tool.AllMethod().(GoInstallMethod)
	if !ok {
		t.Fatalf("AllMethod type = %T, want GoInstallMethod", tool.AllMethod())
	}
	if method.PkgPath != "golang.org/x/tools/gopls@latest" {
		t.Errorf("PkgPath = %q", method.PkgPath)
	}
	if len(tool.Imports) != 1 || tool.Imports[0].Playbook != "go" {
		t.Errorf("Imports = %v", tool.Imports)
	}
}

func TestGhExtension(t *testing.T) {
	tool := GhExtension("gh-dash", "dlvhdr/gh-dash")
	if tool.Command() != "gh-dash" {
		t.Errorf("Command() = %q", tool.Command())
	}
	if !tool.HasAll() {
		t.Error("GhExtension should use PlatformAll")
	}
	method, ok := tool.AllMethod().(GhExtensionInstallMethod)
	if !ok {
		t.Fatalf("AllMethod type = %T, want GhExtensionInstallMethod", tool.AllMethod())
	}
	if method.Repo != "dlvhdr/gh-dash" {
		t.Errorf("Repo = %q", method.Repo)
	}
}
