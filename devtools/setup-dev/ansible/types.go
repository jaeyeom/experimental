package main

import (
	"strings"
	"unicode"
)

// PlatformName represents a valid platform identifier used in tool configurations.
// Using a typed constant prevents typos and provides compile-time safety.
type PlatformName string

const (
	// PlatformAll represents tools that use the same installation method
	// across all platforms.
	PlatformAll PlatformName = "all"
	// PlatformDarwin represents macOS-specific installation. It uses
	// Homebrew.
	PlatformDarwin PlatformName = "darwin"
	// PlatformTermux represents Termux-specific installation. It uses pkg.
	PlatformTermux PlatformName = "termux"
	// PlatformDebianLike represents Debian/Ubuntu systems. It uses apt.
	PlatformDebianLike PlatformName = "debian-like"
	// PlatformDebian represents Debian-specific installation. It uses apt.
	PlatformDebian PlatformName = "debian"
	// PlatformUbuntu represents Ubuntu-specific installation. It uses apt.
	PlatformUbuntu PlatformName = "ubuntu"
)

// AllPlatforms is the canonical list of all valid platform names.
// This is the single source of truth for valid platforms.
var AllPlatforms = []PlatformName{
	PlatformAll,
	PlatformDarwin,
	PlatformTermux,
	PlatformDebianLike,
	PlatformDebian,
	PlatformUbuntu,
}

// Import represents a playbook import with optional conditional logic.
type Import struct {
	Playbook string // Name of the playbook to import
	When     string // Optional Ansible when clause for conditional imports
}

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
	Imports       []Import
	Suffix        string
}

func (p PackageData) Command() string {
	return p.command
}

func (p PackageData) CommandID() string {
	id := strings.ReplaceAll(p.command, "-", "_")
	if len(id) > 0 && unicode.IsDigit(rune(id[0])) {
		id = "cmd_" + id
	}
	return id
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
	GetImports() []Import
	RenderInstallTask(command string) string
	RenderSetupTasks(command string) string       // For PPA, taps, backports, etc.
	RenderBlockInstallTask(command string) string // For use inside blocks
}

// PlatformSpecificTool represents a development tool that can be installed
// using different methods on different platforms. This is the unified approach
// that replaced separate install type arrays (GoInstall, PipInstall, etc.).
type PlatformSpecificTool struct {
	command   string
	platforms map[PlatformName]InstallMethod
	Imports   []Import
	Suffix    string // Additional tasks to run after installation
}

func (p PlatformSpecificTool) Command() string {
	return p.command
}

func (p PlatformSpecificTool) CommandID() string {
	id := strings.ReplaceAll(p.command, "-", "_")
	if len(id) > 0 && unicode.IsDigit(rune(id[0])) {
		id = "cmd_" + id
	}
	return id
}

// HasPlatform returns true if the tool has an install method for the given platform.
func (p PlatformSpecificTool) HasPlatform(platform PlatformName) bool {
	_, exists := p.platforms[platform]
	return exists
}

// Platform-specific helper methods for use in templates.

func (p PlatformSpecificTool) DarwinMethod() InstallMethod { return p.platforms[PlatformDarwin] }
func (p PlatformSpecificTool) TermuxMethod() InstallMethod { return p.platforms[PlatformTermux] }
func (p PlatformSpecificTool) AllMethod() InstallMethod    { return p.platforms[PlatformAll] }
func (p PlatformSpecificTool) DebianLikeMethod() InstallMethod {
	return p.platforms[PlatformDebianLike]
}
func (p PlatformSpecificTool) DebianMethod() InstallMethod { return p.platforms[PlatformDebian] }
func (p PlatformSpecificTool) UbuntuMethod() InstallMethod { return p.platforms[PlatformUbuntu] }

func (p PlatformSpecificTool) HasDarwin() bool     { return p.HasPlatform(PlatformDarwin) }
func (p PlatformSpecificTool) HasTermux() bool     { return p.HasPlatform(PlatformTermux) }
func (p PlatformSpecificTool) HasAll() bool        { return p.HasPlatform(PlatformAll) }
func (p PlatformSpecificTool) HasDebianLike() bool { return p.HasPlatform(PlatformDebianLike) }
func (p PlatformSpecificTool) HasDebian() bool     { return p.HasPlatform(PlatformDebian) }
func (p PlatformSpecificTool) HasUbuntu() bool     { return p.HasPlatform(PlatformUbuntu) }

func (p PlatformSpecificTool) GetAllImports() []Import {
	importsMap := make(map[string]bool)
	var importsOrder []Import

	// Add explicit imports first, maintaining order
	for _, imp := range p.Imports {
		if !importsMap[imp.Playbook] && imp.Playbook != p.command {
			importsMap[imp.Playbook] = true
			importsOrder = append(importsOrder, imp)
		}
	}

	// Add imports from platform methods, maintaining their order
	for _, method := range p.platforms {
		for _, imp := range method.GetImports() {
			if !importsMap[imp.Playbook] && imp.Playbook != p.command {
				importsMap[imp.Playbook] = true
				importsOrder = append(importsOrder, imp)
			}
		}
	}

	return importsOrder
}

// GoTool creates a PlatformSpecificTool for Go tools, allowing installation.
func GoTool(command string, pkgPath string, imports ...Import) PlatformSpecificTool {
	return PlatformSpecificTool{
		command: command,
		platforms: map[PlatformName]InstallMethod{
			PlatformAll: GoInstallMethod{PkgPath: pkgPath},
		},
		Imports: imports,
	}
}
