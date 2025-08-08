package main

import (
	"path"
	"strings"
	"unicode"
)

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
	platforms map[string]InstallMethod
	Imports   []Import
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

func (p PlatformSpecificTool) GetPlatforms() map[string]InstallMethod {
	return p.platforms
}

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
func GoTool(pkgPath string, imports ...Import) PlatformSpecificTool {
	baseName := path.Base(pkgPath)
	if idx := strings.Index(baseName, "@"); idx != -1 {
		baseName = baseName[:idx]
	}
	command := baseName
	return PlatformSpecificTool{
		command: command,
		platforms: map[string]InstallMethod{
			"all": GoInstallMethod{PkgPath: pkgPath},
		},
		Imports: imports,
	}
}
