// Binary generate_packages generates Ansible yaml files for the package
// installations.
//
// This tool generates Ansible playbooks for installing development tools
// across different platforms (macOS, Linux, Termux) using various installation
// methods defined in separate files:
//
// - types.go: Core types, interfaces, and data structures
// - install_methods.go: All InstallMethod implementations
// - templates.go: Ansible playbook templates
// - packages_data.go: Package and tool definitions
package main

import (
	"bufio"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"

	// Generates Ansible YAML playbooks, not HTML.
	// nosemgrep: import-text-template
	"text/template"
)

// Commander interface is implemented by all tool types to provide
// a unified way to get command names for file generation.
type Commander interface {
	Command() string
}

func generatePackages[T Commander](tmpl *template.Template, pkgs []T) {
	for _, pkg := range pkgs {
		outf, err := os.Create(pkg.Command() + ".yml")
		if err != nil {
			panic(err)
		}
		defer outf.Close()

		err = tmpl.Execute(outf, pkg)
		if err != nil {
			panic(err)
		}
	}
}

func getAllYmlFiles() ([]string, error) {
	var ymlFiles []string
	err := filepath.WalkDir(".", func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() && strings.HasSuffix(path, ".yml") && !strings.Contains(path, "/") {
			ymlFiles = append(ymlFiles, strings.TrimSuffix(path, ".yml"))
		}
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("walk directory: %w", err)
	}
	sort.Strings(ymlFiles)
	return ymlFiles, nil
}

func getGeneratedRuleNames() []string {
	var generated []string

	for _, pkg := range packages {
		generated = append(generated, pkg.Command())
	}

	for _, pkg := range platformSpecificTools {
		generated = append(generated, pkg.Command())
	}

	sort.Strings(generated)
	return generated
}

func getManualPlaybooks() ([]string, error) {
	allYml, err := getAllYmlFiles()
	if err != nil {
		return nil, err
	}

	generated := getGeneratedRuleNames()
	generatedMap := make(map[string]bool)
	for _, name := range generated {
		generatedMap[name] = true
	}

	var manual []string
	for _, yml := range allYml {
		if !generatedMap[yml] {
			manual = append(manual, yml+".yml")
		}
	}

	return manual, nil
}

func updateReadmeManualPlaybooks() error {
	readmePath := "README.org"

	file, err := os.Open(readmePath)
	if err != nil {
		return fmt.Errorf("failed to open README.org: %w", err)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	inManualSection := false

	for scanner.Scan() {
		line := scanner.Text()
		if strings.Contains(line, "* Manual Playbooks") {
			inManualSection = true
			lines = append(lines, line)
			lines = append(lines, "These files are playbooks not generated from =generate_packages.go=:")
			lines = append(lines, "")

			manualPlaybooks, err := getManualPlaybooks()
			if err != nil {
				return fmt.Errorf("failed to get manual playbooks: %w", err)
			}

			for _, playbook := range manualPlaybooks {
				lines = append(lines, "- "+playbook)
			}

			for scanner.Scan() {
				nextLine := scanner.Text()
				if strings.HasPrefix(nextLine, "* ") && nextLine != "* Manual Playbooks" {
					lines = append(lines, nextLine)
					break
				}
			}
		} else if !inManualSection {
			lines = append(lines, line)
		}
	}

	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		return fmt.Errorf("error reading README.org: %w", err)
	}

	output := strings.Join(lines, "\n") + "\n"
	err = os.WriteFile(readmePath, []byte(output), 0o600)
	if err != nil {
		return fmt.Errorf("failed to write README.org: %w", err)
	}

	return nil
}

// getPlaybookImports returns a list of YAML files imported by the given playbook (direct imports only).
func getPlaybookImports(ymlFile string) ([]string, error) {
	file, err := os.Open(ymlFile + ".yml")
	if err != nil {
		return nil, fmt.Errorf("failed to open %s.yml: %w", ymlFile, err)
	}
	defer file.Close()

	var imports []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.Contains(line, "import_playbook:") {
			// Remove comments before processing
			lineBeforeComment, _, _ := strings.Cut(line, "#")

			// Extract the filename from "- import_playbook: filename.yml"
			parts := strings.Split(lineBeforeComment, "import_playbook:")
			if len(parts) > 1 {
				filename := strings.TrimSpace(parts[1])
				filename = strings.TrimSuffix(filename, ".yml")
				imports = append(imports, filename)
			}
		}
	}

	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("error reading %s.yml: %w", ymlFile, err)
	}

	return imports, nil
}

// getAllDependencies recursively collects all transitive dependencies of a playbook.
func getAllDependencies(ymlFile string, visited map[string]bool) ([]string, error) {
	if visited[ymlFile] {
		return nil, nil
	}
	visited[ymlFile] = true

	directImports, err := getPlaybookImports(ymlFile)
	if err != nil {
		return nil, err
	}

	var allDeps []string
	for _, imp := range directImports {
		allDeps = append(allDeps, imp)
		// Recursively get dependencies of this import
		transitiveDeps, err := getAllDependencies(imp, visited)
		if err != nil {
			return nil, err
		}
		allDeps = append(allDeps, transitiveDeps...)
	}

	return allDeps, nil
}

const defaultManualContent = `load("@rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    srcs = [
        "generate_packages.go",
        "install_methods.go",
        "packages_data.go",
        "templates.go",
        "types.go",
    ],
    importpath = "github.com/jaeyeom/experimental/devtools/setup-dev/ansible",
    visibility = ["//visibility:private"],
)

go_binary(
    name = "generate_packages",
    embed = [":go_default_library"],
    visibility = ["//visibility:public"],
)

`

// readManualContent reads the manual content from BUILD.bazel before the marker.
func readManualContent(buildPath string) (string, error) {
	file, err := os.Open(buildPath)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return defaultManualContent, nil
		}
		return "", fmt.Errorf("failed to open BUILD.bazel: %w", err)
	}
	defer file.Close()

	var manualContent strings.Builder
	scanner := bufio.NewScanner(file)
	foundMarker := false

	for scanner.Scan() {
		line := scanner.Text()
		if strings.Contains(line, "# GENERATED ANSIBLE TESTS - DO NOT EDIT BELOW THIS LINE") {
			foundMarker = true
			break
		}
		manualContent.WriteString(line)
		manualContent.WriteString("\n")
	}

	if err := scanner.Err(); err != nil {
		return "", fmt.Errorf("error reading BUILD.bazel: %w", err)
	}

	if !foundMarker {
		return defaultManualContent, nil
	}

	return manualContent.String(), nil
}

// generateTestRule generates a single sh_test rule for a playbook.
func generateTestRule(yml string) (string, error) {
	// Get all transitive dependencies
	visited := make(map[string]bool)
	allDeps, err := getAllDependencies(yml, visited)
	if err != nil {
		return "", fmt.Errorf("failed to get dependencies for %s: %w", yml, err)
	}

	// Remove duplicates and sort (including the main file)
	depMap := make(map[string]bool)
	depMap[yml] = true // Include the main file
	for _, dep := range allDeps {
		depMap[dep] = true
	}
	var dataFiles []string
	for dep := range depMap {
		dataFiles = append(dataFiles, dep)
	}
	sort.Strings(dataFiles)

	// Format for Bazel
	var quotedFiles []string
	for _, f := range dataFiles {
		quotedFiles = append(quotedFiles, fmt.Sprintf("\"%s.yml\"", f))
	}

	var dataStr string
	if len(quotedFiles) == 1 {
		dataStr = quotedFiles[0]
	} else {
		dataStr = "\n        " + strings.Join(quotedFiles, ",\n        ") + ",\n    "
	}

	return fmt.Sprintf(`
sh_test(
    name = "%s_syntax_test",
    srcs = ["test_ansible_syntax.sh"],
    args = ["%s.yml"],
    data = [%s],
    tags = [
        "ansible",
        "local",
    ],
)
`, yml, yml, dataStr), nil
}

func generateBuildBazel() error {
	allYml, err := getAllYmlFiles()
	if err != nil {
		return fmt.Errorf("failed to get yml files: %w", err)
	}

	manualContent, err := readManualContent("BUILD.bazel")
	if err != nil {
		return err
	}

	var sb strings.Builder
	sb.WriteString(manualContent)
	sb.WriteString("# GENERATED ANSIBLE TESTS - DO NOT EDIT BELOW THIS LINE\n")
	sb.WriteString("# Individual syntax check test for each playbook (for better caching)\n")

	for _, yml := range allYml {
		testRule, err := generateTestRule(yml)
		if err != nil {
			return err
		}
		sb.WriteString(testRule)
	}

	// Create sorted list of test names with suffix for test_suite
	testNames := make([]string, len(allYml))
	for i, yml := range allYml {
		testNames[i] = yml + "_syntax_test"
	}
	sort.Strings(testNames)

	sb.WriteString(`
# Test suite that runs all syntax tests
test_suite(
    name = "ansible_syntax_tests",
    tags = [
        "ansible",
        "local",
    ],
    tests = [
`)

	for _, testName := range testNames {
		sb.WriteString(fmt.Sprintf("        \":%s\",\n", testName))
	}

	sb.WriteString(`    ],
)
`)

	err = os.WriteFile("BUILD.bazel", []byte(sb.String()), 0o600)
	if err != nil {
		return fmt.Errorf("failed to write BUILD.bazel: %w", err)
	}

	return nil
}

// validatePlatformNames checks that all platform names in platformSpecificTools
// use the typed constants. This should now be enforced at compile-time, but we
// keep this function for potential runtime checks on dynamically loaded configs.
func validatePlatformNames() error {
	// Build a set from the canonical list in types.go
	validPlatforms := make(map[PlatformName]bool)
	for _, platform := range AllPlatforms {
		validPlatforms[platform] = true
	}

	for _, tool := range platformSpecificTools {
		for platform := range tool.platforms {
			if !validPlatforms[platform] {
				// This should never happen with typed constants, but keeping for safety
				return fmt.Errorf(
					"invalid platform name '%s' for tool '%s'.\nValid platforms are: %v",
					platform,
					tool.command,
					AllPlatforms,
				)
			}
		}
	}

	return nil
}

// buildCommandAptInfo builds a map from command name to AptPackageInfo for
// every tool that installs an apt package on Debian-like systems. Non-apt
// install methods (Go, Cargo, npm, etc.) produce no entry.
func buildCommandAptInfo() map[string]AptPackageInfo {
	info := make(map[string]AptPackageInfo)

	// All entries in the packages slice use the system package manager
	// (apt) on Debian-like systems.
	for _, pkg := range packages {
		entry := AptPackageInfo{PackageName: pkg.DebianPkgName()}
		if pkg.UbuntuPPA != "" {
			entry.SourceType = AptSourcePPA
			entry.PPA = pkg.UbuntuPPA
		}
		info[pkg.Command()] = entry
	}

	// From platformSpecificTools, extract only apt-based install methods.
	for _, tool := range platformSpecificTools {
		for _, platform := range []PlatformName{PlatformDebianLike, PlatformDebian, PlatformUbuntu} {
			method, ok := tool.platforms[platform]
			if !ok {
				continue
			}
			switch m := method.(type) {
			case PackageInstallMethod:
				if _, exists := info[tool.Command()]; !exists {
					info[tool.Command()] = AptPackageInfo{PackageName: m.Name}
				}
			case AptRepoInstallMethod:
				if _, exists := info[tool.Command()]; !exists {
					info[tool.Command()] = AptPackageInfo{
						PackageName: m.Name,
						SourceType:  AptSourceAptRepo,
						AptRepo:     &m,
					}
				}
			case DebianPkgInstallMethod:
				if _, exists := info[tool.Command()]; !exists {
					info[tool.Command()] = AptPackageInfo{
						PackageName: m.Name,
						SourceType:  AptSourceBackports,
					}
				}
			case UbuntuPkgInstallMethod:
				if _, exists := info[tool.Command()]; !exists {
					entry := AptPackageInfo{PackageName: m.Name}
					if m.PPA != "" {
						entry.SourceType = AptSourcePPA
						entry.PPA = m.PPA
					}
					info[tool.Command()] = entry
				}
			}
		}
	}

	return info
}

// discoverProfiles scans for profile-*.yml files and returns profile names
// (e.g., "minimal" from "profile-minimal.yml").
func discoverProfiles() ([]string, error) {
	matches, err := filepath.Glob("profile-*.yml")
	if err != nil {
		return nil, fmt.Errorf("glob profile files: %w", err)
	}
	var profiles []string
	for _, m := range matches {
		name := strings.TrimPrefix(m, "profile-")
		name = strings.TrimSuffix(name, ".yml")
		profiles = append(profiles, name)
	}
	sort.Strings(profiles)
	return profiles, nil
}

// getProfileAptPackages returns sorted, deduplicated AptPackageInfo entries
// for the given profile by tracing all transitive playbook dependencies and
// looking up each in the command-to-apt-info map.
func getProfileAptPackages(profileName string) ([]AptPackageInfo, error) {
	visited := make(map[string]bool)
	deps, err := getAllDependencies("profile-"+profileName, visited)
	if err != nil {
		return nil, fmt.Errorf("trace profile %q dependencies: %w", profileName, err)
	}

	aptInfo := buildCommandAptInfo()
	seen := make(map[string]bool)
	var result []AptPackageInfo
	for _, dep := range deps {
		info, ok := aptInfo[dep]
		if !ok {
			// Not an apt-installed command (e.g., Go tool, Cargo tool).
			continue
		}
		if seen[info.PackageName] {
			continue
		}
		seen[info.PackageName] = true
		result = append(result, info)
	}
	sort.Slice(result, func(i, j int) bool {
		return result[i].PackageName < result[j].PackageName
	})
	return result, nil
}

// generateQubesTemplatevmScript writes qubes-templatevm-{profile}.sh, a
// self-contained script that configures apt sources and installs packages.
func generateQubesTemplatevmScript(profileName string, pkgs []AptPackageInfo) error {
	var sb strings.Builder
	sb.WriteString("#!/bin/bash\n")
	sb.WriteString("# Auto-generated TemplateVM bootstrap for profile: " + profileName + "\n")
	sb.WriteString("# Install apt packages and configure their sources.\n")
	sb.WriteString("set -euo pipefail\n\n")

	// Collect sources by type.
	var (
		ppas     []AptPackageInfo
		repos    []AptPackageInfo
		backport bool
		names    []string
	)
	for _, p := range pkgs {
		names = append(names, p.PackageName)
		switch p.SourceType {
		case AptSourcePPA:
			ppas = append(ppas, p)
		case AptSourceBackports:
			backport = true
		case AptSourceAptRepo:
			repos = append(repos, p)
		}
	}

	if backport {
		sb.WriteString("# Add Debian backports (no-op on Ubuntu).\n")
		sb.WriteString("CODENAME=$(lsb_release -cs)\n")
		sb.WriteString("if grep -q '^ID=debian' /etc/os-release 2>/dev/null; then\n")
		sb.WriteString("  echo \"deb http://deb.debian.org/debian ${CODENAME}-backports main contrib non-free non-free-firmware\" \\\n")
		sb.WriteString("    > /etc/apt/sources.list.d/backports.list\n")
		sb.WriteString("fi\n\n")
	}

	if len(ppas) > 0 {
		sb.WriteString("# Add Ubuntu PPAs (no-op on Debian).\n")
		sb.WriteString("if command -v add-apt-repository >/dev/null 2>&1; then\n")
		for _, p := range ppas {
			sb.WriteString("  add-apt-repository -y " + p.PPA + "\n")
		}
		sb.WriteString("fi\n\n")
	}

	for _, r := range repos {
		repo := r.AptRepo
		sb.WriteString("# Add custom repo for " + r.PackageName + ".\n")
		sb.WriteString("curl -fsSL " + repo.GPGKeyURL + " | gpg --dearmor -o " + repo.GPGKeyPath + "\n")
		codename := repo.Codename
		if codename == "" {
			codename = "$(lsb_release -cs)"
		}
		archOption := ""
		if repo.Arch != "" {
			archOption = "arch=" + repo.Arch + " "
		}
		fmt.Fprintf(&sb, "echo \"deb [%ssigned-by=%s] %s %s %s\" > /etc/apt/sources.list.d/%s.list\n\n",
			archOption, repo.GPGKeyPath, repo.RepoURL, codename, repo.RepoComponents, r.PackageName)
	}

	sb.WriteString("apt-get update\n")
	sb.WriteString("apt-get install -y \\\n")
	for i, n := range names {
		if i < len(names)-1 {
			sb.WriteString("  " + n + " \\\n")
		} else {
			sb.WriteString("  " + n + "\n")
		}
	}

	filename := "qubes-templatevm-" + profileName + ".sh"
	if err := os.WriteFile(filename, []byte(sb.String()), 0o600); err != nil {
		return fmt.Errorf("write %s: %w", filename, err)
	}
	if err := os.Chmod(filename, 0o755); err != nil {
		return fmt.Errorf("chmod %s: %w", filename, err)
	}
	return nil
}

// generateProfileOutputs generates TemplateVM bootstrap scripts for all
// discovered profiles.
func generateProfileOutputs() error {
	profiles, err := discoverProfiles()
	if err != nil {
		return err
	}
	for _, profile := range profiles {
		pkgs, err := getProfileAptPackages(profile)
		if err != nil {
			return fmt.Errorf("profile %q: %w", profile, err)
		}
		if err := generateQubesTemplatevmScript(profile, pkgs); err != nil {
			return err
		}
	}
	return nil
}

func main() {
	// Validate platform names before generating packages
	if err := validatePlatformNames(); err != nil {
		fmt.Fprintf(os.Stderr, "Platform validation failed: %v\n", err)
		os.Exit(1)
	}

	tmpl := template.Must(template.New("packages").Parse(packagesTemplate))
	template.Must(tmpl.New("platformspecific").Parse(platformSpecificTemplate))

	generatePackages(tmpl.Lookup("packages"), packages)
	generatePackages(tmpl.Lookup("platformspecific"), platformSpecificTools)

	err := updateReadmeManualPlaybooks()
	if err != nil {
		panic(err)
	}

	err = generateBuildBazel()
	if err != nil {
		panic(err)
	}

	err = generateProfileOutputs()
	if err != nil {
		panic(err)
	}
}
