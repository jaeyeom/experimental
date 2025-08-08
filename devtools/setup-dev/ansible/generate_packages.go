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
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
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

func main() {
	tmpl := template.Must(template.New("packages").Parse(packagesTemplate))
	template.Must(tmpl.New("platformspecific").Parse(platformSpecificTemplate))

	generatePackages(tmpl.Lookup("packages"), packages)
	generatePackages(tmpl.Lookup("platformspecific"), platformSpecificTools)

	err := updateReadmeManualPlaybooks()
	if err != nil {
		panic(err)
	}
}
