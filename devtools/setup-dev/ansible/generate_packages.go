// Binary generate_packages generates Ansible yaml files for the package
// installations.
package main

import (
	"os"
	"text/template"
)

var packagesTemplate = `---
- name: Ensure {{.Command}} is present
  hosts: all
  tasks:
    - name: Ensure {{.Command}} is present on non-Termux systems
      package:
        name: {{.DebianPkgName}}
        state: present
      when: ansible_env.TERMUX_VERSION is not defined
      become: yes

    - name: Ensure {{.Command}} is present on Termux
      shell: command -v {{.Command}} || pkg install -y {{.TermuxPkgName}}
      when: ansible_env.TERMUX_VERSION is defined
`

type PackageData struct {
	Command       string
	debianPkgName string
	termuxPkgName string
}

func (p PackageData) DebianPkgName() string {
	if p.debianPkgName != "" {
		return p.debianPkgName
	}
	return p.Command
}

func (p PackageData) TermuxPkgName() string {
	if p.termuxPkgName != "" {
		return p.termuxPkgName
	}
	return p.Command
}

var packages = []PackageData{
	{Command: "curl"},
	{Command: "emacs"},
	{Command: "git"},
	{Command: "jq"},
	{Command: "ssh", debianPkgName: "openssh-client", termuxPkgName: "openssh"},
}

func main() {
	tmpl, err := template.New("packages").Parse(packagesTemplate)
	if err != nil {
		panic(err)
	}

	for _, pkg := range packages {
		f, err := os.Create(pkg.Command + ".yml")
		if err != nil {
			panic(err)
		}
		defer f.Close()
		err = tmpl.Execute(f, pkg)
		if err != nil {
			panic(err)
		}
	}
}
