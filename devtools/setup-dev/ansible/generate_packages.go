// Binary generate_packages generates Ansible yaml files for the package
// installations.
package main

import (
	"os"
	"text/template"
)

var packagesTemplate = `---
- name: Ensure {{.PkgName}} is present
  hosts: all
  tasks:
    - name: Ensure {{.PkgName}} is present on non-Termux systems
      package:
        name: {{.PkgName}}
        state: present
      when: ansible_env.TERMUX_VERSION is not defined
      become: yes

    - name: Ensure {{.PkgName}} is present on Termux
      command: command -v {{.PkgName}} || pkg install -y {{.PkgName}}
      when: ansible_env.TERMUX_VERSION is defined
`

type PackageData struct {
	PkgName string
}

var packages = []PackageData{
	{"curl"},
	{"git"},
	{"jq"},
}

func main() {
	tmpl, err := template.New("packages").Parse(packagesTemplate)
	if err != nil {
		panic(err)
	}

	for _, pkg := range packages {
		f, err := os.Create(pkg.PkgName + ".yml")
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
