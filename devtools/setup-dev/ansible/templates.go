package main

var packagesTemplate = `---
{{- range .Imports }}
- import_playbook: {{.Playbook}}.yml{{if .When}}
  when: {{.When}}{{end}}
{{- end }}
- name: Ensure {{.Command}} is present
  hosts: all
  tasks:
    - name: Include guard for {{.Command}} playbook
      block:
        - name: Stop early if the {{.Command}} playbook is already included
          meta: end_play
          when: {{.CommandID}}_playbook_imported is defined
        - name: Ensure the {{.Command}} playbook is not included
          set_fact:
            {{.CommandID}}_playbook_imported: true
          when: {{.CommandID}}_playbook_imported is not defined
{{ if .UbuntuPPA }}
    - name: Ensure {{.Command}} PPA is present in Ubuntu
      apt_repository:
        repo: "{{.UbuntuPPA}}"
        state: present
        update_cache: yes
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin" and ansible_facts['distribution'] == "Ubuntu"
      become: yes

    - name: Ensure bookworm-backports is added to sources.list.d
      ansible.builtin.apt_repository:
        repo: "deb http://deb.debian.org/debian bookworm-backports main contrib non-free non-free-firmware"
        state: present
        update_cache: yes
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin" and ansible_facts['distribution'] == "Debian" and ansible_facts['distribution_major_version'] == "12"
      become: yes
{{ end }}{{ if .BrewTap }}
    - name: Tap {{.BrewTap}} for {{.Command}}
      community.general.homebrew_tap:
        name: {{.BrewTap}}
        state: present
      when: ansible_facts['os_family'] == "Darwin"
{{ end }}
    - name: Ensure {{.Command}} is present on MacOS
      block:
        - name: Check if {{.Command}} is installed
          shell: command -v {{.Command}}
          changed_when: False
      rescue:
        - name: Install {{.Command}} on MacOS
          community.general.homebrew:
            name: {{.BrewPkgName}}
            state: present{{ if .BrewOptions }}
            install_options:{{ range .BrewOptions }}
              - {{ . }}{{ end }}{{ end }}
      when: ansible_facts['os_family'] == "Darwin"

    - name: Ensure {{.Command}} is present on non-Termux, non-MacOS systems
      block:
        - name: Check if {{.Command}} is installed
          shell: command -v {{.Command}}
          changed_when: False
      rescue:
        - name: Install {{.Command}} on non-Termux, non-MacOS systems
          package:
            name: {{.DebianPkgName}}
            state: present
          become: yes
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin"

    - name: Ensure {{.Command}} is present on Termux
      block:
        - name: Check if {{.Command}} is installed
          shell: command -v {{.Command}}
          changed_when: False
      rescue:
        - name: Install {{.Command}} on Termux
          command: pkg install -y {{.TermuxPkgName}}
      when: ansible_env.TERMUX_VERSION is defined{{.Suffix}}
`

var platformSpecificTemplate = `---
{{- range .GetAllImports }}
- import_playbook: {{.Playbook}}.yml{{if .When}}
  when: {{.When}}{{end}}
{{- end }}

- name: Ensure {{.Command}} is present
  hosts: all
  tasks:
    - name: Include guard for {{.Command}} playbook
      block:
        - name: Stop early if the {{.Command}} playbook is already included
          meta: end_play
          when: {{.CommandID}}_playbook_imported is defined
        - name: Ensure the {{.Command}} playbook is not included
          set_fact:
            {{.CommandID}}_playbook_imported: true
          when: {{.CommandID}}_playbook_imported is not defined
{{- $platforms := .GetPlatforms }}
{{- if index $platforms "darwin" }}
{{- $method := index $platforms "darwin" }}

    - name: Ensure {{.Command}} is present on MacOS
{{$method.RenderInstallTask .Command}}
      when: ansible_facts['os_family'] == "Darwin"
{{- end }}
{{- if index $platforms "termux" }}
{{- $method := index $platforms "termux" }}

    - name: Ensure {{.Command}} is present on Termux
{{$method.RenderInstallTask .Command}}
      when: ansible_env.TERMUX_VERSION is defined
{{- end }}
{{- if index $platforms "all" }}
{{- $method := index $platforms "all" }}

{{$method.RenderInstallTask .Command}}
{{- else }}
{{- if index $platforms "debian-like" }}
{{- $method := index $platforms "debian-like" }}
{{- if or (eq $method.GetMethodType "pip") (eq $method.GetMethodType "uv") }}

{{$method.RenderInstallTask .Command}}
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin"
{{- else if eq $method.GetMethodType "cargo" }}

    - name: Install {{.Command}} via cargo on Debian/Ubuntu
      block:
{{$method.RenderBlockInstallTask .Command}}
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin"
{{- else }}

    - name: Install {{.Command}} via {{$method.GetMethodType}} on Debian/Ubuntu
{{$method.RenderInstallTask .Command}}
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin"
{{- end }}
{{- else }}
{{- if index $platforms "debian" }}
{{- $method := index $platforms "debian" }}

    - name: Install {{.Command}} via {{$method.GetMethodType}} on Debian
{{$method.RenderInstallTask .Command}}
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin" and ansible_facts['distribution'] == "Debian"
{{- end }}
{{- if index $platforms "ubuntu" }}
{{- $method := index $platforms "ubuntu" }}

    - name: Install {{.Command}} via {{$method.GetMethodType}} on Ubuntu
{{$method.RenderInstallTask .Command}}
      when: ansible_env.TERMUX_VERSION is not defined and ansible_facts['os_family'] != "Darwin" and ansible_facts['distribution'] == "Ubuntu"
{{- end }}
{{- end }}
{{- end }}
`
