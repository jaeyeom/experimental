package main

import (
	"strings"
	"testing"
)

func TestGoInstallMethodRenderInstallTask(t *testing.T) {
	method := GoInstallMethod{PkgPath: "example.com/mytool@latest"}
	got := method.RenderInstallTask("my-tool")

	// Hyphens in the command name must become underscores in Ansible vars.
	for _, want := range []string{
		"my_tool_installed",
		"my_tool_module_version",
		"my_tool_latest",
		"my_tool_build_go",
		"my_tool_upgrade",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("RenderInstallTask() missing variable %q", want)
		}
	}

	// Build Go version is extracted from the binary metadata.
	if !strings.Contains(got, "go version -m $(command -v my-tool)") {
		t.Error("RenderInstallTask() missing go version -m for build Go version")
	}
	if !strings.Contains(got, "my_tool_build_go") {
		t.Error("RenderInstallTask() missing build Go version register")
	}

	// Upgrade when module is missing/outdated OR built with a different toolchain.
	for _, want := range []string{
		"my_tool_module_version is not defined",
		"my_tool_module_version == \"\"",
		"my_tool_module_version != my_tool_latest.stdout",
		"my_tool_build_go.stdout | default('') != go_toolchain_version",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("RenderInstallTask() missing upgrade condition fragment %q\nfull output:\n%s", want, got)
		}
	}

	// Install uses the configured package path.
	if !strings.Contains(got, "go install example.com/mytool@latest") {
		t.Error("RenderInstallTask() missing go install package path")
	}
}

func TestGoInstallMethodGetImports(t *testing.T) {
	method := GoInstallMethod{PkgPath: "example.com/mytool@latest"}
	imports := method.GetImports()
	if len(imports) != 1 || imports[0].Playbook != "setup-user-go-bin-directory" {
		t.Errorf("GetImports() = %v, want setup-user-go-bin-directory", imports)
	}
}
