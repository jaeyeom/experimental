package detector

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

func TestJSToolsOmitsMissingNpmScripts(t *testing.T) {
	withBinsOnPath(t, "npm", "prettier", "eslint", "jest")
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		"package.json": `{"name":"app","scripts":{"test":"jest","format":"prettier --write ."}}`,
		"src/main.ts":  "export const x = 1;",
	})
	scan := mustScan(t, dir)

	tools := jsTools(dir, scan)
	assertContainsTool(t, tools[config.ToolTypeFormat], "npm run format")
	assertContainsTool(t, tools[config.ToolTypeTest], "npm test")
	assertNotContainsTool(t, tools[config.ToolTypeLint], "npm run lint")
}

func TestGoToolsOmitsMissingBinaries(t *testing.T) {
	binDir := t.TempDir()
	writeExecutable(t, filepath.Join(binDir, "gofmt"))
	writeExecutable(t, filepath.Join(binDir, "go"))
	t.Setenv("PATH", binDir)

	tools := goTools("", nil)
	assertContainsTool(t, tools[config.ToolTypeFormat], "gofmt")
	assertNotContainsTool(t, tools[config.ToolTypeFormat], "gofumpt")
	assertNotContainsTool(t, tools[config.ToolTypeLint], "golangci-lint")
	assertContainsTool(t, tools[config.ToolTypeTest], "go test")
}

func TestPythonToolsOmitsMissingBinaries(t *testing.T) {
	binDir := t.TempDir()
	writeExecutable(t, filepath.Join(binDir, "black"))
	writeExecutable(t, filepath.Join(binDir, "pytest"))
	t.Setenv("PATH", binDir)

	tools := pythonTools("", nil)
	assertContainsTool(t, tools[config.ToolTypeFormat], "black")
	assertNotContainsTool(t, tools[config.ToolTypeFormat], "ruff format")
	assertNotContainsTool(t, tools[config.ToolTypeLint], "ruff check")
	assertContainsTool(t, tools[config.ToolTypeTest], "pytest")
}

func writeExecutable(t *testing.T, path string) {
	t.Helper()
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	if err := os.Chmod(path, 0o755); err != nil {
		t.Fatal(err)
	}
}

func TestGoToolsListsFormatters(t *testing.T) {
	withBinsOnPath(t, "gofumpt", "gofmt", "golangci-lint", "go")

	tools := goTools("", nil)
	if len(tools[config.ToolTypeFormat]) == 0 {
		t.Error("Expected format tools for Go")
	}
	if len(tools[config.ToolTypeLint]) == 0 {
		t.Error("Expected lint tools for Go")
	}
	assertContainsTool(t, tools[config.ToolTypeFormat], "gofumpt")
}

func mustScan(t *testing.T, dir string) *ScanResult {
	t.Helper()
	result, err := NewScanner(DefaultScanOptions()).Scan(dir)
	if err != nil {
		t.Fatalf("Scan() error = %v", err)
	}
	return result
}
