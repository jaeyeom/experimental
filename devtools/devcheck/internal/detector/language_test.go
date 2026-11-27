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
	assertNotContainsTool(t, tools[config.ToolTypeLint], "unnecessary-interface-assertion-linter")
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

func TestGoToolsListsIfaceAssertLinterWhenPresent(t *testing.T) {
	withBinsOnPath(t, "golangci-lint", "unnecessary-interface-assertion-linter", "go")

	tools := goTools("", nil)
	assertContainsTool(t, tools[config.ToolTypeLint], "golangci-lint")
	assertContainsTool(t, tools[config.ToolTypeLint], "unnecessary-interface-assertion-linter")
}

func TestGoToolsIncludesGoVetFallback(t *testing.T) {
	withBinsOnPath(t, "gofmt", "go")

	tools := goTools("", nil)
	assertContainsTool(t, tools[config.ToolTypeLint], "go vet")
	assertContainsTool(t, tools[config.ToolTypeTest], "go test")
}

func TestPythonToolsUsesPython3AndUvFallback(t *testing.T) {
	withBinsOnPath(t, "python3", "uv")
	dir := t.TempDir()
	writeTree(t, dir, map[string]string{
		"pyproject.toml": "[project]\nname = \"app\"\n",
		"uv.lock":        "version = 1\n",
	})
	scan := mustScan(t, dir)

	tools := pythonTools(dir, scan)
	assertContainsTool(t, tools[config.ToolTypeTest], "python3 -m unittest")
	assertContainsTool(t, tools[config.ToolTypeFormat], "uv run --no-sync ruff format")
	assertContainsTool(t, tools[config.ToolTypeLint], "uv run --no-sync ruff check")
	assertContainsTool(t, tools[config.ToolTypeTest], "uv run --no-sync pytest")
}

func TestJSToolsDetectsPackageManagersBiomeAndNestedDir(t *testing.T) {
	t.Run("pnpm and biome", func(t *testing.T) {
		withBinsOnPath(t, "pnpm", "biome", "prettier", "eslint")
		dir := t.TempDir()
		writeTree(t, dir, map[string]string{
			"package.json":   `{"name":"app","scripts":{"lint":"biome lint ."}}`,
			"pnpm-lock.yaml": "lockfileVersion: '9.0'\n",
			"biome.json":     `{}`,
			"src/index.ts":   "export const x = 1;\n",
		})
		scan := mustScan(t, dir)

		tools := jsTools(dir, scan)
		assertContainsTool(t, tools[config.ToolTypeLint], "pnpm run lint")
		assertContainsTool(t, tools[config.ToolTypeFormat], "biome format --write")
		assertContainsTool(t, tools[config.ToolTypeLint], "biome lint")
	})

	t.Run("bun without test script uses bun test", func(t *testing.T) {
		withBinsOnPath(t, "bun")
		dir := t.TempDir()
		writeTree(t, dir, map[string]string{
			"package.json": `{"name":"app"}`,
			"bun.lock":     "{}",
			"index.ts":     "export const x = 1;\n",
		})
		scan := mustScan(t, dir)

		tools := jsTools(dir, scan)
		assertContainsTool(t, tools[config.ToolTypeTest], "bun test")
	})

	t.Run("nested package.json records Dir", func(t *testing.T) {
		withBinsOnPath(t, "npm")
		dir := t.TempDir()
		writeTree(t, dir, map[string]string{
			"frontend/package.json": `{"name":"web","scripts":{"test":"vitest"}}`,
			"frontend/index.ts":     "export const x = 1;\n",
		})
		scan := mustScan(t, dir)

		tools := jsTools(dir, scan)
		got := tools[config.ToolTypeTest]
		if len(got) == 0 || got[0].Dir != "frontend" {
			t.Fatalf("tools[test] = %+v, want Dir=frontend", got)
		}
	})

	t.Run("omits biome without biome config", func(t *testing.T) {
		withBinsOnPath(t, "biome", "prettier", "eslint")
		dir := t.TempDir()
		writeTree(t, dir, map[string]string{
			"package.json": `{"name":"app"}`,
			"index.js":     "const x = 1;\n",
		})
		scan := mustScan(t, dir)

		tools := jsTools(dir, scan)
		assertNotContainsTool(t, tools[config.ToolTypeFormat], "biome format --write")
		assertNotContainsTool(t, tools[config.ToolTypeLint], "biome lint")
	})
}

func mustScan(t *testing.T, dir string) *ScanResult {
	t.Helper()
	result, err := NewScanner(DefaultScanOptions()).Scan(dir)
	if err != nil {
		t.Fatalf("Scan() error = %v", err)
	}
	return result
}
