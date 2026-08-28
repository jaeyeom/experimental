package detector

import (
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
)

var (
	bazelFormatLabels = []string{"//tools:format", "//:format"}
	bazelLintLabels   = []string{"//tools:lint", "//:lint"}
	makeTargetRe      = regexp.MustCompile(`^([A-Za-z0-9_./-]+)\s*:`)
	bazelNameRe       = regexp.MustCompile(`name\s*=\s*"([^"]+)"`)
)

func selectBuildSystem(root string, candidates []config.BuildSystem) config.BuildSystem {
	if len(candidates) == 0 {
		return config.BuildSystemNone
	}
	hasBazel, hasMake := false, false
	for _, candidate := range candidates {
		switch candidate {
		case config.BuildSystemBazel:
			hasBazel = true
		case config.BuildSystemMake:
			hasMake = true
		}
	}
	if hasBazel && hasMake {
		if bazelHasFormatAndLint(root) {
			return config.BuildSystemBazel
		}
		return config.BuildSystemMake
	}
	return candidates[0]
}

func bazelHasFormatAndLint(root string) bool {
	return bazelHasAnyLabel(root, bazelFormatLabels) && bazelHasAnyLabel(root, bazelLintLabels)
}

func bazelHasAnyLabel(root string, labels []string) bool {
	for _, label := range labels {
		if bazelLabelExists(root, label) {
			return true
		}
	}
	return false
}

func bazelLabelExists(root, label string) bool {
	pkg, name, ok := splitBazelLabel(label)
	if !ok {
		return false
	}
	dir := root
	if pkg != "" {
		dir = filepath.Join(root, filepath.FromSlash(pkg))
	}
	for _, build := range []string{"BUILD.bazel", "BUILD"} {
		if packageHasBazelTarget(filepath.Join(dir, build), name) {
			return true
		}
	}
	return false
}

func splitBazelLabel(label string) (pkg, name string, ok bool) {
	if !strings.HasPrefix(label, "//") {
		return "", "", false
	}
	pkg, name, found := strings.Cut(strings.TrimPrefix(label, "//"), ":")
	if !found || name == "" {
		return "", "", false
	}
	return pkg, name, true
}

func packageHasBazelTarget(buildFile, name string) bool {
	data, err := os.ReadFile(buildFile)
	if err != nil {
		return false
	}
	for _, match := range bazelNameRe.FindAllStringSubmatch(string(data), -1) {
		if match[1] == name {
			return true
		}
	}
	return false
}

func makefileHasTarget(root, target string) bool {
	for _, name := range []string{"Makefile", "makefile"} {
		if makefileContentHasTarget(filepath.Join(root, name), target) {
			return true
		}
	}
	return false
}

func makefileContentHasTarget(path, target string) bool {
	data, err := os.ReadFile(path)
	if err != nil {
		return false
	}
	for _, line := range strings.Split(string(data), "\n") {
		match := makeTargetRe.FindStringSubmatch(strings.TrimRight(line, "\r"))
		if len(match) == 2 && match[1] == target {
			return true
		}
	}
	return false
}

func availableCommands(specs ...string) []string {
	var out []string
	for _, spec := range specs {
		if commandAvailable(spec) {
			out = append(out, spec)
		}
	}
	return out
}

func commandAvailable(spec string) bool {
	fields := strings.Fields(spec)
	if len(fields) == 0 {
		return false
	}
	_, err := exec.LookPath(fields[0])
	return err == nil
}

type npmPackage struct {
	Scripts map[string]string `json:"scripts"`
}

func loadNpmScripts(rootPath string) map[string]string {
	path, ok := findPackageJSON(rootPath)
	if !ok {
		return nil
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil
	}
	var pkg npmPackage
	if err := json.Unmarshal(data, &pkg); err != nil {
		return nil
	}
	return pkg.Scripts
}

func findPackageJSON(rootPath string) (string, bool) {
	rootFile := filepath.Join(rootPath, "package.json")
	if _, err := os.Stat(rootFile); err == nil {
		return rootFile, true
	}
	scanner := NewScanner(DefaultScanOptions())
	result, err := scanner.Scan(rootPath)
	if err != nil {
		return "", false
	}
	best := ""
	bestDepth := -1
	for _, file := range result.Files {
		if filepath.Base(file) != "package.json" {
			continue
		}
		depth := strings.Count(file, string(filepath.Separator))
		if best == "" || depth < bestDepth {
			best = file
			bestDepth = depth
		}
	}
	if best == "" {
		return "", false
	}
	return filepath.Join(rootPath, best), true
}
