// Package main provides a simple CLI tool to test the DevCheck project detection system.
package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
	"github.com/jaeyeom/experimental/devtools/devcheck/internal/detector"
)

func main() {
	// Get the directory to analyze
	var dir string
	if len(os.Args) > 1 {
		dir = os.Args[1]
	} else {
		// Use the original working directory when invoked with bazel run
		if wd := os.Getenv("BUILD_WORKING_DIRECTORY"); wd != "" {
			dir = wd
		} else {
			// Fallback to current directory
			var err error
			dir, err = os.Getwd()
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error getting current directory: %v\n", err)
				os.Exit(1)
			}
		}
	}

	err := detectAndPrint(dir, os.Stdout)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

// detectAndPrint runs project detection on the given directory and prints results to the writer.
func detectAndPrint(dir string, w io.Writer) error {
	// Create project detector
	projectDetector := detector.NewProjectDetector()

	// Run detection
	projectConfig, err := projectDetector.Detect(dir)
	if err != nil {
		return fmt.Errorf("failed to detect project configuration: %w", err)
	}

	// Print results
	fmt.Fprintf(w, "🔍 DevCheck Project Detection Results\n")
	fmt.Fprintf(w, "=====================================\n\n")

	// Path information
	absPath, _ := filepath.Abs(dir)
	fmt.Fprintf(w, "Path: %s\n", absPath)

	// Languages
	if len(projectConfig.Languages) > 0 {
		var languages []string
		for _, lang := range projectConfig.Languages {
			languages = append(languages, string(lang))
		}
		sort.Strings(languages)
		fmt.Fprintf(w, "Languages: %s\n", strings.Join(languages, ", "))
	} else {
		fmt.Fprintf(w, "Languages: none detected\n")
	}

	// Build system
	fmt.Fprintf(w, "Build System: %s\n", projectConfig.BuildSystem)

	// Git repository
	if projectConfig.HasGit {
		fmt.Fprintf(w, "Git Repository: yes\n")
	} else {
		fmt.Fprintf(w, "Git Repository: no\n")
	}

	// Tools
	if len(projectConfig.Tools) > 0 {
		fmt.Fprintf(w, "\nTools:\n")

		// Sort tool types for consistent output
		var toolTypes []string
		for toolType := range projectConfig.Tools {
			toolTypes = append(toolTypes, string(toolType))
		}
		sort.Strings(toolTypes)

		for _, toolTypeStr := range toolTypes {
			tools := projectConfig.Tools[config.ToolType(toolTypeStr)]
			if len(tools) > 0 {
				// Show only the first (highest priority) tool for simplicity
				fmt.Fprintf(w, "  %s: %s\n", toolTypeStr, tools[0])
			}
		}
	} else {
		fmt.Fprintf(w, "\nTools: none detected\n")
	}

	// Configuration files
	if len(projectConfig.ConfigFiles) > 0 {
		fmt.Fprintf(w, "\nConfiguration Files:\n")

		// Sort config files for consistent output
		var tools []string
		for tool := range projectConfig.ConfigFiles {
			tools = append(tools, tool)
		}
		sort.Strings(tools)

		for _, tool := range tools {
			fmt.Fprintf(w, "  %s: %s\n", tool, projectConfig.ConfigFiles[tool])
		}
	}

	// Detection time
	fmt.Fprintf(w, "\nDetection completed at: %s\n", projectConfig.DetectionTime.Format("2006-01-02 15:04:05"))

	return nil
}
