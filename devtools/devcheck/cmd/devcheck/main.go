// Package main provides a CLI tool for DevCheck project detection and tool execution demo.
package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/devcheck/internal/config"
	"github.com/jaeyeom/experimental/devtools/devcheck/internal/detector"
	"github.com/jaeyeom/experimental/devtools/devcheck/internal/runner"
)

func main() {
	var (
		dir        string
		demoMode   bool
		concurrent bool
		maxWorkers int
	)

	// Parse command line flags
	flag.BoolVar(&demoMode, "demo", false, "Run tool executor demo")
	flag.BoolVar(&concurrent, "concurrent", true, "Use concurrent execution in demo mode")
	flag.IntVar(&maxWorkers, "max-workers", 3, "Maximum concurrent workers in demo mode")
	flag.Parse()

	// Get the directory to analyze
	if flag.NArg() > 0 {
		dir = flag.Arg(0)
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

	// Run appropriate mode
	var err error
	if demoMode {
		err = runExecutorDemo(dir, os.Stdout, concurrent, maxWorkers)
	} else {
		err = detectAndPrint(dir, os.Stdout)
	}

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

// runExecutorDemo demonstrates the tool executor framework capabilities.
func runExecutorDemo(dir string, w io.Writer, concurrent bool, maxWorkers int) error {
	printDemoHeader(w)

	projectConfig, err := detectProject(dir, w)
	if err != nil {
		return err
	}

	demoConfigs, err := prepareDemoConfigs(projectConfig, dir)
	if err != nil {
		return err
	}

	if len(demoConfigs) == 0 {
		fmt.Fprintf(w, "\nNo demo tools available to run.\n")
		return nil
	}

	return executeDemoConfigs(demoConfigs, w, concurrent, maxWorkers)
}

func printDemoHeader(w io.Writer) {
	fmt.Fprintf(w, "🚀 DevCheck Tool Executor Demo\n")
	fmt.Fprintf(w, "==============================\n\n")
}

func detectProject(dir string, w io.Writer) (*config.ProjectConfig, error) {
	projectDetector := detector.NewProjectDetector()
	projectConfig, err := projectDetector.Detect(dir)
	if err != nil {
		return nil, fmt.Errorf("failed to detect project: %w", err)
	}

	absPath, _ := filepath.Abs(dir)
	fmt.Fprintf(w, "Path: %s\n", absPath)

	showDetectedTools(w, projectConfig)
	return projectConfig, nil
}

func showDetectedTools(w io.Writer, projectConfig *config.ProjectConfig) {
	fmt.Fprintf(w, "\nDetected Tools:\n")
	if len(projectConfig.Tools) > 0 {
		for toolType, tools := range projectConfig.Tools {
			if len(tools) > 0 {
				fmt.Fprintf(w, "  %s: %s\n", toolType, tools[0])
			}
		}
	}
}

func prepareDemoConfigs(projectConfig *config.ProjectConfig, dir string) ([]runner.ToolConfig, error) {
	var demoConfigs []runner.ToolConfig
	basicExec := runner.NewBasicExecutor()

	// Add Bazel support
	if projectConfig.BuildSystem == "bazel" && basicExec.IsAvailable("bazel") {
		demoConfigs = append(demoConfigs, createBazelConfig(dir))
	}

	// Add language-specific tools
	langConfigs := addLanguageSpecificTools(projectConfig, dir, basicExec)
	demoConfigs = append(demoConfigs, langConfigs...)

	// Add git status
	if projectConfig.HasGit && basicExec.IsAvailable("git") {
		demoConfigs = append(demoConfigs, createGitConfig(dir))
	}

	return demoConfigs, nil
}

func createBazelConfig(dir string) runner.ToolConfig {
	return runner.ToolConfig{
		Command:    "bazel",
		Args:       []string{"info", "workspace"},
		WorkingDir: dir,
		Timeout:    15 * time.Second,
	}
}

func createGitConfig(dir string) runner.ToolConfig {
	return runner.ToolConfig{
		Command:    "git",
		Args:       []string{"status", "--short"},
		WorkingDir: dir,
	}
}

func addLanguageSpecificTools(projectConfig *config.ProjectConfig, dir string, basicExec runner.Executor) []runner.ToolConfig {
	var configs []runner.ToolConfig

	for _, lang := range projectConfig.Languages {
		switch lang {
		case config.LanguageGo:
			configs = append(configs, addGoTools(dir, basicExec)...)
		case config.LanguagePython:
			configs = append(configs, addPythonTools(projectConfig, dir, basicExec)...)
		}
	}

	return configs
}

func addGoTools(dir string, basicExec runner.Executor) []runner.ToolConfig {
	var configs []runner.ToolConfig

	if basicExec.IsAvailable("go") {
		configs = append(configs, runner.ToolConfig{
			Command:    "go",
			Args:       []string{"list", "./devtools/devcheck/..."},
			WorkingDir: dir,
		})
	}

	if basicExec.IsAvailable("golangci-lint") {
		configs = append(configs, runner.ToolConfig{
			Command:    "golangci-lint",
			Args:       []string{"run", "--timeout=30s", "./devtools/devcheck/..."},
			WorkingDir: dir,
			Timeout:    45 * time.Second,
		})
	}

	return configs
}

func addPythonTools(projectConfig *config.ProjectConfig, dir string, basicExec runner.Executor) []runner.ToolConfig {
	var configs []runner.ToolConfig

	if basicExec.IsAvailable("ruff") && projectConfig.ConfigFiles["ruff"] != "" {
		configs = append(configs, runner.ToolConfig{
			Command:    "ruff",
			Args:       []string{"check", "--statistics"},
			WorkingDir: dir,
			Timeout:    10 * time.Second,
		})
	}

	return configs
}

func executeDemoConfigs(demoConfigs []runner.ToolConfig, w io.Writer, concurrent bool, maxWorkers int) error {
	signalExecutor := runner.NewExecutorWithSignalHandling()
	ctx, err := signalExecutor.Start()
	if err != nil {
		return fmt.Errorf("failed to start signal handler: %w", err)
	}
	defer signalExecutor.Stop()

	if concurrent {
		fmt.Fprintf(w, "\nRunning %d tools concurrently (max %d workers)...\n\n", len(demoConfigs), maxWorkers)
		return runConcurrentDemo(ctx, w, signalExecutor, demoConfigs, maxWorkers)
	}

	fmt.Fprintf(w, "\nRunning %d tools sequentially...\n\n", len(demoConfigs))
	return runSequentialDemo(ctx, w, signalExecutor, demoConfigs)
}

// runSequentialDemo runs tools one by one.
func runSequentialDemo(ctx context.Context, w io.Writer, executor runner.Executor, configs []runner.ToolConfig) error {
	startTime := time.Now()
	successCount := 0

	for i, cfg := range configs {
		fmt.Fprintf(w, "[%d/%d] Running: %s %s\n", i+1, len(configs), cfg.Command, strings.Join(cfg.Args, " "))

		cmdStart := time.Now()
		result, err := executor.Execute(ctx, cfg)
		duration := time.Since(cmdStart)

		switch {
		case err != nil:
			fmt.Fprintf(w, "  ❌ ERROR: %v\n", err)
		case result.ExitCode != 0:
			fmt.Fprintf(w, "  ❌ FAILED (exit code %d)\n", result.ExitCode)
			if result.Stderr != "" {
				fmt.Fprintf(w, "  stderr: %s\n", strings.TrimSpace(result.Stderr))
			}
		default:
			successCount++
			fmt.Fprintf(w, "  ✅ SUCCESS (%v)\n", duration.Round(time.Millisecond))
			if result.Output != "" {
				fmt.Fprintf(w, "  output: %s\n", strings.TrimSpace(result.Output))
			}
		}
		fmt.Fprintln(w)
	}

	totalDuration := time.Since(startTime)
	fmt.Fprintf(w, "Summary: %d/%d tools passed (total time: %v)\n",
		successCount, len(configs), totalDuration.Round(time.Millisecond))

	return nil
}

// runConcurrentDemo runs tools concurrently.
func runConcurrentDemo(ctx context.Context, w io.Writer, executor runner.Executor, configs []runner.ToolConfig, maxWorkers int) error {
	startTime := time.Now()

	// Create concurrent executor
	concurrentExec := runner.NewConcurrentExecutor(executor)
	concurrentExec.SetMaxConcurrency(maxWorkers)

	// Execute all tools concurrently
	results, err := concurrentExec.ExecuteAll(ctx, configs)
	if err != nil {
		return fmt.Errorf("concurrent execution failed: %w", err)
	}

	// Process and display results
	successCount := 0
	fmt.Fprintf(w, "┌─────────────────────────────────┬─────────┬──────────┬───────────┐\n")
	fmt.Fprintf(w, "│ Tool                            │ Status  │ Duration │ Exit Code │\n")
	fmt.Fprintf(w, "├─────────────────────────────────┼─────────┼──────────┼───────────┤\n")

	for _, res := range results {
		cmd := res.Config.Command
		if len(res.Config.Args) > 0 {
			cmd = fmt.Sprintf("%s %s", cmd, strings.Join(res.Config.Args, " "))
		}
		if len(cmd) > 32 {
			cmd = cmd[:29] + "..."
		}

		status := "✅ PASS"
		exitCode := "0"
		duration := "N/A"

		if res.Error != nil {
			status = "❌ ERROR"
			exitCode = "N/A"
		} else if res.Result != nil {
			duration = res.Result.Duration().Round(time.Millisecond).String()
			if res.Result.ExitCode != 0 {
				status = "❌ FAIL"
				exitCode = fmt.Sprintf("%d", res.Result.ExitCode)
			} else {
				successCount++
			}
		}

		fmt.Fprintf(w, "│ %-31s │ %-7s │ %-8s │ %-9s │\n", cmd, status, duration, exitCode)
	}

	fmt.Fprintf(w, "└─────────────────────────────────┴─────────┴──────────┴───────────┘\n")

	// Show error details for failed commands
	for _, res := range results {
		if res.Error != nil {
			cmd := res.Config.Command
			if len(res.Config.Args) > 0 {
				cmd = fmt.Sprintf("%s %s", cmd, strings.Join(res.Config.Args, " "))
			}
			fmt.Fprintf(w, "\n❌ %s failed: %v\n", cmd, res.Error)
		}
	}

	totalDuration := time.Since(startTime)
	fmt.Fprintf(w, "\nSummary: %d/%d tools passed (total time: %v with concurrency)\n",
		successCount, len(configs), totalDuration.Round(time.Millisecond))

	return nil
}
