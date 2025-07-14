package sync

import (
	"bufio"
	"fmt"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/jaeyeom/experimental/devtools/repo-sync/internal/config"
)

// RsyncResult represents the result of an rsync operation.
type RsyncResult struct {
	FilesSynced      int
	BytesTransferred int64
	Duration         time.Duration
	Output           string
}

// generateRsyncPatterns creates rsync include/exclude patterns from project config.
func (e *Engine) generateRsyncPatterns() ([]string, error) {
	var patterns []string

	// Add include patterns
	for _, pattern := range e.project.SyncPatterns.Include {
		patterns = append(patterns, "--include="+pattern)
	}

	// Add exclude patterns
	for _, pattern := range e.project.SyncPatterns.Exclude {
		patterns = append(patterns, "--exclude="+pattern)
	}

	// If no patterns specified, use global defaults
	if len(e.project.SyncPatterns.Include) == 0 && len(e.project.SyncPatterns.Exclude) == 0 {
		globalConfig, err := config.LoadGlobalConfig()
		if err == nil {
			for _, pattern := range globalConfig.DefaultSyncPatterns.Include {
				patterns = append(patterns, "--include="+pattern)
			}
			for _, pattern := range globalConfig.DefaultSyncPatterns.Exclude {
				patterns = append(patterns, "--exclude="+pattern)
			}
		}
	}

	// Exclude everything else if we have include patterns
	if len(e.project.SyncPatterns.Include) > 0 {
		patterns = append(patterns, "--exclude=*")
	}

	return patterns, nil
}

// performRsync executes rsync with the specified parameters.
func (e *Engine) performRsync(src, dst string, dryRun bool) (*RsyncResult, error) {
	startTime := time.Now()

	// Generate rsync patterns
	patterns, err := e.generateRsyncPatterns()
	if err != nil {
		return nil, fmt.Errorf("failed to generate rsync patterns: %w", err)
	}

	// Build rsync command
	args := []string{
		"-avz",               // archive, verbose, compress
		"--delete",           // delete files that don't exist in source
		"--delete-excluded",  // delete excluded files from destination
		"--prune-empty-dirs", // don't create empty directories
		"--human-readable",   // human readable output
		"--progress",         // show progress
		"--stats",            // show statistics
	}

	// Add compression level
	globalConfig, err := config.LoadGlobalConfig()
	if err == nil {
		compressionLevel := globalConfig.Performance.RsyncCompressionLevel
		args = append(args, fmt.Sprintf("--compress-level=%d", compressionLevel))
	}

	// Add dry-run flag if requested
	if dryRun {
		args = append(args, "--dry-run")
	}

	// Add patterns
	args = append(args, patterns...)

	// Add source and destination (ensure trailing slash for directories)
	if !strings.HasSuffix(src, "/") {
		src += "/"
	}
	if !strings.HasSuffix(dst, "/") {
		dst += "/"
	}
	args = append(args, src, dst)

	// Execute rsync command
	cmd := exec.Command("rsync", args...)

	// Create pipes for output
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, fmt.Errorf("failed to create stdout pipe: %w", err)
	}

	stderr, err := cmd.StderrPipe()
	if err != nil {
		return nil, fmt.Errorf("failed to create stderr pipe: %w", err)
	}

	// Start command
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("failed to start rsync command: %w", err)
	}

	// Read output
	var outputLines []string
	var errorLines []string

	// Read stdout
	go func() {
		scanner := bufio.NewScanner(stdout)
		for scanner.Scan() {
			line := scanner.Text()
			outputLines = append(outputLines, line)
			e.logger.Debug("rsync stdout: " + line)
		}
	}()

	// Read stderr
	go func() {
		scanner := bufio.NewScanner(stderr)
		for scanner.Scan() {
			line := scanner.Text()
			errorLines = append(errorLines, line)
			e.logger.Debug("rsync stderr: " + line)
		}
	}()

	// Wait for command to complete
	if err := cmd.Wait(); err != nil {
		errorOutput := strings.Join(errorLines, "\n")
		return nil, fmt.Errorf("rsync command failed: %w\nOutput: %s", err, errorOutput)
	}

	// Parse rsync output
	result := &RsyncResult{
		Duration: time.Since(startTime),
		Output:   strings.Join(outputLines, "\n"),
	}

	if err := e.parseRsyncOutput(outputLines, result); err != nil {
		e.logger.Warn("Failed to parse rsync output", "error", err)
	}

	return result, nil
}

// parseRsyncOutput parses rsync output to extract statistics.
func (e *Engine) parseRsyncOutput(lines []string, result *RsyncResult) error {
	// Regular expressions for parsing rsync output
	fileRegex := regexp.MustCompile(`^(>|\.|<)[f]`)
	totalSizeRegex := regexp.MustCompile(`Total transferred file size: ([\d,]+) bytes`)
	totalBytesRegex := regexp.MustCompile(`Total bytes sent: ([\d,]+)`)

	fileCount := 0
	for _, line := range lines {
		// Count transferred files
		if fileRegex.MatchString(line) {
			fileCount++
		}

		// Extract total transferred size
		if matches := totalSizeRegex.FindStringSubmatch(line); len(matches) > 1 {
			sizeStr := strings.ReplaceAll(matches[1], ",", "")
			if size, err := strconv.ParseInt(sizeStr, 10, 64); err == nil {
				result.BytesTransferred = size
			}
		}

		// Extract total bytes sent (fallback)
		if result.BytesTransferred == 0 {
			if matches := totalBytesRegex.FindStringSubmatch(line); len(matches) > 1 {
				sizeStr := strings.ReplaceAll(matches[1], ",", "")
				if size, err := strconv.ParseInt(sizeStr, 10, 64); err == nil {
					result.BytesTransferred = size
				}
			}
		}
	}

	result.FilesSynced = fileCount
	return nil
}
