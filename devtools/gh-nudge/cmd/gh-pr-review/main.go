package main

import (
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"

	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/argparser"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/models"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/prreview"
	"github.com/jaeyeom/experimental/devtools/gh-nudge/internal/storage"
)

// createOutputFormatter creates the appropriate output formatter based on jsonOutput flag.
func createOutputFormatter(jsonOutput bool) prreview.OutputFormatter {
	if jsonOutput {
		return models.NewJSONFormatter()
	}
	return models.NewTextFormatter()
}

const (
	version = "1.0.0"
)

// autoDetectOwnerRepo gets the default owner/repo using gh CLI.
func autoDetectOwnerRepo() (string, error) {
	cmd := exec.Command("gh", "repo", "set-default", "-v")
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get default repo: %w", err)
	}

	return strings.TrimSpace(string(output)), nil
}

// autoDetectIdentifier gets the PR number if available, otherwise the current branch.
func autoDetectIdentifier() (string, error) {
	// Try to get PR number first
	cmd := exec.Command("gh", "pr", "view", "--json", "number", "-t", "{{.number}}")
	if output, err := cmd.Output(); err == nil {
		return strings.TrimSpace(string(output)), nil
	}

	// Fall back to current branch
	cmd = exec.Command("git", "branch", "--show-current")
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get current branch: %w", err)
	}

	return strings.TrimSpace(string(output)), nil
}

// autoDetectArgs fills in missing owner/repo and identifier arguments.
func autoDetectArgs(args []string) ([]string, error) {
	if len(args) < 2 {
		// Need to auto-detect both owner/repo and identifier
		ownerRepo, err := autoDetectOwnerRepo()
		if err != nil {
			return nil, fmt.Errorf("auto-detecting owner/repo: %w", err)
		}

		identifier, err := autoDetectIdentifier()
		if err != nil {
			return nil, fmt.Errorf("auto-detecting identifier: %w", err)
		}

		// Insert the auto-detected values at the beginning
		newArgs := make([]string, 0, len(args)+2)
		newArgs = append(newArgs, ownerRepo, identifier)
		newArgs = append(newArgs, args...)
		return newArgs, nil
	}

	// Check if first argument looks like owner/repo format
	if !strings.Contains(args[0], "/") {
		// First argument doesn't look like owner/repo, try to auto-detect
		ownerRepo, err := autoDetectOwnerRepo()
		if err != nil {
			return nil, fmt.Errorf("auto-detecting owner/repo: %w", err)
		}

		// Insert owner/repo at the beginning
		newArgs := make([]string, 0, len(args)+1)
		newArgs = append(newArgs, ownerRepo)
		newArgs = append(newArgs, args...)
		return newArgs, nil
	}

	return args, nil
}

// autoDetectArgsForComment handles auto-detection for comment command which has additional required args.
func autoDetectArgsForComment(args []string) ([]string, error) {
	// Count non-option arguments (those that don't start with --)
	nonOptionArgs := make([]string, 0)
	options := make([]string, 0)

	for _, arg := range args {
		if strings.HasPrefix(arg, "--") {
			options = append(options, arg)
		} else {
			nonOptionArgs = append(nonOptionArgs, arg)
		}
	}

	// We need at least 3 non-option args: file, line, comment
	if len(nonOptionArgs) < 3 {
		return args, nil // Not enough args even with auto-detection
	}

	// If we have exactly 3 non-option args, we need to auto-detect owner/repo and identifier
	if len(nonOptionArgs) == 3 {
		ownerRepo, err := autoDetectOwnerRepo()
		if err != nil {
			return nil, fmt.Errorf("auto-detecting owner/repo: %w", err)
		}

		identifier, err := autoDetectIdentifier()
		if err != nil {
			return nil, fmt.Errorf("auto-detecting identifier: %w", err)
		}

		// Rebuild args with auto-detected values first
		newArgs := make([]string, 0, len(args)+2)
		newArgs = append(newArgs, ownerRepo, identifier)
		newArgs = append(newArgs, nonOptionArgs...)
		newArgs = append(newArgs, options...)
		return newArgs, nil
	}

	// If we have 4 non-option args, check if first looks like owner/repo
	if len(nonOptionArgs) == 4 && !strings.Contains(nonOptionArgs[0], "/") {
		ownerRepo, err := autoDetectOwnerRepo()
		if err != nil {
			return nil, fmt.Errorf("auto-detecting owner/repo: %w", err)
		}

		// Rebuild args with auto-detected owner/repo first
		newArgs := make([]string, 0, len(args)+1)
		newArgs = append(newArgs, ownerRepo)
		newArgs = append(newArgs, nonOptionArgs...)
		newArgs = append(newArgs, options...)
		return newArgs, nil
	}

	// Return original args if no auto-detection needed
	return args, nil
}

func main() {
	if len(os.Args) < 2 {
		showUsage()
		os.Exit(1)
	}

	command := os.Args[1]
	args := os.Args[2:]

	executeCommand(command, args)
}

func executeCommand(command string, args []string) {
	handler := getCommandHandler(command)
	if handler != nil {
		handler(args)
		return
	}

	if isHelpCommand(command) {
		showUsage()
		return
	}

	if command == "version" {
		fmt.Printf("gh-pr-review version %s\n", version)
		return
	}

	fmt.Fprintf(os.Stderr, "Unknown command: %s\n", command)
	showUsage()
	os.Exit(1)
}

func getCommandHandler(command string) func([]string) {
	handlers := map[string]func([]string){
		"capture": handleCapture,
		"comment": handleComment,
		"submit":  handleSubmit,
		"list":    handleList,
		"delete":  handleDelete,
		"clear":   handleClear,
		"adjust":  handleAdjust,
	}
	return handlers[command]
}

func isHelpCommand(command string) bool {
	return command == "help" || command == "-h" || command == "--help"
}

func showUsage() {
	fmt.Printf(`gh-pr-review - GitHub PR Review CLI Utility

Usage: gh-pr-review <command> [options]

Commands:
  capture [<owner>/<repo>] [<identifier>]         Capture diff hunks (PR or branch)
  comment [<owner>/<repo>] [<identifier>] <file> <line> "<comment>"  Add line comment
  list [<owner>/<repo>] [<identifier>]            List stored comments (PR or branch)
  clear [<owner>/<repo>] [<identifier>]           Clear all comments (PR or branch)
  delete [<owner>/<repo>] [<identifier>] --comment-id <ID>  Delete comment by ID (PR or branch)
  submit <owner>/<repo> <pr_number>               Submit review to GitHub (PR only)
  adjust [<owner>/<repo>] [<identifier>] [file] --diff <spec>     Adjust comment line numbers
  adjust [<owner>/<repo>] [<identifier>] [file] --unified-diff <spec>  Adjust using git diff output
  version                                         Show version information
  help                                           Show this help message

Auto-detection:
  <owner>/<repo>  Auto-detected from 'gh repo set-default -v' if omitted
  <identifier>    Auto-detected from current PR or git branch if omitted

Identifier Types:
  PR numbers     Pure integers (e.g., 42, 123)
  Branch names   Any non-numeric string (e.g., feature/auth, main, my-branch)

Examples:
  # Auto-detect repo and PR/branch
  gh-pr-review capture

  # Auto-detect repo, specify branch
  gh-pr-review capture feature/auth-fix

  # Explicit repo and PR
  gh-pr-review capture octocat/Hello-World 42

  # Auto-detect repo and PR/branch, add comment
  gh-pr-review comment src/main.js 15 "Consider using const instead of let"

  # Auto-detect repo, specify branch, add comment
  gh-pr-review comment feature/auth-fix src/main.js 15 "Use const instead of let"

  # Explicit repo, PR, and multi-line comment
  gh-pr-review comment octocat/Hello-World 42 src/main.js 15-20 "This function needs refactoring"

  # Auto-detect and list comments
  gh-pr-review list --format json

  # Auto-detect repo, specify branch, filter by file
  gh-pr-review list feature/auth-fix --file src/auth.js

  # Auto-detect and clear all comments
  gh-pr-review clear --confirm

  # Submit review (PR only, clears local comments by default)
  gh-pr-review submit octocat/Hello-World 42 --body "Code review completed" --event APPROVE

  # Submit review for specific file only
  gh-pr-review submit octocat/Hello-World 42 --file src/main.js --body "JavaScript review"

  # Submit review and keep local comments (PR only)
  gh-pr-review submit octocat/Hello-World 42 --body "First pass" --after keep

  # Delete specific comment from PR by ID
  gh-pr-review delete octocat/Hello-World 42 --comment-id a1b2c3d4

  # Delete specific comment from branch by ID
  gh-pr-review delete octocat/Hello-World feature/auth-fix --comment-id b5f6e7d8 --confirm

Environment Variables:
  GH_STORAGE_HOME         Storage directory [default: ~/.config/gh-nudge/storage]
  GH_STORAGE_LOCK_TIMEOUT Lock timeout in seconds [default: 30]
  GH_STORAGE_DEBUG        Enable debug logging [default: false]
`)
}

func handleCapture(args []string) {
	parser := argparser.NewArgParser(args)

	if parser.IsHelp() {
		fmt.Println("Usage: gh-pr-review capture [<owner>/<repo>] [<identifier>] [options]")
		fmt.Println("  <owner>/<repo>  Repository (auto-detected if omitted)")
		fmt.Println("  <identifier>    PR number or branch name (auto-detected if omitted)")
		fmt.Println("  --force         Overwrite existing diff hunks")
		return
	}

	// Auto-detect missing arguments
	detectedArgs, err := autoDetectArgs(args)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error auto-detecting arguments: %v\n", err)
		os.Exit(1)
	}
	parser = argparser.NewArgParser(detectedArgs)

	if err := parser.ValidateOptions([]string{"force"}); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if err := parser.RequireExactPositionals(2, "gh-pr-review capture <owner>/<repo> <identifier> [options]"); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	repoSpec := parser.GetPositional(0)
	identifier := parser.GetPositional(1)

	owner, repo, err := storage.ParseRepoAndPR(repoSpec)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	force := parser.HasOption("force")

	handler, err := prreview.NewCommandHandler(prreview.GetStorageHome())
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing handler: %v\n", err)
		os.Exit(1)
	}

	if err := handler.CaptureCommand(owner, repo, identifier, force); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func handleComment(args []string) {
	parser := argparser.NewArgParser(args)

	if parser.IsHelp() {
		fmt.Println("Usage: gh-pr-review comment [<owner>/<repo>] [<identifier>] <file> <line> \"<comment>\" [options]")
		fmt.Println("  <owner>/<repo>  Repository (auto-detected if omitted)")
		fmt.Println("  <identifier>    PR number or branch name (auto-detected if omitted)")
		fmt.Println("  --side SIDE     Side of diff (LEFT, RIGHT) [default: RIGHT]")
		fmt.Println("  --force         Add comment even if duplicate detected")
		return
	}

	// Auto-detect missing arguments (but keep remaining args for file/line/comment)
	detectedArgs, err := autoDetectArgsForComment(args)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error auto-detecting arguments: %v\n", err)
		os.Exit(1)
	}
	parser = argparser.NewArgParser(detectedArgs)

	if err := parser.ValidateOptions([]string{"side", "force"}); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if err := parser.RequireExactPositionals(5, "gh-pr-review comment <owner>/<repo> <identifier> <file> <line> \"<comment>\" [options]"); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	repoSpec := parser.GetPositional(0)
	identifier := parser.GetPositional(1)
	file := parser.GetPositional(2)
	lineSpec := parser.GetPositional(3)
	commentBody := parser.GetPositional(4)

	owner, repo, err := storage.ParseRepoAndPR(repoSpec)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	side := parser.GetOption("side")
	if side == "" {
		side = "RIGHT"
	}
	if side != "LEFT" && side != "RIGHT" {
		fmt.Fprintf(os.Stderr, "Error: side must be LEFT or RIGHT\n")
		os.Exit(1)
	}

	force := parser.HasOption("force")

	handler, err := prreview.NewCommandHandler(prreview.GetStorageHome())
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing handler: %v\n", err)
		os.Exit(1)
	}

	if err := handler.CommentCommand(owner, repo, identifier, file, lineSpec, commentBody, side, force); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func handleSubmit(args []string) {
	parser := argparser.NewArgParser(args)

	if parser.IsHelp() {
		showSubmitUsage()
		return
	}

	if err := validateSubmitOptions(parser); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	owner, repo, prNumber, err := parseSubmitArgs(parser)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	body := parser.GetOption("body")
	event := parser.GetOption("event")
	file := parser.GetOption("file")
	jsonOutput := parser.HasOption("json")
	afterAction := parser.GetOption("after")

	if err := validateSubmitEvent(event); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	postSubmitAction, err := models.CreatePostSubmitExecutor(afterAction)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	formatter := createOutputFormatter(jsonOutput)

	handler, err := prreview.NewCommandHandler(prreview.GetStorageHome())
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing handler: %v\n", err)
		os.Exit(1)
	}

	if err := handler.SubmitCommand(owner, repo, prNumber, body, event, file, formatter, postSubmitAction); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func showSubmitUsage() {
	fmt.Println("Usage: gh-pr-review submit <owner>/<repo> <pr_number> [options]")
	fmt.Println("  --body TEXT       Review body text")
	fmt.Println("  --event EVENT     Review event (COMMENT, APPROVE, REQUEST_CHANGES)")
	fmt.Println("  --file FILE       Submit comments for specific file only")
	fmt.Println("  --json            Output result in JSON format")
	fmt.Println("  --after ACTION    What to do with local comments after submission")
	fmt.Println("                    (clear, keep, archive) [default: clear]")
}

func validateSubmitOptions(parser *argparser.ArgParser) error {
	if err := parser.ValidateOptions([]string{"body", "event", "file", "json", "after"}); err != nil {
		return fmt.Errorf("validating options: %w", err)
	}
	if err := parser.RequireExactPositionals(2, "gh-pr-review submit <owner>/<repo> <pr_number> [options]"); err != nil {
		return fmt.Errorf("validating positionals: %w", err)
	}
	return nil
}

func parseSubmitArgs(parser *argparser.ArgParser) (string, string, int, error) {
	repoSpec := parser.GetPositional(0)
	prNumberStr := parser.GetPositional(1)

	owner, repo, err := storage.ParseRepoAndPR(repoSpec)
	if err != nil {
		return "", "", 0, fmt.Errorf("parsing repo spec: %w", err)
	}

	prNumber, err := strconv.Atoi(prNumberStr)
	if err != nil {
		return "", "", 0, fmt.Errorf("invalid PR number '%s'", prNumberStr)
	}

	return owner, repo, prNumber, nil
}

func validateSubmitEvent(event string) error {
	if event == "" {
		return nil
	}
	validEvents := []string{"COMMENT", "APPROVE", "REQUEST_CHANGES"}
	for _, validEvent := range validEvents {
		if event == validEvent {
			return nil
		}
	}
	return fmt.Errorf("invalid event '%s', must be one of: %s", event, strings.Join(validEvents, ", "))
}

func handleList(args []string) {
	parser := argparser.NewArgParser(args)

	if parser.IsHelp() {
		showListUsage()
		return
	}

	// Auto-detect missing arguments
	detectedArgs, err := autoDetectArgs(args)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error auto-detecting arguments: %v\n", err)
		os.Exit(1)
	}
	parser = argparser.NewArgParser(detectedArgs)

	if err := validateListOptions(parser); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	owner, repo, identifier, err := parseListArgs(parser)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	format, file, line, side, err := parseListFilters(parser)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	formatter := createOutputFormatter(format == "json")

	handler, err := prreview.NewCommandHandler(prreview.GetStorageHome())
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing handler: %v\n", err)
		os.Exit(1)
	}

	if err := handler.ListCommand(owner, repo, identifier, formatter, file, line, side); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func showListUsage() {
	fmt.Println("Usage: gh-pr-review list [<owner>/<repo>] [<identifier>] [options]")
	fmt.Println("  <owner>/<repo>    Repository (auto-detected if omitted)")
	fmt.Println("  <identifier>      PR number or branch name (auto-detected if omitted)")
	fmt.Println("  --format FORMAT   Output format (table, json) [default: table]")
	fmt.Println("  --file FILE       Filter by file path")
	fmt.Println("  --line LINE       Filter by line number or range (e.g., 15 or 15-20)")
	fmt.Println("  --side SIDE       Filter by side (LEFT, RIGHT)")
}

func validateListOptions(parser *argparser.ArgParser) error {
	if err := parser.ValidateOptions([]string{"format", "file", "line", "side"}); err != nil {
		return fmt.Errorf("validating options: %w", err)
	}
	if err := parser.RequireExactPositionals(2, "gh-pr-review list <owner>/<repo> <identifier> [options]"); err != nil {
		return fmt.Errorf("validating positionals: %w", err)
	}
	return nil
}

func parseListArgs(parser *argparser.ArgParser) (string, string, string, error) {
	repoSpec := parser.GetPositional(0)
	identifier := parser.GetPositional(1)

	owner, repo, err := storage.ParseRepoAndPR(repoSpec)
	if err != nil {
		return "", "", "", fmt.Errorf("parsing repo spec: %w", err)
	}

	return owner, repo, identifier, nil
}

func parseListFilters(parser *argparser.ArgParser) (string, string, string, string, error) {
	format := parser.GetOption("format")
	if format == "" {
		format = "table"
	}
	if format != "table" && format != "json" {
		return "", "", "", "", fmt.Errorf("format must be 'table' or 'json'")
	}

	file := parser.GetOption("file")
	line := parser.GetOption("line")
	side := parser.GetOption("side")

	if side != "" && side != "LEFT" && side != "RIGHT" {
		return "", "", "", "", fmt.Errorf("side must be LEFT or RIGHT")
	}

	return format, file, line, side, nil
}

func handleDelete(args []string) {
	parser := argparser.NewArgParser(args)

	if parser.IsHelp() {
		fmt.Println("Usage: gh-pr-review delete [<owner>/<repo>] [<identifier>] --comment-id <ID>")
		fmt.Println("  <owner>/<repo>    Repository (auto-detected if omitted)")
		fmt.Println("  <identifier>      PR number or branch name (auto-detected if omitted)")
		fmt.Println("  --comment-id ID   Delete comment by ID prefix (required)")
		fmt.Println("  --confirm         Skip confirmation prompt")
		fmt.Println("  --json            Output results in JSON format")
		return
	}

	// Auto-detect missing arguments
	detectedArgs, err := autoDetectArgs(args)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error auto-detecting arguments: %v\n", err)
		os.Exit(1)
	}
	parser = argparser.NewArgParser(detectedArgs)

	if err := parser.ValidateOptions([]string{"confirm", "json", "comment-id"}); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	// Comment ID is now required
	commentID := parser.GetOption("comment-id")
	if commentID == "" {
		fmt.Fprintf(os.Stderr, "Error: --comment-id is required\n")
		fmt.Fprintf(os.Stderr, "Usage: gh-pr-review delete <owner>/<repo> <identifier> --comment-id <ID>\n")
		os.Exit(1)
	}

	if err := parser.RequireExactPositionals(2, "gh-pr-review delete <owner>/<repo> <identifier> --comment-id <ID>"); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	repoSpec := parser.GetPositional(0)
	identifier := parser.GetPositional(1)

	owner, repo, err := storage.ParseRepoAndPR(repoSpec)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	confirm := parser.HasOption("confirm")
	jsonOutput := parser.HasOption("json")

	formatter := createOutputFormatter(jsonOutput)

	handler, err := prreview.NewCommandHandler(prreview.GetStorageHome())
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing handler: %v\n", err)
		os.Exit(1)
	}

	if err := handler.DeleteCommand(owner, repo, identifier, commentID, confirm, formatter); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func handleClear(args []string) {
	parser := argparser.NewArgParser(args)

	if parser.IsHelp() {
		fmt.Println("Usage: gh-pr-review clear [<owner>/<repo>] [<identifier>] [options]")
		fmt.Println("  <owner>/<repo>    Repository (auto-detected if omitted)")
		fmt.Println("  <identifier>      PR number or branch name (auto-detected if omitted)")
		fmt.Println("  --file FILE       Clear comments for specific file only")
		fmt.Println("  --confirm         Skip confirmation prompt")
		return
	}

	// Auto-detect missing arguments
	detectedArgs, err := autoDetectArgs(args)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error auto-detecting arguments: %v\n", err)
		os.Exit(1)
	}
	parser = argparser.NewArgParser(detectedArgs)

	if err := parser.ValidateOptions([]string{"file", "confirm"}); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if err := parser.RequireExactPositionals(2, "gh-pr-review clear <owner>/<repo> <identifier> [options]"); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	repoSpec := parser.GetPositional(0)
	identifier := parser.GetPositional(1)

	owner, repo, err := storage.ParseRepoAndPR(repoSpec)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	file := parser.GetOption("file")
	confirm := parser.HasOption("confirm")

	handler, err := prreview.NewCommandHandler(prreview.GetStorageHome())
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing handler: %v\n", err)
		os.Exit(1)
	}

	if err := handler.ClearCommand(owner, repo, identifier, file, confirm); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func handleAdjust(args []string) {
	parser := argparser.NewArgParser(args)

	if parser.IsHelp() {
		showAdjustUsage()
		return
	}

	// Note: Adjust command has complex argument handling, so we'll do auto-detection
	// at the parseAdjustArgs level instead of here

	if err := validateAdjustOptions(parser); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	owner, repo, identifier, file, diffSpec, err := parseAdjustArgs(parser)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	dryRun, force, format, err := parseAdjustOptions(parser)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	handler, err := prreview.NewCommandHandler(prreview.GetStorageHome())
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing handler: %v\n", err)
		os.Exit(1)
	}

	if err := handler.AdjustCommand(owner, repo, identifier, file, diffSpec, dryRun, force, format); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func showAdjustUsage() {
	fmt.Println("Usage:")
	fmt.Println("  gh-pr-review adjust [<owner>/<repo>] [<identifier>] <file> --diff <spec> [options]")
	fmt.Println("  gh-pr-review adjust [<owner>/<repo>] [<identifier>] [file] --unified-diff <spec> [options]")
	fmt.Println()
	fmt.Println("Arguments:")
	fmt.Println("  <owner>/<repo>    Repository (auto-detected if omitted)")
	fmt.Println("  <identifier>      PR number or branch name (auto-detected if omitted)")
	fmt.Println("  <file>            File path to adjust comments for (required with --diff, optional with --unified-diff)")
	fmt.Println()
	fmt.Println("Options:")
	fmt.Println("  --diff SPEC       Diff specification (classic or simple mapping format)")
	fmt.Println("  --unified-diff SPEC  Unified diff specification (git diff output)")
	fmt.Println("  --dry-run         Show what would be adjusted without making changes")
	fmt.Println("  --format FORMAT   Output format (table, json) [default: table]")
	fmt.Println("  --force           Apply adjustments even if validation fails")
	fmt.Println()
	fmt.Println("Unified Diff Mode:")
	fmt.Println("  When using --unified-diff:")
	fmt.Println("  - If <file> is specified: Process only that file from the diff")
	fmt.Println("  - If <file> is omitted: Process ALL files found in the unified diff")
	fmt.Println()
	fmt.Println("Examples:")
	fmt.Println("  # Process all files in git diff")
	fmt.Println("  gh-pr-review adjust owner/repo 123 --unified-diff \"$(git diff HEAD~1 HEAD)\"")
	fmt.Println()
	fmt.Println("  # Process specific file from git diff")
	fmt.Println("  gh-pr-review adjust owner/repo 123 src/main.js --unified-diff \"$(git diff HEAD~1 HEAD)\"")
	fmt.Println()
	fmt.Println("  # Traditional single-file mode")
	fmt.Println("  gh-pr-review adjust owner/repo 123 src/main.js --diff \"15:-2;30:+3\"")
	fmt.Println()
	fmt.Println("Supported diff formats (auto-detected):")
	fmt.Println()
	fmt.Println("Unified Diff Format:")
	fmt.Println("  Standard git diff output with file headers and hunk headers")
	fmt.Println("  @@ -1,7 +1,6 @@     Hunk showing old range (-1,7) and new range (+1,6)")
	fmt.Println("  - deleted line      Lines removed from old file")
	fmt.Println("  + added line        Lines added to new file")
	fmt.Println("    context line      Unchanged context lines")
	fmt.Println()
	fmt.Println("Simple Mapping Format:")
	fmt.Println("  IMPORTANT: All line numbers refer to the ORIGINAL file, not the current state")
	fmt.Println("  line:offset       Apply offset to lines starting at 'line' in original file")
	fmt.Println("  15:-2             Delete 2 lines starting at original line 15")
	fmt.Println("  30:+3             Insert 3 lines before original line 30")
	fmt.Println("  15:-2;30:+3       Multiple operations on original line numbers")
	fmt.Println()
	fmt.Println("Classic Diff Format:")
	fmt.Println("  15,17d14          Delete original lines 15-17, next line becomes 14")
	fmt.Println("  30a31,33          After original line 30, insert what becomes lines 31-33")
	fmt.Println("  5,7c6,8           Replace original lines 5-7 with content at lines 6-8")
	fmt.Println("  15d14;30a31       Multiple operations separated by semicolons")
}

func validateAdjustOptions(parser *argparser.ArgParser) error {
	if err := parser.ValidateOptions([]string{"diff", "unified-diff", "dry-run", "format", "force"}); err != nil {
		return fmt.Errorf("validating options: %w", err)
	}

	// Check mutually exclusive diff options
	hasDiff := parser.GetOption("diff") != ""
	hasUnifiedDiff := parser.GetOption("unified-diff") != ""

	if hasDiff && hasUnifiedDiff {
		return fmt.Errorf("--diff and --unified-diff options are mutually exclusive")
	}

	if !hasDiff && !hasUnifiedDiff {
		return fmt.Errorf("either --diff or --unified-diff option is required")
	}

	// For unified diff, file argument is optional (enables multi-file mode)
	if hasUnifiedDiff {
		positionals := parser.GetPositionals()
		if len(positionals) < 2 || len(positionals) > 3 {
			return fmt.Errorf("wrong number of arguments for unified diff: gh-pr-review adjust <owner>/<repo> <identifier> [file] --unified-diff <spec> [options]")
		}
	} else {
		if err := parser.RequireExactPositionals(3, "gh-pr-review adjust <owner>/<repo> <identifier> <file> --diff <spec> [options]"); err != nil {
			return fmt.Errorf("validating positionals: %w", err)
		}
	}

	return nil
}

// parseAdjustArgsCase0 handles case when no positionals are provided.
func parseAdjustArgsCase0() (string, string, string, error) {
	ownerRepo, err := autoDetectOwnerRepo()
	if err != nil {
		return "", "", "", fmt.Errorf("auto-detecting owner/repo: %w", err)
	}
	identifier, err := autoDetectIdentifier()
	if err != nil {
		return "", "", "", fmt.Errorf("auto-detecting identifier: %w", err)
	}
	return ownerRepo, identifier, "", nil // file remains empty for unified-diff multi-file mode
}

// parseAdjustArgsCase1 handles case when one positional is provided.
func parseAdjustArgsCase1(arg string) (string, string, string, error) {
	if strings.Contains(arg, "/") {
		// Looks like owner/repo, auto-detect identifier
		identifier, err := autoDetectIdentifier()
		if err != nil {
			return "", "", "", fmt.Errorf("auto-detecting identifier: %w", err)
		}
		return arg, identifier, "", nil
	}

	// Doesn't look like owner/repo, assume it's file and auto-detect both
	ownerRepo, err := autoDetectOwnerRepo()
	if err != nil {
		return "", "", "", fmt.Errorf("auto-detecting owner/repo: %w", err)
	}
	identifier, err := autoDetectIdentifier()
	if err != nil {
		return "", "", "", fmt.Errorf("auto-detecting identifier: %w", err)
	}
	return ownerRepo, identifier, arg, nil
}

// parseAdjustArgsCase2 handles case when two positionals are provided.
func parseAdjustArgsCase2(positionals []string) (string, string, string, error) {
	if strings.Contains(positionals[0], "/") {
		return positionals[0], positionals[1], "", nil
	}

	// Auto-detect owner/repo, treat both args as identifier and file
	ownerRepo, err := autoDetectOwnerRepo()
	if err != nil {
		return "", "", "", fmt.Errorf("auto-detecting owner/repo: %w", err)
	}
	return ownerRepo, positionals[0], positionals[1], nil
}

func parseAdjustArgs(parser *argparser.ArgParser) (string, string, string, string, string, error) {
	// Get diff spec from either --diff or --unified-diff
	diffSpec := parser.GetOption("diff")
	if diffSpec == "" {
		diffSpec = parser.GetOption("unified-diff")
	}

	positionals := parser.GetPositionals()

	// Auto-detect missing owner/repo and identifier based on positional count
	var repoSpec, identifier, file string
	var err error

	switch len(positionals) {
	case 0:
		repoSpec, identifier, file, err = parseAdjustArgsCase0()
	case 1:
		repoSpec, identifier, file, err = parseAdjustArgsCase1(positionals[0])
	case 2:
		repoSpec, identifier, file, err = parseAdjustArgsCase2(positionals)
	default:
		// Three or more positionals, use them directly
		repoSpec = positionals[0]
		identifier = positionals[1]
		if len(positionals) >= 3 {
			file = positionals[2]
		}
	}

	if err != nil {
		return "", "", "", "", "", err
	}

	owner, repo, err := storage.ParseRepoAndPR(repoSpec)
	if err != nil {
		return "", "", "", "", "", fmt.Errorf("parsing repo spec: %w", err)
	}

	return owner, repo, identifier, file, diffSpec, nil
}

func parseAdjustOptions(parser *argparser.ArgParser) (bool, bool, string, error) {
	dryRun := parser.HasOption("dry-run")
	force := parser.HasOption("force")
	format := parser.GetOption("format")
	if format == "" {
		format = "table"
	}
	if format != "table" && format != "json" {
		return false, false, "", fmt.Errorf("format must be 'table' or 'json'")
	}

	return dryRun, force, format, nil
}
