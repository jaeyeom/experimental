package main

import (
	"fmt"
	"os"
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
  capture <owner>/<repo> <identifier>             Capture diff hunks (PR or branch)
  comment <owner>/<repo> <identifier> <file> <line> "<comment>"  Add line comment
  list <owner>/<repo> <identifier>                List stored comments (PR or branch)
  clear <owner>/<repo> <identifier>               Clear all comments (PR or branch)
  delete <owner>/<repo> <identifier> --comment-id <ID>  Delete comment by ID (PR or branch)
  submit <owner>/<repo> <pr_number>               Submit review to GitHub (PR only)
  adjust <owner>/<repo> <identifier> <file> --diff <spec>  Adjust comment line numbers
  version                                         Show version information
  help                                           Show this help message

Identifier Types:
  PR numbers     Pure integers (e.g., 42, 123)
  Branch names   Any non-numeric string (e.g., feature/auth, main, my-branch)

Examples:
  # Capture PR diff hunks
  gh-pr-review capture octocat/Hello-World 42

  # Capture branch diff hunks
  gh-pr-review capture octocat/Hello-World feature/auth-fix

  # Add line comment to PR
  gh-pr-review comment octocat/Hello-World 42 src/main.js 15 "Consider using const instead of let"

  # Add line comment to branch
  gh-pr-review comment octocat/Hello-World feature/auth-fix src/main.js 15 "Use const instead of let"

  # Add multi-line comment to branch
  gh-pr-review comment octocat/Hello-World my-branch src/main.js 15-20 "This function needs refactoring"

  # List comments from PR
  gh-pr-review list octocat/Hello-World 42 --format json

  # List comments from branch
  gh-pr-review list octocat/Hello-World feature/auth-fix --file src/auth.js

  # Clear all comments from branch
  gh-pr-review clear octocat/Hello-World my-branch --confirm

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
		fmt.Println("Usage: gh-pr-review capture <owner>/<repo> <identifier> [options]")
		fmt.Println("  <identifier>  PR number (e.g., 42) or branch name (e.g., feature/auth)")
		fmt.Println("  --force       Overwrite existing diff hunks")
		return
	}

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
		fmt.Println("Usage: gh-pr-review comment <owner>/<repo> <identifier> <file> <line> \"<comment>\" [options]")
		fmt.Println("  <identifier>  PR number (e.g., 42) or branch name (e.g., feature/auth)")
		fmt.Println("  --side SIDE   Side of diff (LEFT, RIGHT) [default: RIGHT]")
		fmt.Println("  --force       Add comment even if duplicate detected")
		return
	}

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
	fmt.Println("Usage: gh-pr-review list <owner>/<repo> <identifier> [options]")
	fmt.Println("  <identifier>      PR number (e.g., 42) or branch name (e.g., feature/auth)")
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
		fmt.Println("Usage: gh-pr-review delete <owner>/<repo> <identifier> --comment-id <ID>")
		fmt.Println("  <identifier>      PR number (e.g., 42) or branch name (e.g., feature/auth)")
		fmt.Println("  --comment-id ID   Delete comment by ID prefix (required)")
		fmt.Println("  --confirm         Skip confirmation prompt")
		fmt.Println("  --json            Output results in JSON format")
		return
	}

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
		fmt.Println("Usage: gh-pr-review clear <owner>/<repo> <identifier> [options]")
		fmt.Println("  <identifier>      PR number (e.g., 42) or branch name (e.g., feature/auth)")
		fmt.Println("  --file FILE       Clear comments for specific file only")
		fmt.Println("  --confirm         Skip confirmation prompt")
		return
	}

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
	fmt.Println("Usage: gh-pr-review adjust <owner>/<repo> <identifier> <file> --diff <spec> [options]")
	fmt.Println("  <identifier>      PR number (e.g., 42) or branch name (e.g., feature/auth)")
	fmt.Println("  <file>            File path to adjust comments for")
	fmt.Println("  --diff SPEC       Diff specification (auto-detected format, required)")
	fmt.Println("  --dry-run         Show what would be adjusted without making changes")
	fmt.Println("  --format FORMAT   Output format (table, json) [default: table]")
	fmt.Println("  --force           Apply adjustments even if validation fails")
	fmt.Println()
	fmt.Println("Supported diff formats (auto-detected):")
	fmt.Println()
	fmt.Println("Simple Mapping Format:")
	fmt.Println("  IMPORTANT: All line numbers refer to the ORIGINAL file, not the current state")
	fmt.Println("  line:offset       Apply offset to lines starting at 'line' in original file")
	fmt.Println("  15:-2             Delete 2 lines starting at original line 15")
	fmt.Println("  30:+3             Insert 3 lines before original line 30")
	fmt.Println("  15:-2;30:+3       Multiple operations on original line numbers")
	fmt.Println()
	fmt.Println("  Example walkthrough for '10:-1;20:+2':")
	fmt.Println("    1. Delete 1 line at original line 10 → '10d9'")
	fmt.Println("    2. Insert 2 lines before original line 20 → '19a20,21'")
	fmt.Println("    Result: Original line 20 becomes new line 21 (after deletion + insertion)")
	fmt.Println()
	fmt.Println("Classic Diff Format:")
	fmt.Println("  15,17d14          Delete original lines 15-17, next line becomes 14")
	fmt.Println("  30a31,33          After original line 30, insert what becomes lines 31-33")
	fmt.Println("  5,7c6,8           Replace original lines 5-7 with content at lines 6-8")
	fmt.Println("  15d14;30a31       Multiple operations separated by semicolons")
}

func validateAdjustOptions(parser *argparser.ArgParser) error {
	if err := parser.ValidateOptions([]string{"diff", "dry-run", "format", "force"}); err != nil {
		return fmt.Errorf("validating options: %w", err)
	}
	if err := parser.RequireExactPositionals(3, "gh-pr-review adjust <owner>/<repo> <identifier> <file> --diff <spec> [options]"); err != nil {
		return fmt.Errorf("validating positionals: %w", err)
	}
	return nil
}

func parseAdjustArgs(parser *argparser.ArgParser) (string, string, string, string, string, error) {
	diffSpec := parser.GetOption("diff")
	if diffSpec == "" {
		return "", "", "", "", "", fmt.Errorf("--diff option is required")
	}

	repoSpec := parser.GetPositional(0)
	identifier := parser.GetPositional(1)
	file := parser.GetPositional(2)

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
