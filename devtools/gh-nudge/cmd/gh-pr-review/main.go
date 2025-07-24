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

	switch command {
	case "capture":
		handleCapture(args)
	case "comment":
		handleComment(args)
	case "submit":
		handleSubmit(args)
	case "list":
		handleList(args)
	case "delete":
		handleDelete(args)
	case "clear":
		handleClear(args)
	case "version":
		fmt.Printf("gh-pr-review version %s\n", version)
	case "help", "-h", "--help":
		showUsage()
	default:
		fmt.Fprintf(os.Stderr, "Unknown command: %s\n", command)
		showUsage()
		os.Exit(1)
	}
}

func showUsage() {
	fmt.Printf(`gh-pr-review - GitHub PR Review CLI Utility

Usage: gh-pr-review <command> [options]

Commands:
  capture <owner>/<repo> <identifier>             Capture diff hunks (PR or branch)
  comment <owner>/<repo> <identifier> <file> <line> "<comment>"  Add line comment
  list <owner>/<repo> <identifier>                List stored comments (PR or branch)
  clear <owner>/<repo> <identifier>               Clear all comments (PR or branch)
  delete <owner>/<repo> <identifier> <file> <line>  Delete line comment (PR or branch)
  submit <owner>/<repo> <pr_number>               Submit review to GitHub (PR only)
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

  # Submit review and keep local comments (PR only)
  gh-pr-review submit octocat/Hello-World 42 --body "First pass" --after keep

  # Delete specific comment from PR by ID (recommended)
  gh-pr-review delete octocat/Hello-World 42 --comment-id a1b2c3d4

  # Delete specific comment from PR by line (deprecated)
  gh-pr-review delete octocat/Hello-World 42 src/main.js 15

  # Delete specific comment from branch
  gh-pr-review delete octocat/Hello-World feature/auth-fix src/auth.js 25 --confirm

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
		fmt.Println("Usage: gh-pr-review submit <owner>/<repo> <pr_number> [options]")
		fmt.Println("  --body TEXT       Review body text")
		fmt.Println("  --event EVENT     Review event (COMMENT, APPROVE, REQUEST_CHANGES)")
		fmt.Println("  --json            Output result in JSON format")
		fmt.Println("  --after ACTION    What to do with local comments after submission")
		fmt.Println("                    (clear, keep, archive) [default: clear]")
		return
	}

	if err := parser.ValidateOptions([]string{"body", "event", "json", "after"}); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if err := parser.RequireExactPositionals(2, "gh-pr-review submit <owner>/<repo> <pr_number> [options]"); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	repoSpec := parser.GetPositional(0)
	prNumberStr := parser.GetPositional(1)

	owner, repo, err := storage.ParseRepoAndPR(repoSpec)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	prNumber, err := strconv.Atoi(prNumberStr)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: invalid PR number '%s'\n", prNumberStr)
		os.Exit(1)
	}

	body := parser.GetOption("body")
	event := parser.GetOption("event")
	jsonOutput := parser.HasOption("json")
	afterAction := parser.GetOption("after")

	// Validate event if provided
	if event != "" {
		validEvents := []string{"COMMENT", "APPROVE", "REQUEST_CHANGES"}
		valid := false
		for _, validEvent := range validEvents {
			if event == validEvent {
				valid = true
				break
			}
		}
		if !valid {
			fmt.Fprintf(os.Stderr, "Error: invalid event '%s', must be one of: %s\n",
				event, strings.Join(validEvents, ", "))
			os.Exit(1)
		}
	}

	// Parse post-submit action
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

	if err := handler.SubmitCommand(owner, repo, prNumber, body, event, formatter, postSubmitAction); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func handleList(args []string) {
	parser := argparser.NewArgParser(args)

	if parser.IsHelp() {
		fmt.Println("Usage: gh-pr-review list <owner>/<repo> <identifier> [options]")
		fmt.Println("  <identifier>      PR number (e.g., 42) or branch name (e.g., feature/auth)")
		fmt.Println("  --format FORMAT   Output format (table, json) [default: table]")
		fmt.Println("  --file FILE       Filter by file path")
		fmt.Println("  --line LINE       Filter by line number or range (e.g., 15 or 15-20)")
		fmt.Println("  --side SIDE       Filter by side (LEFT, RIGHT)")
		return
	}

	if err := parser.ValidateOptions([]string{"format", "file", "line", "side"}); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if err := parser.RequireExactPositionals(2, "gh-pr-review list <owner>/<repo> <identifier> [options]"); err != nil {
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

	format := parser.GetOption("format")
	if format == "" {
		format = "table"
	}
	if format != "table" && format != "json" {
		fmt.Fprintf(os.Stderr, "Error: format must be 'table' or 'json'\n")
		os.Exit(1)
	}

	file := parser.GetOption("file")
	line := parser.GetOption("line")
	side := parser.GetOption("side")

	if side != "" && side != "LEFT" && side != "RIGHT" {
		fmt.Fprintf(os.Stderr, "Error: side must be LEFT or RIGHT\n")
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

func handleDelete(args []string) {
	parser := argparser.NewArgParser(args)

	if parser.IsHelp() {
		fmt.Println("Usage: gh-pr-review delete <owner>/<repo> <identifier> <file> <line> [options]")
		fmt.Println("     OR: gh-pr-review delete <owner>/<repo> <identifier> --comment-id <ID>")
		fmt.Println("  <identifier>      PR number (e.g., 42) or branch name (e.g., feature/auth)")
		fmt.Println("  --comment-id ID   Delete comment by ID prefix (recommended)")
		fmt.Println("  --side SIDE       Side of diff (LEFT, RIGHT) [default: RIGHT]")
		fmt.Println("  --confirm         Skip confirmation prompt")
		fmt.Println("  --all             Delete all comments on the specified line")
		fmt.Println("  --index INDEX     Delete comment at specific index (0-based, deprecated)")
		fmt.Println("  --json            Output results in JSON format")
		return
	}

	if err := parser.ValidateOptions([]string{"side", "confirm", "all", "index", "json", "comment-id"}); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	// Check if using comment-id mode
	commentID := parser.GetOption("comment-id")

	var requiredPositionals int
	if commentID != "" {
		requiredPositionals = 2 // Only need owner/repo and identifier
	} else {
		requiredPositionals = 4 // Need owner/repo, identifier, file, and line
	}

	usage := "gh-pr-review delete <owner>/<repo> <identifier> <file> <line> [options]"
	if commentID != "" {
		usage = "gh-pr-review delete <owner>/<repo> <identifier> --comment-id <ID>"
	}

	if err := parser.RequireExactPositionals(requiredPositionals, usage); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	repoSpec := parser.GetPositional(0)
	identifier := parser.GetPositional(1)

	var file, lineSpec string
	if commentID == "" {
		file = parser.GetPositional(2)
		lineSpec = parser.GetPositional(3)
	}

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

	confirm := parser.HasOption("confirm")
	all := parser.HasOption("all")
	jsonOutput := parser.HasOption("json")

	var index *int
	if parser.HasOption("index") {
		indexStr := parser.GetOption("index")
		indexVal, err := strconv.Atoi(indexStr)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: invalid index '%s'\n", indexStr)
			os.Exit(1)
		}
		index = &indexVal
	}

	formatter := createOutputFormatter(jsonOutput)

	handler, err := prreview.NewCommandHandler(prreview.GetStorageHome())
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error initializing handler: %v\n", err)
		os.Exit(1)
	}

	if err := handler.DeleteCommand(owner, repo, identifier, file, lineSpec, side, all, index, commentID, confirm, formatter); err != nil {
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
