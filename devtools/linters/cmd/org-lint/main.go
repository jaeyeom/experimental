// Package main implements org-lint, a command-line tool for linting Org-mode files.
//
// org-lint is a Go wrapper around Emacs org-lint functionality that provides
// command-line linting for Org-mode files. It automatically loads the user's
// Emacs packages and configuration to provide comprehensive linting including
// support for org-babel source blocks.
//
// Features:
//   - Lints one or more .org files using Emacs org-lint
//   - Returns appropriate exit codes (0 for clean, 1 for issues)
//   - Formats output as filename:line: (trust-level) description
//   - Automatically loads user's Emacs packages from elpa directory
//   - Loads all available ob-* packages for org-babel source block support
//   - Auto-discovers and loads all *-mode packages for comprehensive language support
//   - Gracefully handles package loading errors
//   - Supports both traditional (~/.emacs.d) and modern (~/.config/emacs) configurations
//   - Only loads packages for the current Emacs version
//
// Usage:
//
//	org-lint file1.org [file2.org ...]
//
// Environment Variables:
//
//	DEBUG - if set, prints debug information including the load-path
//	ORG_LINT_NORMALIZED - if set, prints filename:line: checker-name: description
//	  (the format used by the org-lint-parity harness)
//
// The tool uses Emacs in batch mode with the following approach:
//  1. Detects user's Emacs configuration directory using user-emacs-directory
//  2. Adds package directories to load-path for the current Emacs version
//  3. Loads all available ob-* packages for org-babel support
//  4. Auto-discovers and loads all *-mode packages for language support
//  5. Runs org-lint on each specified file
//  6. Returns exit code 1 if any issues are found, 0 if all files are clean
package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"os/exec"

	pkgversion "github.com/jaeyeom/experimental/devtools/pkg/version"
	executor "github.com/jaeyeom/go-cmdexec"
)

func main() {
	os.Exit(run(os.Args[1:], os.Stdout, os.Stderr))
}

func run(args []string, stdout, stderr io.Writer) int {
	if len(args) == 0 {
		printUsage(stdout)
		return 1
	}

	opts := parseArgs(args)
	if opts.showHelp {
		printUsage(stdout)
		return 0
	}
	if opts.showVersion {
		fmt.Fprintln(stdout, pkgversion.Format("org-lint"))
		return 0
	}
	if len(opts.files) == 0 {
		printUsage(stdout)
		return 1
	}

	// Check if emacs is available
	if code, done := checkEmacsAvailable(opts.skipIfNoEmacs, stderr); done {
		return code
	}

	// Debug: print all arguments with %q formatting
	if os.Getenv("DEBUG") != "" {
		fmt.Fprintf(stderr, "DEBUG: Total arguments: %d\n", len(args)+1)
		for i, arg := range args {
			fmt.Fprintf(stderr, "DEBUG: Arg[%d]: %q\n", i+1, arg)
		}
	}

	if lintFiles(opts.files, stdout, stderr) {
		return 1
	}
	return 0
}

type options struct {
	showHelp      bool
	showVersion   bool
	skipIfNoEmacs bool
	files         []string
}

func parseArgs(args []string) options {
	var opts options
	for _, arg := range args {
		switch arg {
		case "--help", "-h", "help":
			opts.showHelp = true
		case "--version", "-version":
			opts.showVersion = true
		case "--skip-if-no-emacs":
			opts.skipIfNoEmacs = true
		default:
			opts.files = append(opts.files, arg)
		}
	}
	return opts
}

func checkEmacsAvailable(skipIfNoEmacs bool, stderr io.Writer) (int, bool) {
	if _, err := exec.LookPath("emacs"); err != nil {
		if skipIfNoEmacs {
			if os.Getenv("DEBUG") != "" {
				fmt.Fprintf(stderr, "DEBUG: Emacs not found, skipping (--skip-if-no-emacs)\n")
			}
			return 0, true
		}
		fmt.Fprintf(stderr, "Error: emacs command not found in PATH\n")
		fmt.Fprintf(stderr, "Hint: Use --skip-if-no-emacs to succeed when emacs is not available\n")
		return 1, true
	}
	return 0, false
}

func lintFiles(files []string, stdout, stderr io.Writer) bool {
	hasIssues := false
	var validFiles []string

	for _, filename := range files {
		// Debug: print each file being processed
		if os.Getenv("DEBUG") != "" {
			fmt.Fprintf(stderr, "DEBUG: Processing file: %q\n", filename)
		}

		// Check if file exists
		if _, err := os.Stat(filename); errors.Is(err, fs.ErrNotExist) {
			fmt.Fprintf(stderr, "Error: File %q does not exist\n", filename)
			hasIssues = true
			continue
		}

		validFiles = append(validFiles, filename)
	}

	if len(validFiles) > 0 {
		if err := lintOrgFiles(validFiles, stdout, stderr); err != nil {
			hasIssues = true
		}
	}

	return hasIssues
}

func printUsage(w io.Writer) {
	fmt.Fprintf(w, `org-lint - Lint Org-mode files using Emacs org-lint

USAGE:
    org-lint [OPTIONS] <file1.org> [file2.org ...]

ARGUMENTS:
    <files>    One or more .org files to lint

OPTIONS:
    -h, --help            Show this help message
    --version             Print version information and exit
    --skip-if-no-emacs    Exit successfully if emacs is not available
                          (useful for CI/CD environments)

ENVIRONMENT VARIABLES:
    DEBUG         If set, prints debug information including load-path

EXAMPLES:
    # Lint a single file
    org-lint README.org

    # Lint multiple files
    org-lint file1.org file2.org file3.org

    # Lint all org files in current directory
    org-lint *.org

    # Use in scripts (check exit code)
    if org-lint *.org; then
        echo "All files are clean"
    else
        echo "Issues found"
    fi

    # Skip linting if emacs is not installed (CI/CD friendly)
    org-lint --skip-if-no-emacs *.org

    # Enable debug output
    DEBUG=1 org-lint README.org

OUTPUT:
    Issues are reported in the format: filename:line: (trust-level) description

    Trust levels:
    - low:      Minor style issues
    - standard: Standard org-mode violations
    - high:     Serious structural problems

EXIT CODES:
    0    All files are clean (no issues found) or emacs not available with --skip-if-no-emacs
    1    Issues found or error occurred

FEATURES:
    - Uses your Emacs configuration and installed packages
    - Supports org-babel source blocks (loads ob-* packages)
    - Auto-discovers language modes for comprehensive linting
    - Works with both ~/.emacs.d and ~/.config/emacs setups
    - Can gracefully skip when emacs is not available (--skip-if-no-emacs)

For more information, visit: https://github.com/jaeyeom/experimental/tree/main/devtools/linters
`)
}

func lintOrgFiles(files []string, stdout, stderr io.Writer) error {
	emacsCode := `
(progn
  (setq debug-on-error nil)
  (setq message-log-max nil)

  (let* ((inhibit-message t)
         (files command-line-args-left)
         (has-issues nil))
    ;; Prevent Emacs from processing remaining args as files to visit.
    (setq command-line-args-left nil)

    ;; Add package directories to load-path using user-emacs-directory
    (let ((elpa-dir (expand-file-name "elpa/" user-emacs-directory))
          (current-version (format "%d.%d" emacs-major-version emacs-minor-version)))
      (when (file-directory-p elpa-dir)
        ;; Handle both flat structure (traditional) and versioned structure (modern)
        (dolist (entry (directory-files elpa-dir t "^[^.]"))
          (cond
           ;; If it's the current version directory, go deeper
           ((and (file-directory-p entry)
                 (string-equal current-version (file-name-nondirectory entry)))
            (dolist (type-dir (directory-files entry t "^[^.]"))
              (when (file-directory-p type-dir)
                (dolist (pkg-dir (directory-files type-dir t "^[^.]"))
                  (when (file-directory-p pkg-dir)
                    (add-to-list 'load-path pkg-dir))))))
           ;; If it's not a version directory (no digits), treat as direct package directory
           ((and (file-directory-p entry)
                 (not (string-match "^[0-9]" (file-name-nondirectory entry))))
            (add-to-list 'load-path entry))))))

    ;; Debug: print load-path
    (if (getenv "DEBUG")
	(progn
	  (princ "DEBUG: load-path:\n")
	  (dolist (path load-path)
	    (princ (format "  %s\n" path)))
	  (princ "\n")))

    ;; Shim for org-src-get-lang-mode-if-bound (added in newer org-mode).
    ;; Older versions only have org-src-get-lang-mode; provide a safe fallback.
    (unless (fboundp 'org-src-get-lang-mode-if-bound)
      (defun org-src-get-lang-mode (lang)
        "Return the major mode for LANG, or nil if not available."
        (let ((mode (intern (concat (or lang "") "-mode"))))
          (if (fboundp mode) mode nil)))
      (defalias 'org-src-get-lang-mode-if-bound 'org-src-get-lang-mode))

    ;; Load all ob-* packages for org-babel source block support
    (dolist (path load-path)
      (when (file-directory-p path)
        (dolist (file (directory-files path nil "^ob-.*\\.el$"))
          (let ((feature (intern (file-name-sans-extension file))))
            (condition-case nil
                (require feature nil t)
              (error nil))))))
    ;; Load all available *-mode packages for language support
    (dolist (path load-path)
      (when (file-directory-p path)
        (dolist (file (directory-files path nil ".*-mode\\.el$"))
          (let ((feature (intern (file-name-sans-extension file))))
            (condition-case nil
                (require feature nil t)
              (error nil))))))

    ;; Process each file in the single Emacs session
    (dolist (filename files)
      (let ((buf (find-file-noselect filename)))
        (with-current-buffer buf
          ;; Fixtures use a non-.org extension to stay out of pre-commit lint.
          (unless (derived-mode-p 'org-mode) (org-mode))
          (let ((issues (org-lint)))
            (dolist (issue issues)
              (let* ((vec (cadr issue))
                     (line (string-to-number (aref vec 0)))
                     (trust (aref vec 1))
                     (description (aref vec 2)))
                (princ (if (getenv "ORG_LINT_NORMALIZED")
                           (format "%s:%d: %s: %s\n" filename line
                                   (org-lint-checker-name (aref vec 3))
                                   description)
                         (format "%s:%d: (%s) %s\n" filename line trust description)))))
            (when issues
              (setq has-issues t))))
        (kill-buffer buf)))
    (kill-emacs (if has-issues 1 0))))`

	ctx := context.Background()
	exec := executor.NewBasicExecutor()

	args := append([]string{"-Q", "--batch", "-l", "org", "--eval", emacsCode}, files...)

	result, err := exec.Execute(ctx, executor.ToolConfig{
		Command: "emacs",
		Args:    args,
	})

	// Print output
	if result != nil {
		if result.Output != "" {
			fmt.Fprint(stdout, result.Output)
		}
		if result.Stderr != "" {
			fmt.Fprint(stderr, result.Stderr)
		}
	}

	if err != nil {
		return fmt.Errorf("failed to run emacs: %v", err)
	}

	if result.ExitCode != 0 {
		return fmt.Errorf("linting failed with exit code %d", result.ExitCode)
	}

	return nil
}
