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
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
)

func main() {
	if len(os.Args) < 2 {
		printUsage()
		os.Exit(1)
	}

	// Check for help flags
	if os.Args[1] == "--help" || os.Args[1] == "-h" || os.Args[1] == "help" {
		printUsage()
		return
	}

	hasIssues := false

	// Debug: print all arguments with %q formatting
	if os.Getenv("DEBUG") != "" {
		fmt.Fprintf(os.Stderr, "DEBUG: Total arguments: %d\n", len(os.Args))
		for i, arg := range os.Args {
			fmt.Fprintf(os.Stderr, "DEBUG: Arg[%d]: %q\n", i, arg)
		}
	}

	for _, filename := range os.Args[1:] {
		// Debug: print each file being processed
		if os.Getenv("DEBUG") != "" {
			fmt.Fprintf(os.Stderr, "DEBUG: Processing file: %q\n", filename)
		}

		// Check if file exists
		if _, err := os.Stat(filename); errors.Is(err, fs.ErrNotExist) {
			fmt.Fprintf(os.Stderr, "Error: File %q does not exist\n", filename)
			hasIssues = true
			continue
		}

		if err := lintOrgFile(filename); err != nil {
			hasIssues = true
		}
	}

	if hasIssues {
		os.Exit(1)
	}
}

func printUsage() {
	fmt.Printf(`org-lint - Lint Org-mode files using Emacs org-lint

USAGE:
    org-lint [OPTIONS] <file1.org> [file2.org ...]

ARGUMENTS:
    <files>    One or more .org files to lint

OPTIONS:
    -h, --help    Show this help message

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

    # Enable debug output
    DEBUG=1 org-lint README.org

OUTPUT:
    Issues are reported in the format: filename:line: (trust-level) description

    Trust levels:
    - low:      Minor style issues
    - standard: Standard org-mode violations
    - high:     Serious structural problems

EXIT CODES:
    0    All files are clean (no issues found)
    1    Issues found or error occurred

FEATURES:
    - Uses your Emacs configuration and installed packages
    - Supports org-babel source blocks (loads ob-* packages)
    - Auto-discovers language modes for comprehensive linting
    - Works with both ~/.emacs.d and ~/.config/emacs setups

For more information, visit: https://github.com/jaeyeom/experimental/tree/main/devtools/linters
`)
}

func lintOrgFile(filename string) error {
	emacsCode := `
(progn
  (setq debug-on-error nil)
  (setq message-log-max nil)

  (let* ((inhibit-message t)
         (filename (getenv "FILENAME"))
         (buf (find-file-noselect filename)))
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

    (with-current-buffer buf
      (let ((issues (org-lint)))
        (dolist (issue issues)
          (let* ((vec (cadr issue))
                (line (string-to-number (aref vec 0)))
                (trust (aref vec 1))
                (description (aref vec 2)))
            (princ (format "%s:%d: (%s) %s\n" filename line trust description))))
        (kill-emacs (if issues 1 0))))))`

	cmd := exec.Command("emacs", "-Q", "--batch", "-l", "org", "--eval", emacsCode)
	cmd.Env = append(os.Environ(), fmt.Sprintf("FILENAME=%s", filename))
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	err := cmd.Run()
	if err != nil {
		if exitError, ok := err.(*exec.ExitError); ok {
			return fmt.Errorf("linting failed with exit code %d", exitError.ExitCode())
		}
		return fmt.Errorf("failed to run emacs: %v", err)
	}

	return nil
}
