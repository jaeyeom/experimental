;;; Directory Local Variables for experimental repository
;;; For more information see (info "(emacs) Directory Variables")

;;; Usage:
;;; Copy this file to .dir-locals.el in the repository root:
;;;   cp .dir-locals.sample.el .dir-locals.el
;;;
;;; This configuration enables code coverage visualization in Emacs using the
;;; 'cov' package, which displays line-by-line coverage information from lcov.info.
;;;
;;; Prerequisites:
;;; 1. Install the 'cov' package in Emacs (available on MELPA)
;;;    Spacemacs configuration at spacemacs/.spacemacs already include 'cov'.
;;; 2. Generate lcov.info by running tests with coverage:
;;;      make test-coverage
;;;    or manually with:
;;;      go test -coverprofile=coverage.out ./...
;;;      go tool cover -html=coverage.out -o lcov.info
;;;
;;; What this does:
;;; - Automatically enables coverage mode when opening Go files
;;; - Shows green/red highlighting on covered/uncovered lines
;;; - Displays coverage statistics in the mode line
;;;
;;; Configuration:
;;; - cov-coverage-mode: Enable coverage visualization globally
;;; - cov-lcov-file-name: Path to the lcov.info coverage data file
;;; - cov-lcov-project-root: Repository root directory
;;; - (eval . (cov-mode 1)): Auto-enable cov-mode for Go files

((nil . ((cov-coverage-mode . t)
         (cov-lcov-file-name . "~/go/src/github.com/jaeyeom/experimental/lcov.info")
         (cov-lcov-project-root . "~/go/src/github.com/jaeyeom/experimental/")))
 (go-mode . ((eval . (cov-mode 1)))))
