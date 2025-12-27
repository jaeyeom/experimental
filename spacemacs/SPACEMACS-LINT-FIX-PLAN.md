# Spacemacs .spacemacs Byte-Compile Fix Plan

This document outlines an incremental plan to fix Emacs Lisp byte-compile
warnings in the `.spacemacs` file.

## Phase 1: Move Function/Variable Definitions Before Use [COMPLETED]

**Problem:** `my/termux-p`, `my/crostini-p`, `my/macos-p` are defined in
`dotspacemacs/user-init` but referenced earlier in `dotspacemacs/layers`
(line 65).

**Affected Lines:** 65, 704, 709, 714, 848, 1213, 2050

**Fix:** Move these function and variable definitions to the top of the file
or into `dotspacemacs/layers` before they are used in layer configuration.

```elisp
;; Move to top of file or beginning of dotspacemacs/layers
(defun my/crostini-p ()
  (file-exists-p "/dev/.cros_milestone"))
(defvar my/crostini-p (my/crostini-p))

(defun my/termux-p ()
  (not (null (getenv "TERMUX_VERSION"))))
(defvar my/termux-p (my/termux-p))

(defun my/macos-p ()
  (eq system-type 'darwin))
(defvar my/macos-p (my/macos-p))
```

---

## Phase 2: Add `declare-function` for External Functions [COMPLETED]

**Problem:** ~70 functions are "not known to be defined" because they come
from packages loaded later.

**Fix:** Add `declare-function` statements near the top of `dotspacemacs/user-config`.

### Spacemacs Core Functions
```elisp
(declare-function spacemacs/set-default-font "core-fonts-support" ())
(declare-function spacemacs/set-leader-keys "core-keybindings" (&rest bindings))
(declare-function spacemacs/set-leader-keys-for-major-mode "core-keybindings" (mode &rest bindings))
(declare-function spacemacs/toggle-maximize-frame "core-funcs" ())
(declare-function spacemacs/load-spacemacs-env "core-env" ())
```

### Dirvish Functions
```elisp
(declare-function dirvish-override-dired-mode "dirvish" ())
(declare-function dirvish-quit "dirvish" ())
(declare-function dirvish-narrow "dirvish" ())
(declare-function dirvish-subtree-toggle "dirvish" ())
(declare-function dirvish-layout-toggle "dirvish" ())
(declare-function dirvish-layout-switch "dirvish" ())
(declare-function dirvish-dispatch "dirvish" ())
(declare-function dirvish-ls-switches-menu "dirvish" ())
(declare-function evilified-state-evilify-map "evil-evilified-state" (map &rest props))
```

### Dired Functions
```elisp
(declare-function dired-find-file "dired" ())
(declare-function dired-find-file-other-window "dired" ())
(declare-function dired-up-directory "dired" ())
(declare-function dired-get-file-for-visit "dired" ())
(declare-function dired-hide-subdir "dired" ())
(declare-function dired-summary "dired" ())
(declare-function dired-next-dirline "dired" (arg))
(declare-function dired-prev-dirline "dired" (arg))
(declare-function dired-view-file "dired" ())
(declare-function dired-show-file-type "dired" (file))
(declare-function dired-do-chgrp "dired-aux" ())
```

### Magit/Forge Functions
```elisp
(declare-function magit-diff-visit-worktree-file "magit-diff" ())
(declare-function magit-display-buffer-same-window-except-diff-v1 "magit-mode" (buffer))
(declare-function projectile-vc "projectile" ())
(declare-function forge-insert-pullreqs "forge-list" ())
(declare-function forge--clone-buffer-topics-spec "forge-topics" ())
```

### Code Review Functions
```elisp
(declare-function code-review-forge-pr-at-point "code-review" ())
(declare-function code-review-db-get-pullreq "code-review-db" ())
(declare-function code-review-pr-at-point "code-review" ())
(declare-function code-review-reload "code-review" ())
```

### EIEIO Functions
```elisp
(declare-function oref "eieio" (obj slot))
(declare-function oset "eieio" (obj slot value))
```

### ChatGPT/GPTel Functions
```elisp
(declare-function chatgpt-shell-send-to-buffer "chatgpt-shell" ())
(declare-function chatgpt-shell-proofread-paragraph-or-region "chatgpt-shell" ())
(declare-function chatgpt-shell-quick-insert "chatgpt-shell" ())
(declare-function chatgpt-shell-prompt-compose "chatgpt-shell" ())
(declare-function chatgpt-shell-eshell-summarize-last-command-output "chatgpt-shell" ())
(declare-function chatgpt-shell-eshell-whats-wrong-with-last-command "chatgpt-shell" ())
(declare-function ob-chatgpt-shell-setup "ob-chatgpt-shell" ())
(declare-function gptel-make-anthropic "gptel" (name &rest args))
(declare-function gptel-make-openai "gptel" (name &rest args))
```

### Copilot Functions
```elisp
(declare-function copilot-mode "copilot" ())
```

### Eshell Functions
```elisp
(declare-function eshell-fn-on-files "em-unix" (file func))
(declare-function eshell-getopts "em-unix" (format &rest args))
(declare-function eshell-command-not-found-mode "eshell-command-not-found" ())
```

### Org Functions
```elisp
(declare-function org-html-export-to-html "ox-html" (&optional async subtreep visible-only body-only ext-plist))
(declare-function org-combine-plists "org-compat" (&rest plists))
(declare-function org-html-close-tag "ox-html" (tag attr info))
(declare-function org-html--make-attribute-string "ox-html" (attributes))
(declare-function org-export-define-derived-backend "ox" (child parent &rest body))
(declare-function org-roam-db-autosync-mode "org-roam" ())
(declare-function markdown-mode "markdown-mode" ())
```

### EWW Functions
```elisp
(declare-function eww-browse-with-external-browser "eww" (&optional url))
```

### Evil Functions
```elisp
(declare-function evil-define-key "evil-core" (state keymap key def &rest bindings))
(declare-function yas-minor-mode "yasnippet" (&optional arg))
```

### Dash/S.el Functions
```elisp
(declare-function -map "dash" (fn list))
(declare-function -flatten "dash" (l))
(declare-function --map "dash" (form list))
(declare-function --filter "dash" (form list))
(declare-function s-trim "s" (s))
(declare-function s-join "s" (separator strings))
(declare-function s-starts-with? "s" (prefix s &optional ignore-case))
```

### Other Functions
```elisp
(declare-function prodigy-define-service "prodigy" (&rest args))
(declare-function slack-register-team "slack" (&rest args))
(declare-function slack-select-unread-rooms "slack" ())
(declare-function alert "alert" (message &rest args))
(declare-function alert-add-rule "alert" (&rest args))
(declare-function ansi-color-apply-on-region "ansi-color" (begin end))
(declare-function unfill-region "unfill" (start end))
(declare-function ring-insert "ring" (ring item))
(declare-function ring-ref "ring" (ring index))
(declare-function f-file-p "f" (path))
(declare-function thread-last "subr-x" (&rest args))
(declare-function browse-url-interactive-arg "browse-url" (prompt))
(declare-function url-host "url-parse" (url))
(declare-function url-filename "url-parse" (url))
(declare-function gofmt "go-mode" ())
(declare-function lsp-organize-imports "lsp-mode" ())
(declare-function ebbflow-mode "ebbflow" (&optional arg))
```

### Local Functions (defined elsewhere in file)
```elisp
(declare-function my/dired-find-file-smart "init" ())
(declare-function my/go-mode-setup "init" ())
(declare-function my/go-mode-lsp-setup "init" ())
(declare-function my/visible-buffer-text "init" ())
(declare-function my/chatgpt-shell-purpose-of-email "init" ())
(declare-function my/chatgpt-shell-reply-email "init" ())
(declare-function my/chatgpt-shell-insert-natural-english "init" ())
(declare-function my/claude-code-display-buffer-right "init" ())
(declare-function my/claude-hook-listener "init" ())
(declare-function my/evil-paste-fix-clipboard-advice "init" ())
(declare-function my/code-review-url-from-pullreq "init" ())
(declare-function my/code-review-browse-with-external-browser "init" ())
(declare-function my/forge-insert-pullreqs-to-review "init" ())
(declare-function my/projectile-project-root-ignore-remote "init" ())
(declare-function my/termux-eshell-script-interpreter "init" ())
(declare-function my/browse-url-can-use-xdg-open-advice "init" ())
(declare-function my/skip-image-display-advice "init" ())
(declare-function my/image-ascii-display "init" ())
(declare-function my/get-image-dimensions-imk "init" ())
(declare-function my/safe-eaf-call-sync "init" ())
(declare-function my/nov-font-setup "init" ())
(declare-function my/set-reading-font "init" ())
(declare-function my/first-font-available "init" (fonts))
(declare-function my/compilation-find-file "init" ())
(declare-function my/reddigg-view-comments-no-query-string "init" ())
(declare-function ha-eshell-last-output "init" ())
(declare-function ha-eshell-ebb-switch-to-buffer "init" ())
(declare-function ha-eshell-ebb-string "init" ())
(declare-function ha-eshell-ebb-output "init" ())
(declare-function ha-eshell-store-file-output "init" ())
(declare-function ha-eshell-output "init" ())
(declare-function eshell/output "init" ())
(declare-function eshell-flow-buffer-contents "init" ())
```

---

## Phase 3: Add `defvar` Declarations for External Variables [COMPLETED]

**Problem:** ~30 references to free variables from external packages.

**Fix:** Add `defvar` declarations (without values) to silence warnings.

```elisp
;; Vertico/Compleseus
(defvar vertico-map)

;; Eshell
(defvar eshell-mode-map)
(defvar eshell-command-not-found-command)
(defvar eshell-visual-commands)
(defvar eshell-predicate-alist)
(defvar eshell-interpreter-alist)
(defvar eshell-last-output-start)
(defvar eshell-prompt-regexp)
(defvar eshell-last-input-end)
(defvar eshell-last-command-status)

;; Browse URL
(defvar browse-url-handlers)
(defvar browse-url-secondary-browser-function)

;; EWW
(defvar eww-mode-map)

;; SQLite
(defvar sqlite-mode-map)

;; Vterm
(defvar vterm-mode-map)

;; Copilot
(defvar copilot-completion-map)
(defvar company-frontends)

;; OpenAI/GPTel
(defvar openai-key)

;; Highlight Chars
(defvar hc-other-chars)

;; Dired/Dirvish
(defvar dirvish-mode-map)
(defvar dired-mode)

;; Magit/Forge
(defvar magit-diff-section-map)
(defvar forge-pullreq-mode-map)
(defvar magit-status-sections-hook)
(defvar code-review-mode-map)

;; Code Review (oref variables)
(defvar owner)
(defvar repo)
(defvar number)
(defvar reviewer)
(defvar github-username)

;; Go Mode
(defvar gofmt-command)

;; ChatGPT Shell
(defvar chatgpt-shell-prompt-query-response-style)

;; Spacemacs
(defvar dotspacemacs-default-font)

;; Dash anaphoric variables
(defvar it)
(defvar arg)
```

---

## Phase 4: Prefix Unused Lexical Arguments with `_` [COMPLETED]

**Problem:** Unused lexical arguments generate warnings.

**Affected Lines and Fixes:**

| Line | Current | Fixed |
|------|---------|-------|
| 932 | `directory` | `_directory` |
| 1040 | `args` | `_args` |
| 1607 | `ignored` | `_ignored` |
| 1611 | `ignored` | `_ignored` |
| 1615 | `ignored` | `_ignored` |

Also fix unused lexical variables:
| Line | Current | Fixed |
|------|---------|-------|
| 1093 | `browse-url-secondary-browser-function` | `_browse-url-secondary-browser-function` |
| 1769 | `gofmt-command` | `_gofmt-command` |
| 1854 | `chatgpt-shell-prompt-query-response-style` | `_chatgpt-shell-prompt-query-response-style` |
| 1864 | `chatgpt-shell-prompt-query-response-style` | `_chatgpt-shell-prompt-query-response-style` |
| 1947 | `json-data` | `_json-data` |
| 1948 | `args` | `_args` |

---

## Phase 5: Fix Docstrings

### 5a: Docstrings Wider Than 80 Characters

**Affected Lines:** 849, 861, 1152, 1467, 1707, 1714

**Fix:** Break lines at 80 characters maximum.

Example (line 849):
```elisp
;; Before:
"An alist mapping specific directories to fallback file lists which should have / at the end."

;; After:
"An alist mapping specific directories to fallback file lists.
Each directory should have / at the end."
```

### 5b: Wrong Single Quote Usage

**Affected Lines:** 1321, 1707

**Fix:** Replace `'` with `\\='` in docstrings.

Example:
```elisp
;; Before:
"This function uses 'some-var' to do things."

;; After:
"This function uses \\='some-var\\=' to do things."
```

---

## Phase 6: Fix Miscellaneous Issues

### 6a: Replace `mapcar` with `mapc` (Line 1276)

```elisp
;; Before:
(mapcar ...)  ;; value unused

;; After:
(mapc ...)
```

### 6b: Replace `next-line` with `forward-line` (Line 1532)

```elisp
;; Before:
(next-line)

;; After:
(forward-line 1)
```

### 6c: Fix Unused `boundp` Call (Line 1392)

Remove or use the return value properly.

### 6d: Move `make-variable-buffer-local` to Toplevel (Line 1397)

```elisp
;; Move outside of function to toplevel
(make-variable-buffer-local 'some-variable)
```

---

## Phase 7: Wrap Code in `with-eval-after-load`

Ensure code that references package variables/functions is properly wrapped:

```elisp
(with-eval-after-load 'eshell
  ;; eshell-specific code here
  )

(with-eval-after-load 'magit
  ;; magit-specific code here
  )

(with-eval-after-load 'forge
  ;; forge-specific code here
  )
```

---

## Phase 8: Verify All Fixes

Run byte-compile to verify all warnings are resolved:

```bash
cd ~/go/src/github.com/jaeyeom/experimental/spacemacs
emacs -Q --batch -L . -f batch-byte-compile .spacemacs
```

Expected output: No warnings or errors.

---

## Implementation Order

For minimal risk, implement in this order:

1. **Phase 1** - Most critical, fixes load-order issues
2. **Phase 4** - Quick wins, simple renames
3. **Phase 5** - Docstring fixes, no logic changes
4. **Phase 6** - Small targeted fixes
5. **Phase 3** - Variable declarations
6. **Phase 2** - Function declarations
7. **Phase 7** - Structural changes (most invasive)
8. **Phase 8** - Final verification

After each phase, run byte-compile to verify progress.
