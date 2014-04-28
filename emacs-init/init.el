;;; init.el --- Jaehyun Yeom's Emacs init file.
;;
;; Filename: init.el
;; Date: 2007--2013
;;


;;;; Basic Directory Settings

(defvar emacs-init-dir "~/.emacs.d/"
  "Root directory of emacs initializer.")
(defvar emacs-site-lisp-dir (concat emacs-init-dir "site-lisp/")
  "Directory where custom lisp files are.")
(defvar emacs-plugins-dir (concat emacs-init-dir "plugins/")
  "Directory where plugins are.")

;;; Set theme directory
(setq custom-theme-directory (concat emacs-init-dir "themes/"))

;;; Add load-path in site-lisp and plugins
(add-to-list 'load-path emacs-site-lisp-dir)
(add-to-list 'load-path emacs-plugins-dir)
(if (and (fboundp 'normal-top-level-add-subdirs-to-load-path)
         (file-directory-p emacs-plugins-dir))
    (let ((default-directory emacs-plugins-dir))
      (normal-top-level-add-subdirs-to-load-path)))


;;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-for-comint-mode t)
 '(auto-image-file-mode t)
 '(blink-cursor-mode nil)
 '(c-basic-offset 2)
 '(c-default-style (quote ((c-mode . "k&r") (c++-mode . "stroustrup") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(calendar-mark-diary-entries-flag t)
 '(calendar-mark-holidays-flag t)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(diary-display-function (quote (fancy-diary-display)))
 '(diary-display-hook (quote (fancy-diary-display)))
 '(diary-list-entries-hook (quote (sort-diary-entries)))
 '(diary-list-include-blanks t)
 '(elisp-cache-byte-compile-files t)
 '(font-lock-maximum-decoration t)
 '(global-auto-revert-mode t)
 '(global-cwarn-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-url-at-point t)
 '(indent-tabs-mode nil)
 '(jabber-alert-presence-hooks nil)
 '(kill-ring-max 70)
 '(list-diary-entries-hook (quote (sort-diary-entries)))
 '(mark-diary-entries-in-calendar t)
 '(mark-holidays-in-calendar t)
 '(menu-bar-mode t)
 '(message-send-mail-function (quote message-smtpmail-send-it) t)
 '(mouse-wheel-mode t)
 '(org-agenda-include-diary t)
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-window-setup (quote reorganize-frame))
 '(org-deadline-warning-days 14)
 '(org-export-with-LaTeX-fragments t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-reverse-note-order t)
 '(org-special-ctrl-a/e t)
 '(org-stuck-projects (quote ("+LEVEL=2/-DONE" ("NEXT" "STARTED") ("SOMEDAY") "")))
 '(org-todo-keywords (quote ((sequence "TODO" "NEXT" "STARTED(@)" "|" "DONE(@)") (type "WAITING(@)" "|" "DEFERRED(@)" "CANCELED(@)"))))
 '(pop3-leave-mail-on-server t)
 '(python-use-skeletons t)
 '(require-final-newline (quote ask))
 '(scroll-bar-mode nil)
 '(sgml-xml-mode t)
 '(show-paren-mode t)
 '(svn-status-preserve-window-configuration t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(visible-bell t nil nil "Bell is annoying, flash will be enough.")
 '(vm-mime-default-face-charsets (quote ("utf-8")))
 '(w3m-coding-system (quote utf-8))
 '(w3m-default-coding-system (quote utf-8))
 '(w3m-file-coding-system (quote utf-8))
 '(w3m-file-name-coding-system (quote utf-8))
 '(w3m-terminal-coding-system (quote utf-8))
 '(w3m-use-cookies t)
 '(winner-mode t nil (winner))
 '(x-select-enable-clipboard t)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "white"))))
 '(cursor ((t (:background "yellow"))))
 '(diff-added ((t (:foreground "green"))))
 '(diff-added-face ((t (:foreground "green"))) t)
 '(diff-changed ((t (:foreground "yellow"))))
 '(diff-changed-face ((t (:foreground "yellow"))) t)
 '(diff-header ((t (:foreground "cyan"))))
 '(diff-header-face ((t (:foreground "cyan"))) t)
 '(diff-hunk-header ((t (:foreground "magenta"))))
 '(diff-hunk-header-face ((t (:foreground "magenta"))) t)
 '(diff-removed ((t (:foreground "red"))))
 '(diff-removed-face ((t (:foreground "red"))) t)
 '(font-lock-comment-face ((t (:foreground "chocolate1"))))
 '(magit-diff-add ((nil (:foreground "green"))))
 '(magit-item-highlight ((nil (:background "dark blue"))))
 '(magit-log-head-label ((nil (:foreground "black"))))
 '(magit-log-tag-label ((nil (:foreground "black"))))
 '(menu ((t (:background "white" :foreground "black"))))
 '(p4-depot-unmapped-face ((t (:foreground "red"))) t)
 '(p4-diff-file-face ((t (:background "dark slate gray"))) t)
 '(p4-diff-head-face ((t (:background "dark slate blue"))) t)
 '(p4-diff-ins-face ((t (:foreground "green"))) t))


;;;; try-require

(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (or (and (not (stringp feature)) (featurep feature))
      (condition-case err
          ;; protected form
          (progn
            (message "Checking for library `%s'..." feature)
            (if (stringp feature)
                (load-library feature)
              (require feature))
            (message "Checking for library `%s'... Found" feature))
        ;; error handler
        (file-error  ; condition
         (progn
           (message "Checking for library `%s'... Missing" feature)
           (add-to-list 'missing-packages-list feature 'append))
         nil))))


;;;; Package Management

;;; Package Initialize
(when (try-require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/")))

;;; El-Get
(add-to-list 'load-path (concat emacs-init-dir "el-get/el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (concat emacs-init-dir "el-get-user/recipes"))
(el-get 'sync)


;;;; Display Settings

(setq split-width-threshold 170)
(modify-frame-parameters (selected-frame) default-frame-alist)

(if (fboundp 'transparent-this-frame)
    (message "Function `transparent-this-frame' is already bounded")
  (defun transparent-this-frame ()
    (interactive)
    ;; TODO(jaeyeom): This function has hard-coded values.
    (set-frame-parameter (selected-frame) 'alpha '(85 70))))

(if (fboundp 'restore-this-frame)
    (message "Function `restore-this-frame' is already bounded")
  (defun restore-this-frame ()
    (interactive)
    ;; TODO(jaeyeom): This function has hard-coded values.
    (set-frame-parameter (selected-frame) 'width 80)
    (set-frame-parameter (selected-frame) 'height 40)))

(if (fboundp 'transparent-default-frame)
    (message "Function `transparent-default-frame' is already bounded")
  (defun transparent-default-frame ()
    (interactive)
    (add-to-list 'default-frame-alist '(alpha 80 65))))

;;; Alternative color settings
;; 8 colors
;; ("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white")
(when (= (display-color-cells) 8)
  t)


;;;; Editing

;;; Korean language and UTF-8 settings.
;; Forcing to use utf-8 coding system.
;; TODO(jaeyeom): Find a better way to detect system coding
(when (or (not window-system)
          (not (eq (window-system) 'w32)))
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (setq-default coding-system-for-read 'utf-8)
  (setq-default file-name-coding-system 'utf-8))

;; I don't like to put space after magic prefix #!
(setq-default executable-prefix "#!")

;; Make searches case insensative by default
(setq-default case-fold-search t)

;; I like line moves point by logical lines not by visible lines.
;; (setq-default ...) does not work here.
(setq line-move-visual nil)

;; Emacs 23 likes to pop up real X windows for tooltips, which is
;; highly annoying on slow connections, especially using VNC or
;; NX. This makes it use the echo-area like it used to.
;; (setq-default ...) does not work here.
(setq tooltip-use-echo-area t)

;; change save interval from 300 to 1000
;; keystrokes so it isn't so annoying
(setq auto-save-interval 1000)

;; Can open minibuffer in the minibuffer
(setq enable-recursive-minibuffers t)

;;; Uniquify
(when (try-require 'uniquify)
  ;; Default from 24.4
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;;; Ace Jump mode
(try-require 'ace-jump-mode)
(eval-after-load 'ace-jump-mode
  '(progn
     (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
     (ace-jump-mode-enable-mark-sync)))

;;; Multiple Cursors
(try-require 'multiple-cursors)
(eval-after-load 'multiple-cursors
  '(progn
     (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
     (global-set-key (kbd "C->") 'mc/mark-next-like-this)
     (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
     (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

;;; Smart Editing
(defun smart-delete-space (&optional n)
  "Delete all spaces and tabs around point or join the line.
If N is non-nil, non-integer value, only delete them before
point. If N is an integer, it calls calls just-one-space with
N. From the second time, it will leaving one space (or N
spaces). If the position is at the beginning or end of the line,
it will join the line."
  (interactive "*P")
  (cond
   ((eq this-command last-command)
    (cond
     ((and (/= (point) (point-max))
           (= (point) (line-end-position)))
      (delete-char 1))
     ((and (/= (point) (point-min))
           (= (point) (line-beginning-position)))
      (delete-char -1)))
    ;; It won't put any spaces by default if the line is empty.
    (or (and (not n)
             (or (= (point) (line-end-position))
                 (= (point) (line-beginning-position)))
             (looking-at-p "^\\|$"))
        (just-one-space n)))
   ((integerp n)
    (just-one-space n))
   (t
    (delete-horizontal-space n))))

;; Mapping key M-\ to smart-delete-space
(global-set-key "\M-\\" 'smart-delete-space)

;; Force alt key as meta key
(setq x-alt-keysym 'meta)

;; for disabling quail completion
(eval-after-load "quail"
  '(progn
     (define-key quail-translation-keymap [tab] nil)
     (define-key quail-translation-keymap "\C-i" nil)))

;;; linum-mode
;; Adds additional 1 space after line number because there is no space
;; between the line number and file contents in terminal Emacs.
(eval-after-load 'linum
  (setq linum-format
        (lambda (line)
          (propertize (format
                       (let ((w (length (number-to-string
                                         (count-lines (point-min) (point-max))))))
                         (concat "%" (number-to-string w) "d ")) line) 'face 'linum))))


;;;; Files and Shell

;;; Dired
(try-require 'dired-x)
(setq-default dired-listing-switches "-alh")  ; Print sizes in human readable format.
(setq-default image-dired-append-when-browsing t)

;;; Tramp
(when (= emacs-major-version 23)
  (eval-after-load 'tramp
    (setq-default tramp-debug-buffer t)
    (setq-default tramp-verbose 10)
    ;; sshx seemed to be good on Emacs 23.
    ;; Default value "scpc" is good on Emacs 24.
    (setq-default tramp-default-method "sshx")))

;;; Shell
(eval-after-load 'shell
  '(progn
     (when (file-exists-p "/proc")
       ;; Copied and modified from
       ;; http://www.emacswiki.org/emacs/ShellDirtrackByProcfs
       ;; With this code shell knows the current directory even if the
       ;; prompt format is uncommon.
       (defun track-shell-directory/procfs ()
         (if (fboundp 'shell-dirtrack-mode)
             (shell-dirtrack-mode 0))
         (add-hook 'comint-preoutput-filter-functions
                   (lambda (str)
                     (prog1 str
                       (when (string-match comint-prompt-regexp str)
                         (cd (file-symlink-p
                              (format "/proc/%s/cwd" (process-id
                                                      (get-buffer-process
                                                       (current-buffer)))))))))
                   nil t))
       (add-hook 'shell-mode-hook 'track-shell-directory/procfs))

     ;; No pager.
     (setenv "PAGER" "cat")))

;;; Eshell
(eval-after-load 'eshell
  '(progn
     ;; These settings may not work well for customization module.
     (defun eshell/clear ()
       "Clears the eshell buffer."
       (interactive)
       (let ((inhibit-read-only t))
         (erase-buffer)))

     (defun eshell/em (filename)
       "Calls find-file."
       (interactive)
       (find-file filename))

     (defun eshell/emacs (filename)
       "Calls find-file."
       (interactive)
       (find-file filename))

     (defun eshell/w3m (url)
       "Calls w3m-browse-url for URL patterns and w3m-find-file
otherwise."
       (interactive)
       (if (string-match "[a-zA-Z]+://" url)
           (w3m-browse-url url)
         (w3m-find-file url)))))

(eval-after-load 'em-prompt
  '(setq-default eshell-before-prompt-hook '(eshell-begin-on-new-line)))

(eval-after-load 'em-unix
  '(progn
     (setq-default eshell-cp-interactive-query t)
     (setq-default eshell-ln-interactive-query t)
     (setq-default eshell-mv-interactive-query t)
     (setq-default eshell-rm-interactive-query t)))

(when (< emacs-major-version 24)
  (eval-after-load 'esh-mode
    '(progn
       (when (and (>= emacs-major-version 23)
                  (not (fboundp 'eshell-handle-ansi-color))
                  (try-require 'ansi-color))
         (defun eshell-handle-ansi-color ()
           (ansi-color-apply-on-region eshell-last-output-start
                                       eshell-last-output-end))
         (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color))

       (if (fboundp 'eshell-watch-for-password-prompt)
           (add-to-list 'eshell-output-filter-functions 'eshell-watch-for-password-prompt))

       (if (fboundp 'eshell-handle-control-codes)
           (add-to-list 'eshell-output-filter-functions 'eshell-handle-control-codes)))))

;;; Bash completion
(when (< emacs-major-version 24)
  (try-require 'bash-completion)
  (eval-after-load 'bash-completion
    '(bash-completion-setup)))

;;; EasyPG
(try-require 'epa-file)
(eval-after-load 'epa-file
  '(progn
     (epa-file-enable)
     ;; Prevent desktop popup for asking password.
     (setenv "GPG_AGENT_INFO" nil)))


;;;; Development

;; Mapping key C-c c to compile
(global-set-key [(?\C-c) (c)] 'compile)

;; Comment Region
(global-set-key "\C-c;" 'comment-region)

;; Keep camel case
(setq dabbrev-case-fold-search nil)

;; Warn long lines.
(if (>= emacs-major-version 22)
    (progn
      (defun font-lock-width-keyword (width)
        "Return a font-lock style keyword for a string beyond width WIDTH
thatuses 'font-lock-warning-face'."
        `((,(format "^%s\\(.+\\)" (make-string width ?.))
           (1 font-lock-warning-face t))))

      (font-lock-add-keywords 'c++-mode (font-lock-width-keyword 80))
      (font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))
      (font-lock-add-keywords 'java-mode (font-lock-width-keyword 100)))
  (progn
    ;; Turn on red highlighting for characters outside of the 80 char limit
    (add-hook 'c++-mode-hook
              '(lambda () (font-lock-set-up-width-warning 80)))
    (add-hook 'java-mode-hook
              '(lambda () (font-lock-set-up-width-warning 100)))
    (add-hook 'python-mode-hook
              '(lambda () (font-lock-set-up-width-warning 80)))))

;;; Helper function for escaping and unescaping double quoted string.
(defun escape-double-quoted-string ()
  "Convert normal string in active region to double quoted
string.  Newline character will be converted to \\n, \" will be
\\\" and for every line it will be enclosed with \"
character. This is reverse conversion of command
\\[unescape-double-quoted-string]."
  (interactive)
  (let ((start (copy-marker (if (region-active-p)
                                (min (region-beginning) (region-end))
                              (point-min))))
        (end (copy-marker (if (region-active-p)
                              (max (region-beginning) (region-end))
                            (point-max)))))
    (goto-char start)
    (while (search-forward "\\" end t)
      (replace-match "\\\\" nil t))
    (goto-char start)
    (while (search-forward "\"" end t)
      (replace-match "\\\"" nil t))
    (goto-char start)
    (while (and (< (point) end) (re-search-forward "$" nil t))
      (replace-match "\\n\"" end t)
      (goto-char (+ (point) 1)))
    (goto-char start)
    (while (re-search-forward "^" end t)
      (replace-match "\"" nil t))))

(defun unescape-double-quoted-string ()
  "Convert double quoted string to normal string.  This is
reverse conversion of command \\[escape-double-quoted-string]."
  (interactive)
  (let ((start (copy-marker (if (region-active-p)
                                (min (region-beginning) (region-end))
                              (point-min))))
        (end (copy-marker (if (region-active-p)
                              (max (region-beginning) (region-end))
                            (point-max)))))
    (goto-char start)
    (while (re-search-forward "^ *\"" end t)
      (replace-match "" nil t))
    (goto-char start)
    (while (re-search-forward "\\(\\\\n\\)?\"$" end t)
      (replace-match "" nil t))
    (goto-char start)
    (while (search-forward "\\\"" end t)
      (replace-match "\"" nil t))
    (goto-char start)
    (while (search-forward "\\\\" end t)
      (replace-match "\\" nil t))))

;;; Yasnippet
(try-require 'yasnippet)
(eval-after-load 'yasnippet
  '(yas-global-mode 1))

;;; Go
(eval-after-load 'go-mode
  '(progn
     (try-require 'go-eldoc)
     (add-hook 'before-save-hook 'gofmt-before-save)
     (add-hook 'go-mode-hook
               (lambda ()
                 (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
     (add-hook 'go-mode-hook
               (lambda ()
                 (local-set-key (kbd "C-c i") 'go-goto-imports)))))

(eval-after-load 'go-eldoc
  '(add-hook 'go-mode-hook 'go-eldoc-setup))

;;; Python
(if (>= emacs-major-version 23)
    (defadvice python-send-buffer (after advice-switch-to-python)
      "Switch to *Python* after C-c C-c"
      (python-switch-to-python t)))

(defadvice python-calculate-indentation (around outdent-closing-brackets)
  "Handle lines beginning with a closing bracket and indent them so that
they line up with the line containing the corresponding opening bracket."
  (save-excursion
    (beginning-of-line)
    (let ((syntax (syntax-ppss)))
      (if (and (not (eq 'string (syntax-ppss-context syntax)))
               (python-continuation-line-p)
               (cadr syntax)
               (skip-syntax-forward "-")
               (looking-at "\\s)"))
          (progn
            (end-of-line)
            (ignore-errors (backward-sexp))
            (setq ad-return-value (current-indentation)))
        ad-do-it))))

(ad-activate 'python-calculate-indentation)

;;; Magit
(try-require 'magit)
(eval-after-load 'magit
  '(progn
     (defadvice vc-dir (around redirect-magit-status activate compile)
       "Redirect to `magit-status' when `vc-dir' was called in a git repository."
       (if (or (string-equal "GIT" (car (vc-deduce-fileset t)))
               (string-equal "Git" (car (vc-deduce-fileset t))))
           (magit-status dir)
         ad-do-it))
     (defadvice vc-diff (around redirect-magit-diff-working-tree activate compile)
       "Redirect to `magit-diff-working-tree' when `vc-diff' was called in a git repository."
       (if (or (string-equal "GIT" (car (vc-deduce-fileset t)))
               (string-equal "Git" (car (vc-deduce-fileset t))))
           (magit-diff-working-tree "HEAD")
         ad-do-it))
     (defadvice vc-print-log (around redirect-magit-log activate compile)
       "Redirect to `magit-log' when `vc-print-log' was called in a git repository."
       (if (or (string-equal "GIT" (car (vc-deduce-fileset t)))
               (string-equal "Git" (car (vc-deduce-fileset t))))
           (magit-log)
         ad-do-it))))


;;;; Apps

;;; W3m
(autoload 'w3m "w3m"
  "Web browser in Emacs."
  t)

(eval-after-load 'w3m
  '(progn
     ;; Set default browser as google-chrome on windows mode.
     (if (not window-system)
         (setq-default browse-url-browser-function 'w3m-browse-url)
       (setq-default browse-url-generic-program "google-chrome")
       (setq-default
        browse-url-browser-function
        '(("^\\(file\\|mailto\\):.+$" . w3m-browse-url) ; w3m takes care of these very well
          ("^.*$" . browse-url-generic))))

     ;; Deletes trailing whitespace whenever the page is loaded.
     (add-hook 'w3m-display-hook
               (lambda (url)
                 (let ((buffer-read-only nil))
                   (delete-trailing-whitespace))))

     ;; Setting w3m-use-title-buffer-name will disambiguate buffer
     ;; name. Lower version which doesn't have this variable needs hook
     ;; function to do this manually.
     (if (boundp 'w3m-use-title-buffer-name)
         (setq w3m-use-title-buffer-name t)
       (add-hook 'w3m-display-hook
                 (lambda (url)
                   (rename-buffer
                    (format "*w3m: %s*" (or w3m-current-title
                                            w3m-current-url)) t))))))

;;; Org Mode
;; I'm also going to use org-mode in archive mode and txt file.
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; These key maps can be used outside of org-mode.
(define-key mode-specific-map [?a] 'org-agenda)
(define-key mode-specific-map [?l] 'org-store-link)
(define-key mode-specific-map [?r] 'org-capture)

(eval-after-load 'org
  '(progn
     ;; C-c x and shortcut will change todo state like the following.
     (define-prefix-command 'org-todo-state-map)
     (define-key org-mode-map "\C-cx" 'org-todo-state-map)
     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))))

(eval-after-load 'org-agenda
  '(progn
     ;; C-n and C-p won't be overridden in org-agenda-mode.
     (define-key org-agenda-mode-map "\C-n" 'next-line)
     (define-key org-agenda-keymap "\C-n" 'next-line)
     (define-key org-agenda-mode-map "\C-p" 'previous-line)
     (define-key org-agenda-keymap "\C-p" 'previous-line)))

;;; GDocs
;; requires emacspeak 35 or higher.
(when (try-require 'gdocs)
  (defun gdocs-refresh-document-text ()
    "Refresh document from Google docs. Current document contents
will be in the buffer *g scratch*."
    (interactive)
    (gdocs-fetch-document-text)
    (with-current-buffer g-scratch-buffer
      (when (/= 10 (char-before (point-max)))
        (goto-char (point-max))
        (insert-char 10 1))
      ;; Cleanup newline style
      (goto-char (point-min))
      (replace-string "" "")
      (goto-char (point-min))
      (replace-string "\n\n" "\n")
      (goto-char (point-min))
      ;; Remove unknown dirty characters at the beginning
      (if (looking-at "\357\273\277")
          (delete-char 3))
      (set-buffer-multibyte t))
    (buffer-swap-text (get-buffer g-scratch-buffer))
    (set-buffer-multibyte t)))

;;; Jabber
(eval-after-load 'jabber-alert
  ;; Message alert hooks
  '(define-jabber-alert echo "Show a message in the echo area"
    (lambda (msg)
      (unless (minibuffer-prompt)
        (message "%s" msg)))))


;;;; Emacs Server

;;; Load Local stuffs
(load "~/.emacs.d/init.local.el" 'noerror)

;;; Miscellaneous
(try-require 'uptime)

;;; Midnight mode
(when (try-require 'midnight)
  (midnight-delay-set 'midnight-delay "4:00am"))

;; For older Emacs which doesn't support MultiTTY and if it's not
;; window system, it'll use screen feature for launching Emacs faster.
(when (and (not window-system) (< emacs-major-version 23))
  (add-hook 'after-init-hook 'server-start)
  (add-hook 'server-done-hook
            (lambda ()
              (set-buffer (get-buffer-create "*screen*"))
              (insert-file-contents "/tmp/emacsclient-caller")
              (send-string-to-terminal
               (concat "\e]83;select " (thing-at-point 'word) "\a"))
              (kill-buffer "*screen*"))))

;;;; Desktop Mode
(when (>= emacs-major-version 23)
  (setq-default desktop-base-file-name (concat ".emacs." (system-name) ".desktop"))
  (setq-default desktop-base-lock-name (concat desktop-base-file-name ".lock"))
  (setq history-length 250)
  (desktop-save-mode 1))

;;;; Emacs Server
(server-start)

;;;; Edit Server
;; For chrome extension at
;; https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh?hl=en
(when (try-require 'edit-server)
  (edit-server-start))
