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
(add-to-list 'load-path emacs-init-dir)
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
 '(auto-image-file-mode t)
 '(blink-cursor-mode nil)
 '(c-basic-offset 2)
 '(c-default-style (quote ((c-mode . "k&r") (c++-mode . "stroustrup") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(diary-display-hook (quote (fancy-diary-display)))
 '(diary-list-include-blanks t)
 '(dynamic-completion-mode t)
 '(elisp-cache-byte-compile-files t)
 '(emacs-lisp-mode-hook (quote ((lambda nil (local-set-key "" (quote byte-compile-file))))))
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
 '(makefile-mode-hook (quote ((lambda nil (local-set-key "" (quote compile))))) t)
 '(mark-diary-entries-in-calendar t)
 '(mark-holidays-in-calendar t)
 '(menu-bar-mode t)
 '(message-send-mail-function (quote message-smtpmail-send-it))
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
 '(org-remember-store-without-prompt t)
 '(org-reverse-note-order t)
 '(org-special-ctrl-a/e t)
 '(pop3-leave-mail-on-server t)
 '(python-use-skeletons t)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
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

;;; ------ TRY REQUIRE ------

(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

;; attempt to load a feature/library, failing silently
(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
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
     nil)))

;; I don't like to put space after magic prefix #!
(setq-default executable-prefix "#!")

;; Make searches case insensative by default
(setq-default case-fold-search t)

;;;; Display Settings

(try-require 'init-convenience-settings)
(setq split-width-threshold 170)
(modify-frame-parameters (selected-frame) default-frame-alist)

(if (fboundp 'transparent-this-frame)
    (message "Function `transparent-this-frame' is already bounded")
  (defun transparent-this-frame ()
    (interactive)
    ;; TODO(jaeyeom): This function has hard-coded values.
    (set-frame-parameter (selected-frame) 'alpha '(80 65))))

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

;;;; Alternative color settings

;;; 8 colors
;; ("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white")
(when (= (display-color-cells) 8)
  t)

;;;; Coding systems

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

;;;; Auto mode lists
;;; PHP
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(autoload 'php-mode "php-mode"
  "Major mode for editing php code."
  t)
;;; Verilog
(add-to-list 'auto-mode-alist '("\\.v" . verilog-mode))
(autoload 'verilog-mode "verilog-mode"
  "Major mode for editing verilog code."
  t)
;;; nML
(add-to-list 'auto-mode-alist '("\\.n[iylp]?" . n-mode))

;;;; Dired

(try-require 'dired-x)
;; Print sizes in human readable format.
(setq-default dired-listing-switches "-alh")
(setq-default image-dired-append-when-browsing t)

;;;; Emacs for Development

;; Mapping key C-c c to compile
(global-set-key [(?\C-c) (c)] 'compile)

;;; Initialize yasnippet
(when (try-require 'yasnippet)
    (yas/initialize)
    (yas/load-directory (concat emacs-plugins-dir "yasnippet/snippets")))

;;; Ebrowse will load BROWSE file when idle time
(when (try-require 'ebrowse)
  (defun revert-ebrowse-tree-if-exists ()
    (save-current-buffer
      (dolist (buf (ebrowse-tree-buffer-list))
        (set-buffer buf)
        (revert-buffer 'ignore-auto 'noconfirm 'preserve-modes))))

  (set 'ebrowse-tree-reload-idle-timer
       (run-with-idle-timer 600 t 'revert-ebrowse-tree-if-exists)))

;; Load Emacs W3M

;;;; W3m configuration

(autoload 'w3m "w3m"
  "Web browser in Emacs."
  t)

(eval-after-load 'w3m
  '(progn
     (setq-default browse-url-generic-program "google-chrome")
     (setq-default
      browse-url-browser-function
      '(("^\\(file\\|mailto\\):.+$" . w3m-browse-url) ; w3m takes care of these very well
        ("^.*$" . browse-url-generic)))


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
                                            w3m-current-url)) t))))

     ;; Fake useragents. Copied from
     ;; http://sachachua.com/blog/2008/08/emacs-and-w3m-fake-your-user-agent/
     (defvar wicked/w3m-fake-user-agents ;; (1)
       `(("w3m" . ,(concat "Emacs-w3m/" emacs-w3m-version " " w3m-version))
         ("ie6" . "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)")
         ("ff3" . "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1) Gecko/2008070206 Firefox/3.0.1")
         ("ff2" . "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.13) Gecko/20080208 Firefox/2.0.0.13")
         ("ie7" . "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)")
         ("ie5.5" . "Mozilla/4.0 (compatible; MSIE 5.5; Windows 98)")
         ("iphone" . "Mozilla/5.0 (iPhone; U; CPU iPhone OS 2_0 like Mac OS X; en-us) AppleWebKit/525.18.1 (KHTML, like Gecko) Version/3.1.1 Mobile/5A347 Safari/525.20")
         ("safari" . "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_5_2; en-us) AppleWebKit/525.13 (KHTML, like Gecko) Version/3.1 Safari/525.13")
         ("chrome19" . "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.21 (KHTML, like Gecko) Chrome/19.0.1042.0 Safari/535.21")
         ("google" . "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)"))
       "*Associative list of user agent names and strings.")

     (defvar wicked/w3m-fake-user-agent-sites ;; (2)
       '(("^https?://www\\.useragentstring\\.com" . "ff2"))
       "*Associative list of regular expressions matching URLs and the agent keyword or value.
 The first matching entry will be used.")

     (defun wicked/w3m-set-user-agent (agent)
       "Set the user agent to AGENT based on `wicked/w3m-fake-user-agents'.
 If AGENT is not defined in `wicked/w3m-fake-user-agents', it is used as the user agent.
 If AGENT is empty, the default w3m user agent will be used."
       (interactive
        (list
         (completing-read "User-agent [w3m]: "
                          (mapcar 'car wicked/w3m-fake-user-agents)
                          nil nil nil nil "w3m"))) ;; (3)
       (if agent
           (progn
             (setq w3m-user-agent
                   (or
                    (and (string= agent "") (assoc "w3m" wicked/w3m-fake-user-agents)) ;; (4)
                    (cdr (assoc agent wicked/w3m-fake-user-agents)) ;; (5)
                    agent)) ;; (6)
             (setq w3m-add-user-agent t))
         (setq w3m-add-user-agent nil)))

     (defun wicked/w3m-reload-this-page-with-user-agent (agent)
       "Browse this page using AGENT based on `wicked/w3m-fake-user-agents'.
 If AGENT is not defined in `wicked/w3m-fake-user-agents', it is used as the user agent.
 If AGENT is empty, the default w3m user agent will be used."
       (interactive (list (completing-read "User-agent [w3m]: "
                                           (mapcar 'car wicked/w3m-fake-user-agents)
                                           nil nil nil nil "w3m")))
       (let ((w3m-user-agent w3m-user-agent)
             (w3m-add-user-agent w3m-add-user-agent))
         (wicked/w3m-set-user-agent agent) ;; (7)
         (w3m-reload-this-page)))

     (defadvice w3m-header-arguments (around wicked activate) ;; (8)
       "Check `wicked/w3m-fake-user-agent-sites' for fake user agent definitions."
       (let ((w3m-user-agent w3m-user-agent)
             (w3m-add-user-agent w3m-add-user-agent)
             (sites wicked/w3m-fake-user-agent-sites))
         (while sites
           (if (string-match (caar sites) (ad-get-arg 1))
               (progn
                 (wicked/w3m-set-user-agent (cdar sites))
                 (setq sites nil))
             (setq sites (cdr sites))))
         ad-do-it))))

;; I'm also going to use org-mode in archive mode and txt file.
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; These key maps can be used outside of org-mode.
(define-key mode-specific-map [?a] 'org-agenda)
(define-key mode-specific-map [?l] 'org-store-link)
(define-key mode-specific-map [?r] 'remember)

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

;; gdocs - requires emacspeak 35 or higher.
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

;; Remember mode
(try-require 'remember)
(eval-after-load 'remember
  '(progn
     (add-hook 'remember-mode-hook 'org-remember-apply-template)
     (define-key global-map [(control meta ?r)] 'remember)))

;; Org agenda TODO list will show only unscheduled items. I usually
;; check scheduled item from agenda for current week mode, and adds
;; unscheduled items from TODO lists if necessary.
;; (setq org-agenda-todo-ignore-scheduled t)

;; This doesn't show sub TODO items. So TODO list looks much shorter
;; and brief.
;; (setq org-agenda-todo-list-sublevels nil)

(load "init-org-publish-alist.el" 'noerror)

;; Load PHP mode
(try-require 'php-mode)

;; Load PSVN
(try-require 'psvn)

;; Load Magit
(when (try-require 'magit)
  (global-set-key (kbd "C-x v b") 'magit-status))

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

(if (>= emacs-major-version 22)
    (progn
      (defun font-lock-width-keyword (width)
        "Return a font-lock style keyword for a string beyond width WIDTH
thatuses 'font-lock-warning-face'."
        `((,(format "^%s\\(.+\\)" (make-string width ?.))
           (1 font-lock-warning-face t))))

      (font-lock-add-keywords 'c++-mode (font-lock-width-keyword 80))
      (font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))
      (font-lock-add-keywords 'java-mode (font-lock-width-keyword 80))
      )
  (progn
    ;; Turn on red highlighting for characters outside of the 80 char limit
    (add-hook 'c++-mode-hook
              '(lambda () (font-lock-set-up-width-warning 80)))
    (add-hook 'java-mode-hook
              '(lambda () (font-lock-set-up-width-warning 80)))
    (add-hook 'python-mode-hook
              '(lambda () (font-lock-set-up-width-warning 80)))
    ))

;; Python
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

;;; CoffeeScript

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(autoload 'coffee-mode "coffee-mode"
  "Major mode for editing CoffeeScript code."
  t)
;; Unfortunately, this should be loaded before coffee-mode.
(setq-default coffee-tab-width 2)
(setq-default coffee-cygwin-mode nil)

;;;; Shell

(eval-after-load 'shell
  '(progn
     (when (file-exists-p "/proc")
       ;; Copied and modified from
       ;; http://www.emacswiki.org/emacs/ShellDirtrackByProcfs
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

     ;; Colorful shell mode
     (if (try-require 'ansi-color)
         (setq-default ansi-color-for-comint-mode t))))


;;;; Eshell Settings

(when (try-require 'eshell)
  ;; These settings may not work well for customization module.
  (setq-default eshell-after-prompt-hook (quote (eshell-show-maximum-output)))
  (setq-default eshell-before-prompt-hook (quote (eshell-begin-on-new-line)))
  (setq-default eshell-cp-interactive-query t)
  (setq-default eshell-ln-interactive-query t)
  (setq-default eshell-mv-interactive-query t)
  (setq-default eshell-rm-interactive-query t)

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
      (w3m-find-file url)))

  ;; Eshell Color
  (when (and (>= emacs-major-version 23)
             (try-require 'ansi-color))
    (defun eshell-handle-ansi-color ()
      (ansi-color-apply-on-region eshell-last-output-start
                                  eshell-last-output-end))
    (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color))

  (if (fboundp 'eshell-watch-for-password-prompt)
      (add-to-list 'eshell-output-filter-functions 'eshell-watch-for-password-prompt))

  (if (fboundp 'eshell-handle-control-codes)
      (add-to-list 'eshell-output-filter-functions 'eshell-handle-control-codes)))

;;;; About Tramp mode
(when (>= emacs-major-version 23)
  (setq-default tramp-debug-buffer t)
  (setq-default tramp-verbose 10)
  (setq-default tramp-default-method "sshx"))

;;;; EasyPG
(if (try-require 'epa-file)
    (epa-file-enable))

;;;; Jabber
(when (try-require 'jabber)
  ;; Message alert hooks
  (define-jabber-alert echo "Show a message in the echo area"
    (lambda (msg)
      (unless (minibuffer-prompt)
        (message "%s" msg)))))

;;;; linum-mode
;; Adds additional 1 space after line number because there is no space
;; between the line number and file contents in terminal Emacs.
(eval-after-load 'linum
  (setq linum-format
        (lambda (line)
          (propertize (format
                       (let ((w (length (number-to-string
                                         (count-lines (point-min) (point-max))))))
                         (concat "%" (number-to-string w) "d ")) line) 'face 'linum))))

(try-require 'init-directed-switch)

;;;; Bash completion
(when (try-require 'bash-completion)
  (bash-completion-setup))

;;;; Editing

;; I like line moves point by logical lines not by visible lines.
;; (setq-default ...) does not work here.
(setq line-move-visual nil)

;; Emacs 23 likes to pop up real X windows for tooltips, which is
;; highly annoying on slow connections, especially using VNC or
;; NX. This makes it use the echo-area like it used to.
;; (setq-default ...) does not work here.
(setq tooltip-use-echo-area t)

(defun goto-matching-paren-or-insert (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert it."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-sexp 1))
        ((looking-back "\\s\)") (backward-sexp 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key "%" 'goto-matching-paren-or-insert)

;; Can open minibuffer in the minibuffer
(setq enable-recursive-minibuffers t)

;; Keep camel case
(setq dabbrev-case-fold-search nil)

;;; Uniquify features
(defun uniquify-region (beg end)
  "Remove duplicate adjacent lines in the given region from BEG
to END. This works similar to unix uniq command in the region."
  (interactive "*r")
  (goto-char beg)
  (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
    (replace-match "\\1")))

(defun uniquify-buffer ()
  "Remove duplicate adjacent lines in the current buffer. This
works similar to unix uniq command to the current buffer."
  (interactive)
  (uniquify-region (point-min) (point-max)))

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

;;; Comment Region
(global-set-key "\C-c;" 'comment-region)

;; for disabling quail completion
(eval-after-load "quail"
  '(progn
     (define-key quail-translation-keymap [tab] nil)
     (define-key quail-translation-keymap "\C-i" nil)))

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

;;; Miscellaneous
(try-require 'uptime)

;; Load Local stuffs
(load "init.local.el" 'noerror)

;;; Rebind find-file
;; I don't use this anymore because ido-mode supports this. Only load
;; this when ido-mode is missing.
(when (and (not (fboundp 'ido-mode))
           (try-require 'ffap))
  (define-key ctl-x-map "\C-f" 'find-file-at-point))

;;; Midnight mode
(when (try-require 'midnight)
  (midnight-delay-set 'midnight-delay "4:00am"))

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
