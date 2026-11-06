;;; decide-test.el --- Tests for decide.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch tests for argument lists.  They do not run the decide program.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'decide)

(ert-deftest decide-answer-args-keep-raw-text ()
  (let ((text "hello\nthere \"quoted\""))
    (should (equal (decide--answer-args "D-001" text)
                   (list "answer" "--json" "D-001" text)))))

(ert-deftest decide-answer-cancel-builds-no-command ()
  (should (null (decide--submit-args 'cancel "D-001" "hello\nthere"))))

(ert-deftest decide-compose-args-and-cancel ()
  (let ((text "hello\nthere"))
    (should (equal (decide--send-args "alpha" text)
                   (list "send" "--to" "alpha" "--json" text)))
    (should (equal (decide--submit-args 'send "alpha" text)
                   (list "send" "--to" "alpha" "--json" text)))
    (should (null (decide--submit-args 'cancel "alpha" text)))))

(ert-deftest decide-status-stale-row-is-dim ()
  (with-temp-buffer
    (decide-status-render
     '(("agents" .
        ((("name" . "alpha")
          ("state" . "working")
          ("text" . "fixing")
          ("updatedAt" . "2026-11-06T12:00:00Z"))
         (("name" . "beta")
          ("state" . "paused")
          ("text" . "old")
          ("updatedAt" . "2026-11-06T11:00:00Z"))))
       ("resources" .
        ((("name" . "workspace")
          ("availability" . "free")
          ("note" . "paused")
          ("updatedAt" . "2026-11-06T12:00:00Z")))))
     (date-to-time "2026-11-06T12:00:00Z"))
    (goto-char (point-min))
    (search-forward "alpha")
    (should-not (eq (get-text-property (match-beginning 0) 'face) 'shadow))
    (search-forward "beta")
    (should (eq (get-text-property (match-beginning 0) 'face) 'shadow))
    (search-forward "workspace")
    (should-not (eq (get-text-property (match-beginning 0) 'face) 'shadow))
    (let* ((agent-header (decide--test-line "agent"))
           (alpha (decide--test-line "alpha"))
           (beta (decide--test-line "beta"))
           (res-header (decide--test-line "free/busy"))
           (workspace (decide--test-line "workspace")))
      (should (= (string-match-p "state" agent-header)
                 (string-match-p "working" alpha)))
      (should (= (string-match-p "state" agent-header)
                 (string-match-p "paused" beta)))
      (should (= (string-match-p "text" agent-header)
                 (string-match-p "fixing" alpha)))
      (should (= (string-match-p "free/busy" res-header)
                 (string-match-p "free" workspace)))
      (should (= (string-match-p "note" res-header)
                 (string-match-p "paused" workspace))))))

(defun decide--test-line (needle)
  "Return the first buffer line that contains NEEDLE."
  (save-excursion
    (goto-char (point-min))
    (search-forward needle)
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(ert-deftest decide-status-columns-stay-inside-the-width ()
  (with-temp-buffer
    (decide-status-render
     '(("agents" .
        ((("name" . "alpha")
          ("state" . "working")
          ("text" . "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
          ("updatedAt" . "2026-11-06T12:00:00Z")))))
     (date-to-time "2026-11-06T12:00:00Z"))
    (goto-char (point-min))
    (while (not (eobp))
      (should (<= (string-width (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position)))
                  80))
      (forward-line 1))
    (should (string-match-p "…" (decide--test-line "alpha")))))

(ert-deftest decide-status-timer-stops-when-the-buffer-is-killed ()
  (unwind-protect
      (progn
        (with-current-buffer (get-buffer-create "*decide status*")
          (decide-status-mode)
          (decide--status-ensure-timer)
          (should (timerp decide--status-timer)))
        (kill-buffer "*decide status*")
        (should (null decide--status-timer)))
    (when decide--status-timer
      (cancel-timer decide--status-timer)
      (setq decide--status-timer nil))
    (when (get-buffer "*decide status*")
      (kill-buffer "*decide status*"))))

(ert-deftest decide-plain-g-still-refreshes-without-evil-keys ()
  (should (eq (lookup-key decide-list-mode-map (kbd "g")) 'decide-refresh))
  (should (eq (lookup-key decide-status-mode-map (kbd "g")) 'decide-status-refresh)))

(ert-deftest decide-revert-keeps-the-same-item ()
  (let ((rows '((("id" . "D-1") ("owner" . "ada") ("question" . "one"))
                (("id" . "D-2") ("owner" . "bea") ("question" . "two")))))
    (cl-letf (((symbol-function 'decide--json)
               (lambda (_args) rows)))
      (unwind-protect
          (progn
            (decide-list "inbox")
            (with-current-buffer "*decide*"
              (decide--goto-id "D-2")
              (setq rows (reverse rows))
              (revert-buffer)
              (should (string-prefix-p
                       "D-2"
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))))
            (let ((item '(("id" . "D-9")
                          ("owner" . "ada")
                          ("state" . "open")
                          ("kind" . "relay")
                          ("question" . "keep this line\nstill here"))))
              (setq rows nil)
              (cl-letf (((symbol-function 'decide--json)
                         (lambda (_args) item)))
                (decide-show-id "D-9")
                (with-current-buffer "*decide D-9*"
                  (search-backward "keep")
                  (let ((pos (point)))
                    (revert-buffer)
                    (should (= (point) pos))
                    (should (eq revert-buffer-function #'decide-item-revert))))))
            (with-current-buffer (get-buffer-create "*decide status*")
              (decide-status-mode)
              (should (eq revert-buffer-function #'decide-status-revert))
              (cl-letf (((symbol-function 'decide--json)
                         (lambda (_args)
                           '(("agents" .
                              ((("name" . "alpha")
                                ("state" . "working")
                                ("text" . "fixing")
                                ("updatedAt" . "2026-11-06T12:00:00Z"))))))))
                (decide-status-refresh)
                (goto-char (point-min))
                (search-forward "alpha")
                (let ((pos (point)))
                  (revert-buffer)
                  (should (= (point) pos))))))
        (dolist (name '("*decide*" "*decide D-9*" "*decide status*"))
          (when (get-buffer name)
            (kill-buffer name)))))))

(defun decide-test--add-elpa (file)
  "Add the directory of FILE from a user elpa tree to `load-path'."
  (dolist (root (list (expand-file-name "~/.config/emacs/elpa")
                      (expand-file-name "~/.emacs.d/elpa")))
    (when (file-directory-p root)
      (dolist (path (directory-files-recursively
                     root (concat "\\`" (regexp-quote file) "\\'")))
        (add-to-list 'load-path (file-name-directory path))))))

(defun decide-test--evil-ready-p ()
  "Non-nil when evil and goto-chg can be loaded."
  (decide-test--add-elpa "goto-chg.el")
  (decide-test--add-elpa "evil.el")
  (and (require 'goto-chg nil t)
       (require 'evil nil t)))

(ert-deftest decide-evil-normal-state-keys ()
  (if (not (decide-test--evil-ready-p))
      (progn
        (should (eq (lookup-key decide-list-mode-map (kbd "g")) 'decide-refresh))
        (should (assq 'evil after-load-alist)))
    (should (eq (evil-initial-state 'decide-list-mode) 'emacs))
    (should (eq (evil-initial-state 'decide-item-mode) 'emacs))
    (should (eq (evil-initial-state 'decide-status-mode) 'emacs))
    (dolist (mode '(decide-list-mode decide-item-mode decide-status-mode))
      (with-temp-buffer
        (funcall mode)
        (evil-local-mode 1)
        (evil-normal-state)
        (let ((inhibit-read-only t))
          (insert "one\ntwo\nthree\n"))
        (goto-char (point-min))
        (should (eq (key-binding (kbd "j")) #'next-line))
        (should (eq (key-binding (kbd "k")) #'previous-line))
        (should (eq (key-binding (kbd "q")) #'quit-window))
        (should (eq (key-binding (kbd "g r")) #'revert-buffer))
        (should (eq (key-binding (kbd "g i")) #'decide-list-inbox))
        (should (eq (key-binding (kbd "g a")) #'decide-list-answered))
        (should (eq (key-binding (kbd "g d")) #'decide-list-done))
        (should (eq (key-binding (kbd "g w")) #'decide-list-withdrawn))
        (should (eq (key-binding (kbd "g s")) #'decide-status))
        (call-interactively (key-binding (kbd "j")))
        (should (= (line-number-at-pos) 2))
        (call-interactively (key-binding (kbd "k")))
        (should (= (line-number-at-pos) 1))))
    (let ((buf (generate-new-buffer "*decide quit*")))
      (with-current-buffer buf
        (decide-list-mode))
      (pop-to-buffer buf)
      (evil-local-mode 1)
      (evil-normal-state)
      (should (eq (key-binding (kbd "q")) #'quit-window))
      (call-interactively (key-binding (kbd "q")))
      (should (buffer-live-p buf))
      (should-not (eq (current-buffer) buf))
      (should-not (get-buffer-window buf))
      (kill-buffer buf))
    (with-temp-buffer
      (decide-list-mode)
      (evil-local-mode 1)
      (should (evil-emacs-state-p))
      (should (eq (key-binding (kbd "g")) #'decide-refresh)))))

(ert-deftest decide-list-o-opens-item-and-shows-keys ()
  (let ((rows '((("id" . "D-4") ("owner" . "ada") ("question" . "pick"))))
        (item '(("id" . "D-4")
                ("owner" . "ada")
                ("state" . "open")
                ("kind" . "choice")
                ("question" . "pick")
                ("options" . ((("key" . "a") ("label" . "yes"))
                              (("key" . "b") ("label" . "no")))))))
    (cl-letf (((symbol-function 'decide--json)
               (lambda (args)
                 (if (member "show" args) item rows))))
      (unwind-protect
          (progn
            (decide-list "inbox")
            (with-current-buffer "*decide*"
              (decide--goto-id "D-4")
              (should (eq (key-binding (kbd "o")) #'decide-show))
              (should (eq (key-binding (kbd "RET")) #'decide-show))
              (call-interactively (key-binding (kbd "o"))))
            (with-current-buffer "*decide D-4*"
              (should (equal header-line-format
                             "a  b   0 f free text   q quit"))
              (should (eq (lookup-key decide-item-mode-map (kbd "o"))
                          #'decide-answer-option))
              (should (eq (lookup-key decide-item-mode-map (kbd "f"))
                          #'decide-answer-free))
              (should (eq (lookup-key decide-item-mode-map (kbd "0"))
                          #'decide-answer-free))))
        (dolist (name '("*decide*" "*decide D-4*" "*decide answer D-4*"))
          (when (get-buffer name)
            (kill-buffer name)))))))

(ert-deftest decide-evil-open-and-free-text ()
  (if (not (decide-test--evil-ready-p))
      (progn
        (should (eq (lookup-key decide-list-mode-map (kbd "o")) #'decide-show))
        (should (eq (lookup-key decide-list-mode-map (kbd "RET")) #'decide-show))
        (should (eq (lookup-key decide-item-mode-map (kbd "o"))
                    #'decide-answer-option))
        (should (eq (lookup-key decide-item-mode-map (kbd "f"))
                    #'decide-answer-free))
        (should (eq (lookup-key decide-item-mode-map (kbd "0"))
                    #'decide-answer-free))
        (should (assq 'evil after-load-alist)))
    (with-temp-buffer
      (decide-list-mode)
      (evil-local-mode 1)
      (evil-normal-state)
      (should (eq (key-binding (kbd "o")) #'decide-show))
      (should (eq (key-binding (kbd "RET")) #'decide-show)))
    (with-temp-buffer
      (decide-item-mode)
      (evil-local-mode 1)
      (evil-normal-state)
      (should (eq (key-binding (kbd "o")) #'decide-answer-option))
      (should (eq (key-binding (kbd "f")) #'decide-answer-free))
      (should (eq (key-binding (kbd "0")) #'decide-answer-free)))
    (with-temp-buffer
      (setq decide--item-id "D-4")
      (decide-answer-free))
    (unwind-protect
        (with-current-buffer "*decide answer D-4*"
          (should (eq (key-binding (kbd "C-c C-c")) #'decide-answer-send))
          (should (eq (key-binding (kbd "C-c C-k")) #'decide-answer-cancel))
          (should (evil-insert-state-p)))
      (when (get-buffer "*decide answer D-4*")
        (kill-buffer "*decide answer D-4*")))
    (decide-test--add-elpa "hybrid-mode.el")
    (when (require 'hybrid-mode nil t)
      (with-temp-buffer
        (setq decide--item-id "D-4")
        (let ((evil-default-state 'hybrid))
          (decide-answer-free)))
      (unwind-protect
          (with-current-buffer "*decide answer D-4*"
            (should (evil-hybrid-state-p)))
        (when (get-buffer "*decide answer D-4*")
          (kill-buffer "*decide answer D-4*"))))))

(provide 'decide-test)

;;; decide-test.el ends here
