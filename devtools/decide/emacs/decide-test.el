;;; decide-test.el --- Tests for decide.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch tests for argument lists.  They do not run the decide program.

;;; Code:

(require 'ert)
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

(provide 'decide-test)

;;; decide-test.el ends here
