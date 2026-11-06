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

(provide 'decide-test)

;;; decide-test.el ends here
