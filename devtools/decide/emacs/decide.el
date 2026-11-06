;;; decide.el --- Decision inbox from the decide CLI -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools
;; URL: https://github.com/jaeyeom/experimental

;;; Commentary:

;; List, open, and answer decision items by calling the decide CLI.
;; The program is started with an argument list.  The answer text is
;; one argument, not a shell string.

;;; Code:

(require 'json)
(require 'subr-x)

(defgroup decide nil
  "Decision inbox."
  :group 'external
  :prefix "decide-")

(defcustom decide-program "decide"
  "Name or path of the decide executable."
  :type 'string
  :group 'decide)

(defconst decide-folders
  '(("inbox" . "open")
    ("answered" . "answered")
    ("done" . "done")
    ("withdrawn" . "withdrawn"))
  "Display name and `decide list --state' value.")

(defvar decide--folder "inbox")
(defvar-local decide--items nil)
(defvar-local decide--item-id nil)

(defun decide--answer-args (id text)
  "Argument list that answers ID with TEXT."
  (list "answer" "--json" id text))

(defun decide--submit-args (action id text)
  "Argument list for ACTION, or nil when nothing should be sent.
ACTION is `answer' or `cancel'."
  (and (eq action 'answer) (decide--answer-args id text)))

(defun decide--command (args)
  "Run `decide-program' with ARGS.  Return stdout.
Signal an error when the program exits non-zero."
  (unless args
    (error "Decide command is empty"))
  (with-temp-buffer
    (let ((status (apply #'call-process decide-program nil (current-buffer) nil args)))
      (unless (eq status 0)
        (error "Decide failed: %s" (string-trim (buffer-string))))
      (buffer-string))))

(defun decide--read-json (raw)
  "Parse RAW decide JSON into alists and lists."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string)
        (json-false nil))
    (json-read-from-string raw)))

(defun decide--json (args)
  "Run ARGS and parse the JSON stdout."
  (decide--read-json (decide--command args)))

(defun decide--field (item key)
  "Return ITEM's KEY, or an empty string."
  (or (alist-get key item nil nil #'equal) ""))

(defun decide--first-line (text)
  "First line of TEXT."
  (car (split-string (or text "") "\n")))

(defun decide--list-line (item)
  "One list-buffer line for ITEM."
  (format "%s  %s  %s"
          (decide--field item "id")
          (decide--field item "owner")
          (decide--first-line (decide--field item "question"))))

(defun decide-list (&optional folder)
  "Show the FOLDER list.  FOLDER is inbox, answered, done, or withdrawn."
  (interactive)
  (let* ((folder (or folder decide--folder "inbox"))
         (state (cdr (assoc folder decide-folders)))
         (items (decide--json (list "list" "--json" "--state" (or state "open")))))
    (setq decide--folder folder)
    (let ((buf (get-buffer-create "*decide*")))
      (with-current-buffer buf
        (decide-list-mode)
        (setq decide--items items)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Decide %s\n" folder))
          (if (null items)
              (insert "(empty)\n")
            (dolist (item items)
              (insert (decide--list-line item) "\n")))))
      (pop-to-buffer buf))))

(defun decide-refresh ()
  "Reload the current list."
  (interactive)
  (decide-list decide--folder))

(defun decide--item-at-point ()
  "Item on the current list line, or nil."
  (let ((n (- (line-number-at-pos) 2)))
    (and decide--items (>= n 0) (nth n decide--items))))

(defun decide-show ()
  "Open the item on the current list line."
  (interactive)
  (let ((item (decide--item-at-point)))
    (unless item
      (user-error "No item on this line"))
    (decide-show-id (decide--field item "id"))))

(defun decide-show-id (id)
  "Open item ID from `decide show --json'."
  (let ((item (decide--json (list "show" "--json" id))))
    (with-current-buffer (get-buffer-create (format "*decide %s*" id))
      (decide-item-mode)
      (setq decide--item-id id)
      (setq decide--items (list item))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (decide--item-text item)))
      (pop-to-buffer (current-buffer)))))

(defun decide--item-text (item)
  "Plain text for an item buffer."
  (let ((parts (list
                (format "id: %s" (decide--field item "id"))
                (format "owner: %s" (decide--field item "owner"))
                (format "state: %s" (decide--field item "state"))
                (format "kind: %s" (decide--field item "kind"))
                ""
                (decide--field item "question")
                "")))
    (dolist (opt (alist-get "options" item nil nil #'equal))
      (setq parts (append parts (list (format "%s  %s"
                                             (decide--field opt "key")
                                             (decide--field opt "label"))))))
    (dolist (link (alist-get "links" item nil nil #'equal))
      (setq parts (append parts (list link))))
    (mapconcat #'identity parts "\n")))

(defun decide--option-keys (item)
  "Option keys on ITEM."
  (mapcar (lambda (opt) (decide--field opt "key"))
          (or (alist-get "options" item nil nil #'equal) nil)))

(defun decide-answer-option ()
  "Answer the open item with one of its option keys."
  (interactive)
  (let* ((item (car decide--items))
         (keys (decide--option-keys item))
         (key (completing-read "Option key: " keys nil t))
         (args (decide--submit-args 'answer decide--item-id key)))
    (when args
      (decide--command args))
    (when (get-buffer "*decide*")
      (decide-refresh))))

(defun decide-answer-free ()
  "Edit a free-text answer in an ordinary buffer."
  (interactive)
  (unless decide--item-id
    (user-error "No item"))
  (let ((id decide--item-id))
    (with-current-buffer (get-buffer-create (format "*decide answer %s*" id))
      (decide-answer-mode)
      (setq decide--item-id id)
      (pop-to-buffer (current-buffer)))))

(defun decide-answer-send ()
  "Send this buffer as the answer."
  (interactive)
  (let ((args (decide--submit-args 'answer decide--item-id (buffer-string)))
        (id decide--item-id))
    (when args
      (decide--command args))
    (kill-buffer (current-buffer))
    (when (get-buffer "*decide*")
      (with-current-buffer "*decide*"
        (decide-refresh)))
    (when id
      (message "Answered %s" id))))

(defun decide-answer-cancel ()
  "Close this answer buffer and send nothing."
  (interactive)
  (let ((args (decide--submit-args 'cancel decide--item-id (buffer-string))))
    (when args
      (decide--command args))
    (kill-buffer (current-buffer))))

(define-derived-mode decide-list-mode special-mode "Decide"
  "List of decision items."
  (setq-local buffer-read-only t))

(define-key decide-list-mode-map (kbd "g") #'decide-refresh)
(define-key decide-list-mode-map (kbd "RET") #'decide-show)
(define-key decide-list-mode-map (kbd "i") (lambda () (interactive) (decide-list "inbox")))
(define-key decide-list-mode-map (kbd "a") (lambda () (interactive) (decide-list "answered")))
(define-key decide-list-mode-map (kbd "d") (lambda () (interactive) (decide-list "done")))
(define-key decide-list-mode-map (kbd "w") (lambda () (interactive) (decide-list "withdrawn")))
(define-key decide-list-mode-map (kbd "q") #'quit-window)

(define-derived-mode decide-item-mode special-mode "Decide-Item"
  "One decision item."
  (setq-local buffer-read-only t))

(define-key decide-item-mode-map (kbd "o") #'decide-answer-option)
(define-key decide-item-mode-map (kbd "f") #'decide-answer-free)
(define-key decide-item-mode-map (kbd "q") #'quit-window)

(define-derived-mode decide-answer-mode text-mode "Decide-Answer"
  "Free-text answer.  `C-c C-c' sends it and `C-c C-k' cancels."
  (setq-local header-line-format "C-c C-c send   C-c C-k cancel"))

(define-key decide-answer-mode-map (kbd "C-c C-c") #'decide-answer-send)
(define-key decide-answer-mode-map (kbd "C-c C-k") #'decide-answer-cancel)

(provide 'decide)

;;; decide.el ends here
