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
(require 'time-date)

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
(defvar-local decide--recipient nil)
(defvar decide--status-timer nil)

(defconst decide-stale-seconds (* 5 60)
  "A status row older than this many seconds is dimmed.")

(defun decide--answer-args (id text)
  "Argument list that answers ID with TEXT."
  (list "answer" "--json" id text))

(defun decide--send-args (name text)
  "Argument list that sends TEXT to recipient NAME."
  (list "send" "--to" name "--json" text))

(defun decide--submit-args (action id text)
  "Argument list for ACTION, or nil when nothing should be sent.
ACTION is `answer', `send', or `cancel'."
  (cond ((eq action 'answer) (decide--answer-args id text))
        ((eq action 'send) (decide--send-args id text))
        (t nil)))

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
(define-key decide-list-mode-map (kbd "c") #'decide-compose)
(define-key decide-list-mode-map (kbd "s") #'decide-status)
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

(defun decide-compose ()
  "Compose a note to a recipient from `decide targets --json'."
  (interactive)
  (let* ((targets (decide--json (list "targets" "--json")))
         (names (mapcar (lambda (tg) (decide--field tg "name")) targets))
         (name (completing-read "Recipient: " names nil t)))
    (with-current-buffer (get-buffer-create "*decide compose*")
      (decide-compose-mode)
      (setq decide--recipient name)
      (pop-to-buffer (current-buffer)))))

(defun decide-compose-send ()
  "Send this buffer to the chosen recipient."
  (interactive)
  (let ((args (decide--submit-args 'send decide--recipient (buffer-string)))
        (name decide--recipient))
    (when args
      (decide--command args))
    (kill-buffer (current-buffer))
    (message "Sent to %s" name)))

(defun decide-compose-cancel ()
  "Close this compose buffer and send nothing."
  (interactive)
  (let ((args (decide--submit-args 'cancel decide--recipient (buffer-string))))
    (when args
      (decide--command args))
    (kill-buffer (current-buffer))))

(define-derived-mode decide-compose-mode text-mode "Decide-Compose"
  "Note to one recipient.  `C-c C-c' sends it and `C-c C-k' cancels."
  (setq-local header-line-format "C-c C-c send   C-c C-k cancel"))

(define-key decide-compose-mode-map (kbd "C-c C-c") #'decide-compose-send)
(define-key decide-compose-mode-map (kbd "C-c C-k") #'decide-compose-cancel)

(defun decide--stale-p (updated now)
  "Non-nil when UPDATED is more than `decide-stale-seconds' before NOW.
UPDATED is an RFC3339 string.  NOW is an Emacs time."
  (and (stringp updated)
       (not (string-empty-p updated))
       (> (float-time (time-subtract now (date-to-time updated)))
          decide-stale-seconds)))

(defun decide--age (updated now)
  "Short age of UPDATED at NOW."
  (let ((secs (floor (float-time (time-subtract now (date-to-time updated))))))
    (cond ((< secs 60) "now")
          ((< secs 3600) (format "%dm" (/ secs 60)))
          ((< secs 86400) (format "%dh" (/ secs 3600)))
          (t (format "%dd" (/ secs 86400))))))

(defun decide--insert-status-row (text stale)
  "Insert TEXT, dimmed when STALE is non-nil."
  (insert (propertize text 'face (and stale 'shadow)) "\n"))

(defun decide-status-render (board now)
  "Insert BOARD, a status JSON alist, as of NOW."
  (dolist (agent (alist-get "agents" board nil nil #'equal))
    (let ((updated (decide--field agent "updatedAt")))
      (decide--insert-status-row
       (format "%s %s %s %s"
               (decide--field agent "name")
               (decide--field agent "state")
               (decide--age updated now)
               (decide--field agent "text"))
       (decide--stale-p updated now))))
  (dolist (res (alist-get "resources" board nil nil #'equal))
    (let* ((updated (decide--field res "updatedAt"))
           (note (decide--field res "note"))
           (line (if (string-empty-p note)
                     (format "%s: %s"
                             (decide--field res "name")
                             (decide--field res "availability"))
                   (format "%s: %s (%s)"
                           (decide--field res "name")
                           (decide--field res "availability")
                           note))))
      (decide--insert-status-row line (decide--stale-p updated now)))))

(defun decide-status-refresh ()
  "Replace the status buffer from `decide status list --json'."
  (interactive)
  (let ((board (decide--json (list "status" "list" "--json")))
        (inhibit-read-only t))
    (erase-buffer)
    (decide-status-render board (current-time))))

(defun decide--status-stop-if-ours ()
  "Stop the status timer when this status buffer goes away."
  (when (and decide--status-timer
             (eq (current-buffer) (get-buffer "*decide status*")))
    (cancel-timer decide--status-timer)
    (setq decide--status-timer nil)))

(defun decide--status-ensure-timer ()
  "Refresh the visible status buffer every two seconds."
  (unless decide--status-timer
    (setq decide--status-timer
          (run-with-timer
           2 2
           (lambda ()
             (let ((buf (get-buffer "*decide status*")))
               (cond
                ((or (null buf) (not (buffer-live-p buf)))
                 (when decide--status-timer
                   (cancel-timer decide--status-timer)
                   (setq decide--status-timer nil)))
                ((get-buffer-window buf t)
                 (with-current-buffer buf
                   (decide-status-refresh))))))))))

(defun decide-status ()
  "Show the status board and keep it refreshing while visible."
  (interactive)
  (let ((buf (get-buffer-create "*decide status*")))
    (with-current-buffer buf
      (decide-status-mode)
      (decide-status-refresh))
    (pop-to-buffer buf)
    (decide--status-ensure-timer)))

(define-derived-mode decide-status-mode special-mode "Decide-Status"
  "Agent and resource status."
  (add-hook 'kill-buffer-hook #'decide--status-stop-if-ours nil t))

(define-key decide-status-mode-map (kbd "g") #'decide-status-refresh)
(define-key decide-status-mode-map (kbd "q") #'quit-window)

(provide 'decide)

;;; decide.el ends here
