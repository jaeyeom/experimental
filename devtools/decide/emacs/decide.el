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

(defun decide--reply-args (id text)
  "Argument list that appends TEXT to thread ID and notifies its owner."
  (list "send" "--id" id "--json" text))

(defun decide--done-args (id outcome)
  "Argument list that marks ID done with OUTCOME."
  (list "done" "--json" id outcome))

(defun decide--submit-args (action id text)
  "Argument list for ACTION, or nil when nothing should be sent.
ACTION is `answer', `send', `reply', or `cancel'."
  (cond ((eq action 'answer) (decide--answer-args id text))
        ((eq action 'send) (decide--send-args id text))
        ((eq action 'reply) (decide--reply-args id text))
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

(defun decide--goto-id (id)
  "Move point to the list line for ID when that item is present."
  (when (and (stringp id) (not (string-empty-p id)))
    (let ((items decide--items)
          (n 0)
          found)
      (while (and items (not found))
        (if (equal (decide--field (car items) "id") id)
            (setq found t)
          (setq n (1+ n)
                items (cdr items))))
      (when found
        (goto-char (point-min))
        (forward-line (1+ n))))))

(defun decide-list-revert (&rest _ignore)
  "Reload this list and stay on the same item when it remains."
  (let ((item (decide--item-at-point))
        (id nil))
    (when item
      (setq id (decide--field item "id")))
    (decide-list decide--folder)
    (when (get-buffer "*decide*")
      (with-current-buffer "*decide*"
        (decide--goto-id id)))))

(defun decide-list-inbox ()
  "Show the inbox folder."
  (interactive)
  (decide-list "inbox"))

(defun decide-list-answered ()
  "Show the answered folder."
  (interactive)
  (decide-list "answered"))

(defun decide-list-done ()
  "Show the done folder."
  (interactive)
  (decide-list "done"))

(defun decide-list-withdrawn ()
  "Show the withdrawn folder."
  (interactive)
  (decide-list "withdrawn"))

(defun decide-item-revert (&rest _ignore)
  "Reload this item and keep point when the item remains."
  (when decide--item-id
    (let ((id decide--item-id)
          (pos (point)))
      (decide-show-id id)
      (let ((buf (get-buffer (format "*decide %s*" id))))
        (when buf
          (with-current-buffer buf
            (goto-char (min pos (point-max)))))))))

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
      (setq-local header-line-format (decide--item-keys-line item))
      (pop-to-buffer (current-buffer)))))

(defun decide--thread-text (item)
  "Thread text of ITEM, oldest first.
An item with no messages shows its question."
  (let ((messages (alist-get "messages" item nil nil #'equal)))
    (if (null messages)
        (decide--field item "question")
      (mapconcat
       (lambda (msg)
         (let ((author (decide--field msg "author"))
               (text (decide--field msg "text")))
           (if (string-empty-p author)
               text
             (concat author "\n" text))))
       messages
       "\n\n"))))

(defun decide--item-text (item)
  "Plain text for an item buffer.
The thread comes first.  The reply line and the numbered options follow."
  (let ((parts (list
                (format "id: %s" (decide--field item "id"))
                (format "owner: %s" (decide--field item "owner"))
                (format "state: %s" (decide--field item "state"))
                (format "kind: %s" (decide--field item "kind"))
                ""
                (decide--thread-text item)
                ""
                "reply:"))
        (n 1))
    (dolist (opt (alist-get "options" item nil nil #'equal))
      (setq parts (append parts
                          (list (format "%d %s %s"
                                        n
                                        (decide--field opt "key")
                                        (decide--field opt "label")))))
      (setq n (1+ n)))
    (setq parts (append parts (list "0 free text")))
    (dolist (link (alist-get "links" item nil nil #'equal))
      (setq parts (append parts (list link))))
    (mapconcat #'identity parts "\n")))

(defun decide--option-keys (item)
  "Option keys on ITEM."
  (mapcar (lambda (opt) (decide--field opt "key"))
          (or (alist-get "options" item nil nil #'equal) nil)))

(defun decide--item-keys-line (item)
  "Header of the keys that answer, reply, archive, or quit ITEM."
  (let ((keys (decide--option-keys item)))
    (concat (mapconcat #'identity keys "  ")
            (if keys "   " "")
            "0 f free text   r reply   y archive   q quit")))

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

(defun decide-answer-number ()
  "Answer with the option numbered by this key."
  (interactive)
  (let* ((n (if (numberp last-command-event)
                (- last-command-event ?0)
              0))
         (item (car decide--items))
         (opt (nth (1- n) (alist-get "options" item nil nil #'equal))))
    (unless (and decide--item-id opt)
      (user-error "No option %s" n))
    (decide--command (decide--answer-args decide--item-id (decide--field opt "key")))
    (when (get-buffer "*decide*")
      (decide-refresh))
    (message "Answered %s" decide--item-id)))

(defun decide-archive ()
  "Mark this notification done.
A thread that still has options needs an answer, so it stays open."
  (interactive)
  (let ((item (car decide--items)))
    (unless (and item decide--item-id)
      (user-error "No item"))
    (if (alist-get "options" item nil nil #'equal)
        (message "needs an answer")
      (decide--command (decide--done-args decide--item-id "archived"))
      (message "done %s" decide--item-id)
      (when (get-buffer "*decide*")
        (decide-refresh)))))

(declare-function evil-insert-state "evil" (&optional arg))
(declare-function evil-hybrid-state "evil" (&optional arg))
(defvar evil-default-state)

(defun decide--prepare-answer-state ()
  "Leave the answer buffer ready to type.
Use hybrid state when evil's default state is hybrid."
  (cond
   ((not (fboundp 'evil-insert-state)) nil)
   ((and (boundp 'evil-default-state)
         (eq evil-default-state 'hybrid)
         (fboundp 'evil-hybrid-state))
    (evil-hybrid-state))
   (t (evil-insert-state))))

(defun decide-answer-free ()
  "Edit a free-text answer in an ordinary buffer."
  (interactive)
  (unless decide--item-id
    (user-error "No item"))
  (let ((id decide--item-id))
    (with-current-buffer (get-buffer-create (format "*decide answer %s*" id))
      (decide-answer-mode)
      (setq decide--item-id id)
      (pop-to-buffer (current-buffer))
      (decide--prepare-answer-state))))

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

(defun decide-reply ()
  "Edit a reply in an ordinary buffer.
Sending it appends a human message and notifies the owner pane."
  (interactive)
  (unless decide--item-id
    (user-error "No item"))
  (let ((id decide--item-id))
    (with-current-buffer (get-buffer-create (format "*decide reply %s*" id))
      (decide-reply-mode)
      (setq decide--item-id id)
      (pop-to-buffer (current-buffer))
      (decide--prepare-answer-state))))

(defun decide-reply-send ()
  "Send this buffer as a reply on the open thread."
  (interactive)
  (let ((args (decide--submit-args 'reply decide--item-id (buffer-string)))
        (id decide--item-id))
    (when args
      (decide--command args))
    (kill-buffer (current-buffer))
    (when id
      (let ((buf (get-buffer (format "*decide %s*" id))))
        (when buf
          (with-current-buffer buf
            (decide-item-revert))))
      (message "Replied %s" id))))

(defun decide-reply-cancel ()
  "Close this reply buffer and send nothing."
  (interactive)
  (kill-buffer (current-buffer)))

(define-derived-mode decide-list-mode special-mode "Decide"
  "List of decision items."
  (setq-local buffer-read-only t)
  (setq-local revert-buffer-function #'decide-list-revert))

(define-key decide-list-mode-map (kbd "g") #'decide-refresh)
(define-key decide-list-mode-map (kbd "o") #'decide-show)
(define-key decide-list-mode-map (kbd "RET") #'decide-show)
(define-key decide-list-mode-map (kbd "c") #'decide-compose)
(define-key decide-list-mode-map (kbd "s") #'decide-status)
(define-key decide-list-mode-map (kbd "i") #'decide-list-inbox)
(define-key decide-list-mode-map (kbd "a") #'decide-list-answered)
(define-key decide-list-mode-map (kbd "d") #'decide-list-done)
(define-key decide-list-mode-map (kbd "w") #'decide-list-withdrawn)
(define-key decide-list-mode-map (kbd "q") #'quit-window)

(define-derived-mode decide-item-mode special-mode "Decide-Item"
  "One decision item.
Long lines wrap, so a draft in the thread can be scrolled."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local revert-buffer-function #'decide-item-revert)
  (visual-line-mode 1))

(define-key decide-item-mode-map (kbd "o") #'decide-answer-option)
(define-key decide-item-mode-map (kbd "f") #'decide-answer-free)
(define-key decide-item-mode-map (kbd "0") #'decide-answer-free)
(define-key decide-item-mode-map (kbd "r") #'decide-reply)
(define-key decide-item-mode-map (kbd "y") #'decide-archive)
(define-key decide-item-mode-map (kbd "q") #'quit-window)
(dotimes (i 9)
  (define-key decide-item-mode-map (kbd (number-to-string (1+ i)))
              #'decide-answer-number))

(define-derived-mode decide-answer-mode text-mode "Decide-Answer"
  "Free-text answer.  `C-c C-c' sends it and `C-c C-k' cancels."
  (setq-local header-line-format "C-c C-c send   C-c C-k cancel"))

(define-key decide-answer-mode-map (kbd "C-c C-c") #'decide-answer-send)
(define-key decide-answer-mode-map (kbd "C-c C-k") #'decide-answer-cancel)

(define-derived-mode decide-reply-mode text-mode "Decide-Reply"
  "Reply on the open thread.  `C-c C-c' sends it and `C-c C-k' cancels."
  (setq-local header-line-format "C-c C-c send   C-c C-k cancel"))

(define-key decide-reply-mode-map (kbd "C-c C-c") #'decide-reply-send)
(define-key decide-reply-mode-map (kbd "C-c C-k") #'decide-reply-cancel)

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

(defun decide--status-width ()
  "Column count for the status tables."
  (if (get-buffer-window (current-buffer))
      (window-body-width)
    80))

(defun decide--fit-cell (text width)
  "Fit TEXT into WIDTH columns."
  (let ((text (or text "")))
    (if (<= (string-width text) width)
        text
      (truncate-string-to-width text width nil nil "…"))))

(defun decide--format-cells (cells widths)
  "Pad CELLS to WIDTHS and join them with one space."
  (let ((parts nil)
        (i 0))
    (while (< i (length widths))
      (let* ((width (nth i widths))
             (fitted (decide--fit-cell (nth i cells) width))
             (pad (max 0 (- width (string-width fitted)))))
        (push (concat fitted (make-string pad ?\s)) parts))
      (setq i (1+ i)))
    (string-join (nreverse parts) " ")))

(defun decide--column-widths (headers rows total)
  "Return column widths for HEADERS and ROWS that fit in TOTAL."
  (let* ((n (length headers))
         (widths (mapcar #'string-width headers))
         (i 0)
         (fixed 0)
         (gaps (1- n))
         (remain 0))
    (dolist (row rows)
      (setq i 0)
      (while (< i (1- n))
        (let ((w (string-width (or (nth i row) ""))))
          (when (> w (nth i widths))
            (setcar (nthcdr i widths) w)))
        (setq i (1+ i))))
    (when (<= total 0)
      (setq total 80))
    (setq i 0)
    (while (< i (1- n))
      (setq fixed (+ fixed (nth i widths)))
      (setq i (1+ i)))
    (setq remain (- total fixed gaps))
    (when (< remain 1)
      (let ((overflow (- (+ fixed gaps 1) total)))
        (setq i (- n 2))
        (while (and (>= i 0) (> overflow 0))
          (let ((spare (1- (nth i widths))))
            (when (> spare 0)
              (let ((cut (min spare overflow)))
                (setcar (nthcdr i widths) (- (nth i widths) cut))
                (setq overflow (- overflow cut)))))
          (setq i (1- i))))
      (setq fixed 0
            i 0)
      (while (< i (1- n))
        (setq fixed (+ fixed (nth i widths)))
        (setq i (1+ i)))
      (setq remain (- total fixed gaps))
      (when (< remain 1)
        (setq remain 1)))
    (setcar (nthcdr (1- n) widths) remain)
    widths))

(defun decide--render-status-table (headers rows stale-flags)
  "Insert HEADERS and ROWS. STALE-FLAGS dims each data row."
  (let* ((widths (decide--column-widths headers rows (decide--status-width)))
         (i 0))
    (decide--insert-status-row (decide--format-cells headers widths) nil)
    (dolist (row rows)
      (decide--insert-status-row
       (decide--format-cells row widths)
       (nth i stale-flags))
      (setq i (1+ i)))))

(defun decide--status-rows (entries fields now)
  "Build table rows and stale flags for ENTRIES.
FIELDS are JSON keys. The third column is the age of updatedAt."
  (let ((rows nil)
        (stale nil))
    (dolist (entry entries)
      (let ((updated (decide--field entry "updatedAt")))
        (setq rows (append rows (list (list (decide--field entry (nth 0 fields))
                                            (decide--field entry (nth 1 fields))
                                            (decide--age updated now)
                                            (decide--field entry (nth 2 fields))))))
        (setq stale (append stale (list (decide--stale-p updated now))))))
    (cons rows stale)))

(defun decide-status-render (board now)
  "Insert BOARD, a status JSON alist, as of NOW."
  (let* ((agents (decide--status-rows
                  (alist-get "agents" board nil nil #'equal)
                  '("name" "state" "text") now))
         (resources (decide--status-rows
                     (alist-get "resources" board nil nil #'equal)
                     '("name" "availability" "note") now)))
    (when (car agents)
      (decide--render-status-table '("agent" "state" "age" "text")
                                   (car agents) (cdr agents)))
    (when (and (car agents) (car resources))
      (insert "\n"))
    (when (car resources)
      (decide--render-status-table '("resource" "free/busy" "age" "note")
                                   (car resources) (cdr resources)))))

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

(defun decide-status-revert (&rest _ignore)
  "Reload the status board and keep point."
  (let ((pos (point)))
    (decide-status-refresh)
    (goto-char (min pos (point-max)))))

(define-derived-mode decide-status-mode special-mode "Decide-Status"
  "Agent and resource status."
  (setq-local revert-buffer-function #'decide-status-revert)
  (add-hook 'kill-buffer-hook #'decide--status-stop-if-ours nil t))

(define-key decide-status-mode-map (kbd "g") #'decide-status-refresh)
(define-key decide-status-mode-map (kbd "q") #'quit-window)

(declare-function evil-set-initial-state "evil" (mode state))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))

(defun decide--evil-normal-keys (map)
  "Bind normal-state motion and folder chords on MAP."
  (apply #'evil-define-key* 'normal map
         (list (kbd "j") #'next-line
               (kbd "k") #'previous-line
               (kbd "q") #'quit-window
               (kbd "g r") #'revert-buffer
               (kbd "g i") #'decide-list-inbox
               (kbd "g a") #'decide-list-answered
               (kbd "g d") #'decide-list-done
               (kbd "g w") #'decide-list-withdrawn
               (kbd "g s") #'decide-status)))

(defun decide--evil-bind (map bindings)
  "Bind normal-state BINDINGS on MAP."
  (apply #'evil-define-key* 'normal map bindings))

(defun decide--evil-setup ()
  "Give the decide modes an emacs initial state, then bind normal-state keys.
The mode maps win in that initial state.  Normal state still moves by
line, quits, reverts, and opens a folder.  The list opens an item, and
the item answers with an option or with free text.  r replies and
y archives a notification.  Both are bound in normal state."
  (dolist (mode '(decide-list-mode decide-item-mode decide-status-mode))
    (evil-set-initial-state mode 'emacs))
  (dolist (map (list decide-list-mode-map decide-item-mode-map decide-status-mode-map))
    (decide--evil-normal-keys map))
  (decide--evil-bind
   decide-list-mode-map
   (list (kbd "o") #'decide-show
         (kbd "RET") #'decide-show))
  (decide--evil-bind
   decide-item-mode-map
   (list (kbd "o") #'decide-answer-option
         (kbd "f") #'decide-answer-free
         (kbd "0") #'decide-answer-free
         (kbd "r") #'decide-reply
         (kbd "y") #'decide-archive)))

(with-eval-after-load 'evil
  (decide--evil-setup))

(provide 'decide)

;;; decide.el ends here
