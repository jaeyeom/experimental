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
(require 'seq)
(require 'subr-x)
(require 'time-date)

(defgroup decide nil
  "Decision inbox."
  :group 'external
  :prefix "decide-")

(defface decide-requester
  '((t :inherit font-lock-type-face))
  "Face for the requester in the decide list."
  :group 'decide)

(defface decide-time
  '((t :inherit shadow))
  "Face for the relative time in the decide list."
  :group 'decide)

(defface decide-badge
  '((t :inherit font-lock-keyword-face))
  "Face for the kind or state badge in the decide list."
  :group 'decide)

(defface decide-human
  '((t :inherit font-lock-function-name-face))
  "Face for a human author in a thread message."
  :group 'decide)

(defface decide-agent
  '((t :inherit font-lock-variable-name-face))
  "Face for an agent author in a thread message."
  :group 'decide)

(defface decide-current
  '((t :inverse-video t))
  "Face for the current message header."
  :group 'decide)

(defcustom decide-program "decide"
  "Name or path of the decide executable."
  :type 'string
  :group 'decide)

(defcustom decide-herdr-program "herdr"
  "Name or path of the herdr executable that focuses an owner's tab."
  :type 'string
  :group 'decide)

(defcustom decide-display-buffer-action '(display-buffer-same-window)
  "Display action for the list, an open item, and the status board.
The default reuses the selected window, and `q' returns to the previous
buffer in that window."
  :type 'sexp
  :group 'decide)

(defun decide--display (buffer)
  "Show BUFFER with `decide-display-buffer-action'."
  (pop-to-buffer buffer decide-display-buffer-action))

(defconst decide-folders
  '(("inbox" . "open")
    ("answered" . "answered")
    ("done" . "done")
    ("withdrawn" . "withdrawn"))
  "Display name and `decide list --state' value.")

(defvar decide--folder "inbox")
(defvar decide--filter ""
  "List filter query.  An empty string shows every row.")
(defvar-local decide--items nil)
(defvar-local decide--item-id nil)
(defvar-local decide--message nil
  "Index of the current thread message, or nil outside an item.")
(defvar-local decide--recipient nil)
(defvar decide--status-timer nil)

(defconst decide-stale-seconds (* 5 60)
  "A status row older than this many seconds is dimmed.")

(defun decide--answer-args (id text)
  "Argument list that answers ID with TEXT, recorded as from the UI."
  (list "answer" "--json" "--via" "ui" id text))

(defun decide--send-args (name text)
  "Argument list that sends TEXT to recipient NAME, recorded as from the UI."
  (list "send" "--to" name "--via" "ui" "--json" text))

(defun decide--reply-args (id text)
  "Argument list that appends TEXT to thread ID and notifies its owner.
An answer it gives to an open question is recorded as from the UI."
  (list "send" "--id" id "--via" "ui" "--json" text))

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

(defun decide--list-age (created)
  "Relative age of CREATED, an RFC3339 time, or an empty string."
  (if (or (null created) (string-empty-p created))
      ""
    (decide--age created (current-time))))

(defun decide--badge (item)
  "ITEM's kind, or its state when the kind is empty."
  (let ((kind (decide--field item "kind")))
    (if (string-empty-p kind)
        (decide--field item "state")
      kind)))

(defun decide--list-line (item)
  "One list-buffer line for ITEM.
The requester, relative time, and kind or state badge use their faces."
  (format "%s  %s  %s  %s  %s"
          (decide--field item "id")
          (propertize (decide--field item "owner") 'face 'decide-requester)
          (propertize (decide--list-age (decide--field item "createdAt"))
                      'face 'decide-time)
          (propertize (decide--badge item) 'face 'decide-badge)
          (decide--first-line (decide--field item "question"))))

(defun decide--item-matches-p (item query)
  "Non-nil when ITEM's id, owner, question, or a message text has QUERY.
Case is ignored."
  (let ((case-fold-search t)
        (re (regexp-quote query)))
    (or (seq-some (lambda (key) (string-match-p re (decide--field item key)))
                  '("id" "owner" "question"))
        (seq-some (lambda (msg) (string-match-p re (decide--field msg "text")))
                  (alist-get "messages" item nil nil #'equal)))))

(defun decide--filter-items (items query)
  "ITEMS that match QUERY.  An empty QUERY keeps every item."
  (if (string-empty-p (or query ""))
      items
    (seq-filter (lambda (item) (decide--item-matches-p item query)) items)))

(defun decide--list-title (folder shown total)
  "Title line for FOLDER, naming the filter with SHOWN of TOTAL rows."
  (if (string-empty-p (or decide--filter ""))
      (format "Decide %s" folder)
    (format "Decide %s  /%s  (%d of %d)" folder decide--filter shown total)))

(defun decide-list (&optional folder)
  "Show the FOLDER list.  FOLDER is inbox, answered, done, or withdrawn.
The rows follow `decide--filter'."
  (interactive)
  (let* ((folder (or folder decide--folder "inbox"))
         (state (cdr (assoc folder decide-folders)))
         (all (decide--json (list "list" "--json" "--state" (or state "open"))))
         (items (decide--filter-items all decide--filter)))
    (setq decide--folder folder)
    (let ((buf (get-buffer-create "*decide*")))
      (with-current-buffer buf
        (decide-list-mode)
        (setq decide--items items)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (decide--list-title folder (length items) (length all)) "\n")
          (if (null items)
              (insert (if all "no matches\n" "(empty)\n"))
            (dolist (item items)
              (insert (decide--list-line item) "\n")))))
      (decide--display buf))))

(defun decide-list-filter (query)
  "Show only the rows that match QUERY.  An empty QUERY clears the filter.
Quitting the prompt keeps the current filter."
  (interactive (list (read-string "Filter (empty clears): ")))
  (setq decide--filter query)
  (decide-list decide--folder))

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
      (setq decide--message 0)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (decide--item-text item)))
      (setq-local header-line-format (decide--item-keys-line item))
      (decide--display (current-buffer)))))

(defun decide--option-keys-text (msg)
  "Option keys of MSG, or an empty string."
  (mapconcat (lambda (opt) (decide--field opt "key"))
             (alist-get "options" msg nil nil #'equal)
             " "))

(defun decide--message-header (msg latest &optional current)
  "One header line for MSG.  LATEST marks the newest message.
The author uses `decide-human' or `decide-agent', and the time uses
`decide-time'.  Option keys are dim, not badges, because answers use
the option numbers.  CURRENT marks the selected message.  A message
with `by' set is shown as that sender.  Otherwise it shows `author'."
  (let* ((author (decide--field msg "author"))
         (by (decide--field msg "by"))
         (face (if (equal author "human") 'decide-human 'decide-agent))
         (age (decide--list-age (decide--field msg "time")))
         (keys (decide--option-keys-text msg))
         (parts nil)
         (line nil))
    (unless (string-empty-p by)
      (setq author by))
    (unless (string-empty-p author)
      (setq parts (append parts (list (propertize author 'face face)))))
    (unless (string-empty-p age)
      (setq parts (append parts (list (propertize age 'face 'decide-time)))))
    (unless (string-empty-p keys)
      (setq parts (append parts (list (propertize keys 'face 'shadow)))))
    (when latest
      (setq parts (append parts (list "latest"))))
    (setq line (mapconcat #'identity parts "  "))
    (if current
        (propertize line 'face 'decide-current)
      line)))

(defun decide--indent-body (text)
  "Indent each line of TEXT under a message header."
  (mapconcat (lambda (line) (concat "  " line))
             (split-string (or text "") "\n")
             "\n"))

(defun decide--thread-text (item)
  "Thread text of ITEM, oldest first.
Each message is a block: a header, then an indented body.  A horizontal
rule separates one message from the next.  An item with no messages shows
its question."
  (let ((messages (alist-get "messages" item nil nil #'equal)))
    (if (null messages)
        (decide--field item "question")
      (let ((last (1- (length messages)))
            (i 0)
            (parts nil))
        (dolist (msg messages)
          (when (> i 0)
            (setq parts (append parts (list "────────"))))
          (setq parts (append parts
                              (list (decide--message-header
                                     msg (= i last)
                                     (and (numberp decide--message)
                                          (= i decide--message)))
                                    (decide--indent-body
                                     (decide--field msg "text")))))
          (setq i (1+ i)))
        (mapconcat #'identity parts "\n")))))

(defun decide--thread-parties (item)
  "Requester of ITEM and, when the thread was addressed elsewhere, that recipient.
A human compose stores the recipient as the owner and the requester as the
first message author.  The two values are returned as a list."
  (let* ((owner (decide--field item "owner"))
         (messages (alist-get "messages" item nil nil #'equal))
         (author (if messages (decide--field (car messages) "author") "")))
    (if (or (null messages)
            (string-empty-p author)
            (equal author owner))
        (list owner "")
      (list author owner))))

(defun decide--header-time (value)
  "VALUE as UTC RFC3339, or an empty string when it is missing."
  (if (or (null value) (string-empty-p value))
      ""
    (format-time-string "%Y-%m-%dT%H:%M:%SZ" (date-to-time value) t)))

(defun decide--owner-location-text (loc)
  "Where the owner pane in LOC, the `ownerLocation' of an item, lives.
LOC comes from `decide show --json'.  Nil or no pane is \"unknown\"."
  (let ((pane (decide--field loc "pane"))
        (machine (decide--field loc "machine")))
    (if (string-empty-p pane)
        "unknown"
      (let ((prefix (if (string-empty-p machine) "" (concat machine ": ")))
            (id (format "(%s)" pane)))
        (concat prefix
                (pcase (decide--field loc "state")
                  ("live" (format "%s / %s %s"
                                  (decide--field loc "workspace")
                                  (decide--field loc "tab")
                                  id))
                  ("closed" (concat id " — tab closed"))
                  ("unreachable" (concat id " (unreachable)"))
                  (_ id)))))))

(defun decide--header-rows (item)
  "Header rows of ITEM as (LABEL . VALUE) pairs.
To is included only when the thread has a recipient."
  (let* ((parties (decide--thread-parties item))
         (rows (list (cons "From" (nth 0 parties))))
         (to (nth 1 parties)))
    (unless (string-empty-p to)
      (setq rows (append rows (list (cons "To" to)))))
    (append rows
            (list (cons "Owner" (format "%s at %s"
                                        (decide--field item "owner")
                                        (decide--owner-location-text
                                         (alist-get "ownerLocation" item
                                                    nil nil #'equal))))
                  (cons "Kind" (decide--field item "kind"))
                  (cons "State" (decide--field item "state"))
                  (cons "Created" (decide--header-time
                                   (decide--field item "createdAt")))
                  (cons "Answered" (decide--header-time
                                    (decide--field item "answeredAt")))))))

(defun decide--header-lines (item)
  "Aligned `Label: value' lines for ITEM.  The colons share one column."
  (let* ((rows (decide--header-rows item))
         (width (apply #'max (mapcar (lambda (row) (string-width (car row)))
                                     rows))))
    (mapcar (lambda (row)
              (format "%s%s: %s"
                      (make-string (- width (string-width (car row))) ?\s)
                      (car row)
                      (cdr row)))
            rows)))

(defun decide--item-text (item)
  "Text for an item buffer.
Header rows come first, then a blank line, then the thread.
The reply line and the numbered options follow."
  (let ((parts (append (decide--header-lines item)
                       (list ""
                             (decide--thread-text item)
                             ""
                             "reply:")))
        (n 1))
    (dolist (opt (alist-get "options" item nil nil #'equal))
      (setq parts (append parts
                          (list (format "%d  %s%s"
                                        n
                                        (decide--field opt "label")
                                        (if (alist-get "recommended" opt
                                                       nil nil #'equal)
                                            " (recommended)"
                                          "")))))
      (setq n (1+ n)))
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
            "0 reply   y archive   O owner   A avatar   "
            (if (decide--item-links item) "l link   " "")
            "q quit")))

(defun decide--herdr (machine args)
  "Run herdr ARGS on MACHINE, empty for this machine.
Return nil on success, or the error output."
  (with-temp-buffer
    (let ((status (apply #'call-process decide-herdr-program nil t nil
                         (append (unless (string-empty-p machine)
                                   (list "--machine" machine))
                                 args))))
      (unless (eq status 0)
        (let ((out (string-trim (buffer-string))))
          (if (string-empty-p out) (format "exit %s" status) out))))))

(defun decide--focus-owner (loc)
  "Focus the workspace and then the tab of LOC, an `ownerLocation'.
Return the status text.  herdr cannot focus a pane by id, so the tab
is the target.  A location that is not live runs nothing and says why."
  (let ((machine (decide--field loc "machine"))
        (state (decide--field loc "state"))
        (workspace-id (decide--field loc "workspaceId"))
        (tab-id (decide--field loc "tabId")))
    (cond
     ((equal state "closed") "owner tab closed; nothing to focus")
     ((equal state "unreachable")
      (format "owner is on %s, which did not answer; nothing to focus" machine))
     ((or (not (equal state "live"))
          (string-empty-p workspace-id)
          (string-empty-p tab-id))
      "owner location unknown; nothing to focus")
     (t
      (let ((err (or (decide--herdr machine (list "workspace" "focus" workspace-id))
                     (decide--herdr machine (list "tab" "focus" tab-id)))))
        (cond
         ((and err (not (string-empty-p machine)))
          (format "owner is on %s; not focusable from here" machine))
         (err (concat "focus owner: " err))
         (t (format "focused %s%s / %s"
                    (if (string-empty-p machine) "" (concat machine ": "))
                    (decide--field loc "workspace")
                    (decide--field loc "tab")))))))))

(defun decide-focus-owner ()
  "Jump to the herdr tab of the open item's owner.
It uses the location from opening the item; \\[revert-buffer] looks it
up again.  The item is not marked read or answered."
  (interactive)
  (let ((item (car decide--items)))
    (unless item
      (user-error "No item"))
    (message "%s" (decide--focus-owner
                   (alist-get "ownerLocation" item nil nil #'equal)))))

;; The same pattern as the inbox, so both offer the same numbered links.
(defconst decide--url-regexp "https?://[^[:space:]]+"
  "A URL in item text.")

(defun decide--item-links (item)
  "URLs ITEM shows, in view order, without repeats.
URLs in the thread come first (the question when there is no thread),
then the item's links."
  (let ((messages (alist-get "messages" item nil nil #'equal))
        (links nil))
    (dolist (text (if messages
                      (mapcar (lambda (msg) (decide--field msg "text")) messages)
                    (list (decide--field item "question"))))
      (let ((start 0))
        (while (string-match decide--url-regexp text start)
          (push (match-string 0 text) links)
          (setq start (match-end 0)))))
    (dolist (link (alist-get "links" item nil nil #'equal))
      (push link links))
    (delete-dups (nreverse links))))

(defun decide--link-rows (links)
  "Numbered `completing-read' rows for LINKS, numbers right-aligned."
  (let ((width (length (number-to-string (length links))))
        (n 0))
    (mapcar (lambda (link)
              (setq n (1+ n))
              (format (format "%%%dd  %%s" width) n link))
            links)))

(defun decide-open-link ()
  "Open a link of the open item with `browse-url'.
One link opens at once.  Several are offered numbered, in the order
the item shows them."
  (interactive)
  (let ((links (decide--item-links (car decide--items))))
    (cond
     ((null links) (user-error "No link"))
     ((null (cdr links)) (browse-url (car links)))
     (t (let ((row (completing-read "Open link: " (decide--link-rows links) nil t)))
          (browse-url (nth (1- (string-to-number row)) links)))))))

(defun decide--avatars-set-args (owner choice)
  "Argument list that sets OWNER's avatar to CHOICE."
  (list "avatars" "set" owner choice))

(defun decide-set-avatar ()
  "Change the avatar of the open item's owner, as the inbox's A does.
Offer every built-in id plus default and none, then save the choice
with `decide avatars set'. Hub mode saves it on the hub."
  (interactive)
  (let ((owner (decide--field (car decide--items) "owner")))
    (when (string-empty-p owner)
      (user-error "No owner"))
    (let* ((ids (append '("default" "none")
                        (split-string (decide--command '("avatars" "list" "--ids"))
                                      "\n" t)))
           (choice (completing-read (format "Avatar for %s: " owner) ids nil t)))
      (message "%s" (string-trim
                     (decide--command (decide--avatars-set-args owner choice)))))))

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

(defun decide-message-next ()
  "Select the next newer message and show its header at the top."
  (interactive)
  (decide--shift-message 1))

(defun decide-message-previous ()
  "Select the previous older message and show its header at the top."
  (interactive)
  (decide--shift-message -1))

(defun decide--shift-message (delta)
  "Move the current message by DELTA and keep it inside the thread."
  (let ((n (length (alist-get "messages" (car decide--items) nil nil #'equal))))
    (unless (> n 0)
      (user-error "No message"))
    (unless (numberp decide--message)
      (setq decide--message 0))
    (setq decide--message (max 0 (min (1- n) (+ decide--message delta))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (decide--item-text (car decide--items))))
    (decide--scroll-current-message)))

(defun decide--scroll-current-message ()
  "Put the current message header at the top of the window."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (if (eq (get-text-property (point) 'face) 'decide-current)
          (setq found (line-beginning-position))
        (forward-line 1)))
    (unless found
      (setq found (point-min)))
    (goto-char found)
    (let ((win (get-buffer-window (current-buffer) t)))
      (when win
        (set-window-start win found)))))

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
  "Open the same composer as `decide-reply'."
  (interactive)
  (decide-reply))

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
(define-key decide-list-mode-map (kbd "/") #'decide-list-filter)
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
(define-key decide-item-mode-map (kbd "0") #'decide-reply)
(define-key decide-item-mode-map (kbd "y") #'decide-archive)
(define-key decide-item-mode-map (kbd "O") #'decide-focus-owner)
(define-key decide-item-mode-map (kbd "A") #'decide-set-avatar)
(define-key decide-item-mode-map (kbd "l") #'decide-open-link)
(define-key decide-item-mode-map (kbd "n") #'decide-message-next)
(define-key decide-item-mode-map (kbd "p") #'decide-message-previous)
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
      (pop-to-buffer (current-buffer))
      (decide--prepare-answer-state))))

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

(defun decide--resource-note (entry)
  "ENTRY's note, ending with (guess) when the lease could not be read."
  (let ((note (decide--field entry "note")))
    (if (alist-get "guess" entry nil nil #'equal)
        (string-trim (concat note " (guess)"))
      note)))

(defun decide-status-render (board now)
  "Insert BOARD, a status JSON alist, as of NOW."
  (let* ((agents (decide--status-rows
                  (alist-get "agents" board nil nil #'equal)
                  '("name" "state" "text") now))
         (resources (decide--status-rows
                     (mapcar (lambda (entry)
                               (cons (cons "note" (decide--resource-note entry)) entry))
                             (alist-get "resources" board nil nil #'equal))
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
    (decide--display buf)
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

(defun decide--evil-bind (map bindings)
  "Bind BINDINGS on MAP for both normal and motion state."
  (dolist (state '(normal motion))
    (apply #'evil-define-key* state map bindings)))

(defun decide--evil-normal-keys (map)
  "Bind line motion, quit, revert, and folder chords on MAP."
  (decide--evil-bind
   map
   (list (kbd "j") #'next-line
         (kbd "k") #'previous-line
         (kbd "q") #'quit-window
         (kbd "g r") #'revert-buffer
         (kbd "g i") #'decide-list-inbox
         (kbd "g a") #'decide-list-answered
         (kbd "g d") #'decide-list-done
         (kbd "g w") #'decide-list-withdrawn
         (kbd "g s") #'decide-status)))

(defun decide--evil-setup ()
  "Start the list, item, and status modes in normal state.
The same keys are bound for motion state.  Answer, reply, and compose
stay in insert state.  The list opens an item.  The item answers, replies,
or archives."
  (dolist (mode '(decide-list-mode decide-item-mode decide-status-mode))
    (evil-set-initial-state mode 'normal))
  (dolist (mode '(decide-answer-mode decide-reply-mode decide-compose-mode))
    (evil-set-initial-state mode 'insert))
  (dolist (map (list decide-list-mode-map decide-item-mode-map decide-status-mode-map))
    (decide--evil-normal-keys map))
  (decide--evil-bind
   decide-list-mode-map
   (list (kbd "o") #'decide-show
         (kbd "RET") #'decide-show
         (kbd "/") #'decide-list-filter))
  (decide--evil-bind
   decide-item-mode-map
   (list (kbd "o") #'decide-answer-option
         (kbd "O") #'decide-focus-owner
         (kbd "A") #'decide-set-avatar
         (kbd "l") #'decide-open-link
         (kbd "0") #'decide-reply
         (kbd "y") #'decide-archive
         (kbd "n") #'decide-message-next
         (kbd "p") #'decide-message-previous)))

(with-eval-after-load 'evil
  (decide--evil-setup))

(provide 'decide)

;;; decide.el ends here
