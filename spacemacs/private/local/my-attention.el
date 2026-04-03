;;; my-attention.el --- Cross-perspective attention queue -*- lexical-binding: t; -*-

;; Author: Generated from Perplexity conversation
;; Keywords: convenience, perspectives
;; Package-Requires: ((emacs "27.1") (perspective "2.0"))

;;; Commentary:

;; A cross-perspective attention queue for Emacs with perspective.el.
;; Provides a "red bubble" style notification system: items are enqueued
;; when something needs attention (failed compilation, waiting prompt, etc.)
;; and dismissed when you visit the target buffer.
;;
;; Usage:
;;   (my-attention-mode 1)
;;   (global-set-key (kbd "C-c a n") #'my-attention-next)
;;   (global-set-key (kbd "C-c a p") #'my-attention-previous)
;;   (global-set-key (kbd "C-c a l") #'my-attention-list)
;;   (global-set-key (kbd "C-c a c") #'my-attention-clear-all)

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup my-attention nil
  "Attention queue across perspectives and buffers."
  :group 'convenience)

(cl-defstruct my-attention-item
  id persp buffer message severity created-at jump-fn)

(defvar my-attention--items nil)
(defvar my-attention--next-id 0)

(defcustom my-attention-auto-dismiss-on-visit t
  "Dismiss attention items when their target buffer becomes current."
  :type 'boolean)

(defcustom my-attention-error-face 'error
  "Face used for error attention."
  :type 'face)

(defcustom my-attention-warning-face 'warning
  "Face used for warning attention."
  :type 'face)

(defcustom my-attention-info-face 'success
  "Face used for info attention."
  :type 'face)

(defcustom my-attention-mode-line-format
  '(:eval (my-attention--mode-line-segment))
  "Mode-line construct that shows attention status for current perspective."
  :type 'sexp)

(defun my-attention--persp-name-current ()
  "Return current perspective name for perspective.el."
  (when (fboundp 'persp-current-name)
    (persp-current-name)))

(defun my-attention--persp-switch (name)
  "Switch to perspective NAME."
  (unless (fboundp 'persp-switch)
    (user-error "perspective.el not loaded"))
  (persp-switch name))

(defun my-attention--buffer-live-p (buf)
  "Return non-nil if BUF is a live buffer."
  (and buf (buffer-live-p buf)))

(defun my-attention--cleanup-dead-items ()
  "Drop items whose buffers are dead."
  (setq my-attention--items
        (cl-remove-if-not
         (lambda (it)
           (my-attention--buffer-live-p (my-attention-item-buffer it)))
         my-attention--items)))

(defun my-attention--items-for-persp (persp-name)
  "Return attention items for PERSP-NAME."
  (my-attention--cleanup-dead-items)
  (cl-remove-if-not
   (lambda (it)
     (equal (my-attention-item-persp it) persp-name))
   my-attention--items))

(defun my-attention--highest-severity (items)
  "Return the highest severity among ITEMS."
  (cond
   ((cl-find 'error items :key #'my-attention-item-severity) 'error)
   ((cl-find 'warning items :key #'my-attention-item-severity) 'warning)
   ((cl-find 'info items :key #'my-attention-item-severity) 'info)
   (t nil)))

(defun my-attention--severity-face (sev)
  "Return the face for severity SEV."
  (pcase sev
    ('error   my-attention-error-face)
    ('warning my-attention-warning-face)
    ('info    my-attention-info-face)
    (_        'mode-line)))

(defun my-attention--mode-line-segment ()
  "Mode-line segment for current perspective's attention."
  (let* ((persp (my-attention--persp-name-current))
         (items (and persp (my-attention--items-for-persp persp)))
         (count (length items)))
    (when (> count 0)
      (let* ((sev   (my-attention--highest-severity items))
             (face  (my-attention--severity-face sev))
             (label (format " ⚑%d" count)))
        (propertize label 'face face)))))

;;;###autoload
(define-minor-mode my-attention-mode
  "Global minor mode showing an attention indicator and commands."
  :global t
  (if my-attention-mode
      (progn
        (unless (memq 'my-attention-mode-line-format mode-line-format)
          (setq-default mode-line-format
                        (append mode-line-format
                                '(my-attention-mode-line-format))))
        (add-hook 'buffer-list-update-hook
                  #'my-attention--maybe-auto-dismiss))
    (setq-default mode-line-format
                  (remove 'my-attention-mode-line-format mode-line-format))
    (remove-hook 'buffer-list-update-hook
                 #'my-attention--maybe-auto-dismiss)))

(defun my-attention-add (&optional buffer message severity jump-fn persp-name)
  "Add a new attention item.

BUFFER defaults to current buffer.
MESSAGE defaults to buffer name.
SEVERITY defaults to `warning'.
JUMP-FN is called after switching to the buffer.
PERSP-NAME defaults to current perspective."
  (interactive)
  (let* ((buf   (or buffer (current-buffer)))
         (persp (or persp-name (my-attention--persp-name-current)))
         (msg   (or message (buffer-name buf)))
         (sev   (or severity 'warning))
         (item  (make-my-attention-item
                 :id (cl-incf my-attention--next-id)
                 :persp persp
                 :buffer buf
                 :message msg
                 :severity sev
                 :created-at (float-time)
                 :jump-fn jump-fn)))
    (push item my-attention--items)
    (force-mode-line-update t)
    item))

(defun my-attention-dismiss (&optional item)
  "Dismiss ITEM or all attention items for current buffer in current persp."
  (interactive)
  (if item
      (setq my-attention--items
            (delq item my-attention--items))
    (let ((buf   (current-buffer))
          (persp (my-attention--persp-name-current)))
      (setq my-attention--items
            (cl-remove-if
             (lambda (it)
               (and (eq (my-attention-item-buffer it) buf)
                    (equal (my-attention-item-persp it) persp)))
             my-attention--items))))
  (force-mode-line-update t))

(defun my-attention-clear-all ()
  "Clear all attention items."
  (interactive)
  (setq my-attention--items nil)
  (force-mode-line-update t)
  (message "Cleared all attention items"))

(defun my-attention--sorted-items ()
  "Return items sorted by creation time (oldest first)."
  (my-attention--cleanup-dead-items)
  (sort (copy-sequence my-attention--items)
        (lambda (a b)
          (< (my-attention-item-created-at a)
             (my-attention-item-created-at b)))))

(defun my-attention-next ()
  "Jump to the next pending attention item across perspectives."
  (interactive)
  (let ((items (my-attention--sorted-items)))
    (unless items
      (user-error "No pending attention items"))
    (let* ((it   (car items))
           (persp (my-attention-item-persp it))
           (buf   (my-attention-item-buffer it))
           (jump  (my-attention-item-jump-fn it)))
      (when persp
        (my-attention--persp-switch persp))
      (switch-to-buffer buf)
      (when jump
        (funcall jump))
      (when my-attention-auto-dismiss-on-visit
        (my-attention-dismiss it))
      (message "[%s] %s"
               (symbol-name (my-attention-item-severity it))
               (my-attention-item-message it)))))

(defun my-attention-previous ()
  "Jump to the most recently added pending attention item."
  (interactive)
  (let ((items (reverse (my-attention--sorted-items))))
    (unless items
      (user-error "No pending attention items"))
    (let* ((it   (car items))
           (persp (my-attention-item-persp it))
           (buf   (my-attention-item-buffer it))
           (jump  (my-attention-item-jump-fn it)))
      (when persp
        (my-attention--persp-switch persp))
      (switch-to-buffer buf)
      (when jump
        (funcall jump))
      (when my-attention-auto-dismiss-on-visit
        (my-attention-dismiss it))
      (message "[%s] %s"
               (symbol-name (my-attention-item-severity it))
               (my-attention-item-message it)))))

(defun my-attention-list ()
  "List pending attention items."
  (interactive)
  (my-attention--cleanup-dead-items)
  (if (null my-attention--items)
      (message "No pending attention items")
    (with-current-buffer (get-buffer-create "*Attention List*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (format "%-5s %-14s %-10s %-24s %s\n"
                        "ID" "Perspective" "Severity" "Buffer" "Message"))
        (insert (make-string 80 ?-))
        (insert "\n")
        (dolist (it (sort (copy-sequence my-attention--items)
                          (lambda (a b)
                            (< (my-attention-item-created-at a)
                               (my-attention-item-created-at b)))))
          (insert (format "%-5d %-14s %-10s %-24s %s\n"
                          (my-attention-item-id it)
                          (or (my-attention-item-persp it) "")
                          (symbol-name (my-attention-item-severity it))
                          (buffer-name (my-attention-item-buffer it))
                          (my-attention-item-message it)))))
      (pop-to-buffer (current-buffer)))))

(defun my-attention--maybe-auto-dismiss ()
  "Dismiss attention for current buffer if enabled."
  (when my-attention-auto-dismiss-on-visit
    (let ((buf   (current-buffer))
          (persp (my-attention--persp-name-current)))
      (when (cl-find-if
             (lambda (it)
               (and (eq (my-attention-item-buffer it) buf)
                    (equal (my-attention-item-persp it) persp)))
             my-attention--items)
        (my-attention-dismiss)))))

(defun my-attention-add-process-sentinel (process message &optional persp-name)
  "Attach a sentinel to PROCESS that enqueues attention on failure.
MESSAGE is shown in the attention queue."
  (set-process-sentinel
   process
   (lambda (proc event)
     (when (memq (process-status proc) '(exit signal))
       (let ((code (process-exit-status proc)))
         (when (/= code 0)
           (my-attention-add
            (process-buffer proc)
            (format "%s (%s, exit %d)" message (string-trim event) code)
            'error
            nil
            persp-name)))))))

(provide 'my-attention)
;;; my-attention.el ends here
