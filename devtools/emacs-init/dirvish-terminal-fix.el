;;; dirvish-terminal-fix.el --- Fix dirvish image preview in text terminal Emacs -*- lexical-binding: t -*-

;;; Commentary:
;; This file provides a workaround for dirvish image preview in text terminal Emacs
;; where the `image-size' function is not available.

;;; Code:

(require 'dirvish)

;; Provide a stub for image-size if it doesn't exist
(unless (fboundp 'image-size)
  (defun image-size (spec &optional pixels frame)
    "Stub function for text terminal Emacs.
Returns a default size since we can't actually display images."
    (cons 10 10)))

;; Override the image preview dispatch method to handle text terminals
(cl-defmethod dirvish-preview-dispatch ((recipe (head img)) dv)
  "Insert RECIPE as an image at preview window of DV.
This version handles text terminal Emacs where images cannot be displayed."
  (with-current-buffer (dirvish--special-buffer 'preview dv t)
    (let ((img (cdr recipe))
          (file (with-current-buffer (cdr (dv-index dv))
                  (dirvish-prop :index)))
          buffer-read-only)
      (erase-buffer)
      (remove-overlays)
      (if (display-graphic-p)
          ;; Original implementation for GUI Emacs
          (progn
            (insert " ")
            (add-text-properties 1 2 `(display ,img rear-nonsticky t keymap ,image-map))
            (pcase-let ((`(,iw . ,ih) (image-size img)))
              (let* ((p-window (dv-preview-window dv))
                     (w-pad (max (round (/ (- (window-width p-window) iw) 2)) 0))
                     (h-pad (max (round (/ (- (window-height p-window) ih) 2)) 0)))
                (goto-char 1)
                (insert (make-string (if dirvish-show-media-properties 2 h-pad) ?\n)
                        (make-string w-pad ?\s))
                (when dirvish-show-media-properties
                  (let* ((beg (progn (goto-char (point-max)) (point)))
                         (ext (downcase (or (file-name-extension file) "")))
                         (type (cond ((member ext dirvish-image-exts) 'image)
                                     ((member ext dirvish-video-exts) 'video)
                                     ((member ext dirvish-font-exts) 'font)
                                     ((equal ext "pdf") 'pdf)
                                     (t (user-error "Not a media file")))))
                    (insert "\n\n\n" (dirvish-media-metadata (cons type file))
                            (make-string (* h-pad 2) ?\n))
                    (align-regexp beg (point) "\\(\\\t\\)[^\\\t\\\n]+" 1 4 t)
                    (goto-char 1))))))
        ;; Text terminal implementation
        (let ((p-window (dv-preview-window dv)))
          (insert "\n\n")
          (insert (propertize " [Image Preview]\n" 'face 'bold))
          (insert (format " File: %s\n" (file-name-nondirectory file)))
          (when (file-exists-p file)
            (let ((attrs (file-attributes file)))
              (insert (format " Size: %s bytes\n" (file-attribute-size attrs)))
              (insert (format " Modified: %s\n" 
                              (format-time-string "%Y-%m-%d %H:%M:%S" 
                                                  (file-attribute-modification-time attrs))))))
          (insert "\n")
          (insert " [Image cannot be displayed in text terminal]\n")
          (insert " Use your ASCII art viewer by opening the file directly.\n")
          ;; If you have an ASCII art image viewer command, you could call it here
          ;; For example:
          ;; (when (executable-find "your-ascii-art-viewer")
          ;;   (insert "\n" (shell-command-to-string 
          ;;                 (format "your-ascii-art-viewer %s" 
          ;;                         (shell-quote-argument file)))))
          ))
      (current-buffer))))

;; Alternative approach: Disable image previews entirely in text terminals
(defun dirvish-disable-image-previews-in-terminal ()
  "Disable image preview dispatchers when running in text terminal."
  (when (not (display-graphic-p))
    (setq dirvish-preview-dispatchers
          (delq 'image 
                (delq 'gif dirvish-preview-dispatchers)))))

;; You can uncomment this to disable image previews entirely:
;; (add-hook 'dirvish-setup-hook #'dirvish-disable-image-previews-in-terminal)

(provide 'dirvish-terminal-fix)
;;; dirvish-terminal-fix.el ends here