;;; dirvish-termux-fix.el --- Fix dirvish image preview for Termux/text terminal -*- lexical-binding: t -*-

;;; Commentary:
;; This file fixes the image-size error in dirvish when running in text terminal
;; and integrates with your existing image2ascii setup.

;;; Code:

(require 'dirvish)

;; Provide stub for image-size if it doesn't exist (text terminal)
(unless (fboundp 'image-size)
  (defun image-size (spec &optional pixels frame)
    "Stub function for text terminal Emacs.
Returns a reasonable default size for layout calculations."
    ;; Return window dimensions as a reasonable default
    (cons (window-width) (window-height))))

;; Override the image preview dispatch to use image2ascii
(cl-defmethod dirvish-preview-dispatch ((recipe (head img)) dv)
  "Insert RECIPE as ASCII art in preview window of DV for text terminals."
  (if (display-graphic-p)
      ;; Call original method for GUI Emacs
      (cl-call-next-method)
    ;; Text terminal implementation using image2ascii
    (with-current-buffer (dirvish--special-buffer 'preview dv t)
      (let ((file (with-current-buffer (cdr (dv-index dv))
                    (dirvish-prop :index)))
            (inhibit-read-only t)
            buffer-read-only)
        (erase-buffer)
        (remove-overlays)

        ;; Check if image2ascii is available
        (if (executable-find "image2ascii")
            (let* ((p-window (dv-preview-window dv))
                   (window-width (window-width p-window))
                   ;; Get image dimensions if magick is available
                   (img-width (if (executable-find "magick")
                                  (car (my/get-image-dimensions-imk file))
                                100)) ; fallback width
                   (ratio (/ (float (- window-width 2)) img-width)))

              ;; Run image2ascii
              (let ((exit-code (call-process "image2ascii" nil t nil
                                             "-f" file
                                             "-r" (number-to-string ratio))))
                (if (zerop exit-code)
                    (progn
                      ;; Process ANSI color codes
                      (ansi-color-apply-on-region (point-min) (point-max))
                      (goto-char (point-min)))
                  ;; Fallback if image2ascii fails
                  (insert (format "\n Image Preview Failed\n\n File: %s\n"
                                  (file-name-nondirectory file))))))

          ;; Fallback when image2ascii is not available
          (insert "\n Image Preview (Text Terminal)\n"
                  (make-string (window-width) ?─) "\n\n"
                  (format " File: %s\n" (file-name-nondirectory file))
                  (format " Type: %s\n" (upcase (or (file-name-extension file) "unknown")))
                  "\n [Install image2ascii for ASCII art preview]\n"))

        (set-buffer-modified-p nil)
        (current-buffer)))))

;; Function to check if we have the required image dimensions function
(defun dirvish-termux--ensure-dependencies ()
  "Ensure required functions are available."
  (unless (fboundp 'my/get-image-dimensions-imk)
    ;; Define a fallback version if not available
    (defun my/get-image-dimensions-imk (filepath)
      "Fallback image dimensions function."
      (if (executable-find "magick")
          (let* ((output (with-output-to-string
                           (call-process "magick" nil standard-output nil
                                         "identify" "-format" "%w %h" filepath)))
                 (dimensions (split-string output)))
            (cons (or (string-to-number (nth 0 dimensions)) 100)
                  (or (string-to-number (nth 1 dimensions)) 100)))
        ;; Return default dimensions if magick is not available
        (cons 100 100)))))

;; Setup hook
(defun dirvish-termux-setup ()
  "Setup dirvish for Termux/text terminal environment."
  (when (not (display-graphic-p))
    (dirvish-termux--ensure-dependencies)
    ;; Optionally adjust layout for better ASCII art viewing
    (setq dirvish-default-layout '(0 0 0.6))))

;; Auto-setup
(add-hook 'dirvish-setup-hook #'dirvish-termux-setup)

(provide 'dirvish-termux-fix)
;;; dirvish-termux-fix.el ends here
