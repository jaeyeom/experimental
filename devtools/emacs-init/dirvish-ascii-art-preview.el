;;; dirvish-ascii-art-preview.el --- ASCII art preview for dirvish in text terminals -*- lexical-binding: t -*-

;;; Commentary:
;; This file provides ASCII art image preview support for dirvish in text terminal Emacs.
;; It integrates with common ASCII art viewers like jp2a, img2txt, or custom viewers.

;;; Code:

(require 'dirvish)

(defcustom dirvish-ascii-art-viewer-program nil
  "Program to convert images to ASCII art.
Common options include:
- jp2a: JPEG to ASCII converter
- img2txt (from caca-utils): Various formats to colored ASCII
- ascii-image-converter: Modern tool supporting many formats
- chafa: Terminal graphics with unicode support"
  :type '(choice (const :tag "jp2a" "jp2a")
                 (const :tag "img2txt" "img2txt")
                 (const :tag "ascii-image-converter" "ascii-image-converter")
                 (const :tag "chafa" "chafa")
                 (string :tag "Custom command"))
  :group 'dirvish)

(defcustom dirvish-ascii-art-max-width 80
  "Maximum width for ASCII art preview."
  :type 'integer
  :group 'dirvish)

(defcustom dirvish-ascii-art-max-height 40
  "Maximum height for ASCII art preview."
  :type 'integer
  :group 'dirvish)

(defun dirvish-ascii-art--find-viewer ()
  "Find available ASCII art viewer program."
  (or dirvish-ascii-art-viewer-program
      (seq-find #'executable-find
                '("chafa" "ascii-image-converter" "img2txt" "jp2a"))))

(defun dirvish-ascii-art--build-command (program file width height)
  "Build command for PROGRAM to convert FILE with WIDTH and HEIGHT."
  (pcase program
    ("jp2a"
     (format "jp2a --width=%d --height=%d --colors '%s' 2>/dev/null"
             width height (shell-quote-argument file)))
    ("img2txt"
     (format "img2txt -W %d -H %d '%s' 2>/dev/null"
             width height (shell-quote-argument file)))
    ("ascii-image-converter"
     (format "ascii-image-converter -W %d -H %d '%s' 2>/dev/null"
             width height (shell-quote-argument file)))
    ("chafa"
     (format "chafa --size %dx%d '%s' 2>/dev/null"
             width height (shell-quote-argument file)))
    (_
     (format "%s '%s'" program (shell-quote-argument file)))))

;; Provide stub for image-size if it doesn't exist
(unless (fboundp 'image-size)
  (defun image-size (spec &optional pixels frame)
    "Stub function for text terminal Emacs."
    (cons 10 10)))

;; Define ASCII art preview dispatcher
(dirvish-define-preview ascii-art (file ext preview-window)
  "Preview images as ASCII art in text terminals."
  :require ((not (display-graphic-p)))
  (when (and (member ext dirvish-image-exts)
             (dirvish-ascii-art--find-viewer))
    `(ascii-art . ,file)))

;; Custom preview dispatch for ASCII art
(cl-defmethod dirvish-preview-dispatch ((recipe (head ascii-art)) dv)
  "Insert ASCII art preview for RECIPE in DV."
  (with-current-buffer (dirvish--special-buffer 'preview dv t)
    (let ((file (cdr recipe))
          (viewer (dirvish-ascii-art--find-viewer))
          buffer-read-only)
      (erase-buffer)
      (if viewer
          (let* ((p-window (dv-preview-window dv))
                 (width (min (- (window-width p-window) 2) 
                            dirvish-ascii-art-max-width))
                 (height (min (- (window-height p-window) 4)
                             dirvish-ascii-art-max-height))
                 (cmd (dirvish-ascii-art--build-command viewer file width height)))
            (insert (propertize (format " ASCII Art Preview (%s)\n" viewer) 'face 'bold))
            (insert (make-string (window-width p-window) ?─) "\n")
            (let ((output (shell-command-to-string cmd)))
              (if (string-empty-p output)
                  (insert (format "\n Error: Could not convert %s\n" 
                                 (file-name-nondirectory file)))
                (insert output))))
        (insert "\n No ASCII art viewer found.\n"
                " Install one of: chafa, jp2a, img2txt, ascii-image-converter\n")))
    (current-buffer)))

;; Override the original image preview dispatch for text terminals
(cl-defmethod dirvish-preview-dispatch ((recipe (head img)) dv)
  "Handle image preview in both GUI and text terminal."
  (if (display-graphic-p)
      ;; Original implementation for GUI
      (cl-call-next-method)
    ;; Convert to ASCII art preview for text terminal
    (let ((file (with-current-buffer (cdr (dv-index dv))
                  (dirvish-prop :index))))
      (dirvish-preview-dispatch 
       (cons 'ascii-art file) dv))))

;; Setup function to configure dirvish for text terminals
(defun dirvish-ascii-art-setup ()
  "Setup dirvish for ASCII art preview in text terminals."
  (when (not (display-graphic-p))
    ;; Add ascii-art to preview dispatchers if not already there
    (add-to-list 'dirvish-preview-dispatchers 'ascii-art)
    ;; Optionally adjust layout for text terminals
    (setq dirvish-default-layout '(0 0 0.5))))

;; Auto-setup when loading
(add-hook 'dirvish-setup-hook #'dirvish-ascii-art-setup)

(provide 'dirvish-ascii-art-preview)
;;; dirvish-ascii-art-preview.el ends here