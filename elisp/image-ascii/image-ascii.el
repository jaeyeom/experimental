;;; image-ascii.el --- Display images as ASCII art -*- lexical-binding: t; -*-
;;;
;;; Useful for viewing images in a terminal.
(defvar image-ascii-file-extensions '("jpg" "jpeg" "png" "gif" "bmp" "tiff")
  "List of image file extensions to be displayed as ASCII art.")

(defun image-ascii-file-p (filename)
  "Return non-nil if FILENAME has an image file extension."
  (let ((ext (downcase (file-name-extension filename))))
    (member ext image-ascii-file-extensions)))

(defun get-image-dimensions-imk (filepath)
  "Return the dimensions of the image at FILEPATH as a cons cell.

This function uses the 'magick identify' command to get the dimensions of the image."
  (let* ((output (shell-command-to-string
                  (concat "magick identify -format \"%w %h\" " filepath)))
         (dimensions (split-string output)))
    (cons (string-to-number (nth 0 dimensions))
          (string-to-number (nth 1 dimensions)))))

(defun image-ascii-display ()
  "Display the current buffer's file as ASCII art using image2ascii with color support."
  (when buffer-file-name
    (let ((inhibit-read-only t)
          (ascii-image-ratio (/ (float (- (window-width) 1)) (car (get-image-dimensions-imk buffer-file-name)))))
      (erase-buffer)
      (let ((exit-code (call-process "image2ascii" nil t nil "-f" buffer-file-name
                                     "-r" (number-to-string ascii-image-ratio))))
        (if (zerop exit-code)
            (progn
              ;; Process ANSI color codes
              (ansi-color-apply-on-region (point-min) (point-max))
              (set-buffer-modified-p nil)
              (read-only-mode 1)
              (goto-char (point-min)))
          (message "Error processing image with image2ascii"))))))

(define-derived-mode image-ascii-mode fundamental-mode "Image-ASCII"
  "Major mode for viewing images as ASCII art."
  (image-ascii-display))

(if my/termux-p
    (dolist (ext image-ascii-file-extensions)
      (add-to-list 'auto-mode-alist (cons (concat "\\." ext "\\'") 'image-ascii-mode))))
