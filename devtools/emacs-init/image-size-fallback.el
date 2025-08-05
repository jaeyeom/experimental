;;; image-size-fallback.el --- Implement image-size for Emacs without image support

;; Author: Claude Code
;; Version: 1.0
;; Keywords: images, fallback

;;; Commentary:

;; This package provides a fallback implementation of the `image-size'
;; function for Emacs installations without image support. It uses
;; external tools like `magick' or `convert' to determine image dimensions.

;;; Code:

(defcustom image-size-fallback-command nil
  "Command to use for getting image dimensions.
If nil, will auto-detect from available commands.
Can be set to 'magick or 'convert."
  :type '(choice (const nil)
                 (const magick)
                 (const convert))
  :group 'image-size-fallback)

(defun image-size-fallback--find-command ()
  "Find the best available command for getting image dimensions."
  (cond
   ((executable-find "magick") 'magick)
   ((executable-find "convert") 'convert)
   (t (error "No suitable image processing command found (magick or convert required)"))))

(defun image-size-fallback--get-dimensions (image-file)
  "Get dimensions of IMAGE-FILE using external command.
Returns a cons cell (WIDTH . HEIGHT) or nil if failed."
  (let* ((command (or image-size-fallback-command
                      (image-size-fallback--find-command)))
         (cmd-args (pcase command
                     ('magick (list "magick" "identify" "-format" "%wx%h" image-file))
                     ('convert (list "convert" image-file "-print" "%wx%h\\n" "/dev/null"))
                     (_ (error "Unsupported command: %s" command))))
         (output (with-temp-buffer
                   (let ((exit-code (apply #'call-process
                                           (car cmd-args) nil t nil (cdr cmd-args))))
                     (if (= exit-code 0)
                         (string-trim (buffer-string))
                       nil)))))
    (when (and output (string-match "\\([0-9]+\\)x\\([0-9]+\\)" output))
      (cons (string-to-number (match-string 1 output))
            (string-to-number (match-string 2 output))))))

(defun image-size-fallback--spec-to-file (spec)
  "Extract file path from image SPEC or handle image data.
SPEC can be a string (file path) or a list starting with 'image.
For image data, creates a temporary file.
Uses Emacs image search path logic for relative paths."
  (cond
   ((stringp spec)
    ;; String path
    (if (file-name-absolute-p spec)
        spec
      (image-size-fallback--find-image-file spec)))
   ((and (listp spec) (eq (car spec) 'image))
    ;; Image specification
    (let ((file-path (plist-get (cdr spec) :file))
          (image-data (plist-get (cdr spec) :data))
          (image-type (plist-get (cdr spec) :type)))
      (cond
       (file-path
        ;; File-based image
        (if (file-name-absolute-p file-path)
            file-path
          (image-size-fallback--find-image-file file-path)))
       (image-data
        ;; Data-based image - create temporary file
        (image-size-fallback--data-to-temp-file image-data image-type))
       (t
        (error "Image specification missing :file or :data: %s" spec)))))
   (t (error "Invalid image specification: %s" spec))))

(defun image-size-fallback--data-to-temp-file (data type)
  "Create a temporary file from image DATA of given TYPE.
Returns the path to the temporary file."
  (let* ((extension (pcase type
                      ('png ".png")
                      ('jpeg ".jpg")
                      ('jpg ".jpg")
                      ('gif ".gif")
                      ('bmp ".bmp")
                      ('tiff ".tiff")
                      ('xpm ".xpm")
                      ('pbm ".pbm")
                      ('svg ".svg")
                      (_ ".tmp")))
         (temp-file (make-temp-file "emacs-image-size-" nil extension)))
    (with-temp-file temp-file
      (set-buffer-multibyte nil)
      (insert data))
    ;; Store temp file for cleanup
    (push temp-file image-size-fallback--temp-files)
    temp-file))

(defvar image-size-fallback--temp-files nil
  "List of temporary files created for image data processing.")

(defun image-size-fallback-cleanup-temp-files ()
  "Clean up temporary files created for image data processing."
  (interactive)
  (dolist (file image-size-fallback--temp-files)
    (when (file-exists-p file)
      (delete-file file)))
  (setq image-size-fallback--temp-files nil))

(defun image-size-fallback--find-image-file (file)
  "Find FILE in image search paths, mimicking Emacs' image search behavior."
  (let ((search-paths (if (boundp 'image-load-path)
                          image-load-path
                        ;; Fallback if image-load-path not available
                        (list (if (boundp 'data-directory)
                                  (expand-file-name "images" data-directory)
                                "/usr/share/emacs/etc/images"))))
        (found-file nil))
    (dolist (path search-paths)
      (when (and (not found-file) path)
        (let* ((expanded-path (cond
                               ((eq path 'data-directory)
                                (if (boundp 'data-directory)
                                    (expand-file-name "images" data-directory)
                                  "/usr/share/emacs/etc/images"))
                               ((eq path 'load-path)
                                ;; Search in load-path directories
                                nil) ; Handle separately below
                               ((stringp path)
                                path)
                               (t nil)))
               (candidate (when expanded-path
                            (expand-file-name file expanded-path))))
          (when (and candidate (file-exists-p candidate))
            (setq found-file candidate)))))
    ;; If still not found and 'load-path was in search paths, search load-path
    (when (and (not found-file)
               (boundp 'image-load-path)
               (memq 'load-path image-load-path)
               (boundp 'load-path))
      (dolist (path load-path)
        (when (and (not found-file) (stringp path))
          (let ((candidate (expand-file-name file path)))
            (when (file-exists-p candidate)
              (setq found-file candidate))))))
    (or found-file
        (error "Cannot find image file: %s" file))))

(defun image-size-fallback-original (spec &optional pixels frame)
  "Get the size of image SPEC using fallback method.
This is a fallback implementation when the built-in `image-size'
function is not available or doesn't work (e.g., in batch mode).

SPEC should be an image specification or a file path.
PIXELS non-nil means return size in pixels, otherwise return size in canonical character units.
FRAME is ignored in this implementation.

Returns a cons cell (WIDTH . HEIGHT) representing the image dimensions."
  (let* ((file-path (image-size-fallback--spec-to-file spec))
         (pixel-dimensions (image-size-fallback--get-dimensions file-path)))
    (unless pixel-dimensions
      (error "Could not determine dimensions for image: %s" file-path))
    (if pixels
        ;; Return pixels directly
        pixel-dimensions
      ;; Convert to canonical character units
      (image-size-fallback--pixels-to-chars pixel-dimensions frame))))

(defun image-size-fallback--pixels-to-chars (pixel-dimensions &optional frame)
  "Convert PIXEL-DIMENSIONS to canonical character units.
PIXEL-DIMENSIONS should be a cons cell (WIDTH . HEIGHT) in pixels.
FRAME is used to determine character size (ignored in this implementation)."
  (let* ((pixel-width (car pixel-dimensions))
         (pixel-height (cdr pixel-dimensions))
         ;; Default character dimensions (based on actual Emacs measurements)
         ;; These match the values used by Emacs in typical configurations
         (char-width 10.0)  ; pixels per character width
         (char-height 22.0) ; pixels per character height (including line spacing)
         (char-units-width (/ pixel-width char-width))
         (char-units-height (/ pixel-height char-height)))
    (cons char-units-width char-units-height)))

;;;###autoload
(defun image-size-fallback-install ()
  "Install the fallback image-size function if needed.
This will override the built-in `image-size' function if it's not working
in the current environment (e.g., no window system available).
The original function is preserved as `image-size-original'."
  (interactive)
  (when (or (not (fboundp 'image-size))
            ;; Test if image-size works in current environment
            (condition-case nil
                (progn
                  ;; Try to use image-size with a dummy spec
                  ;; This will fail if no window system is available
                  (image-size '(image :type png :file "nonexistent.png"))
                  nil) ; If no error, image-size works
              (error t))) ; If error, we need fallback
    (message "Installing image-size fallback implementation")
    ;; Preserve the original function if it exists
    (when (fboundp 'image-size)
      (unless (fboundp 'image-size-original)
        (defalias 'image-size-original (symbol-function 'image-size))))
    (defalias 'image-size #'image-size-fallback-original)
    ;; Add cleanup hook
    (add-hook 'kill-emacs-hook #'image-size-fallback-cleanup-temp-files)
    (put 'image-size 'function-documentation
         "Return the size of image SPEC as pair (WIDTH . HEIGHT).
PIXELS non-nil means return the size in pixels, otherwise return the
size in canonical character units.

FRAME is the frame on which the image will be displayed. FRAME nil
or omitted means use the selected frame.

This is a fallback implementation using external tools when Emacs
doesn't have image support or when running in batch mode.

Calling this function will result in the image being stored in the
image cache. If this is not desirable, call 'image-flush' after
calling this function.

Probably introduced at or before Emacs version 21.1.")))

;;;###autoload
(defun image-size-fallback-uninstall ()
  "Uninstall the fallback image-size function and restore the original.
This only works if the original function was preserved during installation."
  (interactive)
  (if (fboundp 'image-size-original)
      (progn
        (defalias 'image-size (symbol-function 'image-size-original))
        (fmakunbound 'image-size-original)
        (message "Restored original image-size function"))
    (message "No original image-size function found to restore")))

;;;###autoload
(defun image-size-fallback-test ()
  "Test the image-size fallback functionality."
  (interactive)
  (let ((test-file "test_image.png"))
    (unless (file-exists-p test-file)
      (error "Test file %s not found. Create it first with: convert -size 100x200 xc:white %s"
             test-file test-file))
    (message "Testing fallback image-size with %s" test-file)
    (let ((size (image-size-fallback-original test-file)))
      (message "Image size: %dx%d" (car size) (cdr size))
      size)))

(provide 'image-size-fallback)

;;; image-size-fallback.el ends here
