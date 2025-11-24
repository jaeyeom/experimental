;;; Test script to compare custom my/image-size with built-in version
;;; Run with: emacs -Q -l test-image-size.el

(defun my/image-size (spec &optional pixels frame)
  "Return the size of image SPEC as pair (WIDTH . HEIGHT).
PIXELS non-nil means return the size in pixels, otherwise return the
size in canonical character units.
FRAME is the frame on which the image will be displayed. FRAME nil
or omitted means use the selected frame.

This implementation uses ImageMagick's `magick identify' command
to determine image dimensions for Emacs builds without image support."
  (unless (and (consp spec) (eq (car spec) 'image))
    (error "Invalid image specification"))
  (let* ((file (plist-get (cdr spec) :file))
         (data (plist-get (cdr spec) :data))
         (temp-file nil)
         (actual-file (cond
                       (file (expand-file-name file))
                       (data
                        ;; Write data to temp file for ImageMagick to read
                        (setq temp-file (make-temp-file "emacs-image-" nil ".img"))
                        (with-temp-file temp-file
                          (set-buffer-multibyte nil)
                          (insert data))
                        temp-file)
                       (t nil)))
         (output (when actual-file
                   (shell-command-to-string
                    (format "magick identify -format '%%w %%h' %s 2>/dev/null"
                            (shell-quote-argument actual-file)))))
         (dimensions (when (and output (string-match "\\([0-9]+\\) \\([0-9]+\\)" output))
                       (cons (string-to-number (match-string 1 output))
                             (string-to-number (match-string 2 output))))))
    ;; Clean up temp file if we created one
    (when (and temp-file (file-exists-p temp-file))
      (delete-file temp-file))
    (if dimensions
        (if pixels
            dimensions
          ;; Convert pixels to canonical character units using float division
          (let ((char-width (float (frame-char-width frame)))
                (char-height (float (frame-char-height frame))))
            (cons (/ (car dimensions) char-width)
                  (/ (cdr dimensions) char-height))))
      ;; Return default (30 . 30) if we couldn't determine dimensions (matches built-in)
      (if pixels
          '(30 . 30)
        (let ((char-width (float (frame-char-width frame)))
              (char-height (float (frame-char-height frame))))
          (cons (/ 30 char-width)
                (/ 30 char-height)))))))

(defun test-image-size ()
  "Compare custom and built-in image-size implementations."
  (let ((test-file-10x10 "/tmp/test-10x10.png")
        (test-file-100x50 "/tmp/test-100x50.png")
        (test-file-nonexistent "/tmp/test-nonexistent-image.png")
        (results '()))

    ;; Create test PNG files using ImageMagick
    (shell-command "magick -size 10x10 xc:red /tmp/test-10x10.png 2>/dev/null")
    (shell-command "magick -size 100x50 xc:blue /tmp/test-100x50.png 2>/dev/null")

    ;; Check if ImageMagick is available
    (let ((magick-available (= 0 (shell-command "which magick >/dev/null 2>&1"))))
      (unless magick-available
        (princ "\nWARNING: ImageMagick 'magick' command not found. Tests may fail.\n\n")))

    ;; Test 1: 10x10 image, pixels mode
    (let* ((spec (list 'image :type 'png :file test-file-10x10))
           (builtin (if (fboundp 'image-size)
                        (condition-case nil
                            (image-size spec t)
                          (error 'error))
                      'not-available))
           (custom (condition-case nil
                       (my/image-size spec t)
                     (error 'error))))
      (push (list "Test 1: 10x10 image (pixels)"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Test 2: 100x50 image, pixels mode
    (let* ((spec (list 'image :type 'png :file test-file-100x50))
           (builtin (if (fboundp 'image-size)
                        (condition-case nil
                            (image-size spec t)
                          (error 'error))
                      'not-available))
           (custom (condition-case nil
                       (my/image-size spec t)
                     (error 'error))))
      (push (list "Test 2: 100x50 image (pixels)"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Test 3: 10x10 image, character units mode
    (let* ((spec (list 'image :type 'png :file test-file-10x10))
           (builtin (if (fboundp 'image-size)
                        (condition-case nil
                            (image-size spec nil)
                          (error 'error))
                      'not-available))
           (custom (condition-case nil
                       (my/image-size spec nil)
                     (error 'error))))
      (push (list "Test 3: 10x10 image (char units)"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Test 4: 100x50 image, character units mode
    (let* ((spec (list 'image :type 'png :file test-file-100x50))
           (builtin (if (fboundp 'image-size)
                        (condition-case nil
                            (image-size spec nil)
                          (error 'error))
                      'not-available))
           (custom (condition-case nil
                       (my/image-size spec nil)
                     (error 'error))))
      (push (list "Test 4: 100x50 image (char units)"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Test 5: Non-existent file (should return default 30x30)
    (let* ((spec (list 'image :type 'png :file test-file-nonexistent))
           (builtin (if (fboundp 'image-size)
                        (condition-case nil
                            (image-size spec t)
                          (error 'error))
                      'not-available))
           (custom (condition-case nil
                       (my/image-size spec t)
                     (error 'error))))
      (push (list "Test 5: Non-existent file (pixels)"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Test 6: Invalid spec (should error)
    (let* ((builtin (if (fboundp 'image-size)
                        (condition-case nil
                            (image-size "not-a-spec" t)
                          (error 'error))
                      'not-available))
           (custom (condition-case nil
                       (my/image-size "not-a-spec" t)
                     (error 'error))))
      (push (list "Test 6: Invalid spec (both should error)"
                  :builtin builtin
                  :custom custom
                  :equal (eq builtin custom))
            results))

    ;; Test 7: Data-based image (pixels)
    (let* ((data (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally test-file-10x10)
                   (buffer-string)))
           (spec (list 'image :type 'png :data data))
           (builtin (if (fboundp 'image-size)
                        (condition-case nil
                            (image-size spec t)
                          (error 'error))
                      'not-available))
           (custom (condition-case nil
                       (my/image-size spec t)
                     (error 'error))))
      (push (list "Test 7: Data-based image (pixels)"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Test 8: Data-based image (char units)
    (let* ((data (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally test-file-100x50)
                   (buffer-string)))
           (spec (list 'image :type 'png :data data))
           (builtin (if (fboundp 'image-size)
                        (condition-case nil
                            (image-size spec nil)
                          (error 'error))
                      'not-available))
           (custom (condition-case nil
                       (my/image-size spec nil)
                     (error 'error))))
      (push (list "Test 8: Data-based 100x50 (char units)"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Print results
    (princ "\n=== IMAGE-SIZE COMPARISON TEST ===\n\n")
    (princ (format "frame-char-width: %S\n" (frame-char-width)))
    (princ (format "frame-char-height: %S\n\n" (frame-char-height)))

    (dolist (result (reverse results))
      (princ (format "%s\n" (car result)))
      (princ (format "  Built-in: %S\n" (plist-get (cdr result) :builtin)))
      (princ (format "  Custom:   %S\n" (plist-get (cdr result) :custom)))
      (princ (format "  Equal:    %S\n\n" (plist-get (cdr result) :equal))))

    ;; Check if built-in is available
    (if (fboundp 'image-size)
        (princ "\n[OK] Built-in image-size is available\n")
      (princ "\n[WARN] Built-in image-size is NOT available\n"))

    ;; Summary
    (let ((all-equal t)
          (passed 0)
          (total 0))
      (dolist (r results)
        (when (not (eq (plist-get (cdr r) :builtin) 'not-available))
          (setq total (1+ total))
          (if (plist-get (cdr r) :equal)
              (setq passed (1+ passed))
            (setq all-equal nil))))
      (princ (format "\nResults: %d/%d tests passed\n" passed total))
      (if all-equal
          (princ "[OK] All tests PASSED - implementations are identical\n")
        (princ "[WARN] Some tests FAILED - implementations differ\n")))

    ;; Clean up
    (when (file-exists-p test-file-10x10)
      (delete-file test-file-10x10))
    (when (file-exists-p test-file-100x50)
      (delete-file test-file-100x50))))

;; Run the test
(test-image-size)
