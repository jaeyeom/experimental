;;; Test script to compare custom create-image with built-in version

(defun my/create-image (file-or-data &optional type data-p &rest props)
  "Create an image spec for FILE-OR-DATA.
TYPE is a symbol indicating the image type (e.g., png, jpeg, svg).
If DATA-P is non-nil, FILE-OR-DATA is the actual image data as a string.
PROPS are additional properties to add to the image spec.

This implementation creates a minimal image spec without actual image
support, allowing other functions like `my/image-size' to work with the spec."
  (let* ((image-type (or type 'png))
         (has-scale (plist-member props :scale))
         (has-rotation (plist-member props :rotation))
         (scale-value (plist-get props :scale))
         (spec (if data-p
                   (list 'image :type image-type :data file-or-data)
                 (list 'image :type image-type :file file-or-data))))
    ;; Append user props first
    (setq spec (append spec props))
    ;; Add scale default if not specified
    (unless has-scale
      (setq spec (append spec '(:scale default))))
    ;; Add transform-smoothing based on transformations
    (when (or has-scale has-rotation)
      (unless (plist-member props :transform-smoothing)
        (setq spec (append spec (list :transform-smoothing
                                     (if has-rotation
                                         nil
                                       (if (and has-scale (not (eq scale-value 'default)))
                                           t nil)))))))
    spec))

(defun test-create-image ()
  "Compare custom and built-in create-image implementations."
  (let ((test-file-nonexistent "/tmp/test-nonexistent.png")
        (test-file-existent "/tmp/test-existent.png")
        (test-data "fake-image-data")
        (results '()))

    ;; Create a minimal PNG file for testing
    (with-temp-file test-file-existent
      (set-buffer-multibyte nil)
      ;; Minimal 1x1 PNG file
      (insert "\x89PNG\r\n\x1a\n")  ;; PNG signature
      (insert "\x00\x00\x00\rIHDR\x00\x00\x00\x01\x00\x00\x00\x01\x08\x06\x00\x00\x00\x1f\x15\xc4\x89")
      (insert "\x00\x00\x00\nIDATx\x9cc\x00\x01\x00\x00\x05\x00\x01\r\n-\xb4")
      (insert "\x00\x00\x00\x00IEND\xaeB`\x82"))

    ;; Test 1: File that doesn't exist
    (let ((builtin (if (fboundp 'create-image)
                       (create-image test-file-nonexistent)
                     'not-available))
          (custom (my/create-image test-file-nonexistent)))
      (push (list "Test 1: Non-existent file"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Test 2: File that exists
    (let ((builtin (if (fboundp 'create-image)
                       (create-image test-file-existent)
                     'not-available))
          (custom (my/create-image test-file-existent)))
      (push (list "Test 2: Existent file"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Test 3: Existent file with type
    (let ((builtin (if (fboundp 'create-image)
                       (create-image test-file-existent 'png)
                     'not-available))
          (custom (my/create-image test-file-existent 'png)))
      (push (list "Test 3: Existent file with type"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Test 4: Existent file with type and properties
    (let ((builtin (if (fboundp 'create-image)
                       (create-image test-file-existent 'png nil :scale 2 :rotation 90)
                     'not-available))
          (custom (my/create-image test-file-existent 'png nil :scale 2 :rotation 90)))
      (push (list "Test 4: Existent file with props"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Test 5: Non-existent file with explicit type
    (let ((builtin (if (fboundp 'create-image)
                       (create-image test-file-nonexistent 'jpeg)
                     'not-available))
          (custom (my/create-image test-file-nonexistent 'jpeg)))
      (push (list "Test 5: Non-existent file, explicit type"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Test 6: Create image from data
    (let ((builtin (if (fboundp 'create-image)
                       (create-image test-data 'png t)
                     'not-available))
          (custom (my/create-image test-data 'png t)))
      (push (list "Test 6: Data with type"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Test 7: Create image from data with properties
    (let ((builtin (if (fboundp 'create-image)
                       (create-image test-data 'png t :scale 1.5)
                     'not-available))
          (custom (my/create-image test-data 'png t :scale 1.5)))
      (push (list "Test 7: Data with props"
                  :builtin builtin
                  :custom custom
                  :equal (equal builtin custom))
            results))

    ;; Print results
    (princ "\n=== CREATE-IMAGE COMPARISON TEST ===\n\n")
    (dolist (result (reverse results))
      (princ (format "%s\n" (car result)))
      (princ (format "  Built-in: %S\n" (plist-get (cdr result) :builtin)))
      (princ (format "  Custom:   %S\n" (plist-get (cdr result) :custom)))
      (princ (format "  Equal:    %S\n\n" (plist-get (cdr result) :equal))))

    ;; Check if built-in is available
    (if (fboundp 'create-image)
        (princ "\n✓ Built-in create-image is available\n")
      (princ "\n✗ Built-in create-image is NOT available\n"))

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
          (princ "✓ All tests PASSED - implementations are identical\n")
        (princ "✗ Some tests FAILED - implementations differ\n")))

    ;; Clean up
    (when (file-exists-p test-file-existent)
      (delete-file test-file-existent))))

;; Run the test
(test-create-image)
