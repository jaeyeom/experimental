;;; demo-image-size.el --- Demonstration of image-size fallback

;;; Commentary:

;; This file demonstrates the image-size fallback functionality
;; for GitHub issue #37.

;;; Code:

;; Load the fallback implementation
(load (expand-file-name "image-size-fallback.el" (file-name-directory (or load-file-name buffer-file-name))))

;; Install the fallback (this will only install if needed)
(image-size-fallback-install)

;; Test with different image specifications
(defun demo-image-size ()
  "Demonstrate image-size functionality."
  (interactive)
  (message "=== Image Size Demo ===")

  ;; Test with file path (character units) - using absolute path
  (let ((abs-path (expand-file-name "test_image.png")))
    (when (file-exists-p abs-path)
      (let ((char-size (image-size abs-path))
            (pixel-size (image-size abs-path t)))
        (message "test_image.png (chars): %.1fx%.1f" (car char-size) (cdr char-size))
        (message "test_image.png (pixels): %dx%d" (car pixel-size) (cdr pixel-size)))))

  ;; Test with image spec - using absolute path
  (let ((abs-path (expand-file-name "test_image.png")))
    (when (file-exists-p abs-path)
      (let ((size (image-size (create-image abs-path))))
        (message "create-image spec: %dx%d" (car size) (cdr size)))))

  ;; Test with plist spec - using absolute path
  (let ((abs-path (expand-file-name "test_image.png")))
    (when (file-exists-p abs-path)
      (let ((size (image-size `(image :type png :file ,abs-path))))
        (message "plist spec: %dx%d" (car size) (cdr size)))))

  ;; Test with image data (binary data in memory)
  (when (file-exists-p "test_image.png")
    (let ((size (image-size (create-image
                             (with-temp-buffer
                               (set-buffer-multibyte nil)
                               (insert-file-contents-literally "test_image.png")
                               (buffer-string))
                             'png 'data))))
      (message "image data spec: %.1fx%.1f" (car size) (cdr size))))

  ;; Test with different format - using absolute path
  (let ((abs-path (expand-file-name "test_image2.jpg")))
    (when (file-exists-p abs-path)
      (let ((size (image-size abs-path)))
        (message "test_image2.jpg: %dx%d" (car size) (cdr size)))))

  ;; Test error handling
  (condition-case err
      (image-size "nonexistent.png")
    (error (message "Error handling works: %s" (error-message-string err))))

  ;; Test original function access
  (when (fboundp 'image-size-original)
    (message "Original function is available as 'image-size-original'")
    (condition-case err
        (let ((size (image-size-original "test_image.png")))
          (message "Original function result: %dx%d" (car size) (cdr size)))
      (error (message "Original function error (expected in batch mode): %s" (error-message-string err)))))

  ;; Test uninstall/restore
  (message "Testing uninstall...")
  (image-size-fallback-uninstall)
  (message "Original function restored")

  (message "=== Demo Complete ==="))

;; Run the demo if loaded interactively
(when (called-interactively-p 'any)
  (demo-image-size))

(provide 'demo-image-size)

;;; demo-image-size.el ends here
