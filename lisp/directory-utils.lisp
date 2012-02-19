(in-package #:librusec)

(defun list-directory-stream (directory)
  ;; This stream is closed by bizarre iter:in-stream later anyway
  (external-program:process-output-stream
   (external-program:start "ls" (list (namestring (truename directory))) :output :stream)))

(defun list-directory-filenames (directory)
  (iter (for line in-stream (list-directory-stream directory) using #'read-line)
        (collect line)))

(defun list-directory-count (directory)
  (iter (for char in-stream (list-directory-stream directory) using #'read-char)
        (counting (char-equal char #\Newline))))

(defmacro-driver (for name in-directory-filenames directory)
  `(for ,name in-stream (list-directory-stream ,directory) using #'read-line))
