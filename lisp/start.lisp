(in-package #:librusec)

(defparameter *usage*
  "Usage: construct-db [path-to-fb2s] [path-to-sqlite-db]

Defaults:

- path-to-fb2s: ~/librusec/united/
- path-to-sqlite-db: ~/librusec/librusec.sqlite

After completion you should copy sqlite database to the place where
search will be able to find it.  (By default it is ~/librusec/.)
")

(defun start (argv)
  (when (string-equal (second argv) "--help")
    (write-string *usage* *standard-output*)
    (sb-ext:quit))
  (let ((*verbose* t))
    (handler-case (construct-db)
      (sb-sys:interactive-interrupt ()
        (format *error-output* "~&Keyboard interrupt!~%")
        (sb-ext:quit :unix-status 1)))))
