(in-package #:librusec)

(defun recover-broken-xml-stream (path)
  (external-program:process-output-stream
   (external-program:start
    "xmllint" (list "--recover" "--encode" "utf-8" path)
    :output :stream :error nil)))
