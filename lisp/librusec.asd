(asdf:defsystem #:librusec
  :serial t
  :depends-on (#:alexandria
               #:metatilities-base
               #:split-sequence
               #:iterate
               #:external-program
               #:cl-libxml2
               #:cxml
               #:sqlite)
  :components ((:file "package")
               ;; (:file "obsolete") contains no longer used, but possibly interesting code
               (:file "directory-utils")
               (:file "db")
               (:file "librusec")
               (:file "start")))

