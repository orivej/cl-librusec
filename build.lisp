#-quicklisp
(progn
  (or (probe-file "quicklisp.lisp")
      (sb-ext:run-program "wget" '("http://beta.quicklisp.org/quicklisp.lisp") :search t))
  (or (probe-file "quicklisp.lisp")
      (sb-ext:run-program "curl" '("-O" "http://beta.quicklisp.org/quicklisp.lisp" :search t)))
  (load "quicklisp.lisp")
  (or (probe-file "quicklisp")
      (funcall (find-symbol "INSTALL" 'quicklisp-quickstart) :path "quicklisp/"))
  (load "quicklisp/setup.lisp"))

(ql:quickload :buildapp)
(push (truename "lisp/") asdf:*central-registry*)
(declaim (optimize speed))
(ql:quickload :librusec)

(defun build-construct-db (&optional (executable "construct-db"))
  (buildapp::main
   (list "sbcl"
         #+sb-core-compression "--compress-core"
         "--asdf-path"   (sb-ext:native-namestring (truename "lisp/"))
         "--asdf-tree"   (sb-ext:native-namestring
                          (merge-pathnames "dists/" ql:*quicklisp-home*))
         "--load-system" "librusec"
         "--eval"        "(librusec:preload-resources)"
         "--entry"       "librusec:start"
         "--output"      (sb-ext:native-namestring (merge-pathnames executable)))))

(build-construct-db)

#-sb-core-compression
(format *error-output* "WARNING!  SBCL compiled without :sb-core-compression.~%Resulting binary will be large!~%")

(sb-ext:quit)
