(in-package #:librusec)

(defvar *verbose* nil)

(defvar *docspath* #p"~/librusec/united/")
(defvar *destpath* #p"~/librusec/librusec.sqlite")

(defvar *create.sql-preloaded* nil "SQLite script string to initialize a new database")

(defun enable-verbose (&optional (enable-p t))
  (setf *verbose* enable-p))

(defgeneric sqlite-exec (db source)
  (:documentation "Execute multiple statements one by one (sqlite3_exec not being provided)"))

(defmethod sqlite-exec (db (source string))
  "Execute multiple statements from string"
  (iter (for bare-statement in (split-sequence #\; source))
        (for clean-statement = (metatilities:strip-whitespace bare-statement))
        (unless (string= clean-statement "")
          (sqlite:execute-non-query db clean-statement))))

(defun read-create.sql-into-string ()
  (read-file-into-string
   (make-pathname :name "create.sql" :defaults (asdf:system-source-directory (asdf:find-system (string-downcase (package-name #.*package*)))))))

(defun initialize-db (db)
  "Initialize empty database for librusec"
  (sqlite-exec db (or *create.sql-preloaded* (read-create.sql-into-string))))

(defun get-file-metadata (path)
  (klacks:with-open-source (s (cxml:make-source path))
    (let ((level 0) context2 context3 document tmp-author)
      (iter (for key = (klacks:peek s))
            (while key)
            (case key
              (:start-element
               (incf level)
               (for name = (make-keyword (read-from-string (klacks:current-lname s))))
               (case level
                 (4
                  (setf context2 name)
                  (when (eq name :sequence)
                    (setf (getf document :seq-name) (klacks:get-attribute s "name"))
                    (setf (getf document :seq-number) (klacks:get-attribute s "number"))))
                 (5
                  (when (eq context2 :author)
                    (setf context3 name)))))

              (:end-element
               (decf level)
               (case level
                 (4 (setf context3 nil))
                 (3
                  (when (eq context2 :author)
                    (push tmp-author (getf document :authors))
                    (setf tmp-author nil))
                  (setf context2 nil)))
               (when (string-equal (klacks:current-lname s) "title-info")
                 (finish)))

              (:characters
               (for data = (klacks:current-characters s))
               (when (eql level 4)
                 (case context2
                   (:genre
                    (push (string-downcase data) (getf document :genres)))
                   ((:lang :src-lang)
                    (setf (getf document context2)
                          (case #1=(make-keyword (read-from-string data))
                                ((:ru :en-gb :en-us :rus :russian :ru-ru :русский) :ru)
                                ((:en :eng :english) :en)
                                ((:uk :ua :ukr :ukrain :ukrian :urkain) :uk)
                                ((:fr :french) :fr)
                                ((:de :deu) :de)
                                ((:be :bel) :bel)
                                ((:sp :spa) :sp)
                                ((:tk :turkmen) :tk)
                                ((:zh :chinese) :zh)
                                (t #1#))))
                   (:author nil)
                   (t (setf (getf document context2) data))))
               (when (and (eql level 5) (eq context2 :author))
                 (setf (getf tmp-author context3) data))))
            (klacks:consume s))
      document)))

(defun join-with (separator strings)
  "Concatenate strings in a list with separator, skipping NILs.
With no strings in the list return NIL, not empty string."
  (flet ((join-two-with (&optional first second)
           (or (and first second (concatenate 'string first separator second))
               first second)))
    (reduce #'join-two-with strings)))

(defun author2goodname (author)
  (or
   (join-with " "
              (mapcar (lambda (key) (getf author key))
                      '(:first-name :middle-name :last-name)))
   (getf author :nickname)
   "+Inconnu"))

(defun update-db-with-file (db path)
  (let* ((id (read-from-string (pathname-name path)))
         (metadata (get-file-metadata path))
         (lang-id (and #1=(getf metadata :lang) (db-getset-lang db #1#)))
         fullname)
    (iter (for genre in (getf metadata :genres))
          (for genre-id = (db-getset-genre db genre))
          (db-set-docgenre db id genre-id))
    (iter (for author in (getf metadata :authors))
          (for name = (author2goodname author))
          (collect name into names)
          (for author-id = (db-getset-author db name author))
          (db-set-docauthor db id author-id)
          (finally (setf fullname (join-with ", " names))))
    (let* ((book-title (or (getf metadata :book-title) "+Inconnu"))
           (seq-number (format nil "~@[~a. ~]" (getf metadata :seq-number)))
           (seq-name   (format nil "~@[~a % ~]"  (getf metadata :seq-name)))
           (fulltitle   (format nil "~@[~a - ~]~@{~@[~a~]~}" fullname seq-number seq-name book-title)))
      (db-set-document db id fulltitle (getf metadata :book-title) lang-id (getf metadata :date)))))

(defun construct-db (&optional (docspath *docspath*) (destpath *destpath*))
  (ignore-errors (delete-file destpath))
  (sqlite:with-open-database (db destpath)
    ;; initialize-db initiates one large transaction covering everything else
    (initialize-db db)
    (sqlite:with-transaction db
      (iter (with *default-pathname-defaults* = (truename docspath))
            (with files = (list-directory-filenames docspath))
            (with total = (length files))
            (for count from 1)
            (for file in files)
            (for path = (truename (make-pathname :name file :defaults *docspath*)))
            (when *verbose*
              ;; ANSI 100 characters back and clear the line
              (format *error-output* "~c[100D~c[K~a of ~a: ~a" #\Esc #\Esc count total file)
              (force-output *error-output*))
            (handler-case
                (update-db-with-file db path)
              (cxml:well-formedness-violation ()
                (format *error-output* "~&~A is broken, skipping~&" (pathname-name path)))))))
  (when *verbose*
    (write-line "" *error-output*))
  t)

(defun preload-resources ()
  "Prelad resources to make standalone binary independent"
  (setf *create.sql-preloaded* (read-create.sql-into-string)))
