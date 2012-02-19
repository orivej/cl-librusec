(in-package #:librusec)

(defun db-get-genre (db genre)
  (sqlite:execute-single db "select id from genre where genre=?" genre))

(defun db-set-genre (db genre)
  (sqlite:execute-non-query db "insert into genre (genre) values (?)" genre)
  (sqlite:last-insert-rowid db))

(defun db-getset-genre (db genre)
  (or (db-get-genre db genre)
      (db-set-genre db genre)))

(defun db-set-docgenre (db id genre-id)
  (sqlite:execute-non-query db "insert into docgenre values (?, ?)" id genre-id))

(defun db-get-author (db name)
  (sqlite:execute-single db "select id from author where name=?" name))

(defun db-set-author (db name author)
  (apply
   #'sqlite:execute-non-query
   db "insert into author values (?, ?, ?, ?, ?, ?, ?, ?)"
   nil name (string-downcase name)
   (mapcar (lambda (key) (getf author key))
           '(:first-name :middle-name :last-name :nickname :email)))
  (sqlite:last-insert-rowid db))

(defun db-getset-author (db name author)
  (or (db-get-author db name)
      (db-set-author db name author)))

(defun db-set-docauthor (db id author-id)
  (sqlite:execute-non-query db "insert into docauthor values (?, ?)" id author-id))

(defun lang->string (lang)
  (string-downcase (symbol-name lang)))

(defun db-get-lang (db lang)
  (sqlite:execute-single db "select id from lang where lang=?" (lang->string lang)))

(defun db-set-lang (db lang)
  (sqlite:execute-non-query db "insert into lang (lang) values (?)" (lang->string lang))
  (sqlite:last-insert-rowid db))

(defun db-getset-lang (db lang)
  (or (db-get-lang db lang)
      (db-set-lang db lang)))

(defun db-set-document (db id fulltitle title lang-id date)
  (sqlite:execute-non-query
   db "insert into document values (?, ?, ?, ?, ?, ?, ?)"
   id fulltitle (string-downcase fulltitle) title (string-downcase title) lang-id date))
