;;================================================
;; sample-logging.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :chimi)

(defvar *logger-error*
  (chimi:make-logger :location *error-output*))
(defvar *logger-file*
  (chimi:make-logger :location "hoge.log"))
(defvar *logger-file-and-error*
  (chimi:make-logger :location (list *error-output*
				     "fuga.log")))
				     
(chimi:log-format *logger-error* "message to error output")
(chimi:log-format *logger-file* "message to file")
(chimi:log-format *logger-file-and-error* "message to file and error output")

(chimi:log-format *logger-file-and-error* "message 1")
(chimi:log-format *logger-file-and-error* "message 2")
(chimi:log-format *logger-file-and-error* "message 2")

(defvar *searcher* (chimi:make-log-searcher "hoge.log"))
