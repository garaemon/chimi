;;================================================
;; chimi.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(declaim (optimize (speed 3)
		   (debug 0)
		   (safety 0)
		   (compilation-speed 0)
		   (space 0)))

(defpackage :chimi
  (:use #:common-lisp #:sb-unix)
  (:export #:defclass*
	   #:symbol->keyword #:string->symbol
           #:symbol-concatenate
	   #:while
	   #:replace-list #:replace-list-flat1
	   #:debug-print-variable
	   #:all-combination
	   #:getenv
	   #:flatten
	   #:with-mutex #:make-thread #:make-mutex
	   #:all #:any #:find-all #:difference-list
	   #:concatenate-string-with
	   #:find-file-in-path
	   #:get-keyword
	   #:null-output
	   #:random-select
	   #:read-from-file
	   #:list-rank
           #:nlet
           #:with-cl-sequence->cffi-array
           #:current-thread
           #:check-null-error
           ;; system.lisp
           #:pwd #:ls #:cd
	   ;; terminal
	   #:escape-string-with-color
	   ;; log.lisp
	   #:make-logger
	   #:log-format
	   #:stop-logger #:start-logger
	   #:make-log-searcher
	   #:update-datum
	   #:datum-of
	   ;; time.lisp
	   #:local-time-string #:decompose-time-string
	   #:make-mtimer #:start-mtimer #:stop-mtimer
	   ;; gnuplot.lisp
	   #:open-gnuplot #:close-gnuplot
	   #:plot-function #:plot-points
	   #:set-graph-properties
	   #:clear-gnuplot-datum
	   #:one-data-plot
	   #:save-plot-to-file
	   #:save-plot-to-pdf #:save-plot-to-png
	   #:save-plot-to-jpg #:save-plot-to-eps
           ;; cffiext.lisp
           #:defcstruct-accessor
           #:defcstruct-accessors)
  (:documentation
   "chimi package provides the some utilities
    efficient in common. ")
  )

(in-package #:chimi)
  
(defmacro while (test &rest args)
  "CL does not provides while macro.
   ;;; (defvar *hoge-list* '(1 2 3))
   ;;; (while (pop *hoge-list*)
   ;;;    (print *hoge-list*))
   ;;; ; => (2 3) (3) NIL
   "
  `(do ()
       ((not ,test))
     ,@args))

(defun concatenate-string-with (string-list space)
  "concatenate string-list with space.

  ;;; (concatenate-string-with '(\"hoge\" \"fuga\" \"piyo\") \"-\")
  ;;; => \"hoge-fuga-piyo\""
  (declare (type list string-list)
	   (type string space))
  (labels ((%concatenate-string-with
	       (string-list space)
	     (declare (type list string-list)
		      (type string space))
	     (cond ((null string-list)
		    nil)
		   ((null (cdr string-list))
		    (list (car string-list)))
		   (t
		    (append (list (car string-list) space)
			    (%concatenate-string-with (cdr string-list) space))))))
    (apply #'concatenate 'string (%concatenate-string-with string-list space))))

(defun find-file-in-path (fname paths)
  "find fname in paths.
   paths is a list of pathname or string."
  (let ((fname-pathname (if (stringp fname) (pathname fname) fname))
        (paths-pathname (mapcar #'(lambda (p) (if (stringp p) (pathname p) p)) paths)))
    (dolist (p paths-pathname)
      (let ((merge-pathname (merge-pathnames p fname-pathname)))
        (if (probe-file merge-pathname)
            (return merge-pathname)
            nil)))))

(defun get-keyword (key args)
  "returns 'key' 's value in args.

   ;;; (get-keyword :hoge '(:hoge 1 :fuga 2)) -> 1"
  (declare (type list args))
  (cadr (member key args)))

(defun read-from-file (fname)
  "open a file and call read.
   This Function is efficient in read a file dumped
   lisp object."
  (with-open-file (f fname :direction :input)
    (read f)))

(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defmacro defun-inline (name args &rest body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args
       ,@body)))

(defun check-null-error (string check-args)
  (iterate:iter
    (iterate:for a in check-args)
    (if (null a)
        (error (format nil "~A~A" string check-args)))))
