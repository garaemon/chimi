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
           #:defcstruct-accessor
           #:defcstruct-accessors)
  (:documentation
   "chimi package provides the some utilities
    efficient in common. 
   ")
  )

(in-package #:chimi)

(cl-interpol:enable-interpol-syntax)
(eval-when (:compile-toplevel)
  (alexandria:define-constant +terminal-escape+ #?"\e[" :test #'string=)
  (alexandria:define-constant +terminal-escape-finish-char+ #\m)
  (alexandria:define-constant +terminal-black+ "0;30" :test #'string=)
  (alexandria:define-constant +terminal-red+ "0;31" :test #'string=)
  (alexandria:define-constant +terminal-green+ "0;32" :test #'string=)
  (alexandria:define-constant +terminal-brown+ "0;33" :test #'string=)
  (alexandria:define-constant +terminal-blue+ "0;34" :test #'string=)
  (alexandria:define-constant +terminal-purple+ "0;35" :test #'string=)
  (alexandria:define-constant +terminal-cyan+ "0;36" :test #'string=)
  (alexandria:define-constant +terminal-light-gray+ "0;37" :test #'string=)
  (alexandria:define-constant +terminal-dark-gray+ "1;30" :test #'string=)
  (alexandria:define-constant +terminal-light-red+ "1;31" :test #'string=)
  (alexandria:define-constant +terminal-light-green+ "1;32" :test #'string=)
  (alexandria:define-constant +terminal-yellow+ "1;33" :test #'string=)
  (alexandria:define-constant +terminal-light-blue+ "1;34" :test #'string=)
  (alexandria:define-constant +terminal-light-purple+ "1;35" :test #'string=)
  (alexandria:define-constant +terminal-light-cyan+ "1;36" :test #'string=)
  (alexandria:define-constant +terminal-white+ "1;37" :test #'string=))

(defun symbol->keyword (sym)
  "convert a symbol to keyword.
   this function is very slow, because it uses read-from-string.
  ;;; (symbol->keyword 'hoge) -> :hoge"
  (declare (type symbol sym))
  (read-from-string (concatenate 'string ":" (string sym))))

(defun string->symbol (str)
  "convert string to symbol.
   this function is very slow, because it uses read-from-string.
  ;;; (string->symbol \"hoge\") -> hoge"
  (declare (type string str))
  (read-from-string str))

(defun symbol-concatenate (a b)
  (string->symbol (concatenate 'string (string a) (string b))))
                  

(defmacro defclass* (class-name supers slots &rest args)
  "defclass* is a rich wrapper of defclass.
   defclass* automatically define accessor and initarg.

   ;;; (defclass* <hoge> () ((a 1) (b 2))) -> <hoge>
   ;;; (defclass* <fuga> () ((a 100) (B 'piyo))
   ;;;     (:documentation .....))
   "
  `(defclass ,class-name
       ,supers
     ,(mapcar
       #'(lambda (x)
	   (if (atom x)
	       (list x :initform nil
		     :initarg (symbol->keyword x)
		     :accessor (string->symbol (concatenate 'string (string x) "-of")))
	       (list (car x) :initform (cadr x)
		     :initarg (symbol->keyword (car x))
		     :accessor (string->symbol (concatenate 'string (string (car x)) "-of")))))
       slots)
     ,@args))
  
(defmacro while (test &rest args)
  "CL does not provides while macro.

   ;;; (while (pop *hoge-list*)
   ;;;    (print *hoge-list*))
   "
  `(do ()
       ((not ,test))
     ,@args))

(defun replace-list (target from to &key (test #'equal))
  "replace 'from' in 'target' to 'to'

  ;;; (replace-list '(1 2 3 4 5) '(1 2 3) '(2 4 6)) -> (2 4 6 4 5)"
  (declare (type list from to))
  (cond ((null target)
         nil)
        ((listp target)
         (cons (replace-list (car target) from to :test test)
               (replace-list (cdr target) from to :test test)))
        (t
         (let ((location (position target from)))
           (if location
               (elt to location)
               target)))))

;; 展開方法が,@的な
(defun replace-list-flat1 (datum froms tos &key (test #'equal))
  (let ((ret nil))
    (dolist (d datum)
      (let ((flag nil))
        (dotimes (fi (length froms))
          (if (funcall test (elt froms fi) d)
              (setq flag (elt tos fi)))
          )
        (if flag
            (push (flatten flag) ret)
            (push d ret))
        ))
    (reverse ret)))

(defmacro debug-print-variable (sym &optional (func-name nil))
  "print symbol and it's value.

  ;;; (defvar *hoge* 100)
  ;;; (debug-print-variable *hoge*) -> \"*hoge* -> 100\""
  `(progn
     (if ',func-name
         (format t "~s -> ~s -- ~s --~%" ',sym ,sym ',func-name)
         (format t "~s -> ~s~%" ',sym ,sym))))

(defun all-combination (lst)
  "make a list of all combination of lst

  ;; (all-combination '((1) (2) (3))) -> ((1 2 3))
  "
  (declare (type list lst))
  (reduce #'(lambda (prev target)
              (declare (type list target))
              (if (eq prev :nil)
                  (mapcar #'list target)
                  (let ((ret nil))
                    (declare (type list ret))
                    (dolist (ta target)
                      (dolist (p prev)
                        (declare (type list p))
                        (push (append p (list ta)) ret)
			))
                    ret)))
          lst :initial-value :nil))

(defun getenv (str)
  "returns environment variable's value as string.

   ;;; (getenv \"HOME\") -> \"/path/to/your/home/directory\""
  (declare (type string str))
  #+sbcl
  (SB-POSIX:GETENV str))

(defmacro defvirtualmethod (method-name args)
  `(defmethod ,method-name ,args
     (error "this is a virtual method ~A" ',method-name))
  )

(defun flatten (lst)
  "flatten a list.

   ;;; (flatten '((1 2) (3 4))) -> (1 2 3 4)"
  (cond ((null lst)
         nil)
        ((atom lst)
         (list lst))
        (t                              ;lst = list
         ;;(declare (type list lst))
         (append (flatten (car lst))
                 (flatten (cdr lst)))
         )))

(defmacro with-mutex (mutex &rest body)
  #+sbcl
  `(sb-thread:with-mutex ,mutex
     ,@body))

(defun make-thread (arg)
  #+sbcl
  (sb-thread:make-thread arg))

(defun make-mutex ()
  #+sbcl
  (sb-thread:make-mutex))

(defun all (proc list)
  (declare (type list list))
  (cond ((null list)
         t)
        ((funcall proc (car list))
         (all proc (cdr list)))
        (t
         nil)))

(defun any (proc list)
  (declare (type list list))
  (cond ((null list)
         nil)
        ((funcall proc (car list))
         (car list))
        (t
         (any proc (cdr list)))))

;; find-all = remove-if-not??
(defun find-all (proc list)
  "is this same to remove-if-not??"
  (declare (type list list))
  (cond ((null list)
         nil)
        ((funcall proc (car list))
         (cons (car list) (find-all proc (cdr list))))
        (t
         (find-all proc (cdr list)))))

(defun difference-list (a b)
  "returns difference between a and b

   ;;; (difference-list '(1 2 3) '(1 1 3)) -> '(2)"
  (declare (type list a b))
  (cond ((null a)
         nil)
        ((equal (car a) (car b))
         (difference-list (cdr a) (cdr b)))
        (t
         (cons (car a) (difference-list (cdr a) (cdr b))))))

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

(defmacro null-output (&rest args)
  "the output of all sexp in null-output is redirect to /dev/null.
   "
  ;;*standard-output* *error-outuput*
  (let ((f (gensym)))
    `(with-open-file (,f "/dev/null" :direction :output :if-exists :append)
       (let ((*standard-output* ,f)
	     (*error-outuput* ,f))
	 ,@args))))

(defun random-select (list)
  "returns a element of list randomly"
  (declare (type list list))
  (let ((len (length list)))
    (elt list (random len))))

(defun read-from-file (fname)
  "open a file and call read.
   This Function is efficient in read a file dumped
   lisp object."
  (with-open-file (f fname :direction :input)
    (read f)))

(defun list-rank (lst)
  "returns rank of lst.

   ;;; (list-rank nil) -> 0
   ;;; (list-rank '(1 2 3)) -> 1
   ;;; (list-rank '((1 2 3))) -> 2"
  (if (atom lst)
      0
      (1+ (list-rank (car lst)))))

(defun keyword->color-string (color-sym)
  (case color-sym
    (:black +terminal-black+)
    (:red +terminal-red+)
    (:green +terminal-green+)
    (:brown +terminal-brown+)
    (:blue +terminal-blue+)
    (:purple +terminal-purple+)
    (:cyan +terminal-cyan+)
    (:light-gray +terminal-light-gray+)
    (:dark-gray +terminal-dark-gray+)
    (:light-red +terminal-light-red+)
    (:light-green +terminal-light-green+)
    (:yellow +terminal-yellow+)
    (:light-blue +terminal-light-blue+)
    (:light-purple +terminal-light-purple+)
    (:light-cyan +terminal-light-cyan+)
    (:white +terminal-white+)
    ))

(defun escape-string-with-color (str color)
  (let ((color-str (keyword->color-string color)))
    (format nil "~A~A~A~A~A~A"
	    +terminal-escape+ color-str +terminal-escape-finish-char+
	    str
	    +terminal-escape+ +terminal-escape-finish-char+)))

(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defmacro with-cl-sequence->cffi-array ((sym v type) &rest args)
  (let ((i (gensym)))
    `(cffi:with-foreign-object
         (,sym ,type (length ,v))
       (iterate:iter                       
        (iterate:for ,i from 0 to (1- (length ,v)))
        (setf (cffi:mem-aref ,sym ,type ,i) (elt ,v ,i)))
       ,@args)))

(defun current-thread ()
  sb-thread:*current-thread*)

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
    

#|
(defcstruct-accessor event- type 'XEvent)
|#
(defmacro defcstruct-accessor (prefix slot c-type)
  (let ((func-name (chimi:symbol-concatenate prefix slot)))
    `(progn
       ;;(format t "#:~A~%" ',func-name)
       (defun ,func-name (arg)
         (cffi:foreign-slot-value arg ,c-type ',slot))
       (defun (setf ,func-name) (arg value)
         (setf (cffi:foreign-slot-value arg ,c-type ',slot) value))
       )))

(defmacro defcstruct-accessors (prefix slots c-type)
  `(progn
     ,@(mapcar #'(lambda (slot)
                  `(defcstruct-accessor ,prefix ,slot ,c-type))
              slots)))
