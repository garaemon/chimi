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
  (:use #:common-lisp)
  (:export #:defclass*
	   #:while
	   #:replace-list)
  (:documentation
   "chimi package provides some utilities efficient in common.
   ")
  )

(in-package #:chimi)

(defun symbol->keyword (sym)
  "convert a symbol to keyword.

  ;;; (symbol->keyword 'hoge) -> :hoge"
  (declare (type symbol sym))
  (read-from-string (concatenate 'string ":" (string sym))))

(defun string->symbol (str)
  "convert string to symbol.

  ;;; (string->symbol \"hoge\") -> hoge"
  (declare (type string str))
  (read-from-string str))

(defmacro defclass* (class-name supers slots &rest args)
  "defclass* is a rich wrapper of defclass.
   defclass* automatically define accessor and initarg.

   ;;; (defclass* <hoge> () ((a 1) (b 2))) -> <hoge>
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

  ;;; (REPLACE-LIST '(1 2 3 4 5) '(1 2 3) '(2 4 6)) -> (2 4 6 4 5)"
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
  `(progn
     (if ',func-name
         (format t "~s -> ~s -- ~s --~%" ',sym ,sym ',func-name)
         (format t "~s -> ~s~%" ',sym ,sym))
     ))

(defun all-combination (lst)
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
                        (push (append p (list ta)) ret))
                      )
                    ret)))
          lst :initial-value :nil))

(defun getenv (str)
  (declare (type string str))
  #+sbcl
  (SB-POSIX:GETENV str))

(defmacro defvirtualmethod (method-name args)
  `(defmethod ,method-name ,args
     (error "this is a virtual method ~A" ',method-name))
  )

(defun flatten (lst)
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
  (cond ((null list)
         t)
        ((funcall proc (car list))
         ;;(cons (car list) (all proc (cdr list))))
         (all proc (cdr list)))
        (t
         ;;(all proc (cdr list)))))
         nil)))

(defun any (proc list)
  (declare (type list list))
  (cond ((null list)
         nil)
        ((funcall proc (car list))
         (car list))
        (t
         (any proc (cdr list)))))

(defun find-all (proc list)
  (declare (type list list))
  (cond ((null list)
         nil)
        ((funcall proc (car list))
         (cons (car list) (find-all proc (cdr list))))
        (t
         (find-all proc (cdr list)))))

(defun difference-list (a b)
  (cond ((null a)
         nil)
        ((equal (car a) (car b))
         (difference-list (cdr a) (cdr b)))
        (t
         (cons (car a) (difference-list (cdr a) (cdr b))))))

(defun local-time-string ()
  "現在の年, 月, 日, 時刻を文字列として返す.
   フォーマットはYYYY-MM-DD-hh-mm-ss."
  (multiple-value-bind (sec min hour day mon year)
      (decode-universal-time (get-universal-time))
    (format nil "~2,'0D-~2,'0D-~2,'0D-~2,'0D-~2,'0D-~2,'0D" year mon day hour min sec)))

(defun find-file-in-path (fname paths)
  "fnameをpathsの中から探す.
   pathsはpathnameまたはstringのリスト."
  (let ((fname-pathname (if (stringp fname) (pathname fname) fname))
        (paths-pathname (mapcar #'(lambda (p) (if (stringp p) (pathname p) p)) paths)))
    (dolist (p paths-pathname)
      (let ((merge-pathname (merge-pathnames p fname-pathname)))
        (if (probe-file merge-pathname)
            (return merge-pathname)
            nil)))))

(defun get-keyword (key args)
  (cadr (member key args)))

