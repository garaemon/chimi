;;================================================
;; util-macro.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 3)
                   (safety 3)))


(in-package :chimi)

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

(defmacro defvirtualmethod (method-name args)
  `(defmethod ,method-name ,args
     (error "this is a virtual method ~A" ',method-name)))
