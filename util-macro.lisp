;;================================================
;; util-macro.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 0)
                   (safety 0)
                   (speed 3)
                   (compilation-speed 0)
                   (space 0)))

(in-package :chimi)

(defmacro while (test &rest args)
  ;;CL ANSI Spec does not provides while macro.
  ;; (defvar *hoge-list* '(1 2 3))
  ;; (while (pop *hoge-list*)
  ;;    (print *hoge-list*))
  ;; ; => (2 3) (3) NIL
  `(do ()
       ((not ,test))
     ,@args))

(defmacro nlet (n letargs &rest body)
  ;; named let like scheme's let.
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defmacro defun-inline (name args &rest body)
  ;; defun and declaim as inline function
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args
       ,@body)))

(defmacro check-args-error (str &rest args)
  `(progn
     ,@(mapcar #'(lambda (arg)
                   `(%check-args-error ,str ,arg))
               args)))

(defmacro %check-args-error (str (value sym func))
  `(if (funcall ,func ,value)
       (error (format nil "~A ~s" ,str ,sym))
       t))

