;;================================================
;; clos.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 3)
                   (safety 3)))


(in-package :chimi)

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
