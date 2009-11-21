;;================================================
;; clos.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 0)
                   (safety 0)
                   (speed 3)
                   (compilation-speed 0)
                   (space 0)))

(in-package :chimi)

(defmacro defclass* (class-name supers slots &rest args)
  ;;defclass* is a rich wrapper of defclass.
  ;; defclass* automatically define accessor and initarg.
  ;;
  ;; (defclass* <hoge> () ((a 1) (b 2))) -> <hoge>
  ;; (defclass* <fuga> () ((a 100) (B 'piyo))
  ;;     (:documentation .....))
  `(defclass ,class-name
       ,supers
     ,(mapcar
       #'(lambda (x)
	   (if (atom x)
	       (list x :initform nil
		     :initarg (symbol->keyword x)
		     :accessor (string->symbol
                                (concatenate 'string
                                             (string x)
                                             "-of")))
	       (list (car x) :initform (cadr x)
		     :initarg (symbol->keyword (car x))
		     :accessor (string->symbol
                                (concatenate 'string
                                             (string (car x))
                                             "-of")))))
       slots)
     ,@args))

#|
(DEFINITIALIZER ((self hoge) &key (aaa) (bbb) (ccc))
    (print (list aaa bbb ccc)))
|#
(defmacro definitializer (((self class) &rest args) &rest body)
  (let ((%args (gensym)))
    `(defmethod initialize-instance :after
         ((,self ,class) &rest ,%args)
       (destructuring-bind (,args)
           (list ,%args)
         ,@body))))
