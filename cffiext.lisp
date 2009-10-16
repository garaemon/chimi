;;================================================
;; cffiext.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 3)
                   (safety 3)))


(in-package :chimi)

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

(defmacro with-cl-sequence->cffi-array ((sym v type) &rest args)
  (let ((i (gensym)))
    `(cffi:with-foreign-object
         (,sym ,type (length ,v))
       (iterate:iter                       
        (iterate:for ,i from 0 to (1- (length ,v)))
        (setf (cffi:mem-aref ,sym ,type ,i) (elt ,v ,i)))
       ,@args)))
