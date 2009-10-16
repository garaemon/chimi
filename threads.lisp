;;================================================
;; threads.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 3)
                   (safety 3)))


(in-package :chimi)

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

(defun current-thread ()
  #+sbcl
  sb-thread:*current-thread*)
