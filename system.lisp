;;================================================
;; system.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :chimi)

(defun pwd ()
  (sb-posix:getcwd))

(defun ls (&optional (arg (pwd)))
  (cl-fad:list-directory arg))
  

(defun cd (&optional (arg (getenv "HOME")))
  (sb-posix:chdir arg)
  (pwd))
