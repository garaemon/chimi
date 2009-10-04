;;================================================
;; system.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(in-package :chimi)

(defun pwd ()
  (sb-posix:getcwd))

(defun ls (&optional (arg (pwd)))
  
  )

(defun cd (&optional (arg (getenv "HOME")))
  (sb-posix:chdir arg)
  (pwd))
