;;================================================
;; system.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 0)
                   (safety 0)
                   (speed 3)
                   (compilation-speed 0)
                   (space 0)))

(in-package :chimi)

(defun pwd ()
  (sb-posix:getcwd))

(defun ls (&optional (arg (pwd)))
  (cl-fad:list-directory arg))
  

(defun cd (&optional (arg (getenv "HOME")))
  (sb-posix:chdir arg)
  (pwd))

(defun getenv (str)
  "returns environment variable's value as string.

   ;;; (getenv \"HOME\") -> \"/path/to/your/home/directory\""
  (declare (type string str))
  #+sbcl
  (SB-POSIX:GETENV str))
