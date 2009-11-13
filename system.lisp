;;================================================
;; system.lisp
;; 
;; Posix like system 
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
  #+sbcl
  (sb-posix:getcwd)
  #+allegro
  (current-directory))

(defun ls (&optional (arg (pwd)))
  (directory arg))

(defun cd (&optional (arg (getenv "HOME")))
  #+sbcl
  (progn
    (sb-posix:chdir arg)
    (pwd))
  #+allegro
  (chdir arg))

(defun getenv (str)
  "returns environment variable's value as string.

   ;;; (getenv \"HOME\") -> \"/path/to/your/home/directory\""
  #+sbcl
  (sb-posix:getenv str)
  #+allegro
  (sys:getenv str))
