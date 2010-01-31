;;================================================
;; system.lisp
;; 
;; Posix like system interface.
;; It works on SBCL and Allegro Common Lisp.
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
  (the list (directory arg)))

(defun getenv (str)
  "returns environment variable's value as string.

   ;;; (getenv \"HOME\") -> \"/path/to/your/home/directory\""
  #+sbcl
  (the string (sb-posix:getenv str))
  #+allegro
  (the string (sys:getenv str)))

(defun cd (&optional (arg (getenv "HOME")))
  #+sbcl
  (sb-posix:chdir arg)
  #+allegro
  (chdir arg)
  (the string (pwd)))                                ;returns current directory

(defun pathname->string (pathname)
  (the string (namestring pathname)))

(defun run-program (program-name &key wait)
  #+sbcl
  (sb-ext:run-program program-name nil
                      :input :stream
                      :output :stream
                      :wait wait
                      :search t)
  #+acl
  (run-shell-command program-name
                     :wait wait
                     :input (or wait :stream)
                     :output :stream))
