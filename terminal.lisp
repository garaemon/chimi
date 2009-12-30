;;================================================
;; terminal.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 0)
                   (safety 0)
                   (speed 3)
                   (compilation-speed 0)
                   (space 0)))

(in-package :chimi)

;; cl-interpol
;;(cl-interpol:enable-interpol-syntax)
;;(alexandria:define-constant +terminal-escape+ #?"\e[" :test #'string=)
(alexandria:define-constant +terminal-escape+
    (format nil "~A[" #\Escape)
  :test #'string=)
(alexandria:define-constant +terminal-escape-finish-char+ #\m)
(alexandria:define-constant +terminal-black+ "0;30" :test #'string=)
(alexandria:define-constant +terminal-red+ "0;31" :test #'string=)
(alexandria:define-constant +terminal-green+ "0;32" :test #'string=)
(alexandria:define-constant +terminal-brown+ "0;33" :test #'string=)
(alexandria:define-constant +terminal-blue+ "0;34" :test #'string=)
(alexandria:define-constant +terminal-purple+ "0;35" :test #'string=)
(alexandria:define-constant +terminal-cyan+ "0;36" :test #'string=)
(alexandria:define-constant +terminal-light-gray+ "0;37" :test #'string=)
(alexandria:define-constant +terminal-dark-gray+ "1;30" :test #'string=)
(alexandria:define-constant +terminal-light-red+ "1;31" :test #'string=)
(alexandria:define-constant +terminal-light-green+ "1;32" :test #'string=)
(alexandria:define-constant +terminal-yellow+ "1;33" :test #'string=)
(alexandria:define-constant +terminal-light-blue+ "1;34" :test #'string=)
(alexandria:define-constant +terminal-light-purple+ "1;35" :test #'string=)
(alexandria:define-constant +terminal-light-cyan+ "1;36" :test #'string=)
(alexandria:define-constant +terminal-white+ "1;37" :test #'string=)

(defun keyword->color-string (color-sym)
  (case color-sym
    (:black +terminal-black+)
    (:red +terminal-red+)
    (:green +terminal-green+)
    (:brown +terminal-brown+)
    (:blue +terminal-blue+)
    (:purple +terminal-purple+)
    (:cyan +terminal-cyan+)
    (:light-gray +terminal-light-gray+)
    (:dark-gray +terminal-dark-gray+)
    (:light-red +terminal-light-red+)
    (:light-green +terminal-light-green+)
    (:yellow +terminal-yellow+)
    (:light-blue +terminal-light-blue+)
    (:light-purple +terminal-light-purple+)
    (:light-cyan +terminal-light-cyan+)
    (:white +terminal-white+)
    ))

(defun escape-string-with-color (str color)
  (let ((color-str (keyword->color-string color)))
    (format nil "~A~A~A~A~A~A"
	    +terminal-escape+ color-str +terminal-escape-finish-char+
	    str
	    +terminal-escape+ +terminal-escape-finish-char+)))

(defun value->colored-string (val color)
  (escape-string-with-color (format nil "~A" val) color))
