;;================================================
;; symbol.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 0)
                   (safety 0)
                   (speed 3)
                   (compilation-speed 0)
                   (space 0)))

(in-package :chimi)

(defun symbol->keyword (sym)
  "convert a symbol to keyword.
   this function is very slow, because it uses read-from-string.
  ;;; (symbol->keyword 'hoge) -> :hoge"
  (declare (type symbol sym))
  (read-from-string (concatenate 'string ":" (string sym))))

(defun string->symbol (str)
  "convert string to symbol.
   this function is very slow, because it uses read-from-string.
  ;;; (string->symbol \"hoge\") -> hoge"
  (declare (type string str))
  (read-from-string str))

(defun symbol-concatenate (a b)
  (string->symbol (concatenate 'string (string a) (string b))))

(defmacro debug-print-variable (sym &optional (func-name nil))
  "print symbol and it's value.

  ;;; (defvar *hoge* 100)
  ;;; (debug-print-variable *hoge*) -> \"*hoge* -> 100\""
  `(progn
     (if ',func-name
         (format t "~s -> ~s -- ~s --~%" ',sym ,sym ',func-name)
         (format t "~s -> ~s~%" ',sym ,sym))))
