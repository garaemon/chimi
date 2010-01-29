;;================================================
;; io.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 0)
                   (safety 0)
                   (speed 3)
                   (compilation-speed 0)
                   (space 0)))


(in-package :chimi)

(defmacro null-output (&rest args)
  "the output of all sexp in null-output is redirect to /dev/null.
   "
  ;;*standard-output* *error-outuput*
  (let ((f (gensym))
        (standard-output (gensym))
        (error-output (gensym)))
    `(let ((,standard-output *standard-output*)
           (,error-output *error-outuput*))
       (with-open-file (,f "/dev/null" :direction :output :if-exists :append)
         (unwind-protect
              (progn
                (setq *standard-output* ,f)
                (setq *error-outuput* ,f)
                ,@args)
           (setq *standard-output* ,standard-output)
           (setq *error-outuput* ,error-output))))))

(defun read-from-file (fname)
  "open a file and call read.
   This Function is efficient in read a file dumped
   lisp object."
  (let ((ret nil))
    (with-open-file (f fname :direction :input)
      (while (push (read f nil nil) ret)))
    (reverse ret)))

(defun find-file-in-path (fname paths)
  "find fname in paths.
   paths is a list of pathname or string."
  (let ((fname-pathname (if (stringp fname) (pathname fname) fname))
        (paths-pathname (mapcar #'(lambda (p)
                                    (if (stringp p) (pathname p) p))
                                paths)))
    (dolist (p paths-pathname)
      (let ((merge-pathname (merge-pathnames p fname-pathname)))
        (if (probe-file merge-pathname)
            (return merge-pathname)
            nil)))))

(defun file->string (fname)
  (with-open-file (f fname :direction :input)
    (let ((str (make-string-output-stream)))
      (let ((tmp nil))
        (while (setq tmp (read-line f nil nil))
          (write-string tmp str))
        (get-output-stream-string str)))))

