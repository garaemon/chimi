;;================================================
;; threads.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
;; TODO:
;;   we have to use bordeaux threads.

(declaim (optimize (debug 0)
                   (safety 0)
                   (speed 3)
                   (compilation-speed 0)
                   (space 0)))

(in-package :chimi)

(defmacro with-mutex ((mutex &key (lock t)) &rest body)
  `(when (or (not ,lock) (bordeaux-threads:acquire-lock ,mutex t))
     (unwind-protect
          (locally ,@body)
       (if ,lock (bordeaux-threads:release-lock ,mutex)))))

(defun make-thread (arg)
  (bordeaux-threads:make-thread arg))

(defun make-mutex ()
  (bordeaux-threads:make-lock))

(defun current-thread ()
  (bordeaux-threads:current-thread))
