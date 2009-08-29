;;================================================
;; log.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(declaim (optimize (speed 3)
		   (debug 0)
		   (safety 0)
		   (compilation-speed 0)
		   (space 0)))

(in-package #:chimi)

(defclass* <logger>
    ()
  ((logger nil)                         ;interface to log4cl
   (file-name nil)                      ;file name of log file
   (verbose nil))
  (:documentation
   "<logger> class is a logging interface class.
    You can choose ")
  )

;; for printing
(defmethod print-object ((logger <logger>) stream)
  (print-unreadable-object (logger stream :type t :identity t)
    (format stream "~A :verbose ~A"
            (file-name-of logger)
            (verbose-of logger))))

(defun make-logger (&key
                    (file-name nil)
                    (verbose nil)
                    (syslog nil))
  "make a <logger> instance.

   We recomend to use this function, make-logger,
   instead of using directly make-instance."
  (if (null file-name)
      (error "You have to set :file-name"))
  (let ((logger (make-instance 'log4cl::logger)))
    ;; rolling file appender
    (let ((rfa (make-instance 'log4cl::rolling-file-appender :file file-name)))
      (log4cl::add-appender logger rfa))
    ;; when verbose, we add console-appender
    (when verbose
      (let ((va (make-instance 'log4cl::console-appender)))
        (log4cl::add-appender logger va)))
    (when syslog
      (let ((sa (make-instance 'log4cl::syslog-appender)))
        (log4cl::add-appender logger sa)))
    (make-instance '<logger>
                   :file-name file-name
                   :verbose verbose
                   :logger logger)))

(defmethod log-format ((logger <logger>) level (str string) &rest args)
  "format to a <logger>.

   usage:
   ;;; (log-format logger :warning
   ;;;       \"~A is must be a integer\" val)
   "
  ;; take an error when level is not suitable.
  (let ((levels '(:error :info :warning :fatal)))
    (if (not (member level levels))
        (error "unkown level ~A" level)))
  (log4cl::log-message (logger-of logger) level (apply #'format nil str args)))
