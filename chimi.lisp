;;================================================
;; chimi.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(declaim (optimize (speed 3)
		   (debug 0)
		   (safety 0)
		   (compilation-speed 0)
		   (space 0)))

(defpackage :chimi
  (:use #:common-lisp #:sb-unix)
  (:export #:defclass*
	   #:symbol->keyword #:string->symbol
           #:symbol-concatenate
	   #:while
	   #:replace-list #:replace-list-flat1
	   #:debug-print-variable
	   #:all-combination
	   #:getenv
	   #:flatten
	   #:with-mutex #:make-thread #:make-mutex
	   #:all #:any #:find-all #:difference-list
	   #:concatenate-string-with
	   #:find-file-in-path
	   #:get-keyword
	   #:null-output
	   #:random-select
	   #:read-from-file
	   #:list-rank
           #:nlet
           #:with-cl-sequence->cffi-array
           #:current-thread
           #:check-args-error
           ;; system.lisp
           #:pwd #:ls #:cd
	   ;; terminal
	   #:escape-string-with-color
	   ;; log.lisp
	   #:make-logger
	   #:log-format
	   #:stop-logger #:start-logger
	   #:make-log-searcher
	   #:update-datum
	   #:datum-of
	   ;; time.lisp
	   #:local-time-string #:decompose-time-string
	   #:make-mtimer #:start-mtimer #:stop-mtimer
	   ;; gnuplot.lisp
	   #:open-gnuplot #:close-gnuplot
	   #:plot-function #:plot-points
	   #:set-graph-properties
	   #:clear-gnuplot-datum
	   #:one-data-plot
	   #:save-plot-to-file
	   #:save-plot-to-pdf #:save-plot-to-png
	   #:save-plot-to-jpg #:save-plot-to-eps
           ;; cffiext.lisp
           #:defcstruct-accessor
           #:defcstruct-accessors)
  (:documentation
   "chimi package provides the some utilities
    efficient in common. ")
  )


  
