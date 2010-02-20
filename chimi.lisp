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
  (:use #:common-lisp 
	#+sbcl #:sb-unix #+sbcl #:sb-posix)
        
  (:export ;; clos.lisp
   #:defclass*
   #:definitializer
   #:make-instance*
   #:derivedp
   ;; gnuplot.lisp
   #:<gnuplot>
   #:open-gnuplot #:close-gnuplot
   #:plot-function #:plot-points
   #:set-graph-properties
   #:clear-gnuplot-datum
   #:one-data-plot
   #:save-plot-to-file
   #:save-plot-to-pdf #:save-plot-to-png
   #:save-plot-to-jpg #:save-plot-to-eps
   ;; io.lisp
   #:null-output
   #:read-from-file
   #:find-file-in-path
   #:file->string
   ;; log.lisp
   #:make-logger
   #:log-format
   #:stop-logger #:start-logger
   #:make-log-searcher
   #:update-datum
   #:datum-of
   ;; sequence.lisp
   #:replace-list #:replace-list-flat1
   #:all-combination
   #:flatten
   #:all #:any #:find-all #:difference-list
   #:random-select
   #:list-rank #:list-rank*
   #:get-keyword
   #:concatenate-string-with
   #:assoc-ref
   #:white-spaces-string-p
   ;; symbol.lisp
   #:symbol->keyword #:string->symbol
   #:symbol-concatenate
   #:enable-debug-reader-macro
   #:debug-print-variable
   ;; system.lisp
   #:pwd #:ls #:cd #:run-program
   #:pathname->string
   #:getenv
   ;; terminal.lisp
   #:escape-string-with-color
   #:value->colored-string
   ;; threads.lisp
   #:with-mutex #:make-thread #:make-mutex
   #:current-thread
   ;; time.lisp
   #:local-time-string #:decompose-time-string
   #:make-mtimer #:start-mtimer #:stop-mtimer
   ;; util-macro.lisp
   #:nlet
   #:check-args-error
   #:while
   #:defun-inline
   ;; cffiext.lisp
   #:defcstruct-accessor
   #:defcstruct-accessors
   #:with-cl-sequence->cffi-array)
  (:documentation
   "chimi package provides the some utilities
    efficient in common. ")
  )
