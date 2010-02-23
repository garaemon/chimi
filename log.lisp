;;================================================
;; log.lisp
;;
;; this file provides a interface to log5.
;; log5 <http://common-lisp.net/project/log5/>
;; 
;; written by R.Ueda (garaemon)
;;================================================

;; log5's important elements are:
;;   category
;;   output
;;   sender
;;
;; in chimi, output is determined.
;; And, category and sender is a containized into
;; <logger> class.
;; 
;; log5 is a bery flexible logger library, but
;; in chimi, we reduce flexibility but increase
;; simplicity.
;;
(declaim (optimize (speed 3)
		   (debug 0)
		   (safety 0)
		   (compilation-speed 0)
		   (space 0)))

(in-package #:chimi)

;; define log5 output
(log5:defoutput open-paren #\()
(log5:defoutput close-paren #\))
(log5:defoutput open-big-paren #\[)
(log5:defoutput close-big-paren #\])
(log5:defoutput newline #\Newline)
(log5:defoutput human-time (chimi:local-time-string))

;; logging APIs
(defclass* <logger>
    ()
  ((category nil)			;log5 categories
   (senders nil)			;log5 senders
   (name nil))				;log5 requires unique name.
  (:documentation
   "<logger> class is a container of sender and category of log5."
   ))

;; customize print of <logger>
(defmethod print-object ((object <logger>) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s" (name-of object))))

(defun logger-log5-format (type)
  "returns output spec.
   type must be :paren or :no-paren.
  
   :paren format is ([2009-01-01-18-30-30] \"WARNING\" message)
   :no-paren format is [2009-01-01-18-30-30] \"WARNING\" message.
   "
  (case type
    (:paren '(open-paren
	      open-big-paren human-time close-big-paren
	      log5:category log5:message close-paren newline))
    (:no-paren '(open-big-paren human-time close-big-paren
		 log5:category log5:message newline))
    (t (error "unkown type ~A" type))
    ))

(defun make-logger (&key
		    (name nil)
		    (category-spec nil)
		    (location *error-output*)
		    (format-type :paren))
  "make a <logger> instance.

  you can set :name for category name.
  if you call this function with :name nil, 
  category name is determined randomly by using gensym.
  
  :location must be a stream, a string of path to the log file,
  or the list of them.
  
  :format-type default is :paren. You can nor choose :no-paren mode.
  If you want to use search API, you have to use :paren mode.
  "
  (let ((ret (make-instance '<logger>))
	(locations (if (listp location) location (list location))))
    ;; if you don't set :name,
    ;; it creates random name using gensym.
    (let ((category-name (or name (gensym))))
      (setf (name-of ret) category-name)
      ;; make a new log5 category
      (setf (category-of ret)
	    (log5::update-category-spec
	     category-name category-spec))
      ;; make a new log5 sender
      (setf (senders-of ret)
      (iterate:iter
       (iterate:for location in locations)
       (let ((sender-name
	      (string->symbol (format nil "~A-sender" (string category-name)))))
	 (iterate:collect
	   (log5::start-sender-fn sender-name ; sender name
				  (list category-name) ; category spec
				  (logger-log5-format format-type) ; output spec
				  'log5::stream-sender ; sender type
				  :location location)))))
      ret)))

(defmethod log-format ((logger <logger>) str &rest args)
  (let ((log-str (apply #'format nil str args)))
    (log5::handle-message
     (log5::category-id (category-of logger))
     log-str)
    log-str))

(defmethod stop-logger ((logger <logger>))
  (iterate:iter
   (iterate:for sender in (senders-of logger))
   (let ((sender-name (slot-value sender 'log5::name)))
     (log5::stop-sender-fn sender-name :warn-if-not-found-p nil)))
  logger)

(defmethod start-logger ((logger <logger>))
  (iterate:iter
   (iterate:for sender in (senders-of logger))
   (unless (find sender (log5::log5-senders (log5::log-manager)))
     (push sender (log5::log5-senders (log5::log-manager)))))
  logger)

;; search APIs
(defclass* <log-searcher>
    ()
  ((file-name nil)
   (datum nil))
  (:documentation ""))

;; customize print of <log-searcher>
(defmethod print-object ((object <log-searcher>) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s" (file-name-of object))))

(defclass* <log-context>
    ()
  ((time nil)
   (category nil)
   (message nil)))

(defun sexp->log-context (sexp)
  ;; sexp format = ( [time] "Context" "Message")
  (let* ((time (cadr sexp))
	 (category (cadddr sexp))
	 (message (cadr (cdddr sexp))))
    (make-instance '<log-context>
		   :time (string time)
		   :category category
		   :message message)))

(defun make-log-searcher (fname)
  "make a <log-searcher> instance from log file."
  (let ((ret (make-instance '<log-searcher> :file-name fname)))
    (update-datum ret)
    ret))

(defmethod update-datum ((searcher <log-searcher>))
  "open file and ..."
  (with-open-file (f (file-name-of searcher) :direction :input)
    (let ((sexp nil))
      (while (setf sexp (read f nil nil))
	(push (sexp->log-context sexp) (datum-of searcher)))))
  (datum-of searcher))

(defmethod search-by-time ((searcher <log-searcher>)
			   &key
			   (from nil)
			   (to nil)
			   (allow-equal t))
  "With this function, you can filter the log-messages
   by log time.

   Keyword :from and :to is must be a time-string, a time list or
   a <time> instance."
  ;; internaly, time is represented in list
  (let ((from-list
	 (cond ((listp from)
		from)
	       ((stringp from)
		(decompose-time-string from))
	       ((subtypep '<time> (type-of from))
		(decompose-time-string (time->string from)))
	       (t
		(error "unkown from type"))))
	(to-list
	 (cond ((listp to)
		to)
	       ((stringp to)
		(decompose-time-string to))
	       ((subtypep '<time> (type-of to))
		(decompose-time-string (time->string to)))
	       (t
		(error "unkown to type")))))
    ;; log-searcher datum -> (x x x x .... x)
    ;;                        <--early late-->
    (let ((rest (member from-list (datum-of searcher)
			:test #'(lambda (a b) (time>= a b)))))
      (reverse (member to-list (reverse rest))))))
      
  
