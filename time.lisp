;;================================================
;; time.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 0)
                   (safety 0)
                   (speed 3)
                   (compilation-speed 0)
                   (space 0)))

(in-package :chimi)

(defun local-time-string (&key
			  ((:year ayear) t)
			  ((:month amonth) t)
			  ((:day aday) t)
			  ((:hour ahour) t)
			  ((:minute aminute) t)
			  ((:second asecond) t))
  "returns current year, month, date, time as a string.
   format is YYYY-MM-DD-hh-mm-ss."
  (multiple-value-bind (sec min hour day mon year)
      (decode-universal-time (get-universal-time))
    (let ((format-str nil)
	  (format-arg nil))
      (when ayear
	(setf format-str (append format-str (list "~2,'0D")))
	(setf format-arg (append format-arg (list year))))
      (when amonth
	(setf format-str (append format-str (list "~2,'0D")))
	(setf format-arg (append format-arg (list mon))))
      (when aday
	(setf format-str (append format-str (list "~2,'0D")))
	(setf format-arg (append format-arg (list day))))
      (when ahour
	(setf format-str (append format-str (list "~2,'0D")))
	(setf format-arg (append format-arg (list hour))))
      (when aminute
	(setf format-str (append format-str (list "~2,'0D")))
	(setf format-arg (append format-arg (list min))))
      (when asecond
	(setf format-str (append format-str (list "~2,'0D")))
	(setf format-arg (append format-arg (list sec))))
      (apply #'format nil (concatenate-string-with format-str "-") format-arg))))

(defun decompose-time-string (str)
  "Parse a string representing time and
   returns second, minute, hour, day, month and year
   in list.

   The Time string format must be YYYY-MM-DD-hh-mm-ss."
  (declare (type string str))
  (let ((split-result (cl-ppcre:split "-" str)))
    ;; split-result
    ;; -> (year month day hour minute second)
    (mapcar #'read-from-string split-result)))

;; sb-unix:unix-gettimeofday returns (sec micro-sec)
(defclass* <mtimer>
    ()
  ((time nil))
  (:documentation
   "in this class, <mtimer>, we use sb-unix:unix-gettimeofday to
    measure micro seconds."
   ))

(defun make-mtimer ()
  (let ((tm (make-instance '<mtimer>)))
    (start-mtimer tm)))

(defmethod start-mtimer ((tm <mtimer>))
  "start timer.
   reset timer."
  (multiple-value-bind (dummy sec micro-sec)
      (sb-unix:unix-gettimeofday)
    (setf (time-of tm) (cons sec micro-sec)))
  tm)

(defmethod stop-mtimer ((tm <mtimer>))
  "returns time in sec unit.
   this method does not reset timer."
  (multiple-value-bind (dummy sec micro-sec)
      (sb-unix:unix-gettimeofday)
    (let ((prev-time (time-of tm)))
      (+ (- sec (car prev-time))
	 (* 1.0e-6 (- micro-sec (cdr prev-time))))
      )))
    
(defclass* <time>
    ()
  ((year nil)
   (month nil)
   (day nil)
   (hour nil)
   (minute nil)
   (second nil))
  (:documentation
   "")
  )

(defmethod time->string ((tm <time>)
			 &key
			 (with-big-paren t))
  (let ((year (year-of tm))
	(month (month-of tm))
	(day (day-of tm))
	(hour (hour-of tm))
	(minute (minute-of tm))
	(second (second-of tm)))
    (format nil (if with-big-paren
		    "[~A-~A-~A-~A-~A-~A]"
		    "~A-~A-~A-~A-~A-~A")
	    year month day
	    hour minute second)))

(defun string->time (str)
  
  )

(defun time< (a b)
  (cond ((or (null a) (null b))
	 nil)
	((< (car a) (car b))
	 t)
	((> (car a) (car b))
	 nil)
	(t
	 (time< (cdr a) (cdr b)))))
	

(defun time<= (a b)
  (cond ((or (null a) (null b))
	 t)
	((< (car a) (car b))
	 t)
	((> (car a) (car b))
	 nil)
	(t
	 (time<= (cdr a) (cdr b)))))

(defun time= (a b)
  (cond ((or (null a) (null b))
	 t)
	((not (= (car a) (car b)))
	 nil)
	(t
	 (time= (cdr a) (cdr b)))))

(defun time> (a b)
  (cond ((or (null a) (null b))
	 nil)
	((> (car a) (car b))
	 t)
	((< (car a) (car b))
	 nil)
	(t
	 (time> (cdr a) (cdr b)))))

(defun time>= (a b)
  (cond ((or (null a) (null b))
	 t)
	((> (car a) (car b))
	 t)
	((< (car a) (car b))
	 nil)
	(t
	 (time>= (cdr a) (cdr b)))))
