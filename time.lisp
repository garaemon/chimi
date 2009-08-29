;;================================================
;; time.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(in-package :chimi)

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
    
