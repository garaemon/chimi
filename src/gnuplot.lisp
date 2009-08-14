;;================================================
;; gnuplot.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(declaim (optimize (debug 3) (safety 3)))

(in-package :chimi)

(defvar *gnuplot* nil
  "instance of <gnuplot>.
   <gnuplot> has the input/output stream to gnuplot process.")

(defclass* <gnuplot>
    ()
  ((stream nil)
   (data nil))
  (:documentation
   "gnuplot's process stream class."))

(defun open-gnuplot (&optional (path "gnuplot"))
  "open gnuplot process. You have to call this function before plotting.
   If you doesn't have gnuplot in your PATH,
   you should set gnuplot path to the \"path\" parameter."
  (setf *gnuplot*
	(make-instance '<gnuplot>
		       :stream (sb-ext:run-program path nil
						   :input :stream
						   :output :stream
						   :wait nil
						   :search t)))
  (unless *gnuplot*
    (error "Cannot create ~A process" path))
  *gnuplot*)

(defun close-gnuplot ()
  "close gnuplot process."
  (if *gnuplot*
      (progn
	(close (stream-of *gnuplot*))
	(setf *gnuplot* nil)))
  t)

(defmethod format-to-gnuplot ((gnuplot <gnuplot>) str &rest args)
  (apply #'format (cl-user::process-input (stream-of gnuplot)) str args)
  (force-output (cl-user::process-input (stream-of gnuplot)))
  t)

(defun set-graph-properties (&key
			     (xrange nil)
			     (yrange nil)
			     (xlabel nil)
			     (ylabel nil)
			     (xlogscale nil)
			     (ylogscale nil)
			     (grid nil)
			     (replot nil))
  (if xrange
      (format-to-gnuplot *gnuplot* "set xrange [~A:~A]~%" (car xrange) (cadr xrange)))
  (if yrange
      (format-to-gnuplot *gnuplot* "set yrange [~A:~A]~%" (car yrange) (cadr yrange)))
  (if xlabel
      (format-to-gnuplot *gnuplot* "set xlabel ~s~%" xlabel))
  (if ylabel
      (format-to-gnuplot *gnuplot* "set ylabel ~s~%" ylabel))
  (if grid
      (format-to-gnuplot *gnuplot* "set grid~%")
      (format-to-gnuplot *gnuplot* "unset grid~%"))
  (if xlogscale
      (format-to-gnuplot *gnuplot* "set logscale x"))
  (if ylogscale
      (format-to-gnuplot *gnuplot* "set logscale y"))
  (if replot
      (format-to-gnuplot *gnuplot* "replot~%"))
  t)

(defun plot-function (func-str
		      &key
		      (dimension 2)
		      (clear t)
		      (title nil)
		      (style nil))
  "plotting a fuction"
  (cond ((= dimension 2)
         (format-to-gnuplot *gnuplot* (if clear "plot ~A " "replot ~A ") func-str))
        ((= dimension 3)
         (format-to-gnuplot *gnuplot* (if clear "splot ~A " "replot ~A ") func-str))
        (t
         (error "diemnsion must be 2 or 3.")))
  (if title
      (format-to-gnuplot *gnuplot* "title ~s " title))
  (if style
      (format-to-gnuplot *gnuplot* "with ~A " style))
  (format-to-gnuplot *gnuplot* "~%")
  t)

(defun plot-points (points
		    &rest
		    args
		    &key
		    (dimension 2)
		    (titles nil)
		    (title nil)
		    (clear t)
		    &allow-other-keys)
  "plotting the points.
   Implementation is ugly, but it works ^^;"
  ;; rank of points 2 -> one points
  ;;                3 -> many points...
  (case dimension
    (2 (format-to-gnuplot *gnuplot* "plot '-' "))
    (3 (format-to-gnuplot *gnuplot* "splot '-' "))
    (t (error "diemnsion must be 2 or 3.")))
  (if titles
      (format-to-gnuplot *gnuplot* " title ~s " (pop titles)))
  (if title
      (format-to-gnuplot *gnuplot* " title ~s " title))
  (let ((apoints (if (= (list-rank points) 2) (list points) points)))
    (iterate:iter
     (iterate:for pp in (cdr apoints))
     (format-to-gnuplot *gnuplot* ", '-'")
     (if titles
	 (format-to-gnuplot *gnuplot* " title ~s " (pop titles)))
     )
    (format-to-gnuplot *gnuplot* "~%")
    (iterate:iter
     (iterate:for pp in apoints)
     (iterate:iter
      (iterate:for p in pp)
      (case dimension
	(2 (format-to-gnuplot *gnuplot* "~A ~A~%" (car p) (cadr p)))
	(3 (format-to-gnuplot *gnuplot* "~A ~A ~A~%" (car p) (cadr p) (caddr p)))
	(t (error "diemnsion must be 2 or 3.")))
      )
     (format-to-gnuplot *gnuplot* "e~%")
     ))
  t)

(defun clear-gnuplot-datum ()
  (setf (data-of *gnuplot*) nil)
  *gnuplot*)

(defun one-data-plot (point
		      &rest
		      args
		      &key
		      (clear nil)
		      &allow-other-keys)
  (if clear (clear-gnuplot-datum))
  (push point (data-of *gnuplot*))
  (apply #'plot-points (data-of *gnuplot*) args)
  t)

(defun save-plot-to-file (fname type)
  (format-to-gnuplot *gnuplot* "set terminal ~A~%" type)
  (format-to-gnuplot *gnuplot* "set output ~s~%" fname)
  ;;(format-to-gnuplot *gnuplot* "replot~%")
  (apply #'plot-points (data-of *gnuplot*) nil)
  (format-to-gnuplot *gnuplot* "replot~%")
  (format-to-gnuplot *gnuplot* "set output~%")
  (format-to-gnuplot *gnuplot* "set terminal aqua~%")
  (format-to-gnuplot *gnuplot* "replot~%")
  )
