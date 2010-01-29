;;================================================
;; gnuplot.lisp
;;
;; gnuplot interface.
;; gnuplot must be in $PATH.
;; this interface is NOT thread-safe.
;;
;; written by R.Ueda (garaemon)
;;================================================

(declaim (optimize (debug 0)
                   (safety 0)
                   (speed 3)
                   (compilation-speed 0)
                   (space 0)))

(in-package :chimi)

(defvar *gnuplot* nil
  "instance of <gnuplot>.
   <gnuplot> has the input/output stream to gnuplot process.")

(defclass* <gnuplot>
    ()
  ((stream nil)
   (data nil)
   (data-length 100)
   (last-command nil))
  (:documentation
   "gnuplot's process stream class."))

(defun open-gnuplot (&optional (path "gnuplot"))
  "open gnuplot process. You have to call this function before plotting.
   If you doesn't have gnuplot in your PATH,
   you should set gnuplot path to the \"path\" parameter."
  (unless *gnuplot*
    (setf *gnuplot*
	  (make-instance '<gnuplot>
			 :stream (run-program path :wait nil))))
  (unless *gnuplot*
    (error "Cannot create ~A process" path))
  *gnuplot*)

(defun close-gnuplot ()
  "close gnuplot process."
  (when *gnuplot*
    (close (stream-of *gnuplot*))
    (setf *gnuplot* nil))
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
			     (label nil)
			     (xlogscale nil)
			     (ylogscale nil)
			     (grid nil)
			     (replot nil)
                             (data-length nil))
  (if xrange
      (format-to-gnuplot *gnuplot*
                         "set xrange [~A:~A]~%" (car xrange) (cadr xrange)))
  (if yrange
      (format-to-gnuplot *gnuplot*
                         "set yrange [~A:~A]~%" (car yrange) (cadr yrange)))
  (if xlabel
      (format-to-gnuplot *gnuplot* "set xlabel ~s~%" xlabel))
  (if ylabel
      (format-to-gnuplot *gnuplot* "set ylabel ~s~%" ylabel))
  (if label
      (format-to-gnuplot *gnuplot* "set label ~s~%" label))
  (if grid
      (format-to-gnuplot *gnuplot* "set grid~%")
      (format-to-gnuplot *gnuplot* "unset grid~%"))
  (if xlogscale
      (format-to-gnuplot *gnuplot* "set logscale x"))
  (if ylogscale
      (format-to-gnuplot *gnuplot* "set logscale y"))
  (if replot
      (format-to-gnuplot *gnuplot* "replot~%"))
  (if data-length
      (setf (data-length-of *gnuplot*) data-length))
  t)

(defun plot-function (func-str
		      &rest args
		      &key
		      (dimension 2)
		      (clear t)
		      (title nil)
		      (style nil)
		      &allow-other-keys)
  "plotting a fuction"
  ;; set last-command
  (setf (last-command-of *gnuplot*)
	(append (list #'plot-function func-str) args))
  (cond ((= dimension 2)
         (format-to-gnuplot *gnuplot*
                            (if clear "plot ~A " "replot ~A ") func-str))
        ((= dimension 3)
         (format-to-gnuplot *gnuplot*
                            (if clear "splot ~A " "replot ~A ") func-str))
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
  ;; set last-command
  (setf (last-command-of *gnuplot*)
	(append (list #'plot-points points) args))
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
	(3 (format-to-gnuplot *gnuplot* "~A ~A ~A~%"
                              (car p) (cadr p) (caddr p)))
	(t (error "diemnsion must be 2 or 3.")))
      )
     (format-to-gnuplot *gnuplot* "e~%")
     ))
  t)

(defun clear-gnuplot-datum ()
  "clear plot datum."
  (setf (data-of *gnuplot*) nil)
  *gnuplot*)

(defun one-data-plot (point
		      &rest
		      args
		      &key
		      (clear nil)
		      &allow-other-keys)
  "plot data interpritingly"
  (if clear (clear-gnuplot-datum))
  ;; add point to the last of data
  (setf (data-of *gnuplot*)
        (nconc (data-of *gnuplot*)
               (list point)))
  ;; check data length
  (when (> (length (data-of *gnuplot*))
           (data-length-of *gnuplot*))
    ;; (a b c ... z) => (b c ... z)
    (setf (data-of *gnuplot*)
          (cdr (data-of *gnuplot*))))
  (apply #'plot-points (data-of *gnuplot*) args)
  t)

(defun save-plot-to-file (fname type)
  "save gnuplot's view to a file."
  (format-to-gnuplot *gnuplot* "set terminal ~A~%" type)
  (format-to-gnuplot *gnuplot* "set output ~s~%" fname)
  (let ((last-command (last-command-of *gnuplot*)))
    (if (eq (car last-command) #'plot-points)
	(apply (car last-command) (cdr last-command))
	(format-to-gnuplot *gnuplot* "replot~%")))
  (format-to-gnuplot *gnuplot* "set output~%")
  (format-to-gnuplot *gnuplot* "set terminal aqua~%")
  (let ((last-command (last-command-of *gnuplot*)))
    (if (eq (car last-command) #'plot-points)
	(apply (car last-command) (cdr last-command))
	(format-to-gnuplot *gnuplot* "replot~%")))
  fname)

(defun save-plot-to-pdf (fname)
  "save gnuplot's view to a pdf."
  (save-plot-to-file fname "pdf"))

(defun save-plot-to-png (fname)
  "save gnuplot's view to a png."
  (save-plot-to-file fname "png"))

(defun save-plot-to-jpg (fname)
  "save gnuplot's view to a jpg."
  (save-plot-to-file fname "jpg"))

(defun save-plot-to-eps (fname)
  "save gnuplot's view to a eps."
  (save-plot-to-file fname "eps"))
