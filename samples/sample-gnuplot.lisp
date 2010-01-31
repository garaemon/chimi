;;================================================
;; sample-gnuplot.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :asdf)
(require :chimi)

;; open gnuplot process
(defvar *gnuplot* (chimi:open-gnuplot))

;; plot function sin
(chimi:plot-function *gnuplot* "sin(x)")
(sleep 1)

;; plot function cos
(chimi:plot-function *gnuplot* "cos(x)")
(sleep 1)

;; over-write plot function -cos
(chimi:plot-function *gnuplot* "-cos(x)" :clear nil)
(sleep 1)

;; plot points
(chimi:clear-gnuplot-datum *gnuplot*)
(let ((points nil))
  (dotimes (i 100)
    (push (list (/ i 100.0)
		(sin (/ i 100.0))) points)
    )
  (chimi:plot-points *gnuplot* points)
  )

;; set graph properties
(chimi:set-graph-properties
 *gnuplot*
 :xrange (list (coerce pi 'single-float) (- (coerce pi 'single-float)))
 :yrange '(-1 1))
;; plot intepritingly
(chimi:clear-gnuplot-datum *gnuplot*)
(dotimes (i 100)
  (chimi:one-data-plot *gnuplot*
                       (list (/ i 100.0)
                             (sin (/ i 10.0))))
  )

;; 3d sample
(chimi:plot-function *gnuplot* "sin(x)" :dimension 3)
(sleep 1)

(chimi:plot-function *gnuplot* "cos(x)"  :dimension 3)
(sleep 1)

(chimi:plot-function *gnuplot* "-cos(x)" :clear nil  :dimension 3)
(sleep 1)

(chimi:clear-gnuplot-datum *gnuplot*)
(let ((points nil))
  (dotimes (i 100)
    (push (list (/ i 100.0)
		0
		(sin (/ i 100.0))) points)
    )
  (chimi:plot-points *gnuplot* points :dimension 3)
  )
(sleep 1)

(chimi:clear-gnuplot-datum *gnuplot*)
(dotimes (i 100)
  (chimi:one-data-plot *gnuplot*
                       (list (/ i 100.0)
                             0
                             (exp (/ i 100.0)))
                       :dimension 3)
  (chimi:one-data-plot *gnuplot*
                       (list 0
                             (/ i 100.0)
                             (exp (/ i 100.0)))
                       :dimension 3)
  )
