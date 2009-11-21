;;================================================
;; sample-gnuplot.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :asdf)
(require :chimi)

(use-package :chimi)

;; open gnuplot process
(open-gnuplot)

;; plot function sin
(plot-function "sin(x)")
(sleep 1)

;; plot function cos
(plot-function "cos(x)")
(sleep 1)

;; over-write plot function -cos
(plot-function "-cos(x)" :clear nil)
(sleep 1)

;; plot points
(clear-gnuplot-datum)
(let ((points nil))
  (dotimes (i 100)
    (push (list (/ i 100.0)
		(sin (/ i 100.0))) points)
    )
  (plot-points points)
  )

;; set graph properties
(set-graph-properties
 :xrange (list (coerce pi 'single-float) (- (coerce pi 'single-float)))
 :yrange '(-1 1))
;; plot intepritingly
(clear-gnuplot-datum)
(dotimes (i 100)
  (one-data-plot (list (/ i 100.0)
		       (sin (/ i 10.0))))
  )

;; 3d sample
(plot-function "sin(x)" :dimension 3)
(sleep 1)

(plot-function "cos(x)"  :dimension 3)
(sleep 1)

(plot-function "-cos(x)" :clear nil  :dimension 3)
(sleep 1)

(clear-gnuplot-datum)
(let ((points nil))
  (dotimes (i 100)
    (push (list (/ i 100.0)
		0
		(sin (/ i 100.0))) points)
    )
  (plot-points points :dimension 3)
  )
(sleep 1)

(clear-gnuplot-datum)
(dotimes (i 100)
  (one-data-plot (list (/ i 100.0)
		       0
		       (exp (/ i 100.0)))
		 :dimension 3)
  (one-data-plot (list 0
		       (/ i 100.0)
		       (exp (/ i 100.0)))
		 :dimension 3)
  )
