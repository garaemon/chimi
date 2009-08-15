(require :asdf)
(asdf:operate 'asdf:load-op 'chimi)
(use-package :chimi)

(defclass* <hoge>
    ()
  ((a 1)
   (b nil)))

(defun sample-read-obj ()
  (let ((inst (make-instance '<hoge>)))
    (format t "use a-of -> ~A~%" (a-of inst))
    (format t "use b-of -> ~A~%" (b-of inst))
    ))

(defun sample-write-obj ()
  (let ((inst (make-instance '<hoge>)))
    (setf (a-of inst) 100)
    (setf (b-of inst) :hoge)
    (format t "use a-of -> ~A~%" (a-of inst))
    (format t "use b-of -> ~A~%" (b-of inst))
    ))


(defun main ()
  (sample-read-obj)
  (sample-write-obj))

