;;================================================
;; sequence.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (debug 0)
                   (safety 0)
                   (speed 3)
                   (compilation-speed 0)
                   (space 0)))

(in-package :chimi)

(defun replace-list (target from to &key (test #'equal))
  "replace 'from' in 'target' to 'to'

  ;;; (replace-list '(1 2 3 4 5) '(1 2 3) '(2 4 6)) -> (2 4 6 4 5)"
  (declare (type list from to))
  (cond ((null target)
         nil)
        ((listp target)
         (cons (replace-list (car target) from to :test test)
               (replace-list (cdr target) from to :test test)))
        (t
         (let ((location (position target from)))
           (if location
               (elt to location)
               target)))))

(defun replace-list-flat1 (datum froms tos &key (test #'equal))
  (let ((ret nil))
    (dolist (d datum)
      (let ((flag nil))
        (dotimes (fi (length froms))
          (if (funcall test (elt froms fi) d)
              (setq flag (elt tos fi)))
          )
        (if flag
            (push (flatten flag) ret)
            (push d ret))
        ))
    (reverse ret)))

(defun all-combination (lst)
  "make a list of all combination of lst

  ;; (all-combination '((1) (2) (3))) -> ((1 2 3))"
  (declare (type list lst))
  (reduce #'(lambda (prev target)
              (declare (type list target))
              (if (eq prev :nil)
                  (mapcar #'list target)
                  (let ((ret nil))
                    (declare (type list ret))
                    (dolist (ta target)
                      (dolist (p prev)
                        (declare (type list p))
                        (push (append p (list ta)) ret)
			))
                    ret)))
          lst :initial-value :nil))

(defun flatten (lst)
  "flatten a list.

   ;;; (flatten '((1 2) (3 4))) -> (1 2 3 4)"
  (cond ((null lst)
         nil)
        ((atom lst)
         (list lst))
        (t                              ;lst = list
         (append (flatten (car lst))
                 (flatten (cdr lst)))
         )))

(defun all (proc list)
  (declare (type list list))
  (cond ((null list)
         t)
        ((funcall proc (car list))
         (all proc (cdr list)))
        (t
         nil)))

(defun any (proc list)
  (declare (type list list))
  (cond ((null list)
         nil)
        ((funcall proc (car list))
         (car list))
        (t
         (any proc (cdr list)))))

;; find-all = remove-if-not??
(defun find-all (proc list)
  "is this same to remove-if-not??"
  (declare (type list list))
  (cond ((null list)
         nil)
        ((funcall proc (car list))
         (cons (car list) (find-all proc (cdr list))))
        (t
         (find-all proc (cdr list)))))

(defun difference-list (a b)
  "returns difference between a and b

   ;;; (difference-list '(1 2 3) '(1 1 3)) -> '(2)"
  (declare (type list a b))
  (cond ((null a)
         nil)
        ((equal (car a) (car b))
         (difference-list (cdr a) (cdr b)))
        (t
         (cons (car a) (difference-list (cdr a) (cdr b))))))

(defun random-select (list)
  "returns a element of list randomly"
  (declare (type list list))
  (let ((len (length list)))
    (elt list (random len))))

(defun list-rank (lst)
  "returns the rank of lst.

   ;;; (list-rank nil) -> 0
   ;;; (list-rank '(1 2 3)) -> 1
   ;;; (list-rank '((1 2 3))) -> 2"
  (if (atom list)
      0
      (max (1+ (list-rank* (car list)))
           (list-rank* (cdr list)))))

(defun list-rank* (list)
  (if (atom list)
      0
      (max (1+ (list-rank* (car list)))
           (list-rank* (cdr list)))))

(defun get-keyword (key args)
  "returns 'key' 's value in args.

   ;;; (get-keyword :hoge '(:hoge 1 :fuga 2)) -> 1"
  (declare (type list args))
  (cadr (member key args)))

(defun concatenate-string-with (string-list space)
  "concatenate string-list with space.

  ;;; (concatenate-string-with '(\"hoge\" \"fuga\" \"piyo\") \"-\")
  ;;; => \"hoge-fuga-piyo\""
  (declare (type list string-list)
	   (type string space))
  (labels ((%concatenate-string-with
	       (string-list space)
	     (declare (type list string-list)
		      (type string space))
	     (cond ((null string-list)
		    nil)
		   ((null (cdr string-list))
		    (list (car string-list)))
		   (t
		    (append (list (car string-list) space)
			    (%concatenate-string-with (cdr string-list)
                                                      space))))))
    (apply #'concatenate 'string
           (%concatenate-string-with string-list space))))
