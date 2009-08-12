(require :asdf)
(require :sb-posix)
(asdf:operate 'asdf:load-op 'lisp-unit)
(asdf:operate 'asdf:load-op 'chimi)

(use-package :lisp-unit)
(use-package :chimi)

;; tests for list utility
(define-test replace-list-test
  (assert-equal '(a b c) (replace-list '(1 2 3) '(1 2 3) '(a b c)))
  (assert-equal '(a 2 3) (replace-list '(1 2 3) '(1) '(a)))
  (assert-equal '(1 a 3) (replace-list '(1 2 3) '(2) '(a)))
  (assert-equal '(1 2 a) (replace-list '(1 2 3) '(3) '(a)))
  (assert-equal '(3 2 1) (replace-list '(1 2 3) '(3 1) '(1 3))))

(define-test flatten-test
  (assert-equal '(a b c) (flatten '((a b c))))
  (assert-equal '(a b c) (flatten '(((a b c)))))
  (assert-equal '(a b c) (flatten '(a (b c))))
  (assert-equal '(a b c) (flatten '(a (b ((c))))))
  (assert-equal '(a b c) (flatten '(a (((b)) c)))))

(define-test all-test
  (assert-true (all #'evenp '(2 4 6 8)))
  (assert-false (all #'evenp '(1 2 4 6 8)))
  (assert-false (all #'evenp '(2 4 6 8 1))))

(define-test any-test
  (assert-true (any #'evenp '(1 2 3 4 5 6)))
  (assert-true (any #'evenp '(1 3 5 6)))
  (assert-false (any #'evenp '(1 3 5 7))))

(define-test find-all-test
  (assert-equal '(2 4 6) (find-all #'evenp '(1 2 3 4 5 6 7)))
  (assert-equal '(1 3 5 7) (find-all #'oddp '(1 2 3 4 5 6 7))))

(define-test difference-list-test
  (assert-equal '(2) (difference-list '(1 2 3) '(1 1 3)))
  (assert-equal '(2 4) (difference-list '(1 2 3 4) '(1 1 3 5))))

(define-test symbol->keyword-test
  (assert-equal :hoge (symbol->keyword 'hoge))
  (assert-equal :fuga (symbol->keyword 'fuga))
  (assert-equal :piyo (symbol->keyword 'piyo)))

(define-test string->symbol-test
  (assert-equal 'hoge (string->symbol "hoge"))
  (assert-equal 'fuga (string->symbol "fuga"))
  (assert-equal 'piyo (string->symbol "piyo")))
  
(define-test get-keyword-test
  (assert-equal 100 (get-keyword :hoge '(:hoge 100 :fuga 200 :piyo 300)))
  (assert-equal 200 (get-keyword :fuga '(:hoge 100 :fuga 200 :piyo 300)))
  (assert-equal 300 (get-keyword :piyo '(:hoge 100 :fuga 200 :piyo 300))))

;; run
(run-tests replace-list-test
	   flatten-test
	   all-test
	   any-test
	   find-all-test
	   difference-list-test
	   symbol->keyword-test
	   string->symbol-test
	   get-keyword-test)
(format t "~%")
(sb-ext:quit)
