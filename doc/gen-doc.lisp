;;================================================
;; gen-doc.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(require :asdf)
(require :sb-posix)
(asdf:operate 'asdf:load-op 'cldoc)
(asdf:operate 'asdf:load-op 'chimi)

(cldoc:extract-documentation 'cldoc:html "doc/html/manual" (asdf:find-system :chimi))

(sb-ext:quit)
