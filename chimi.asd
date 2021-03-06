(defsystem chimi
    :version "0.0.2"
    :depends-on (iterate log5 cl-ppcre alexandria cffi bordeaux-threads
                 #+sbcl sb-posix)
    :components ((:file "chimi")
                 (:file "io" :depends-on ("chimi" "util-macro"))
                 (:file "threads" :depends-on ("chimi"))
                 (:file "cffiext" :depends-on ("chimi"))
                 (:file "util-macro" :depends-on ("chimi" "symbol"))
                 (:file "clos" :depends-on ("chimi" "symbol"))
                 (:file "terminal" :depends-on ("chimi"))
                 (:file "symbol" :depends-on ("chimi"))
                 (:file "sequence" :depends-on ("chimi"))
                 (:file "time" :depends-on ("chimi" "util-macro" "clos"))
		 (:file "log" :depends-on ("chimi" "time" "util-macro"))
                 (:file "system" :depends-on ("chimi"))
		 (:file "gnuplot" :depends-on ("chimi" "util-macro"))))
