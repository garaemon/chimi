(defsystem chimi
    :version "0.0.1"
    :depends-on (iterate cl-interpol log5 cl-ppcre alexandria cffi cl-fad
                 bordeaux-threads)
    :components ((:file "chimi")
                 (:file "io" :depends-on ("chimi"))
                 (:file "threads" :depends-on ("chimi"))
                 (:file "cffiext" :depends-on ("chimi"))
                 (:file "util-macro" :depends-on ("chimi" "symbol"))
                 (:file "clos" :depends-on ("chimi" "symbol"))
                 (:file "terminal" :depends-on ("chimi"))
                 (:file "symbol" :depends-on ("chimi"))
                 (:file "sequence" :depends-on ("chimi"))
                 (:file "time" :depends-on ("chimi" "util-macro"))
		 (:file "log" :depends-on ("chimi" "time" "util-macro"))
                 (:file "system" :depends-on ("chimi"))
		 (:file "gnuplot" :depends-on ("chimi" "util-macro"))))
