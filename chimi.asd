(defsystem chimi
    :version "0.0.1"
    :depends-on (iterate cl-interpol log5 cl-ppcre alexandria cffi cl-fad)
    :components ((:file "chimi")
                 (:file "cffiext" :depends-on ("chimi"))
		 (:file "log" :depends-on ("chimi" "time"))
		 (:file "time" :depends-on ("chimi"))
                 (:file "system" :depends-on ("chimi"))
		 (:file "gnuplot" :depends-on ("chimi"))
		 ))
