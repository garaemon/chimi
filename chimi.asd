(defsystem chimi
    :version "0.0.1"
    :depends-on (iterate cl-interpol log5 cl-ppcre)
    :components ((:file "chimi")
		 (:file "log" :depends-on ("chimi" "time"))
		 (:file "time" :depends-on ("chimi"))
		 (:file "gnuplot" :depends-on ("chimi"))
		 ))
