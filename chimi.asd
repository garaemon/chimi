(defsystem chimi
    :version "0.0.0"
    :depends-on (log4cl iterate cl-interpol)
    :components ((:file "chimi")
		 (:file "log" :depends-on ("chimi"))
		 (:file "time" :depends-on ("chimi"))
		 (:file "gnuplot" :depends-on ("chimi"))
		 ))
