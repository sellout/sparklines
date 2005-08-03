(defsystem sparklines
  :depends-on (:imago)
  :components ((:file "sparklines-backend")
	       (:file "sparklines-imago" :depends-on ("sparklines-backend"))
	       (:file "sparklines" :depends-on ("sparklines-imago"))))