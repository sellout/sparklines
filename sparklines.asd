(defpackage sparklines-system
  (:use #:cl #:asdf))

(in-package #:sparklines-system)

(defsystem sparklines
  :depends-on (vecto flexi-streams)
  :components ((:file "sparklines")
	       (:file "sparklines-vecto" :depends-on ("sparklines"))))