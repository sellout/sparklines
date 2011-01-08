(defpackage sparklines-system
  (:use #:cl #:asdf))

(in-package #:sparklines-system)

(defsystem sparklines
  :depends-on (vecto flexi-streams)
  :pathname "src/"
  :components ((:file "sparklines")
	       (:file "vecto-backend" :depends-on ("sparklines"))))