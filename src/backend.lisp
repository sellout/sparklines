(defpackage :sparklines-backend
  (:use :cl)
  (:export :draw-normal-range :draw-point :draw-tick))

(in-package sparklines-backend)

(defgeneric draw-normal-range (image low high))
(defgeneric draw-point (image x y &key thickness type))
(defgeneric draw-tick (image x on bar hl))
