(defpackage :sparklines-pdf
  (:use :cl :pdf :sparklines-backend)
  (:export :make-image :draw-normal-range :draw-point :plot-line :draw-point2 :draw-tick)
  (:shadow :draw-point))

(in-package sparklines-pdf)

(defvar *normal-range-color* '(0 0 0 30))
(defvar *min-point-color* '(0 0 255))
(defvar *max-point-color* '(0 0 255))
(defvar *first-point-color* '(255 0 0))
(defvar *last-point-color* '(255 0 0))
(defvar *normal-point-color* '(0 0 0 127))
(defvar *hl-tick-color* '(255 0 0))
(defvar *normal-tick-color* '(0 0 0 127))

(defun make-image (width height)
  (make-instance))

(defmethod draw-normal-range ((page page) low high)
  (apply #'pdf:set-rgb-stroke *normal-range-color*)
  (apply #'pdf:set-rgb-fill *normal-range-color*)
  (pdf:rectangle 0 low  (width page) (- high low)))


(defmethod plot-line ((page page) start end)
  (apply #'pdf:set-rgb-stroke *normal-point-color*)
  (pdf:move-to (car start) (cdr start))
  (pdf:line-to (car end) (cdr end)))