(defpackage :sparklines-imago
  (:use :cl :imago :sparklines-backend)
  (:export :make-image :draw-normal-range :draw-point :plot-line :draw-point2 :draw-tick)
  (:shadow :draw-point))

(in-package sparklines-imago)

(defvar *normal-range-color* (make-color 0 0 0 30))
(defvar *min-point-color* (make-color 0 0 255))
(defvar *max-point-color* (make-color 0 0 255))
(defvar *first-point-color* (make-color 255 0 0))
(defvar *last-point-color* (make-color 255 0 0))
(defvar *normal-point-color* (make-color 0 0 0 127))
(defvar *hl-tick-color* (make-color 255 0 0))
(defvar *normal-tick-color* (make-color 0 0 0 127))

(defun norm-y (image y)
  "Inverts the Y value, since Imago has 0 at the top, rather than the bottom"
  (- (image-height image) y 1))

(defun make-image (width height)
  (make-instance 'rgb-image :width width :height height))

;;; Nasty because Imago doesn't have a fill
(defmethod draw-normal-range ((image image) low high)
  (loop for y from low to high 
     do (draw-line image 0 (norm-y image y) (image-width image) (norm-y image y)
		   *normal-range-color*)))

(defmethod plot-line ((image image) start end)
  (draw-line image (car start) (norm-y image (cdr start))
	     (car end) (norm-y image (cdr end))
		    *normal-point-color*))

(defmethod draw-point2 ((image image) point &key type)
  (draw-point image (car point) (cdr point) :type type))

(defmethod draw-point ((image image) x y &key (thickness 1) type)
  (if type
      (draw-circle image x (norm-y image y) 1
		   (case type
		     (min *min-point-color*)
		     (max *max-point-color*)
		     (first *first-point-color*)
		     (last *last-point-color*)
		     (otherwise *normal-point-color*)))
      (imago::draw-point image x (norm-y image y)
			 *normal-point-color*)))

(defmethod draw-tick ((image image) x on bar hl)
  (let ((mid (+ (/ (image-height image) 2)))
        (end (if on 0 (image-height image))))
    (draw-line image x (+ mid (if on -1 1)) x end
	       (if hl *hl-tick-color* *normal-tick-color*))
    (if bar (draw-line image (- x (round (/ bar 2))) mid (+ x (round (/ bar 2))) mid *normal-tick-color*))))