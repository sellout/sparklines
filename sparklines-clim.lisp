;;; drawing functions

(defun draw-normal-range (low high)
  (clim:draw-rectangle *image*
                       (make-point 0 low)
                       (make-point (width *image*) high)
                       :ink *normal-range-color*))

(defun draw-point (x y &key (thickness 1) type)
  (clim:draw-point *image* (clim:make-point x y)
                   :line-thickness (if type
                                       (* 2 thickness)
                                       thickness)                              
                   :ink (case type
                          (min *min-point-color*)
                          (max *max-point-color*)
                          (first *first-point-color*)
                          (last *last-point-color*)
                          (otherwise *normal-point-color*))

(defun draw-tick (on y hl)
  (let* ((mid (/ (height *image*) 2))
         (end (if on (make-point (height *image*) y) (make-point 0 y))))
    (clim:draw-line *image* (make-point mid y) end
                    :ink (if hl *hl-tick-color* *normal-tick-color*))))