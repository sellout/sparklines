(defpackage :sparklines
  (:use #:cl #:vecto)
  (:export :plot-sparkline :plot-sparktick :plot-sparklines :plot-sparkticks))

(in-package sparklines)

(defun apply-to-data-range (fn data &optional normal-range fill)
  (if normal-range
      (if fill
          (apply fn (car normal-range) (cdr normal-range) fill data)
          (apply fn (car normal-range) (cdr normal-range) data))
      (if fill
          (apply fn fill data)
          (apply fn data))))

(defun find-data-range (data &optional normal-range fill)
  "If NORMAL-RANGE is included, we make sure that it is not cropped in
the final graph."
  (- (apply-to-data-range #'max data normal-range fill)
     (apply-to-data-range #'min data normal-range fill)))

(defun calculate-common-values (data-seq normal-range fill width height share)
  (let* ((data-range (case share
                       (range (find-data-range (apply #'append data-seq)
                                               normal-range fill))
                       (dimensions
                        (apply #'max
                               (mapcar (lambda (data)
                                         (find-data-range data
                                                          normal-range fill))
                                       data-seq)))
                       (otherwise (mapcar (lambda (data)
                                            (find-data-range data
                                                             normal-range fill))
                                          data-seq))))
         (data-length (length (car data-seq)))
         (aspect-ratio (average (mapcar #'determine-aspect-ratio data-seq)))
         (image-width (or width (if height
				    (ceiling (* (/ height
                                                   (if (= data-range 0)
                                                       1
                                                       data-range))
                                                data-length
                                                aspect-ratio))
				    data-length)))
         (x-multiplier (/ image-width (1- data-length)))
	 (image-height (ceiling (or height
                                    (/ (* data-range x-multiplier)
                                       (if (= aspect-ratio 0) 1 aspect-ratio)))))
         (y-multiplier (/ image-height (if (= data-range 0) 1 data-range))))
    (values (cons (ceiling (+ image-width (* x-multiplier 2)))
                  (ceiling (+ image-height (* x-multiplier 2))))
            x-multiplier
            y-multiplier)))

(defun calculate-values (data normal-range fill x-multiplier y-multiplier)
  "returns the width/height, coords, min-coord, max-coord, normal-range"
  (let* ((data-min (apply-to-data-range #'min data))
         (data-max (apply-to-data-range #'max data))
         (image-min (apply-to-data-range #'min data normal-range fill))
         (min-index nil)
         (max-index nil)
         (x 0))
    (flet ((adjust-point (point)
             (cons (+ (* (car point) x-multiplier) (ceiling x-multiplier))
                   (+ (- (* (cdr point) y-multiplier)
                         (* image-min y-multiplier))
                      (ceiling x-multiplier)))))
      (let ((coords (mapcar (lambda (y)
                              (let ((result (adjust-point (cons x y))))
                                (when (= y data-min) (setf min-index x))
                                (when (= y data-max) (setf max-index x))
                                (incf x)
                                result))
                            data)))
        (values coords
                (nth min-index coords)
                (nth max-index coords)
                (if normal-range
                    (cons (cdr (adjust-point (cons 0 (car normal-range))))
                          (cdr (adjust-point (cons 0 (cdr normal-range))))))
                (if fill
                    (cdr (adjust-point (cons 0 fill)))))))))

(defun plot-sparkline (data &key normal-range
		       hl-first hl-last hl-min hl-max fill
		       width height)
  (multiple-value-bind (image-dimensions x-multiplier y-multiplier)
      (calculate-common-values (list data) normal-range fill width height
                               'dimensions)
    (multiple-value-bind (coords min-point max-point normal-box fill-limit)
        (calculate-values data normal-range fill x-multiplier y-multiplier)
      (build-sparkline image-dimensions coords min-point max-point
                       normal-box fill-limit (/ x-multiplier 1)
                       hl-first hl-last hl-min hl-max))))

(defun build-sparkline
    (image-dimensions coords min-point max-point normal-range fill-limit thickness
     hl-first hl-last hl-min hl-max)
  (let ((image (make-image (+ (car image-dimensions) 2)
                           (+ (cdr image-dimensions) 2))))
      (if normal-range
	  (draw-normal-range image (car normal-range) (cdr normal-range)))
      (mapcar (lambda (start end) (plot-line image start end))
	      coords (cdr coords))
      (if hl-first (draw-point2 image (car coords) :type 'first))
      (if hl-last (draw-point2 image (car (last coords)) :type 'last))
      (if hl-min (draw-point2 image min-point :type 'min))
      (if hl-max (draw-point2 image max-point :type 'max))
      image))

(defun plot-sparktick (tick &key bar highlight (height 12) width)
  "Creates a sparkline from a list of generalized booleans, with
  optional lists of indices for drawing a bar at the midpoint, or
  highlighting ticks."
  (let* ((image-width (or width (* (length tick) 2)))
	 (multiplier (/ image-width (length tick)))
	 (index 0))
    (build-sparktick (mapcar (lambda (tickp &optional barp highlightp)
                               (incf index)
                               (list (* index multiplier)
                                     tickp
                                     (if barp multiplier)
                                     (if highlightp '(1 0 0))))
                             tick ;; bar highlight
                             )
                     (cons image-width height))))

(defun determine-aspect-ratio (data)
  "Returns a ratio of proper height:width to give the data an average
  change of 45 degrees. A ratio of one means that each data point has
  a change of one unit on average."
  (average (remove 0 (mapcar #'(lambda (first second)
                                 (abs (- first second)))
                             data (cdr data)))))

(defun average (seq)
  (/ (apply #'+ seq) (length seq)))
 
(defun plot-sparklines
    (data-seq &key normal-range hl-first hl-last hl-min hl-max fill
     width height share)
  "Use this to make multiple sparklines that appear together. They
should all have the same number of data points and will be created
with the same aspect ratio. If use-shared-range is T, it means all
sparklines will have the same min and max, otherwise they will share
their aspect ratio, but the values could be shifted up or down within
the image. use-shared-range is useful if absolute change is as
important as relative change (EG, if you overlay the images)"
  (multiple-value-bind (image-dimensions x-multiplier y-multiplier)
      (calculate-common-values data-seq normal-range width height
                               share)
    (mapcar (lambda (data)
              (multiple-value-bind (coords min-point max-point normal-box)
                  (calculate-values data normal-range x-multiplier y-multiplier)
                (build-sparkline image-dimensions coords min-point max-point
                                 normal-box (/ x-multiplier 4) hl-first hl-last hl-min hl-max)))
            data-seq)))


(defun plot-sparkticks (data-seq &key (height 12) width)
  "This does nothing special, but exists for consistency."
  (mapcar #'(lambda (data)
	      (apply #'make-sparktick
		     data (list :height height :width width)))
	  data-seq))
