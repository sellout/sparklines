(defpackage :sparklines
  (:use :cl :sparklines-imago)
  (:export :plot-sparkline :make-sparkline :make-sparktick :make-sparklines))

(in-package sparklines)

(defun calculate-values (data normal-min normal-max width height)
  "returns the width/height, coords, min-coord, max-coord, normal-range"
  (let* ((data-min (apply #'min data))
	 (data-max (apply #'max data))
	 (min-index nil)
	 (max-index nil)
	 (aspect-ratio (determine-aspect-ratio data))
	 (image-width (or width (if height
				    (round (/ height aspect-ratio))
				    (length data))))
	 (data-multiplier (/ image-width (length data)))
	 (image-height (or height
			   (round (* (1+ (- data-max data-min))
					 aspect-ratio data-multiplier))))
	 (x 0)
	 (coords (mapcar #'(lambda (y)
			     (let ((result (cons (1+ (round (* x data-multiplier)))
						 (1+ (round (* y data-multiplier
							       aspect-ratio))))))
			       (cond ((= y data-min) (setf min-index x))
				     ((= y data-max) (setf max-index x)))
			       (incf x)
			       result))
			 data)))
    (values (cons image-width image-height)
	    coords
	    (nth min-index coords)
	    (nth max-index coords)
	    (if (and normal-min normal-max)
		(cons (1+ (round (* normal-min data-multiplier aspect-ratio)))
		      (1+ (round (* normal-max data-multiplier aspect-ratio))))))))

(defun plot-sparkline (data &key normal-min normal-max
		       hl-first hl-last hl-min hl-max fill
		       width height)
  (multiple-value-bind (image-size coords min-point max-point normal-range)
      (calculate-values data normal-min normal-max width height)
    (let ((image (make-image (+ (car image-size) 2) (+ (cdr image-size) 2))))
      (if normal-range
	  (draw-normal-range image (car normal-range) (cdr normal-range)))
      (mapcar #'(lambda (start end)
		  (plot-line image start end))
	      coords (cdr coords))
      (if hl-first (draw-point2 image (car coords) :type 'first))
      (if hl-last (draw-point2 image (car (last coords)) :type 'last))
      (if hl-min (draw-point2 image min-point :type 'min))
      (if hl-max (draw-point2 image max-point :type 'max))
      image)))

(defun plot-sparktick (data &key bar-subset hl-subset (height 12) width)
  "Creates a sparkline from a list of generalized booleans, with
  optional lists of indices for drawing a bar at the midpoint, or
  highlighting ticks."
  (let* ((image-width (or width (* (length data) 2)))
	 (multiplier (/ image-width (length data)))
	 (tick-height (/ height 2))
	 (image (make-image image-width height))
	 (index 0))
    (mapcar #'(lambda (tick bar hl)
		(draw-tick image (round (* index multiplier)) tick (if bar multiplier) hl)
		(incf index))
	    data bar-subset hl-subset)
    image))

(defun find-pixel (value offset multiplier)
  (round (* (- value offset) multiplier)))


(defun determine-aspect-ratio (data)
  "Returns a ratio of proper height:width to give the data an
  average change of 45 degrees. A ratio of one means that each
  data point has a change of one unit on average."
  (/ 1 (average (remove 0 (mapcar #'(lambda (first second)
				      (abs (- first second)))
				  data (cdr data))))))

(defun average (seq)
  (/ (apply #'+ seq) (length seq)))

(defun plot-sparklines (data-seq &key normal-min normal-max
		       hl-first hl-last hl-min hl-max fill
		       width height)
  "Use this to make multiple sparklines that appear
  together. They should all have the same number of data points
  and will be created with the same aspect ratio."
  (if (not (and height width))
      (let ((aspect (apply #'avg (mapcar #'determine-aspect-ratio data-seq))))))
  (mapcar #'(lambda (data) (apply #'sparkline data keys)) data-seq))


(defun plot-sparkticks (data-seq &key (height 12) width)
  "This does nothing special, but exists for consistency."
  (mapcar #'(lambda (data)
	      (apply #'make-sparktick
		     (append data (list :height height :width width))))
	  data-seq))
