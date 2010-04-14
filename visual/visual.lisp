;;; visual stuff for GA simulations
(in-package :ai-fun.visual)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *vis-log-level* 5))
  
(defmacro vis-log (log-level &rest print-list)
  (if (<= log-level *vis-log-level*)
	  `(progn
		 ;; :fixme: replace format with something else
		 (dotimes (i ,log-level) (format t " "))
		 (when (>= ,log-level 3)
		   (format t ":debug:"))
		 (let ((eol t))
		   (dolist (p (list ,@print-list))
			 (if (eql p ':no-eol)
				 (setf eol nil)
				 (format t "~a" p)))
		   (when eol
			 (terpri))))))

(defun save-universe (entities &key (stream t) (time nil) (limits nil)
                      (type :2d-graph))
  "Save a generic universe to a stream (as a list of s-expressions)"
  (write time :stream stream :pretty nil)
  (terpri)
  (write type :stream stream :pretty nil)
  (terpri)
  (write limits :stream stream :pretty nil)
  (terpri)
  (write entities :stream stream :pretty nil)
  (terpri))

;; coords : (x y)
;; transformations : (x-translation x-scale-factor y-translation y-scale-factor)
(defun coord-real-to-viewport (coords tr)
  "Convert coords. according to transformations"
  (list
   (floor (* (+ (first coords) (first tr)) (second tr)))
   (floor (* (+ (second coords) (third tr)) (fourth tr)))))

;; coords : (x y)
;; transformations : (x-translation x-scale-factor y-translation y-scale-factor)
(defun coord-viewport-to-real (coords tr)
  "Convert coords. according to transformations"
  (list
   (floor (- (/ (first coords) (second tr)) (first tr)))
   (floor (- (/ (second coords) (fourth tr)) (third tr)))))

;; extra-param : (png x-translation x-scale-factor y-translation y-scale-factor)
;; png : is the initial png (white, with axis & function graphic)
(defun save-ga-universe-to-png (entities time &optional extra-param)
  "Save GA universe to png file (filename will be 'universeXXXX.png')"
  ;; :fixme: - if extra-param missing - create an empty png
  (let* ((png (copy-png (first extra-param)))
         (image (data-array png))
         (w (width png)) (h (height png)))
    (dolist (ent entities)
      (let* ((y-ent (floor (fitness ent)))
             (x-ent (genome ent))
             (png-coord (coord-real-to-viewport (list x-ent y-ent)
                                                (rest extra-param)))
            (x-png (first png-coord)))
        (vis-log 5 "ent(" x-ent ")=" y-ent)
        (vis-log 5 "vert. line at x=" x-png)
        (when (and (>= x-png 0) (< x-png w))
          (vis-log 5 "drawing vert. line at x=")
          (dotimes (y (- h 1))
            ;;; draw with green
            (setf (aref image (- h y 1) x-png 0) 0)
            (setf (aref image (- h y 1) x-png 0) 255)
            (setf (aref image (- h y 1) x-png 0) 0)))))
    (let ((filename (format nil "universe~4,'0d.png" time)))
      (vis-log 2 "new png image: " filename)
      (write-png png filename))))


(defun png-generate-white-png (width height transformations &optional func)
  "Generate a white png image and draw the func graphic on it."
  (let* ((png (make-instance 'png
							 :color-type :truecolor
							 :width width :height height))
         (image (data-array png)))
    (vis-log 3 "New png created: width=" width ", height=" height)
    ;; make white (:fixme: this is soooo inneficient)
    (loop for x from 0 to (- width 1) do
         (loop for y from 0 to (- height 1) do
              (setf (aref image y x 0) 255)
              (setf (aref image y x 1) 255)
              (setf (aref image y x 2) 255)))
    ;; draw axis
    (let* ((xy0 (coord-real-to-viewport (list 0 0) transformations))
           (x0 (first xy0))
           (y0 (second xy0)))
      (vis-log 3 "xy0=" xy0)
      (when (and (>= x0 0) (< x0 width))
        (loop for y from 0 to (- height 1) do
               (setf (aref image (- height y 1) x0 0) 255)
               (setf (aref image (- height y 1) x0 1) 0)
               (setf (aref image (- height y 1) x0 2) 0)))
      (when (and (>= y0 0) (< y0 width))
        (loop for x from 0 to (- width 1) do
               (setf (aref image (- height y0 1) x 0) 255)
               (setf (aref image (- height y0 1) x 1) 0)
               (setf (aref image (- height y0 1) x 2) 0))))
    (vis-log 3 "axis done")
	;; draw the function
    (when func
      (loop for x from 0 to (- width 1) do
           (let* ((x-real (first (coord-viewport-to-real
                                  (list x 0) transformations)))
                  (y-real (funcall func x-real))
                  (y (second (coord-real-to-viewport
                              (list x-real y-real) transformations))))
             (when (and (>= y 0) (< y height))
               (setf (aref image (- height y 1) x 0) 255)
               (setf (aref image (- height y 1) x 1) 0)
               (setf (aref image (- height y 1) x 2) 0))))
      (vis-log 3 "custom function done"))
    png))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
