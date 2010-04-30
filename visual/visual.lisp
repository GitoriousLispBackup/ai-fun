;;; visual stuff for GA simulations
(in-package :ai-fun.visual)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *vis-log-level* 1))
  
(defmacro vis-log (log-level &rest print-list)
  (if (<= log-level *vis-log-level*)
	  `(progn
		 ;; :fixme: replace format with something else
         ,(when (plusp log-level)
                `(dotimes (i ,log-level) (format t " ")))
		 ,(when (>= log-level 3)
                `(format t ":debug:"))
         ;; :fixme: bad bad - use gensym
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

;; time : if time >= 0, the name of the png will be <prefix>XXXX.png (XXXX=time)
;; extra-param :
;;   (png file-prefix x-translation x-scale-factor y-translation y-scale-factor)
;; png : is the initial png (white, with axis & function graphic)
(defun save-ga-universe-to-png (entities time &optional extra-param)
  "Save GA universe to png file (filename will be '<prefix><time>.png')"
  ;; :fixme: - if extra-param missing - create an empty png
  (let* ((png (copy-png (first extra-param)))
         (w (width png)))
    (dolist (ent entities)
      (let* ((y-ent (floor (fitness ent)))
             (x-ent (genome ent))
             (png-coord (coord-real-to-viewport (list x-ent y-ent)
                                                (cddr extra-param)))
            (x-png (first png-coord)))
        (vis-log 4 "ent(" x-ent ")=" y-ent)
        (vis-log 4 "vert. line at x=" x-png)
        (when (and (>= x-png 0) (< x-png w))
          (vis-log 4 "drawing vert. line at x=")
          ;; draw with green (2 lines)
          (png-vertical-line png x-png (list 0 255 0))
          (png-vertical-line png (+ x-png 1) (list 0 255 0)))))
    (let ((filename
           (if (cadr extra-param) (cadr extra-param) "universe")))
      (when (>= time 0)
        (setf filename
              (concatenate 'string filename (format nil "~4,'0d" time))))
      (setf filename (concatenate 'string filename ".png"))
      (vis-log 2 "new png image: " filename)
      (write-png png filename))))

;;; png : a png image
;;; x-png : x coordinate
;;; rgb : rgb colors (a list)
(defun png-vertical-line (png x-png rgb &optional y1 y2)
  "Draw a vertical line"
  (let ((png-array (data-array png))
        (y-from (if (null y1) 0 (min y1 y2)))
        (y-to (if (null y2) (- (height png) 1) (max y1 y2))))
    (when (and (>= x-png 0) (< x-png (width png)))
      (vis-log 5 "png-vertical-line: from " y-from " to " y-to)
      (loop for y from y-from to y-to do
           (setf (aref png-array y x-png 0) (first rgb))
           (setf (aref png-array y x-png 1) (second rgb))
           (setf (aref png-array y x-png 2) (third rgb))))))

;;; png : a png image
;;; y-png : y coordinate
;;; rgb : rgb colors (a list)
(defun png-horizontal-line (png y-png rgb)
  "Draw a horizontal line"
  (let ((png-array (data-array png)))
    (when (and (>= y-png 0) (< y-png (height png)))
      (loop for x from 0 to (- (width png) 1) do
           (setf (aref png-array y-png x 0) (first rgb))
           (setf (aref png-array y-png x 1) (second rgb))
           (setf (aref png-array y-png x 2) (third rgb))))))

(defun png-generate-white-png (width height transformations &optional func)
  "Generate a white png image and draw the func graphic on it."
  (let* ((png (make-instance 'png
							 :color-type :truecolor
							 :width width :height height))
         (image (data-array png)))
    (vis-log 3 "New png created: width=" width ", height=" height)
    ;; make white (image-data is a vector mapped on data-array array)
    (fill (image-data png) 255)
    ;; draw axis (with double line)
    (let* ((xy0 (coord-real-to-viewport (list 0 0) transformations))
           (x0 (first xy0)) (y0 (second xy0)))
      (vis-log 3 "xy0=" xy0)
      (when (and (>= x0 0) (< x0 width))
        (png-vertical-line png x0 (list 0 0 0))
        (png-vertical-line png (+ x0 1) (list 0 0 0)))
      (when (and (>= y0 0) (< y0 width))
        (png-horizontal-line png (- height y0 1) (list 0 0 0))
        (png-horizontal-line png (- height y0 2) (list 0 0 0))))
	;; draw the function
    (when func
      (vis-log 5 "draw func: x-translation=" (first transformations)
               " x-scale=" (second transformations))
      (let ((y-prev nil) (h1 (- height 1)))
        (loop for x from 0 to (- width 2) do
             (let* ((x-real (first (coord-viewport-to-real (list x 0)
                                                           transformations)))
                    (y-real (funcall func x-real))
                    (y (second (coord-real-to-viewport (list x-real y-real)
                                                       transformations))))
               (when (= (mod x 111) 0)
                 (vis-log 5 "draw func: xy-png=" x "," y " xy-real=" x-real "," 
                          y-real))
               (let* ((y-from nil) (y-to nil))
                 (cond
                   ((minusp y)
                    (when (and (not (null y-prev)) (>= y-prev 0))
                      (setf y-from y-prev)
                      (setf y-to 0)))
                   ((>= y height)
                    (when (and (not (null y-prev)) (< y-prev height))
                      (setf y-from y-prev)
                      (setf y-to h1)))
                   (t
                    (when (not (null y-prev))
                      (setf y-from (cond ((minusp y-prev) 0)
                                         ((>= y-prev height) h1)
                                         (t y-prev))))
                    (setf y-to y)
                    (setf (aref image (- h1 y) x 0) 255)
                    (setf (aref image (- h1 y) x 1) 0)
                    (setf (aref image (- h1 y) x 2) 0)
                    (setf (aref image (- h1 y) (+ x 1) 0) 255)
                    (setf (aref image (- h1 y) (+ x 1) 1) 0)
                    (setf (aref image (- h1 y) (+ x 1) 2) 0)))
                 (when (and (not (null y-from)) (not (null y-to)))
                   (png-vertical-line png x (list 255 0 0)
                                      (- h1 y-from) (- h1 y-to))
                   (png-vertical-line png (+ x 1) (list 255 0 0)
                                      (- h1 y-from) (- h1 y-to)))
                 (setf y-prev y)))))
      (vis-log 3 "custom function done"))
    png))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
