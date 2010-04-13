;;; visual stuff for GA simulations
(in-package :ai-fun.visual)

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


(defun save-ga-universe-to-png-1 (entities time)
  "Save GA universe to png file (1200x1024, named 'universeXXXX.png')"
  (let* ((png (make-instance 'png
							 :color-type :truecolor
							 :width 1200
							 :height 1024))
         (image (data-array png)))
	;; make white (:fixme: this is soooo inneficient)
	(loop for x from 0 to 1199 do
		 (loop for y from 0 to 1023 do
			  (setf (aref image y x 0) 255)
			  (setf (aref image y x 1) 255)
			  (setf (aref image y x 2) 255)))
	;; first draw the function
	(loop for x from 0 to 1199 do
		 (let ((y (floor (funcall (func (first entities)) x))))
		   (when (and (>= y 0) (<= y 1023))
			 (setf (aref image (- 1024 y) x 0) 255)
			 (setf (aref image (- 1024 y) x 1) 0)
			 (setf (aref image (- 1024 y) x 2) 0))))
	(write-png png (format nil "universe~4,'0d.png" time))))


;;; http://www.wolframalpha.com/input/?i=-x^2%2F4%2B30x-25
(defun ga-run-parabola-1-png ()
  "Find max of -x^2/4+30x-25 (max=875 at x=60)"
  (ga-find-max 10 #'(lambda (x) (+ (* (/ -1 4) (* x x)) (* 30 x) -25))
			   0 100000 ; min&max
			   200 ; iterations
			   :mutation-probability 0.5
			   :output-func 'save-ga-universe-to-png-1))

;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
