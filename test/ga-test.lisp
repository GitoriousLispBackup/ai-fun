(in-package :ai-fun.test-unit)


;;; GA & visual tests

;;; http://www.wolframalpha.com/input/?i=%28-x^2%2B1000x%29%2F500
(defun ga-run-parabola-1-png ()
  "Find max of (-x^2+1000x)/500 (max=500 at x=500). Save simulation to png
files."
  (let ((func #'(lambda (x) (/ (+ (* -1 (* x x)) (* 1000 x)) 500)))
        (trans (list 200 0.25 2000 0.25)))
    ;; simulation params:
    ;; population size = 10, 100 iterations, png output size 1200x1000
    (ga-find-max 10 func 0 100000 100
                 :mutation-probability 0.5
                 :output-func 'save-ga-universe-to-png
                 :output-extra-param (append
                                      (list (png-generate-white-png
                                             1200 1000 trans func))
                                      trans))))

;;; http://www.wolframalpha.com/input/?i=%28-x%2B10%29%28x-150%29%28x-1000%29%28x-750%29%2F10^7
(defun ga-run-func-x4-png ()
  "Find max of (-x+10)(x-150)(x-1000)(x-750)/1*10^7 (max~=1003 at x~=893). Save
simulation to png files."
  (let ((func #'(lambda (x) (/ (* (- 10 x) (- x 150) (- x 750) (- x 1000))
							   20000000)))
		(trans (list 0 0.5 1500 0.3)))
    ;; simulation params:
    ;; population size = 10, 50 iterations, png output size 1200x1000
    (ga-find-max 10 func 0 50000 50
                 :mutation-probability 0.5
                 :output-func 'save-ga-universe-to-png
                 :output-extra-param (append
                                      (list (png-generate-white-png
                                             1200 1000 trans func))
                                      trans))))

;;; http://www.wolframalpha.com/input/?i=-x^2%2B15x%2B20
(defun ga-run-parabola-2 ()
  "Find max of -x^2+15x+20 (max=305/4=76.25 at x=15/2=7.5)"
  (ga-find-max 10 #'(lambda (x) (+ (* -1 (* x x)) (* 15 x) 20))
			   0 100000 ; min&max
			   200 ; iterations
			   :mutation-probability 0.5))


;;; PNG tests

(defun png-save-white-png (filename width height transformations
                           &optional (func nil))
  (write-png (png-generate-white-png width height transformations func)
             filename))
