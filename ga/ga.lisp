;;; Genetic Algorithms framework

(defclass ga-entity ()
  ((genome
	:initform nil
	:initarg :genome
	:accessor genome)))


(defclass ga-entity-max-func (ga-entity)
  ((func
	:initform nil
	:initarg :func
	:accessor func)))


(defgeneric ga-entity-fitness (ga-entity)
  (:documentation "The fitness function for a GA entity"))

(defgeneric ga-entity-fitness-acceptable-p (ga-entity iteration)
  (:documentation "Return true if the entity is an acceptable result"))

(defgeneric ga-crossover (ga-entity1 ga-entity2)
  (:documentation "Create a new genome by combination of the two entities
  supplied as parameters"))

(defgeneric ga-mutation (ga-entity)
  (:documentation "Change the genome randomly"))


;;; implementation for fixnums

(defmethod ga-entity-fitness ((pseudo-entity fixnum))
  "Fitness for fixnum objects (used for basic testing). Fitness of a number is
that number (f(n)=n)."
  pseudo-entity)

(defmethod ga-crossover ((ga-entity1 fixnum) (ga-entity2 fixnum))
  "Crossover for fixnums - do nothing"
  (list ga-entity1 ga-entity2))

(defmethod ga-mutation ((ent fixnum))
  "Flip one bit"
  (let ((bit-pos (random 32))) ; :fixme: - add a param?
	(setf ent (boole boole-xor ent (ash 1 bit-pos)))
	(format t ":debug: bit to flip: ~a~%" bit-pos)
	ent))


;;; implementation for ga-entity-max-func

(defmethod print-object ((ent ga-entity-max-func) stream)
  "Print a ga-entity-max-func object"
  (format stream "GA max func. obj: genome=~a, fitness=~a~%"
		  (genome ent) (ga-entity-fitness ent)))

(defmethod ga-entity-fitness ((ent ga-entity-max-func))
  "Fitness for GA entities for calculating max. value of a function"
  (if (null (func ent))
	  (error "GA entity without a function")
	  (funcall (func ent) (genome ent))))

(defmethod ga-mutation ((ent ga-entity-max-func))
  "Flip one bit"
  (let ((bit-pos (random 32))) ; :fixme: - add a param?
	(setf (genome ent) (boole boole-xor (genome ent) (ash 1 bit-pos)))
	(format t ":debug: bit to flip: ~a~%" bit-pos)))


;;; GA functions
  
(defun ga-selection (population new-population-size)
  "Make a selection (select best <new-population-size> entities from <population>"
  ;; validate params
  (when (> new-population-size (length population))
	(error "invalid params in ga-selection (population size too small)"))
  ;; compute fitness for everybody
  (dolist (ent population)
	(ga-entity-fitness ent))
  (let ((len (length population)))
	(butlast (sort population #'>)
			 (- len new-population-size))))

;; :fixme: - a more 'functional' approach?
(defun ga-reproduction (population cross-probability)
  "Perform reproduction"
  (let ((new-population nil) (len (length population)))
	(dotimes (i (floor (/ len 2)))
	  (let ((p1 (nth (random len) population))
			(p2 (nth (random len) population)))
		(if (> (random 1.0) cross-probability)
			  (setf new-population (nconc new-population (ga-crossover p1 p2)))
			  (setf new-population (nconc new-population (list p1 p2))))))
	(when (< (length new-population) len)
	  (setf new-population (nconc new-population (list (nth (random len) population)))))
	(ga-print-population new-population "inside ga-reproduction - ")
	new-population))
	  

(defun ga-finished-p (population current-time max-time)
  "Check if there is an entity that is good enough, or time is up (:todo:)"
  (>= current-time max-time))


(defun ga-print-population (population &optional prefix-string)
  "Print population to stdout"
  (when prefix-string
	(format t prefix-string))
  (format t "population: ")
  (dolist (ent population)
	(format t "~a " ent))
  (format t "~%"))
	
(defun ga-run (population max-time &key (cross-probability 0.5) (mutation-probability 0.5))
  "Run the simulation"
  ;; main loop
  (let ((current-time 0))
	(ga-print-population population "initial ")
	(loop
	   (format t "* time: ~a~%" current-time)
	   (let ((new-population (ga-selection population (length population))))
		 (ga-print-population new-population "after selection - ")
		 ;; reproduction (with or without crossover)
		 (setf new-population (ga-reproduction new-population cross-probability))
		 (ga-print-population new-population "after reproduction - ")
		 ;; mutation
		 (dolist (ent new-population)
		   (when (> (random 1.0) mutation-probability)
			 (setf ent (ga-mutation ent))))
		 (ga-print-population new-population "after mutation - ")
		 (when (ga-finished-p new-population current-time max-time)
		   (return))
		 (setf population new-population))
	   (incf current-time)))) ; :fixme: return what?


(defun ga-test ()
  "Test GA functions / classes"
  (and
   (= (ga-entity-fitness 5) 5)
   (ga-finished-p nil 5 5)
   (equal (ga-selection (list 3 5 9 7 1) 3) (list 9 7 5))
   (eql (ga-entity-fitness
		 (make-instance 'ga-entity-max-func :genome 5 :func #'(lambda (x) (* x x))))
		25)
   ;; add tests above 
))
