;;; Genetic Algorithms framework

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ga-log-level* 2))
  
(defmacro ga-log (log-level &rest print-list)
  (if (<= log-level *ga-log-level*)
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

;; population must be a list
(defmacro ga-print-population (log-level prefix-string &rest population)
  "Print population and fitness to stdout (one line)"
  ;; :fixme: use pprint or equivalent
  (if (<= log-level *ga-log-level*)
	  `(progn
		 (dotimes (i ,log-level) (format t " "))
		 (when (>= ,log-level 3)
		   (format t ":debug:"))
		 (format t ,prefix-string)
		 (dolist (elt ,@population)
		   (format t "~a " elt))
		 (terpri))))


;;; GA classes

(defclass ga-entity ()
  ((genome
	:initform nil
	:initarg :genome
	:accessor genome)

   (fitness
	:initform nil
	:accessor fitness)))


(defclass ga-entity-max-func (ga-entity)
  ((func
	:initform nil
	:initarg :func
	:accessor func)))


(defgeneric ga-entity-duplicate (entity)
  (:documentation "Create a new entity (allocating memory)"))

(defgeneric ga> (ga-entity1 ga-entity2)
  (:documentation "Compare the fitness of two entities"))

(defgeneric ga-entity-fitness-acceptable-p (ga-entity iteration)
  (:documentation "Return true if the entity is an acceptable result"))

(defgeneric ga-crossover (ga-entity1 ga-entity2)
  (:documentation "Create a new genome by combination of the two entities
  supplied as parameters"))

(defgeneric ga-mutation (ga-entity)
  (:documentation "Change the genome randomly"))


;;; implementation for basic ga entities (genome=fixnum, fitness(x)=x)

(defmethod print-object ((ent ga-entity) stream)
  "Print a ga-entity object"
  (format stream "GA:~a(~a)" (genome ent) (fitness ent)))

(defmethod fitness ((ent ga-entity))
  "Reader for fitness slot (compute fitness if not available). For basic
ga-entity, fitness=genome."
  (when (null (slot-value ent 'fitness))
	(setf (slot-value ent 'fitness) (genome ent)))
  (slot-value ent 'fitness))

(defmethod (setf genome) (value (ent ga-entity))
  "setf function for genome"
  (setf (slot-value ent 'genome) value)
  ;; reset fitness
  (setf (slot-value ent 'fitness) nil))

(defmethod ga-entity-duplicate ((ent ga-entity))
  "Allocate a new entity"
  (let ((new-ent (make-instance 'ga-entity :genome (genome ent))))
	(fitness new-ent)
	new-ent))

(defmethod ga> ((ent1 ga-entity) (ent2 ga-entity))
  "Compare two entitites (by fitness)"
  (> (fitness ent1) (fitness ent2)))

(defmethod ga-crossover ((ent1 ga-entity) (ent2 ga-entity))
  "Crossover for ga-entity. Allocates new entities."
  (let ((crossed-list (random-crossover-integers (genome ent1) (genome ent2))))
	(list
	 (make-instance 'ga-entity :genome (first crossed-list))
	 (make-instance 'ga-entity :genome (second crossed-list)))))

(defmethod ga-mutation ((ent ga-entity))
  "Flip one bit and reset the genome"
  (setf (genome ent) (random-bit-flip (genome ent))))


;;; implementation for ga-entity-max-func

(defmethod print-object ((ent ga-entity-max-func) stream)
  "Print a ga-entity-max-func object"
  (format stream "GAMax:~a(~3$)" (genome ent) (fitness ent)))

(defmethod fitness ((ent ga-entity-max-func))
  "Fitness for GA entities for calculating max. value of a function"
  (when (null (slot-value ent 'fitness))
	(if (null (func ent))
		(error "GA entity without a function")
		(setf (fitness ent) (funcall (func ent) (genome ent)))))
  (slot-value ent 'fitness))

(defmethod ga-entity-duplicate ((ent ga-entity-max-func))
  "Allocate a new entity"
  (let ((new-ent (make-instance 'ga-entity-max-func
								:genome (genome ent)
								:func (func ent))))
	(fitness new-ent)
	new-ent))

(defmethod ga-crossover ((ent1 ga-entity-max-func) (ent2 ga-entity-max-func))
  "Crossover for ga-entity-max-func. Allocates new entities."
  (let ((crossed-list (random-crossover-integers (genome ent1) (genome ent2))))
	(list
	 (make-instance 'ga-entity-max-func
					:genome (first crossed-list) :func (func ent1))
	 (make-instance 'ga-entity-max-func
					:genome (second crossed-list) :func (func ent2)))))

(defmethod ga-mutation ((ent ga-entity-max-func))
  "Flip one bit and create a new mutant entity"
  (setf (genome ent) (random-bit-flip (genome ent))))

(defmethod ga> ((ent1 ga-entity-max-func) (ent2 ga-entity-max-func))
  (> (fitness ent1) (fitness ent2)))

;;; helper functions

(defun random-bit-flip (num)
  (let ((bit-pos (random 17))) ; fixnums have at least 15 bits
	(ga-log 4 "bit to flip:" bit-pos)
	(boole boole-xor num (ash 1 bit-pos))))


;;; only for positive integers (:fixme:)
(defun random-crossover-integers (no1 no2 &key
								  (force-position nil position-supplied-p))
  "Switch parts of the binary representation of the two numbers. Return a list
								  with the new numbers"
  (when (= no1 no2 0)
	(return-from random-crossover-integers (list 0 0)))
  (let ((len (max (integer-length no1) (integer-length no2)))
		(pos 0))
	(if position-supplied-p
		(progn
		  (when (> force-position len)
			(error "Invalid force-position parameter"))
		  (setf pos force-position))
		(setf pos (random len))) ; :fixme: len+1 ?
	(let ((lsb (byte (+ pos 1) 0))
		  (msb (byte (- len pos 1) (+ pos 1)))
		  (ret1 0) (ret2 0))
	  (ga-log 5 "pos=" pos ", len=" len ", no1 msb=" (ldb msb no1)
			  ", no1 lsb=" (ldb lsb no1) ", no2 msb=" (ldb msb no1)
			  ", no2 lsb=" (ldb lsb no2))
	  ;; ret1 msb = no1 msb
	  (setf ret1 (dpb (ldb msb no1) msb ret1))
	  ;; ret1 lsb = no2 lsb
	  (setf ret1 (dpb (ldb lsb no2) lsb ret1))
	  ;; ret2 msb = no2 msb
	  (setf ret2 (dpb (ldb msb no2) msb ret2))
	  ;; ret2 lsb = no1 lsb
	  (setf ret2 (dpb (ldb lsb no1) lsb ret2))
	  (list ret1 ret2))))


;;; GA functions
  
(defun ga-selection (population new-population-size)
  "Make a selection (select best <new-population-size> entities from
<population>"
  ;; validate params
  (when (> new-population-size (length population))
	(error "invalid params in ga-selection (population size too small)"))
  (let ((len (length population)))
	(butlast (sort population #'ga>)
			 (- len new-population-size))))

;; :fixme: - a more 'functional' approach?
(defun ga-reproduction (population new-length cross-probability)
  "Perform reproduction"
  (let ((new-population nil) (len (length population)))
	(dotimes (i (floor (/ new-length 2)))
	  (let ((p1 (nth (random len) population))
			(p2 (nth (random len) population)))
		(if (< (random 1.0) cross-probability)
			(setf new-population (nconc new-population (ga-crossover p1 p2)))
			(setf new-population (nconc new-population
										(list (ga-entity-duplicate p1)
											  (ga-entity-duplicate p2)))))))
	(when (< (length new-population) new-length)
	  (setf new-population (nconc new-population
								  (cons (ga-entity-duplicate (nth (random len)
																  population))
										nil))))
	new-population))
	  
(defun ga-mutate (population mutation-probability)
  "Mutate population in list (according to mutation-probability"
  (let ((elt population))
	(loop while elt do
		 (ga-log 5 "pre. elt=" (car elt) " type-of elt=" (type-of elt))
		 (when (< (random 1.0) mutation-probability)
		   (ga-mutation (car elt))
		   (ga-log 5 "elt after mutation=" (car elt)))
		 (ga-log 5 "post. elt=" (car elt) " type-of elt=" (type-of elt))
		 (setf elt (cdr elt)))))

(defun ga-finished-p (population current-time max-time)
  "Check if there is an entity that is good enough, or time is up (:todo:)"
  (declare (ignore population))
  (>= current-time max-time))

(defun ga-run (population max-time &key
			   (cross-probability 0.5) (mutation-probability 0.5))
  "Run the simulation"
  ;; main loop
  (let ((current-time 0) (len (length population)))
	(ga-print-population 3 "initial " population)
	(loop
	   (ga-log 1 "* time: " current-time)
	   (let ((new-population (ga-selection population (floor (/ len 2)))))
		 (ga-print-population 3 "after selection - " new-population)
		 ;; reproduction (with or without crossover)
		 (setf new-population
			   (ga-reproduction new-population len cross-probability))
		 (ga-print-population 3 "after reproduction - " new-population)
		 ;; mutation
		 (ga-mutate new-population mutation-probability)
		 (ga-print-population 3 "after mutation - " new-population)
		 (when (ga-finished-p new-population current-time max-time)
		   (ga-print-population 0 "* final population - " new-population)
		   (return))
		 (setf population new-population))
	   (ga-print-population 2 "* population: " population)
	   (incf current-time)))) ; :fixme: return what?


(defun ga-test ()
  "Test GA functions / classes"
  (let ((simple-ent1 (make-instance 'ga-entity :genome 5))
		(simple-ent2 (make-instance 'ga-entity :genome 7))
		(max-ent1 (make-instance 'ga-entity-max-func :genome 5
								 :func #'(lambda (x) (* x x)))))
	(and
	 (= (fitness simple-ent1) 5)
	 (ga-finished-p nil 5 5)
	 (equal (ga-selection (list simple-ent1 simple-ent2) 2)
			(list simple-ent2 simple-ent1))
	 (eql (fitness max-ent1) 25)
	 (equal (random-crossover-integers 4 7 :force-position 0) (list 5 6))
   ;; add tests above 
)))

(defun ga-make-entity-list ()
  "Create a simple list with ga-entities"
  (list 
		   (make-instance 'ga-entity :genome 1)
		   (make-instance 'ga-entity :genome 3)
		   (make-instance 'ga-entity :genome 5)
		   (make-instance 'ga-entity :genome 7)
		   (make-instance 'ga-entity :genome 9)
		   (make-instance 'ga-entity :genome 11)))

(defun ga-run-1 ()
  "Run a simulation with ga-entities"
  (ga-run (ga-make-entity-list) 5))

(defun ga-generate-random-entities (max-len upper-limit func)
  (let ((generated-list nil))
	(dotimes (i max-len)
	  (setf generated-list
			(nconc generated-list
				   (list (make-instance 'ga-entity-max-func
										:genome (random upper-limit)
										:func func)))))
	generated-list))

;;; http://www.wolframalpha.com/input/?i=-x^2%2B15x%2B20
(defun ga-run-parabola-1 ()
  "Find max of -x^2+15x+20 (max=305/4=76.25 at x=15/2=7.5)"
  (ga-run (ga-generate-random-entities
		   10 100000 #'(lambda (x) (+ (* -1 (* x x)) (* 15 x) 20)))
		  200
		  :mutation-probability 0.5))
