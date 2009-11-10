(defpackage :ai-fun.board-games
  (:use :common-lisp))
(in-package :ai-fun.board-games)
;;; board class

(defclass board ()
  ((size
	:initarg :size
	:initform (error "Must supply board size")
	:reader size)

   ;; :fixme: - probably not needed
   (first-is-black
	:initarg :first-is-black
	:initform t
	:reader first-is-black)

   ;; Some games use the intersection (go), others use the spaces in the grid
   ;; (x-and-0)
   ;; This property is used when the board is displayed
   (point-at-intersection
	:initarg :point-at-intersection
	:initform t
	:reader point-at-intersection)

   ;; Will be initialized by the initialize-instance (see below)
   ;; :todo: - add accessor
   (board-array
	:initform nil)))


(defmethod initialize-instance :after ((brd board) &key)
  (setf (slot-value brd 'board-array)
		(make-array (list (size brd) (size brd)) :initial-element nil)))

;;; end board class


(defgeneric print-board (board)
  (:documentation "Print a game board at the standard output using ASCII
  chars"))

(defmethod print-board ((brd board))
  (flet ( 
		 (print-sep1-line (width)
		   (format t "-")
		   (dotimes (j width)
			 (format t "-"))
		   (format t "-~%"))

		 (print-sep2-line (width)
		   (format t ".")
		   (dotimes (j width)
			 (format t "-."))
		   (format t "~%")))
	(if (point-at-intersection brd)
		;; if point is at intersection (eg. go)
		(progn
		  ;; first line
		  (print-sep1-line (size brd))
		  ;; main content
		  (dotimes (i (size brd))
			(format t "|")
			(dotimes (j (size brd))
			  (if (aref (slot-value brd 'board-array) i j)
				  (format t "~a" (aref (slot-value brd 'board-array) i j))
				  (format t ".")))
			(format t "|~%"))
		  ;; last line
		  (print-sep1-line (size brd)))
		;; else - point in grid space (e.g. x-and-0)
		(progn
		  ;; first line
		  (print-sep2-line (size brd))
		  ;; main content
		  (dotimes (i (size brd))
			(format t "|")
		  (dotimes (j (size brd))
			(if (aref (slot-value brd 'board-array) i j)
				(format t "~a" (aref (slot-value brd 'board-array) i j))
				(format t " "))
			(format t "|"))
		  (format t "~%")
		  (print-sep2-line (size brd)))))))
