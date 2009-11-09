
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

   ;; some games use the intersection (go), others use the spaces in the grid (x-and-0)
   ;; this property is used when the board is displayed
   (point-at-intersection
	:initarg :point-at-intersection
	:initform t
	:reader point-at-intersection)

   ;; will be initialized by the initialize-instance (see below)
   ;; :todo: - add accessor
   (board-array
	:initform nil)))


(defmethod initialize-instance :after ((brd board) &key)
  (setf (slot-value brd 'board-array)
		(make-array (list (size brd) (size brd)) :initial-element nil)))

(defgeneric print-board (board)
  (:documentation "Print a game board at the standard output using ASCII chars"))

;;; end board class

  
(defun parse-move (board move mark)
  (let ((x (/ (- move (mod move 10)) 10)) 
	(y (mod move 10)))
    (if (and (< x 3) (< y 3) 
	 (eql (aref (slot-value board 'board-array) x y) nil)) 
	(setf (aref (slot-value board 'board-array) x y) mark))))

(defun prompt-move (player)
  (format *query-io* "~%~a's move: " player)
  (force-output *query-io*)
  (read-line *query-io*))

(defmethod print-board ((brd board))
  (flet ( (print-sep-line (width)
			  (format t ".")
			  (dotimes (j width)
				(format t "-."))
			  (format t "~%")))
	(if (point-at-intersection brd)
		(error "Not implemented yet.")
		(progn
		  ;; first line
		  (print-sep-line (size brd))
		  ;; main content
		  (dotimes (i (size brd))
			(format t "|")
		  (dotimes (j (size brd))
			(if (aref (slot-value brd 'board-array) i j)
				(format t "~a" (aref (slot-value brd 'board-array) i j))
				(format t " "))
			(format t "|"))
		  (format t "~%")
		  (print-sep-line (size brd)))))))

(defun board-game-run (board)
  (dotimes (i (array-total-size (slot-value board 'board-array)))
    (print-board board)
    (let ((player 0) (mark 0))
      (if (eql (mod i 2) 0)
	  (progn
	    (setq player 1) 
	    (setq mark #\X))
	  (progn
	    (setq player 2)
	    (setq mark #\0)))
      (loop until
	     (parse-move board (parse-integer (prompt-move player)) mark))))
  (print-board board))
