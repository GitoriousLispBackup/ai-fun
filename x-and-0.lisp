;;;(use-package :board-games) ;; :todo:

(defclass x-and-0-board (board)
  ( (size
	 :initform 3)
	(point-at-intersection
	 :initform nil)))

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

(defun x-and-0-run (board)
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
