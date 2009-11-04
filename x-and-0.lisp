<html>
<head><title>gopher://sdf.lonestar.org/0/users/bulibuta/devel/lisp/x-and-0.lisp</title></head>
<body>
<pre>
(defvar *board* (make-array '(3 3) :initial-element nil))

(defun parse-move (move mark)
  (let ((x (/ (- move (mod move 10)) 10)) (y (mod move 10)))
    (setf (aref *board* x y) mark)))

(defun prompt-move (player)
  (format *query-io* "~%~a's move: " player)
  (force-output *query-io*)
  (read-line *query-io*))

(defun print-board ()
  (dotimes (i 3)
    (format t " .---.---.---. ~% | ")
    (dotimes (j 3)
      (if (eql (aref *board* i j) nil)
	  (format t " ")
	  (format t "~a" (aref *board* i j)))
      (format t " | "))
    (format t "~%"))
  (format t " '---'---'---'"))

(defun start-game ()
  (dotimes (i 9)
    (print-board)
    (let ((player 0) (mark 0))
      (if (eql (mod i 2) 0)
	  (progn
	    (setq player 1) (setq mark #\X))
	  (progn
	    (setq player 2) (setq mark #\0)))
      (parse-move (parse-integer (prompt-move player)) mark)))
  (print-board))
.
</pre>

</body></html>