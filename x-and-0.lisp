(in-package :ai-fun.x-and-0)

(defclass x-and-0-board (board)
  ((size :initform 3)
   (point-at-intersection :initform nil)))

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

;;; :fixme: - could be optimized - to check only the last move, not the whole board
(defun x-and-0-end-p (board)
  (labels ((three-in-a-row (char pos-x pos-y direction-x direction-y)
             ;; out of board = finished checking in that direction
             (when (or (< pos-x 0) (< pos-y 0) (> pos-x 2) (> pos-y 2))
               (return-from three-in-a-row t))
             ;; empty = not three in a row
             (when (null (board-elt board pos-x pos-y))
               (return-from three-in-a-row nil))
             (and (eql (board-elt board pos-x pos-y) char)
                  (three-in-a-row char (+ pos-x direction-x) (+ pos-y direction-y)
                                  direction-x direction-y))))
    ;; check diagonals
    (when (or
           (three-in-a-row (board-elt board 0 0) 0 0 1 1)
           (three-in-a-row (board-elt board 0 2) 0 2 1 -1))
      (return-from x-and-0-end-p t))
    ;; check lines
    (dotimes (i 3)
      (when (three-in-a-row (board-elt board 0 i) 0 i 1 0)
        (return-from x-and-0-end-p t)))
    ;; check columns
    (dotimes (i 3)
      (when (three-in-a-row (board-elt board i 0) i 0 0 1)
        (return-from x-and-0-end-p t)))))
  
(defun x-and-0-run (board)
  (let ((i 0))
    (loop
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
              (parse-move board (parse-integer (prompt-move player)) mark)))
       (when (x-and-0-end-p board)
         (format t "game over~%")
         (return))
       (incf i))
    (print-board board)))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
