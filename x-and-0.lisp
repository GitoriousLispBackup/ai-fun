(in-package :ai-fun.x-and-0)

(defclass x-and-0-board (board)
  ((size :initform 3)
   (point-at-intersection :initform nil)))


;;; @param[in] move : string containing a command (e.g. 'quit') or board
;;; coordinates (represented either as a single integer or as two integers
;;; separated by space).
;;;
;;; @return : nil if invalid coordinates, (list x y) on valid move,
;;;   (list -1 -1) on user abort.
(defun parse-move (board move mark)
  (if (stringp move)
      (if (string-equal move "quit" :end1 (min 4 (length move)))
          (board-move board -1 -1 nil)
          ;; else - check if there is only one integer or two
          (let ( (coords (split-sequence:split-sequence #\Space move))
                (x 0) (y 0))
            (if (= (length coords) 2)
                (progn
                  (setf x (parse-integer (elt coords 0)))
                  (setf y (parse-integer (elt coords 1))))
                (let ( (move-int (parse-integer move)))
                  (setf x (/ (- move-int (mod move-int 10)) 10))
                  (setf y (mod move-int 10))))
            (format t ":debug:parse-move: coord. in string: ~a, ~a~%" x y)
            (if (and (< x 3) (< y 3)
                     (eql (aref (slot-value board 'board-array) x y) nil))
                (board-move board x y mark))))
      ;; else: move = list of coords.
      (board-move board (first move) (second move) mark)))


;;; @return the last move as a list of coordinates
(defun make-move (board player mark)
  (let ((coord nil))
    (loop until
         (setf coord (parse-move board (player-prompt-move player) mark)))
    coord))


;;; :fixme: could be optimized to check only the last move, not the whole board
(defun x-and-0-end-p (board)
  (labels ((three-in-a-row (char pos-x pos-y direction-x direction-y)
             ;; out of board = finished checking in that direction
             (when (or (< pos-x 0) (< pos-y 0) (> pos-x 2) (> pos-y 2))
               (return-from three-in-a-row t))
             ;; empty = not three in a row
             (when (null (board-elt board pos-x pos-y))
               (return-from three-in-a-row nil))
             (and (eql (board-elt board pos-x pos-y) char)
                  (three-in-a-row char 
                                  (+ pos-x direction-x)
                                  (+ pos-y direction-y)
                                  direction-x direction-y))))
    ;; check if last move was (-1,-1) === user abort
    (when (and (last-x board) (last-y board)
               (= (last-x board) (last-y board) -1))
      (return-from x-and-0-end-p t))
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
        (return-from x-and-0-end-p t)))
    ;; check full board
    (dotimes (i 3)
      (dotimes (j 3)
        (when (null (board-elt board i j))
          (return-from x-and-0-end-p nil))))
    t))

  
(defun x-and-0-run (board player1 player2)
  "Main loop"
  (player-game-params player1 3 t)
  (player-game-params player2 3 nil)
  (let ((i 0))
    (loop
       (board-print board)
       (if (eql (mod i 2) 0)
           (player-announce-move player2
                                 (make-move board player1 #\X))
           (player-announce-move player1
                                 (make-move board player2 #\0)))
       (when (x-and-0-end-p board)
         (format t "game over~%")
         (player-cleanup player1)
         (player-cleanup player2)
         (return))
       (incf i))
    (board-print board)))


(defun x0-cart-coord-to-int (list)
  "Cartezian coordinates to one dimension array coordinate"
  (+ (first list) (* (second list) 3)))

(defun x0-int-to-cart-coord (int)
  "Integer representing one dim. array coordinate to cartezian coordinates"
  (list (mod int 3) (floor int 3)))

;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
