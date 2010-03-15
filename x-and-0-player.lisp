(in-package :ai-fun.x-and-0)

;;; implementation of a player that plays random moves
(defclass x0-player-random (player)

  ((interface
    :initform :ai)

  ;; human readable name of the player
  (name
   :initform "ai random")

   ;; internal representation of the x0 board = a list (KISS)
   (board
	:initform (list nil nil nil nil nil nil nil nil nil))))
;;; end x0-player-random class


(defmethod player-prompt-move ((player x0-player-random))
  ;; pick a random number between 0 and 9 (exclusive) and return the first board
  ;; position that is empty
  (let ( (start (random 9)))
	(format t ":debug:~a i'm pondering ...~%" player)
	(loop while (elt (slot-value player 'board) start) do
		 (if (= start 8) (setf start 0) (incf start)))
	(format t ":debug:~a my move: ~a~%" player (x0-int-to-cart-coord start))
	(setf (elt (slot-value player 'board) start) 1)
	(x0-int-to-cart-coord start)))


(defmethod player-announce-move ((player x0-player-random) move)
  (format t ":debug:~a peer's move: ~a~%" player move)
  (setf (elt (slot-value player 'board) (x0-cart-coord-to-int move)) 1))


(defmethod player-cleanup ((player x0-player-random))
  ;; nohing to clean
)
