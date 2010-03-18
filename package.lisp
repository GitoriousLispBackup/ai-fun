(defpackage :ai-fun.board-games
  (:use :common-lisp :usocket)
  (:export :print-board
           :board
           :size
           :point-at-intersection
           :board-array
           :board-elt
           :board-move
           :board-print
           :last-x
           :last-y
           :player-game-params
           :player-prompt-move
           :player-announce-move
           :player-cleanup
           :*socket*))

(defpackage :ai-fun.x-and-0
  (:use :common-lisp :ai-fun.board-games)
  (:export :x-and-0-board :parse-move :board-array 
		   :board-elt :x-and-0-end-p :x-and-0-run :board-move))

(defpackage :ai-fun.test-unit
  (:use :common-lisp :ai-fun.board-games :ai-fun.x-and-0)
  (:export :test-automatic-all :test-manual-game :test-game-human-vs-random-ai))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
