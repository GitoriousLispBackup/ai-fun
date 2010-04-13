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
           :player
           :player-game-params
           :player-prompt-move
           :player-announce-move
           :player-cleanup
           :*socket*))

(defpackage :ai-fun.x-and-0
  (:use :common-lisp :ai-fun.board-games)
  (:export :x-and-0-board :parse-move :board-array 
		   :board-elt :x-and-0-end-p :x-and-0-run :board-move
           :x0-player-random))

(defpackage :ai-fun.test-unit
  (:use :common-lisp :ai-fun.board-games :ai-fun.x-and-0)
  (:export :test-automatic-all :test-manual-game :test-game-human-vs-random-ai))

;; Genetic Algorithm package
(defpackage :ai-fun.ga
  (:use :common-lisp)
  (:export :ga-entity
           :ga-entity-max-func
           :ga-run))
           
;; ai-fun.visual uses zpng (http://www.xach.com/lisp/zpng/)
(defpackage :ai-fun.visual
  (:use :common-lisp :zpng)
  (:export :save-universe))

;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
