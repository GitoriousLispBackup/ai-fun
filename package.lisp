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

;; Genetic Algorithm package
(defpackage :ai-fun.ga
  (:use :common-lisp)
  (:export :ga-entity
           :ga-entity-max-func
           :func
           :genome
           :fitness
           :ga-log
           :ga-run
           :ga-find-max))
           
;; ai-fun.visual uses zpng (http://www.xach.com/lisp/zpng/)
(defpackage :ai-fun.visual
  (:use :common-lisp :zpng :ai-fun.ga)
  (:export :save-universe
           :png-generate-white-png
           :save-ga-universe-to-png))

(defpackage :ai-fun.test-unit
  (:use :common-lisp :zpng :ai-fun.board-games :ai-fun.x-and-0 :ai-fun.ga
        :ai-fun.visual)
  (:export :test-automatic-all :test-manual-game :test-game-human-vs-random-ai))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
