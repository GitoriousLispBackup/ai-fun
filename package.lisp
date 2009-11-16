(defpackage :ai-fun.board-games
  (:use :common-lisp)
  (:export :print-board
           :board
           :size
           :point-at-intersection
           :board-array
           :board-elt))

(defpackage :ai-fun.x-and-0
  (:use :common-lisp :ai-fun.board-games)
  (:export :x-and-0-board :parse-move :board-array 
		   :board-elt :x-and-0-end-p :x-and-0-run))

(defpackage :ai-fun.test-unit
  (:use :common-lisp :ai-fun.board-games :ai-fun.x-and-0)
  (:export :test-automatic-all :test-manual-game))
