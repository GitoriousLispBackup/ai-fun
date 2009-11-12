;; run from SLIME or make a shortcut for: 
;;   (asdf:oos 'asdf:load-op 'ai-fun)
;;
(asdf:defsystem :ai-fun
    :components ((:file "board-games")
				 (:file "x-and-0"
						:depends-on ("board-games"))
				 (:file "test/x-and-0-test"
						:depends-on ("board-games"
									 "x-and-0"))))

;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: t
;;; End: