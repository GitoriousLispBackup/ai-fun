;;; run from SLIME or make a shortcut for: 
;;;   (asdf:oos 'asdf:load-op 'ai-fun) or
;;;   (require 'ai-fun)

(asdf:defsystem :ai-fun
    :components ((:file "package")
                 (:file "board-games"
                        :depends-on ("package"))
                 (:file "x-and-0"
                        :depends-on ("package"
                                     "board-games"))
                 (:file "test/x-and-0-test"
                        :depends-on ("package"
                                     "board-games"
                                     "x-and-0"))))

;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
