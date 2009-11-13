(use-package 'ai-fun.x-and-0)


(defun test-parse-move ()
  (let ((board (make-instance 'ai-fun.x-and-0::x-and-0-board)))
    (if (and (eql (ai-fun.x-and-0::parse-move board 68 #\X) nil)
             (eql (ai-fun.x-and-0::parse-move board 92 #\X) nil)
             (eql (ai-fun.x-and-0::parse-move board 22 #\X) #\X))
        (format t "All tests passed!")
        (format t "test-parse-move failed"))))

(defun test-manual-game ()
  (ai-fun.x-and-0::x-and-0-run 
   (make-instance 'ai-fun.x-and-0::x-and-0-board)))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
