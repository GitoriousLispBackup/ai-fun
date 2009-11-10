(use-package 'ai-fun.x-and-0-board)


(defun test-parse-move ()
  (let ((board (make-instance 'x-and-0-board)))
	(if (and (eql (parse-move board 68 #\X) nil)
			 (eql (parse-move board 92 #\X) nil)
			 (eql (parse-move board 22 #\X) #\X))
		(format t "All tests passed!")
		(format t "test-parse-move failed"))))

(defun test-manual-game ()
  (x-and-0-run (make-instance 'x-and-0-board)))