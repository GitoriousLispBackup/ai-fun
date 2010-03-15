(in-package :ai-fun.test-unit)

(defun test-parse-move ()
  (let ((board (make-instance 'x-and-0-board)))
    (if (and (eql (parse-move board "68" #\X) nil)
                  (eql (parse-move board "92" #\X) nil)
                  (equal (parse-move board "22" #\X) (list 2 2)))
        t ; test passed - ret. true
        (progn (format t "test-parse-move failed")
               nil))))

(defun test-end-game ()
  ;; game finished
  (let ((board (make-instance 'x-and-0-board)))
    (setf (aref (board-array board) 0 0) #\X
          (aref (board-array board) 0 1) #\X
          (aref (board-array board) 0 2) #\X
          (aref (board-array board) 1 1) #\0
          (aref (board-array board) 2 2) #\0)
    (when (not (x-and-0-end-p board))
      (format t "x-and-0-end-p failed (for finished game)")
      (return-from test-end-game nil)))
  
  ;; game not finished
  (let ((board (make-instance 'x-and-0-board)))
    (setf (aref (board-array board) 0 0) #\X
          (aref (board-array board) 0 1) #\X
          (aref (board-array board) 2 0) #\X
          (aref (board-array board) 1 1) #\0
          (aref (board-array board) 2 2) #\0)
    (when (x-and-0-end-p board)
      (format t "x-and-0-end-p failed (for non-finished game)")
      (return-from test-end-game nil)))
  t)

(defun test-automatic-all ()
  (and (test-parse-move)
       (test-end-game)))

(defun test-manual-game ()
  (x-and-0-run (make-instance 'x-and-0-board)
               (make-instance 'player :name "lolek" :interface :tcp)
               (make-instance 'player :name "bolek")))

(defun test-game-human-vs-random-ai ()
  (x-and-0-run (make-instance 'x-and-0-board)
               (make-instance 'x0-player-random)
               (make-instance 'player :name "bolek")))

;;; function used for resetting various runtime variables during testing
;;; (sockets, globals, etc.)
(defun test-env-reset ()
  (when *socket*
    (usocket:socket-close *socket*)
    (setf *socket* nil)))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
