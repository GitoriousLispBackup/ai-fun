(in-package :ai-fun.board-games)

;;; Requires usocket library for socket communication (cl-usocket.deb package in Debian/Ubuntu).

;; globals & constants used for tcp communication
(defvar *socket* nil "Socket used to accept connections from players")
(defconstant +tcp-port+ 27182 "TCP bind port")

;;; board class
(defclass board ()
  ((size
    :initarg :size
    :initform (error "Must supply board size")
    :reader size)

   ;; :fixme: - probably not needed
   (first-is-black
    :initarg :first-is-black
    :initform t
    :reader first-is-black)

   ;; Some games use the intersection (go), others use the spaces in the grid
   ;; (x-and-0)
   ;; This property is used when the board is displayed
   (point-at-intersection
    :initarg :point-at-intersection
    :initform t
    :reader point-at-intersection)

   ;; Will be initialized by the initialize-instance (see below)
   (board-array
    :initform nil
    :accessor board-array)
  
  ;; x coord. of the last move
  (last-x
   :initform nil
   :accessor last-x)

  ;; y coord. of the last move
  (last-y
   :initform nil
   :accessor last-y)))


(defmethod initialize-instance :after ((board board) &key)
  (setf (slot-value board 'board-array)
        (make-array (list (size board) (size board)) :initial-element nil)))

;;; end board class


;;; board class functions

(defgeneric board-print (board)
  (:documentation "Print a game board at the standard output using ASCII
  chars"))

;;; ret. board element at (x,y)
(defgeneric board-elt (board x y)
  (:documentation "Return the element at coords. (x,y)"))


(defgeneric board-move (board x y mark)
  (:documentation "Make a move on board. Return the coordinates as a list (list
x y)"))


(defmethod board-elt ((brd board) x y)
  (when (or (< x 0) (>= x (size brd) ) (< y 0) (>= y (size brd)))
    (error "coordinates out of board"))
  (aref (slot-value brd 'board-array) x y))


(defmethod board-move ((board board) x y mark)
  ;; (-1, -1) is a valid move (means user abort)
  (when (or (< x -1) (>= x (size board)) (< y -1) (>= y (size board)))
    (return-from board-move nil))
  (when (and (>= x 0) (>= y 0))
    (when (aref (slot-value board 'board-array) x y)
      (return-from board-move nil)) ; alredy an element at (x,y)
    (setf (aref (board-array board) x y) mark))
  (setf (last-x board) x) 
  (setf (last-y board) y)
  (list x y))


(defmethod board-print ((brd board))
  (flet ((print-sep1-line (width)
           (format t "-")
           (dotimes (j width)
             (format t "-"))
           (format t "-~%"))

         (print-sep2-line (width)
           (format t ".")
           (dotimes (j width)
             (format t "-."))
           (format t "~%")))
    (if (point-at-intersection brd)
        ;; if point is at intersection (eg. go)
        (progn
          ;; first line
          (print-sep1-line (size brd))
          ;; main content
          (dotimes (i (size brd))
            (format t "|")
            (dotimes (j (size brd))
              (if (aref (slot-value brd 'board-array) i j)
                  (format t "~a" (aref (slot-value brd 'board-array) i j))
                  (format t ".")))
            (format t "|~%"))
          ;; last line
          (print-sep1-line (size brd)))
        ;; else - point in grid space (e.g. x-and-0)
        (progn
          ;; first line
          (print-sep2-line (size brd))
          ;; main content
          (dotimes (i (size brd))
            (format t "|")
            (dotimes (j (size brd))
              (if (aref (slot-value brd 'board-array) i j)
                  (format t "~a" (aref (slot-value brd 'board-array) i j))
                  (format t " "))
              (format t "|"))
            (format t "~%")
            (print-sep2-line (size brd)))))))

;;; end board class functions

;;; board player class
(defclass player ()

  ;; possible values: :stdio, :tcp, :ai
  ((interface
    :initarg :interface
    :initform :stdio
    :reader interface)

  ;; human readable name of the player
  (name
   :initarg :name
   :initform "noname"
   :reader name)

   (has-first-move
    :initarg :first-move
    :initform nil
    :reader first-move)

   (board-size
    :initform nil
    :reader board-size)

   ;; used if player interface is over tcp/ip
   (socket
    :initform nil
    :reader socket)))

;;; end player class

;;; helper functions (sockets, input/output)
(defun init-server-socket (tcp-port)
  (setf *socket*
        (usocket:socket-listen "localhost" tcp-port :reuse-address t))
  (format t ":debug: socket opened~%"))


(defun player-accept-socket (player socket)
  (format t  ":debug: waiting for a player (~a)~%" player)
  (setf (slot-value player 'socket) (usocket:socket-accept socket))
  (format t ":debug: socket accepted for ~a~%" player))


;;; stream might be file, socket, etc.
(defun player-read-data (stream)
  (format stream "enter move: ")
  (force-output stream)
  (read-line stream))

;;; player class functions

;; overriding prin-object for player class
(defmethod print-object ((player player) stream)
  (format stream "<player ~a>" (slot-value player 'name)))

(defgeneric player-game-params (player brd-size is-first)
  (:documentation "The player gets the game parameters (board size, color,
  etc.)"))

(defgeneric player-prompt-move (player)
  (:documentation "Asks the user for a move (board coordinates). Result is a
  string (for humans) or a list with 2 coords (for AIs)."))

(defgeneric player-announce-move (player move)
  (:documentation "Announce the move to the other player"))

(defgeneric player-cleanup (player)
  (:documentation "Cleanup stuff for player (sockets, etc.)"))


;;; helper function
(defun announce-game-params (stream brd-size is-first)
  (format stream "board size is ~a~%" brd-size)
  (if is-first
      (format stream "you are first~%")
      (format stream "you are second~%")))


;;; functions implementation for generic player

(defmethod player-game-params ((player player) brd-size is-first)
  (setf (slot-value player 'board-size) brd-size) ; why warning on
                                        ;(setf (board-size player) brd-size) ?
  (setf (slot-value player 'has-first-move) is-first)

  ;; announce game params
  (cond
    ((eql (interface player) :stdio)
     (announce-game-params *query-io* brd-size is-first))

    ((eql (interface player) :tcp)
     ;; init tcp communication
     (when (null (slot-value player 'socket))
       (when (null *socket*)
         (init-server-socket +tcp-port+))
       (player-accept-socket player *socket*))
     (announce-game-params (usocket:socket-stream (socket player))
                           brd-size is-first))

    (t ) ; probably an AI player, does not need printed messages, it should
                                        ; implement this method
    ))


(defmethod player-prompt-move ((player player))
  (cond
    ((eql (interface player) :stdio)
     (format *query-io* "enter move: ")
     (force-output *query-io*)
     (read-line *query-io*))

    ((eql (interface player) :tcp)
     (player-read-data (usocket:socket-stream (slot-value player 'socket))))

    (t
     (error "Not implemented yet"))))


(defmethod player-announce-move ((player player) move)
  (cond
    ((eql (interface player) :stdio)
     (format *query-io* "peer's move:~a,~a~%" (first move) (second move))
     (force-output *query-io*))

    ((eql (interface player) :tcp)
     (format (usocket:socket-stream (slot-value player 'socket))
             "peer move:~a,~a~%" (first move) (second move)))

    (t
     (error "Not implemented yet"))))
  

(defmethod player-cleanup ((player player))
  (when (slot-value player 'socket)
    (usocket:socket-close (slot-value player 'socket))
    (setf (slot-value player 'socket) nil)))

;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
