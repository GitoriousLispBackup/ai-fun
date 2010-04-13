;;; visual stuff for GA simulations
(in-package :ai-fun.visual)

(defun save-universe (entities &key (stream t) (time nil) (limits nil)
					  (type :2d-graph))
  "Save a generic universe to a stream (as a list of s-expressions)"
  (write time :stream stream :pretty nil)
  (terpri)
  (write type :stream stream :pretty nil)
  (terpri)
  (write limits :stream stream :pretty nil)
  (terpri)
  (write entities :stream stream :pretty nil)
  (terpri))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
