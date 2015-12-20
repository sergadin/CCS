(in-package :ccs)

(defparameter *board* (make-instance 'board))

(new-game *board*)

(print-board *board*)