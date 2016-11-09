(in-package :ccs-test)

(defun almost= (a b)
   (< (abs (- a b)) 0.000001))

(deftestsuite chain-construction (root)
  ((the-board (make-instance 'ccs::board)))
  (:setup (progn
	    (ccs::load-from-fen-string ; e4 c5
	     the-board
             "8/7p/8/3N4/8/8/8/8 w - - 0 1")))
  (:documentation
   "Unit tests for constructing chains for piece and target."))

(addtest (chain-construction)
  one-chain-representation
  (let ((pos (ccs::make-position the-board)))
    (ensure-same (length (ccs::position-chains pos))  1
	       :test #'= :report "one-chain-failed")))
