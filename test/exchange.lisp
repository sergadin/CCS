; (in-package :ccs)
(in-package :ccs-test)

(defun almost= (a b)  
   (< (abs (- a b)) 0.000001))

(deftestsuite exchange (root)
  ((the-board (make-instance 'board)))
  (:setup (progn
	    (load-from-fen-string ; e4 c5
	     the-board
	     "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2")))
  (:documentation
   "Unit tests for exchange function."))

(addtest (exchange)
  exchange-value
  (load-from-fen-string
   the-board
   "r3k2r/pp1ppp1p/b5nN/b1pP1p2/4P3/n4BQ1/PPPN1PPP/R3K2R w KQkq c6 0 1")
  (ensure-same (exchange-value the-board #@c2@ :black) -1.0
	       :test 'almost= :report "Value of take move")
  (ensure-same (exchange-value the-board #@d2@ :black) 0.0
	       :test 'almost= :report "Value of exchange move"))

(addtest (exchange)
  optimal-exchange-value
  (load-from-fen-string
   the-board
   "2k5/4q3/8/3p4/4Q3/5P2/3N4/2K5 w - - 0 1")
  (ensure-same (exchange-value the-board #@e4@ :black) -8.0
	       :test 'almost= :report "Value of take move with optimal exchange"))
