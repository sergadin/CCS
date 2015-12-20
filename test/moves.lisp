; (in-package :ccs)
(in-package :ccs-test)


(deftestsuite moves-on-empty-board (root)
  ()
  (:documentation
   "Unit tests for the moves function."))


(addtest (moves-on-empty-board)
  pawn-moves ; test name
  (ensure-same (sort (moves :pawn (string-to-square :e2) :color :white) #'<)
	       (sort (mapcar #'string-to-square '(:e3 :e4 :d3 :f3)) #'<)
	       :test equal)
  (ensure-same (sort (moves :pawn (string-to-square :e2) :color :black) #'<)
	       (sort (mapcar #'string-to-square '(:e1 :d1 :f1)) #'<)
	       :test equal))

(addtest (moves-on-empty-board)
  knight-moves ; test name
  (ensure-same (sort (moves :knight (string-to-square :e4)) #'<)
	       (sort (mapcar #'string-to-square '(:f2 :d2 :g3 :c3 :g5 :f6 :c5 :d6)) #'<)
	       :test equal))

;;; ------------------

(deftestsuite moves-on-board (root)
  ((the-board (make-instance 'board)))
  (:setup (progn
	    (load-from-fen-string ; e4 c5
	     the-board
	     "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2")))
  (:documentation
   "Unit tests for attackers."))

(addtest (moves-on-board)
  attackers
  (ensure-same (sort (attackers the-board (string-to-square :c3)) #'<)
	       (sort (mapcar #'string-to-square '(:b1 :b2 :d2)) #'<)))



(deftestsuite checks-and-castlings (root)
  ((the-board (make-instance 'board)))
  (:setup (progn
	    (load-from-fen-string
	     the-board
	     "r3k2r/pp1ppp1p/b5nN/b1pP1p2/4P3/n4BQ1/PPPN1PPP/R3K2R w KQkq c6 0 1")))
  (:documentation
   "Unit tests for valid castlings, open check."))


(addtest (checks-and-castlings)
  castling-rook-move
  (ensure-same (let ((b2 (make-move the-board #@e1c1@ :clone-board t)))
		 (list (kind (whos-at b2 #@c1@))
		       (kind (whos-at b2 #@d1@))))
	       '(:king :rook)
	       :test equal
	       :report "Rook is not on d1 after 0-0-0")
  (ensure-same (let ((b2 (make-move the-board #@e8g8@ :clone-board t)))
		 (list (kind (whos-at b2 #@g8@))
		       (kind (whos-at b2 #@f8@))))
	       '(:king :rook)
	       :test equal
	       :report "Rook is not on f8 after black's 0-0"))



(addtest (checks-and-castlings)
  open-check-move
  (ensure-same (valid-move-p the-board #@d2c4@) nil :report "Open check"))

(addtest (checks-and-castlings)
  escape-from-check
  (load-from-fen-string
   the-board
   "rnb1k1nr/1ppp1Qpp/p3p3/4P1N1/8/8/PPPb1KPP/RNB2B1R b kq - 0 7")
  (ensure-same (valid-move-p the-board #@e8d8@) t :report "Correct escape from check")
  (ensure-same (valid-move-p the-board #@e8e7@) nil :report "Escape from check under check"))


(addtest (checks-and-castlings)
  castling-over-attacked-field
  (ensure-same (valid-move-p the-board #@e1c1@) t :report "White's 0-0-0")
  (ensure-same (valid-move-p the-board #@e8c8@) t :report "Black's 0-0-0")
  (ensure-same (valid-move-p the-board #@e1g1@) nil :report "White's 0-0 (illegal)")
  (ensure-same (valid-move-p the-board #@g3g7@) nil :report "Move over occupied line (illegal)"))


(addtest (checks-and-castlings)
  pawn-moves
  (ensure-same (valid-move-p the-board #@e4f5@) t :report "Pawn take e4-f5")
  (ensure-same (valid-move-p the-board #@f5e4@) t :report "Black's pawn take f5-e4")
  (ensure-same (valid-move-p the-board #@a2a4@) nil :report "Pawn's move over an occupied field (illegal)")
  (ensure-same (valid-move-p the-board #@f7f5@) nil :report "Pawn's move to occupied field (illegal)")
  (ensure-same (valid-move-p the-board #@c5d4@) nil :report "Black's pawn take c5-d4 (illegal)")
  (ensure-same (valid-move-p the-board #@d5c6@) t :report "En passant")
  (ensure-same (valid-move-p the-board #@h7h6@) nil :report "Black's pawn illegal move")
  (ensure-same (valid-move-p the-board #@a2a3@) nil :report "White's pawn illegal move")
  (ensure-same (valid-move-p the-board #@d7d5@) nil :report "Pawn illegal take"))

