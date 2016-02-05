;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;

; (in-package :ccs)
(in-package :ccs-test)


(deftestsuite simple-tactics (root)
  ((the-board (make-instance 'ccs::board)))
  (:documentation
   "Unit tests for attackers."))

(addtest (simple-tactics)
  double-attack
  (ccs::load-from-fen-string the-board "r2qk2r/1b1nbppp/p3p3/1p1nP1B1/3pN3/3B1N2/PP3PPP/R2Q1RK1 w kq - 0 1")
  (ensure-same (ccs::play the-board) #@e4d6@ :report "Простая вилка: шах + нападение на фигуру.")
  ;;
  (ccs::load-from-fen-string the-board "1n3kn1/1pb2p1Q/8/1P2p3/r7/7P/5PP1/5RK1 w - - 0 1")
  (ensure-same (ccs::play the-board) #@h7c2@ :report "Нападение на две фигуры: 1. Qc2 Rd4 2. Qxc7.")
  ;;
  (ccs::load-from-fen-string the-board "3R4/pp3pkp/2p1rpq1/5b2/8/2Q3N1/PPP3PP/6K1 w - - 0 1")
  (ensure-same (ccs::play the-board) #@c3c5@ :report "Нападение на мат и фигуру: 1. Qc5 1-0.")
  ;; not so simple
  (ccs::load-from-fen-string the-board "2k4r/3rRp2/p1pB4/3b2pp/2q5/Q7/P4PPP/3R2K1 w - - 0 1")
  (ensure-same (ccs::play the-board) #@a3b2@ :report "Вилка пятым ходом: 1. Qb2! Rxe7 2. Qb8 Kd7 3. Qc7 Ke6 4. Qxe7 Kf5 5. Qe5 Kg6 6. Qxh8 1-0")
  ;;
  (ccs::load-from-fen-string the-board "8/Q5b1/6k1/4r2p/8/6PP/5P1K/1r6 w - - 0 1")
  (ensure-same (ccs::play the-board) #@a7a6@ :report "Некуда отступить после шаха: 1. Qa6 Bf6 (1... Kf7 2. Qa2+)  (1... Kh7 2. Qd3+) (1... Kg5 2. f4+) 2. Qd3+ 1-0.")
  ;;
  (ccs::load-from-fen-string the-board "5qk1/1br4p/pN2R1p1/1ppP1p2/1P1Qn3/8/P5PP/5B1K w - - 0 1")
  (ensure-same (ccs::play the-board) #@d4e5@ :report "Нападение на ладью и угрозу связки ферзя: 1. Qe5 1-0.")
  ;;
  (ccs::load-from-fen-string the-board "r1q3rk/1ppbb1p1/4Np1p/p3pP2/P3P3/2N4R/1PP1Q1PP/3R2K1 w - - 0 1")
  (ensure-same (ccs::play the-board) #@e2d2@ :report "Нападение на фигуру и угрозу мата в 2 хода: 1. Qd2 Bf8 (1... Bxe6 2. Rxh6+ gxh6 3. Qxh6#) 2. Qxd7 1-0.")
  ;;
  (ccs::load-from-fen-string the-board "k7/rpp2b2/p7/6K1/8/5Q2/8/8 w - - 0 1")
  (ensure-same (ccs::play the-board) #@f3h3@ :report "Нападение на мат с двух полей: 1. Qh3 1-0.")
  ;;
  (ccs::load-from-fen-string the-board "2k4r/ppp2ppp/2p1bn2/3NN3/3qP3/8/PP3PPP/R4RK1 w - - 0 1")
  (ensure-same (ccs::play the-board) #@d5e7@ :report "Заставить встать короля под вилку: 1. Ne7 Kb8 2. N7xc6+ bxc6 3. Nxc6+ 1-0"))


(addtest (simple-tactics)
  poisoned-piece
  (ccs::load-from-fen-string the-board "K1k4R/B1Nr4/1p6/1P1pp3/8/8/6p1/7b b - - 0 1")
  (ensure-same (ccs::play the-board) #@d7d8@) :report "Взятие фигуры приводит к мату в один ход.")


(addtest (simple-tactics)
  mate-in-one
  ;;
  (ccs::load-from-fen-string the-board "3r2k1/ppp2ppp/6Q1/b7/3n1B2/2p3n1/P4PPP/RN3RK1 b - - 0 1")
  (ensure-same (ccs::play the-board) #@d4e2@ :report "Простой мат в один ход.")
  ;;
  (ccs::load-from-fen-string the-board "4rkr1/p3p1n1/1pp3Qp/6p1/8/BP3pPq/P4P1P/5RK1 w - - 0 1")
  (ensure-same (ccs::play the-board) #@g6f6@ :report "Связка.")
  ;;
  (ccs::load-from-fen-string the-board "r1b1q1kr/ppNnb1pp/5n2/8/3P4/8/PPP2PPP/R1BQKB1R b KQ - 0 1")
  (ensure-same (ccs::play the-board) #@d5h5@ :report "Связка.")
  ;;
  (ccs::load-from-fen-string the-board "2r4k/7q/2b2P1R/p7/3B3p/1P4N1/1PP3rP/5K2 w - - 0 1")
  (ensure-same (ccs::play the-board) #@f6f7@ :report "Вскрытый шах.")
  ;;
  (ccs::load-from-fen-string the-board "r3rn2/pp2pkn1/1qpp1p2/5N1b/3P4/3B1N2/PPP3PP/R1B1R2K w - - 0 1")
  (ensure-same (ccs::play the-board) #@f5h6@ :report "Вскрытая защита поля.")
  ;;
  (ccs::load-from-fen-string the-board "r1b1q1kr/ppNnb1pp/5n2/8/3P4/8/PPP2PPP/R1BQKB1R b KQ - 0 1")
  (ensure-same (ccs::play the-board) #@e7b4@ :report "Двойной шах.")
  ;;
  (ccs::load-from-fen-string the-board "1n2R3/r1p2kpp/p2b4/6q1/8/5BB1/PPP3PP/5R1K w - - 0 1")
  (ensure-same (ccs::play the-board) #@f3h5@ :report "Двойной шах + защита фигуры \"через короля\".")
  ;;
  (ccs::load-from-fen-string the-board "rnbq1bnr/pppp1kP1/7p/4Q3/4pP2/8/PPPP2PP/RNB1KBNR w KQ - 0 1")
  (ensure-same (ccs::play the-board) #@g7h8N@ :report "Превращение пешки в коня!"))


(addtest (simple-tactics)
  mate-in-three
  ;;
  (ccs::load-from-fen-string the-board "3r1n1k/1B4Nb/1p5p/p5p1/P7/2B2PP1/1P5P/2K5 w - - 0 1")
  (ensure-same (ccs::play the-board) #@g7f5@ :report "Простой мат в три хода."))


;; r4k2/ppp2r1p/8/3p1nQ1/2q5/3N4/PPP4P/4RKR1 b - - 0 31     f5e3
;; 2r2r2/1b1pQ3/p6k/1p3p2/6p1/P5R1/1qB2NP1/2R3K1 w - - 0 35 f2g4
;; r1b1qrk1/1p3ppp/p7/3Nb3/5N2/P7/1P4PQ/K1R1R3 w - - 0 1    e1e5
;; 8/5R2/1p4p1/4N1k1/2n5/P5P1/5PK1/2r5 w - - 0 46           f2f4
;; 8/6p1/2r1p1k1/3p2p1/3Pn1P1/1p1KP3/1Pr1R3/1R1N4 b - - 0 39 e6e5
