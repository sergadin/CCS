;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; $Id: printing.lisp 16 2013-07-04 20:53:22Z serg $
;;;;

(in-package :ccs)

;;
;; Colors
;;
#|
<ESC>[{attr};{fg};{bg}m

{attr}, {fg}, {bg} have to be replaced with the correct value to get
the corresponding effect. attr is the attribute like blinking or
underlined etc.. fg and bg are foreground and background colors
respectively. You don't have to put braces around the number. Just
writing the number will suffice.

{attr} is one of following 
0    Reset All Attributes (return to normal mode)
1    Bright (Usually turns on BOLD)
2    Dim
3    Underline
5    Blink
7    Reverse
8    Hidden
 {fg} is one of the following 
30   Black
31   Red
32   Green
33   Yellow
34   Blue
35   Magenta
36   Cyan
37   White
 {bg} is one of the following 
40   Black
41   Red
42   Green
43   Yellow
44   Blue
45   Magenta
46   Cyan
47   White
|#

(defmacro xterm-font-color (&key (attr :rest) (bg-color :reset) (fg-color :reset))
  (flet ((color-to-num (color mode)
	   (let ((pos (position color '(:black :red :green :yellow :blue :magenda :cyan :white))))
	     (if pos
		 (+ 30 pos (if (eql mode :bg) 10 0))
		 0))))
    (format nil "~C[~D;~D;~Dm" #\Esc
	    (if (eql attr :rest) 0 0)
	    (color-to-num fg-color :fg)
	    (color-to-num bg-color :bg))))

(defun print-diagram (board &key (stream t))
  "Выводит текущее положение в виде диаграммы."
  ;; see http://en.wikipedia.org/wiki/Box-drawing_character
  ;; and http://en.wikipedia.org/wiki/Chess_symbols_in_Unicode
  (let ((white (list :pawn (coerce '(#\u2659) 'string)
		     :knight (coerce '(#\u2658) 'string)
		     :bishop (coerce '(#\u2657) 'string)
		     :rook (coerce '(#\u2656) 'string)
		     :queen (coerce '(#\u2655) 'string)
		     :king (coerce '(#\u2654) 'string)))
	(black (list :pawn (coerce '(#\u265f) 'string)
		     :knight (coerce '(#\u265e) 'string)
		     :bishop (coerce '(#\u265d) 'string)
		     :rook (coerce '(#\u265c) 'string)
		     :queen (coerce '(#\u265b) 'string)
		     :king (coerce '(#\u265a) 'string)))
	(vert-line (coerce '(#\u2502) 'string))
	(horiz-line (coerce '(#\u2500) 'string))
	(white-sq " ")
	(black-sq (concatenate 'string
			       (xterm-font-color :bg-color :white)
			       (coerce '(#\u2593) 'string)
			       (xterm-font-color))))
    (setf black-sq " ") ; esc-послед не работают
    (flet ((ij-to-square (i j) (+ (* 8 i) j))
	   (draw-horiz (mode) ; top, middle, or bottom
	     (let ((lc (coerce (ecase mode (:top '(#\u256d)) (:middle '(#\u251c)) (:bottom '(#\u2570))) 'string))
		   (rc (coerce (ecase mode (:top '(#\u256e)) (:middle '(#\u2524)) (:bottom '(#\u256f))) 'string))
		   (cr (coerce (ecase mode (:top '(#\u252c)) (:middle '(#\u253c)) (:bottom '(#\u2534))) 'string)))
	       (format stream "~A~A" lc horiz-line)
	       (dotimes (x 8) (format stream "~A~A~A~A" cr horiz-line horiz-line horiz-line))
	       (format stream "~A~%" rc))))
      (draw-horiz :top)
      ;; доска выводится строчками сверху-вниз; каждое поле занимает 4 смвола.
      (do ((line 7 (1- line)))
	  ((< line 0))
	(format stream "~A~D" vert-line (1+ line)) ; граница доски и номер строки
	(do ((col 1 (1+ col)))
	    ((> col 8))
	  (multiple-value-bind (p c) (whos-at board (ij-to-square line col))
	    (if p
	      (format stream "~A ~A "
		      vert-line
		      (getf (if (eql c :white) white black) (kind p)))
	      (format stream "~A ~A "
		      vert-line
		      (if (= (mod (+ line col) 2) 1) black-sq white-sq)))))
	(format stream "~A~%" vert-line)  ; граница доски
	(draw-horiz :middle)
	(when (= line 0)
	  (format stream "~A " vert-line)
	  (dotimes (x 8) (format stream "~A ~C " vert-line (elt "abcdefgh" x)))
	  (format stream "~A~%" vert-line)))
      ;; bottom line
      (draw-horiz :bottom)))
  t)

