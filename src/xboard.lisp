;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; Функции для связи с xboard
;;;;
;;;; $Id: xboard.lisp 16 2013-07-04 20:53:22Z serg $
;;;;

(in-package :ccs)

(defvar *show-thinking* t "Выводить ли текущий вариант в xboard.")


(defun read-xboard-edit (command state)
  (cond
    ((string= command "#") (clear *board*) t)
    ((string= command "c") (setf state :black) t)
    ((string= command ".") nil)
    (t ; фигура и поле: Ne4 или Pa2
     (let* ((pkind (piece-by-name (elt command 0)))
	    (square (string-to-square (subseq command 1 3)))
	    (the-piece (make-instance 'piece :kind pkind :color state)))
       (setup-piece *board* square the-piece)))))


(defmacro string-prefix (s1 s2)
  `(and (<= (length ,s1) (length ,s2)) (string-equal ,s1 ,s2 :end2 (length ,s1))))

(defun read-xboard-commands (&optional (output-stream t))
  (let ((command "")
	(parser nil)
	(force-mode nil)
	(should-move nil)
	move)
    (format output-stream "feature myname=\"CCS 0.1\"~%")
    (loop
       ;(when (not (listen)) t)
       (setf command (read-line))
       (setf move (parse-move-string command)) ; nil, если это не ход
       (cond
	 ((not (null parser))
	  (let ((pout (funcall parser command)))
	    (when (null pout) (setf parser nil))))
	 ((not (null move))
	  (make-move *board* move)
	  (setf should-move t))

	 ((string-equal "xboard" command)
	  (format output-stream "~%"))
	 ((string-equal "force" command) ; не думать
	  (setf force-mode t))
	 ((string-equal "go" command) ; ходить
	  (setf force-mode nil)
	  (setf should-move t))
	 ((string-equal "new" command) ; новая партия
	  (new-game *board*))
	 ((string-prefix "protover" command) t)

	 ((string-prefix "ping" command)
	  (let* ((space-position (position #\Space command))
		 (arg (parse-integer command :start (1+ space-position))))
	    (format output-stream "pong ~D~%" arg)))

	 ((string-equal "post" command) (setf *show-thinking* t))
	 ((string-equal "nopost" command) (setf *show-thinking* nil))

	 ((string-equal "edit" command)
	  (setf parser (let (data) #'(lambda (command) (read-xboard-edit command data)))))
	 ((string-equal "quit" command)
	  (return-from read-xboard-commands))
	 (t nil)) ; end of cond

       (when (and should-move (not force-mode))
	 (let ((move (play *board*)))
	   (format output-stream "move ~A~A~%"
		   (square-to-string (move-from move))
		   (square-to-string (move-to move))))
	 (setf should-move nil)) ; ждем хода от соперника
       t)))


;;;
;;; Thinking Output
;;;

#|
If the user asks your engine to "show thinking", xboard sends your
engine the "post" command. It sends "nopost" to turn thinking off. In
post mode, your engine sends output lines to show the progress of its
thinking. The engine can send as many or few of these lines as it
wants to, whenever it wants to. Typically they would be sent when the
PV (principal variation) changes or the depth changes. The thinking
output should be in the following format:

ply score time nodes pv

Where:
  ply     Integer giving current search depth.
  score   Integer giving current evaluation in centipawns.
  time    Current search time in centiseconds (ex:1028 = 10.28 seconds).
  nodes   Nodes searched.
  pv      Freeform text giving current "best" line. You can continue 
          the pv onto another line if you start each continuation
          line with at least four space characters.

Example: 
  9 156 1084 48000 Nf3 Nc6 Nc3 Nf6

Meaning: 
  9 ply, score=1.56, time = 10.84 seconds, nodes=48000, PV = "Nf3 Nc6 Nc3 Nf6"

You can use the PV to show other things; 
|#

(defun show-output (pv &key (ply 0) (score 0) (time 0) (nodes 0))
  (when *show-thinking*
    (format *standard-output* "~D ~D ~D ~D ~A~%" ply score time nodes pv)))

