;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;;
;;;; $Id: play.lisp 16 2013-07-04 20:53:22Z serg $
;;;;

(in-package :ccs)

(defvar *board* (make-instance 'board) "Текущая позиция.")


(defun auto-play (board &key (half-moves-limit 10))
  (let ()
    (do ((i 1 (1+ i))
         (move (play board) (play board)))
        ((or (not move) (> i half-moves-limit)))
      (progn
        (print (mapcar #'square-to-string (list (move-from move) (move-to move))))
        (make-move board move)))))



(defun play (board)
  "Находит и делает лучший ход в позиции BOARD. Возвращает сделанный ход."
  (show-output "Let me think...")
  (let ((moves-values (candidate-moves board))
        best-move
        better-move-p
        (color (turn board)))
    (when (null moves-values) (return-from play nil))
    (setf better-move-p (if (eql color :white) #'>= #'<=)) ; How to compare moves
    (flet ((best-move-p (mv) (funcall better-move-p (cdr mv) (cdar moves-values))))
      (setf best-move
            (random-elt (delete-if-not #'best-move-p moves-values))))
    (setf best-move (car best-move))
    (make-move board best-move)
    (cl-log:log-message :trace "Best move: ~A-~A"
                        (square-to-string (move-from best-move))
                        (square-to-string (move-to best-move)))

    best-move))
