;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; $Id: candidates.lisp 18 2013-07-06 11:05:00Z serg $
;;;;
;;;; Выбор ходов-кандидатов
;;;;

(in-package :ccs)

(labels
    ((find-valid-moves (board color &aux res)
       "Возвращает список допустимых ходов фигурами цвета color."
       (do-pieces (board (p from) :color color)
         (push
          (mapcan #'(lambda (to &aux m)
                      (setf m (create-move from to))
                      (if (valid-move-p board m :valid-turn nil) (list m) nil))
                  (moves p from))
          res))
       (apply #'append res)))

  (defun candidate-moves (board &key (color (turn board)))
    "Поиск лучших ходов. Возвращается список cons-ов ход.оценка. Оценка всегда со стороны белых."
    (let ((possible-moves (find-valid-moves board color))
	  moves-values
	  (better-move-p (if (eql color :white) #'>= #'<=)))  ; How to compare moves' goodness
      ;; оценим ходы
      (setf moves-values
	    (sort
	     (mapcar #'(lambda (m)
                         (log-message :trace "Evaluating move ~A-~A"
                                      (square-to-string (move-from m))
                                      (square-to-string (move-to m)))
			 (cons m (value-of (make-move board m :clone-board t))))
		     possible-moves)
             better-move-p
             :key #'cdr))
      moves-values))
) ; end of labels
