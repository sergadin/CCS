;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; $Id: evaluate.lisp 17 2013-07-06 11:03:20Z serg $
;;;;
;;;; Оценка позиции
;;;;

(in-package :ccs)

(defun possible-moves (board from)
  (multiple-value-bind (p color)
      (whos-at board from)
    (mapcan #'(lambda (to &aux (m (create-move from to)))
                (if (valid-move-p board m :valid-turn nil) (list m) nil))
            (moves p from :color color))))

(defun moves-from-path (path)
  "PATH is a list of squares."
  (butlast
   (maplist #'(lambda (path)
                (when (cdr path)
                  (create-move (car path) (cadr path))))
            path)))


(defun better-value-p (x reference color)
  (if (eql color :white) (>= x reference) (<= x reference)))

(defun valid-trajectories (board from to horizon)
  "Find all valid trajectories of a piece on square FROM to TO on BOARD in HORIZON moves."
  (multiple-value-bind (p color)
      (whos-at board from)
    (delete-if-not
     #'(lambda (path) ; check that PATH is a sequence of valid moves
         (reduce #'(lambda (b m)
                     (if (and b
                              (valid-move-p b m)
                              (better-value-p (exchange-value (make-move b m :clone-board t) (move-to m) (opposite-color color)) 0 color))
                         (make-move b m)
                         nil))
                 (moves-from-path path)
                 :initial-value (copy-board board)))
     (find-paths (kind p) from to horizon))))



(defun square-value (board square)
  "Оценка важности поля. Возвращает действительное число из [0,1]. Текущая реализация не учитывает положение фигур на доске и придает больший вес центральным полям."
  (declare (ignorable board))
  (check-type square square)
  (case square
    ((#@e4@ #@e5@ #@d4@ #@d5@) 1.0d0)
    ((#@c4@ #@f4@ #@c5@ #@f5@) 0.8d0)
    (t 0.7d0)))


(defun piece-weakness (board piece-at)
  (multiple-value-bind (p color) (whos-at board piece-at)
    (let ((value (value-of (kind p)))
          (number-of-attackers 0))
      (do-pieces (board (attacker from) :color (opposite-color color))
        (when (< (value-of (kind attacker)) value)
          ;; print trjectories
          #+debug(mapc #'(lambda (path)
                    (print (mapcar #'square-to-string path)))
                (valid-trajectories board from piece-at 2))
          (incf number-of-attackers
                (log (1+ (length (valid-trajectories board from piece-at 2))) 2))))
      number-of-attackers)))


(defun piece-activity (board piece-at)
  "Оценка активности фигуры на поле PIECE-AT. Возвращает действительное число."
  (multiple-value-bind (p color) (whos-at board piece-at)
    (let ((p-moves (possible-moves board
                                   piece-at))
          (controled-fields-value ; total value of fields controlled by piece P
           (reduce #'+
                   (mapcar (lambda (s) (square-value board s))
                           (remove-if-not (lambda (s) (attackers board s :from piece-at))
                                          (moves p piece-at :color color)))))
          (max-moves (length (moves p #@e4@))))
      #+debug
      (print (list p-moves (length p-moves) max-moves controled-fields-value
                   (mapcar #'square-to-string (moves p piece-at :color color))
                   (remove-if-not (lambda (s) (attackers board s :from piece-at))
                                  (moves p piece-at :color color))))
      (+ (/ (length p-moves) max-moves (value-of (kind p)))
         (/ controled-fields-value max-moves)))))



(flet ((total (board color &aux (the-total 0.0d0))
	 (do-pieces (board p :color color)
	   (incf the-total (value-of p)))
	 the-total)

       (exch (board &optional (color (turn board)) &aux (val 0.0d0))
	 (do-pieces (board (p sq) :color (opposite-color color))
	   (let ((ev (exchange-value board sq color))
		 (cmp (if (eql color :white) #'< #'>)))
	     (when (funcall cmp 0 ev)
	       (incf val ev))))
	 val)

       ;; Активность фигур.
       (act (board color &aux (val 0.0d0) (npieces 0))
         (do-pieces (board (p sq) :color color)
           (incf val (piece-activity board sq))
           (incf npieces))
	 (/ val npieces))

       (weakness (board color)
         (let ((w 0.0d0) (npieces 0))
           (do-pieces (board (p sq) :color color)
             (incf npieces)
             (incf w (piece-weakness board sq)))
           (/ w npieces))))

  (defmethod value-of ((b board) &key)
    "Оценка позиции. Позиция всегда оценивается с точки зрения белых."
    (let ((material (- (total b :white) (total b :black))) ; простая стоимость фигур
	  (activity (- (act b :white) (act b :black))) ; активность фигур
	  (exch (exch b))
          (weakness (- (weakness b :white) (weakness b :black))))
      (log-message :trace
                   "Evaluate: material: ~F, act: ~F, exch: ~F: weakness: ~F, TOTAL: ~F"
                   material activity exch weakness
                   (- (+ material activity exch) weakness))
      (- (+ material activity exch) weakness)))

) ; end of flet
