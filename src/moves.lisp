;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; Фнкции, связанные с ходами на пустой доске и проверкой корректности
;;;;
;;;; $Id: moves.lisp 18 2013-07-06 11:05:00Z serg $
;;;;


(in-package :ccs)


;;; Ходы на пустой доске

(defgeneric moves (piece start &key color)
  (:documentation "Возвращает все возможные ходы (список полей) фигуры PIECE цвета COLOR с поля START на пустой доске."))

(defgeneric pre-moves (piece to &key color attack-only)
  (:documentation "Find all squares where a PIECE should be placed in order to reach TO field in one move."))

(defmethod moves ((piece (eql :rook)) start &key (color :white))
  "Rachable squares by rook ладья"
  (declare (ignorable color))
  (loop :for p :from 1 :to 64
     :when (and
	    (not (= p start))
	    (or
	     (eql (mod p 8) (mod start 8))
	     (eql (ceiling p 8) (ceiling start 8))))
     :collecting p))

(defmethod moves ((piece (eql :bishop)) start &key (color :white))
  "Reachable squares by bishop"
  (declare (ignorable color))
  (loop :for p :from 1 :to 64
     :when (and
	    (not (= p start))
	    (eql (abs (- (mod (1- start) 8) (mod (1- p) 8)))
		 (abs (- (ceiling start 8) (ceiling p 8)))))
     :collecting p))


(defmethod moves ((piece (eql :queen)) start &key (color :white))
  (declare (ignorable color))
  (union (moves :rook start) (moves :bishop start)))


(defmethod moves ((piece (eql :king)) start &key (color :white))
  (loop :for p :from 1 :to 64
     :when (or
	    ;; ход на одно поле
	    (and
	     (<= (abs (- (mod (1- p) 8) (mod (1- start) 8))) 1)
	     (<= (abs (- (ceiling p 8) (ceiling start 8))) 1)
	     (not (= p start)))
	    ;; рокировка
	    (and (= start (if (eql color :white) #@e1@ #@e8@))
		 (= (abs (- p start)) 2)))
     :collecting p))


(defmethod moves ((piece (eql :knight)) start &key (color :white))
  (declare (ignore color))
  (loop :for p :from 1 :to 64
     :when (equal
	   (sort (list
		  (abs (- (mod (1- p) 8) (mod (1- start) 8)))
		  (abs (- (ceiling p 8) (ceiling start 8))))
		 #'<=)
	   '(1 2))
     :collecting p))


(defmethod moves ((piece (eql :pawn)) start &key (color :white))
  (let (res)
    (when (and (eql color :white) (< (ceiling start 8) 8))
      (push (+ start 8) res)
      (when (not (= (mod start 8) 1)) (push (+ start 7) res))
      (when (not (= (mod start 8) 0)) (push (+ start 9) res))
      (when (= (ceiling start 8) 2) (push (+ start 16) res)))
    (when (and (eql color :black) (> (ceiling start 8) 1))
      (push (- start 8) res)
      (when (not (= (mod start 8) 1)) (push (- start 9) res))
      (when (not (= (mod start 8) 0)) (push (- start 7) res))
      (when (= (ceiling start 8) 7) (push (- start 16) res)))
    (delete-if-not #'(lambda (sq) (and (<= 1 sq) (<= sq 64))) res)))


(defmethod moves ((piece piece) start &key (color :white))
  (declare (ignore color))
  (moves (kind piece) start :color (color piece)))


(defmethod pre-moves ((piece piece) to &key (color nil) (attack-only nil))
  (case (kind piece)
    (:pawn
       (loop :for sq :from 1 :to 64
          :when (and (member to (moves piece sq :color (or color (color piece))))
                     (or (null attack-only)
                         (/= (mod to 8) (mod sq 8))))
          :collect sq))
    (t (moves (kind piece) to :color (or color (color piece))))))

;;; Поиск возможных траекторий движения фигуры на пустой доске

(fare-memoization:define-memo-function find-paths (piece start end horizon &key (color :white))
  "Return: list of paths; each path is a list of squares."
  (let ((wave `((,start)))
	(result nil))
    ;; generate all paths
    (loop :while (< 0 horizon)
       :do (decf horizon)
	 (setq
	  wave
	  (apply #'nconc
		 (mapcar ; append possible steps to each path
		  #'(lambda (path)
		      (mapcar
		       #'(lambda (next) (append path (list next)))
		       (remove-if ; no loops allowed
			#'(lambda (next) (member next path))
			(moves piece (car (last path)) :color color))))
		  wave)))
	 (setq wave (delete-duplicates wave :test #'equal))
	 (setq result (append result wave)))
    ;; filter out all paths not leading to end
    (delete-if-not
     #'(lambda (p) (eql (car (last p)) end))
     result)))

(defun path-to-moves (path)
  "Split PATH into list of consecutive moves, e.g. CONSes of
squares. (1 2 3) => ((1 . 2) (2 . 3))"
  (loop :for tail :on path
     :when (cdr tail) :collect (cons (first tail) (second tail))))


;;; Проверка допустимости хода

(defun squares-on-line (start end &key (include-frontier t))
  "Список полей между START и END (вертикаль, горизонталь или диагональ), включая FROM и TO."
  (let ((from (min start end))
	(to (max start end))
	offset path)
    (setf offset
	  (cond
	    ((eql (mod from 8) (mod to 8)) 8) ; vertical
	    ((eql (floor (1- from) 8) (floor (1- to) 8)) 1) ; line
	    ((eql (abs (- (mod (1- from) 8) (mod (1- to) 8))) ; diagonal
		  (abs (- (ceiling from 8) (ceiling to 8))))
	     (+ 8 (signum (- (mod (1- to) 8) (mod (1- from) 8)))))
	    (t (return-from squares-on-line nil))))
    (do ((s from (+ s offset)))
	((> s to))
      (when (or include-frontier (and (/= s from) (/= s to))) (push s path)))
    (if (< start end)
        (nreverse path)
        path)))



(defun free-line-p (board from to)
  "Проверка того, что на линии между FROM и TO нет фигур. Занятость полей FROM и TO не учитывается. Возвращает nil, если FROM и ТО не находятся на одной прямой."
  (let ((from (min from to))
	(to (max from to))
	line)
    (setf line (squares-on-line from to))
    (if line
	(every #'(lambda (sq) (or (member sq (list from to))
				  (null (whos-at board sq))))
		 line)
	nil #| no line between from and to |# )))


(defun castling-move-p (piece color move)
  "Проверка того, что ход является рокировкой."
  (let ((offset (if (eql color :white) 0 56)))
    (and
     (eql (kind piece) :king)
     (eql (move-from move) (+ #@e1@ offset))
     (member (move-to move) (list (+ #@c1@ offset)
				  (+ #@g1@ offset))))))


(defun pawn-take-move-p (piece color move)
  "Проверка того, что ход является взятием пешкой."
  (let ((from (move-from move))
	(to (move-to move)))
    (and
     (eql (kind piece) :pawn)
     (member to (moves piece from :color color))
     (not (= (mod from 8) (mod to 8)))))) ; пешка меняет вертикаль


(defun attackers (board to &key (from nil) (color nil) (attack-only t) (free-line t))
  "Возвращает список полей на которых стоят фигуры цвета COLOR, атакующие
   в позиции BOARD поле TO. Если COLOR не задано, то выдаются фигуры обоих
   цветов. Если задано поле FROM, то проверяется только эта фигура.
   Если ATTACK-ONLY равно nil, то добавлются рокировки и ходы пешками по прямой.
   FREE-LINE nil отключает проверку того, что тракектория движения фигуры свободна.
   "
  (check-type board board)
  (let (res)
    (loop :for from-sq :from (or from 1) :to (or from 64) :do
       (multiple-value-bind (p c) (whos-at board from-sq)
         (when (and (not (null p)) ; на поле from есть фигура
                    (or (not color) (eql color c)) ; нужного цвета
                    (member to (moves p from-sq :color c)) ; фигура может попасть на to
                    (or (not free-line) ; проверка не нужна
                        (eql (kind p) :knight) ; не имеет смысла
			(free-line-p board from-sq to)) ; свободны поля на пути
                    (or (not attack-only)
                        (and ; дополнительные условия, если нужны только нападения
                         (or (not (eql (kind p) :pawn))
                             (not (= (mod from-sq 8) (mod to 8)))) ; пешка меняет вертикаль
                         (not (castling-move-p p c (create-move from-sq to)))))) ; не рокировка
           (push from-sq res))))
    res))


(defun check-p (board color)
  "Проверка того, что есть шах королю цвета COLOR."
  (let ((k-square (first (find-pieces board :king color))))
    (attackers board k-square :color (opposite-color color))))


(defun valid-move-p (board move &key (valid-turn nil))
  (declare (board board) (move move))
  "Проверка допустимости хода MOVE. Если VALID-TURN не равно NIL, то проверяется, что на поле from стоит фигура того цвета, чей в данный момент ход."
  (let ((from (move-from move))
	(to (move-to move)))
  (multiple-value-bind (piece color) (whos-at board from)
    (multiple-value-bind (piece-to color-to) (whos-at board to)
      (when (and (or (null valid-turn) (eql color (turn board))) ; наша фигура на поле from
		 (attackers board to :from from :color color :attack-only nil) ; может попасть на поле to
		 (not (eql color color-to)) ; на поле to нет нашей фигуры
		 (not (check-p (make-move (copy-board board) move) color))) ; после хода нам нет шаха
	;; обязательные условия выполнены;
	;; проверим несколько специальных случаев невозможности хода
	(cond
	  ((eql (kind piece) :pawn)
	   (if (pawn-take-move-p piece color move)
	       (when
		   (not (or ; проверим условия взятия
			 (eql color-to (opposite-color color)) ; есть кого есть
			 (eql to (enpassant board)))) ; возможно взятие на проходе
		 (return-from valid-move-p nil))
	       ;; это не взятие - поле to должно быть свободно
	       (when (not (null piece-to))
		 (return-from valid-move-p nil))))
	  ((eql (kind piece) :king)
	   ;; условия невозможности рокировки
	   (when (castling-move-p piece color move)
	     (when (or
		    (not (member to (castlings board))) ; кто-то уже ходил
		    (not (free-line-p board from (if (< to from) (- to 2) (+ to 1)))) ; занято где-то до b1, g1
		    ;; шах или битое поле
		    (some (complement #'null)
			  (mapcar #'(lambda (sq)
				      (attackers board sq :color (opposite-color color)))
				  (squares-on-line from to))))
	       (return-from valid-move-p nil)))))
	;; все условия выполнены
	(return-from valid-move-p t))
    nil))))
