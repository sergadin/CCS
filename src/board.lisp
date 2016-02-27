;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;

(in-package :ccs)

(defclass piece ()
  ((color :type color :accessor color :initarg :color)
   (kind :type piece-kind :accessor kind :initarg :kind :documentation "Тип фигуры."))
  (:documentation "Базовый класс для всех фигур."))


(defclass board ()
  ((pieces
    :initform (make-array 65)
    :accessor pieces
    :documentation "Массив объектов типа piece. Если поле свободно, то элемент равен nil. Нулевой элемент массива не используется.")
   (turn
    :type color :initform :white
    :accessor turn
    :documentation "Who's moving next")
   (castlings
    :accessor castlings
    :documentation "Which castlings are allowed. List of squares c1,g1,c8,g8")
   (enpassant
    :accessor enpassant
    :documentation "Square where enpassant is possible"))
  (:documentation "Описывает положение фигур на доске."))


(defun copy-board (board)
  "Создает копию объекта board."
  (let ((copy (shallow-copy-object board)))
    (setf (pieces copy) (copy-seq (pieces board)))
    copy))

(defun create-board () (make-instance 'board))


(defgeneric whos-at (board sq)
  (:documentation "Возвращает для поля SQ фигуру и её цвет (два значения) или nil.")
  (:method ((board board) sq)
    (let ((w (aref (pieces board) sq)))
      (if w (values w (color w)) nil ))))


(defun empty-square-p (board square)
  (null (whos-at board square)))

(defgeneric load-from-fen-string (board string)
  (:documentation "Инициализирует позицию из строки в нотации FEN."))


(defun piece-to-name (piece)
  (car (rassoc piece
	      '((#\K . :king) (#\Q . :queen) (#\R . :rook)
		(#\N . :knight) (#\B . :bishop) (#\P . :pawn)))))


(defgeneric print-board (board &key stream)
  (:documentation "Выводит текущее положение в стандартной нотации.")
  (:method ((board board) &key (stream t))
    (format stream "~A"
            (with-output-to-string (s)
              (mapcar
               #'(lambda (color)
                   (loop
                      :for sq :from 1 :to 64 :do
                      (when (= sq 1) (format s "~A " (string color)))
                      (let ((pc (aref (slot-value board 'pieces) sq)))
                        (when (and pc (eql (color pc) color))
                          (format s "~C~A "
                                  (piece-to-name (kind pc))
                                  (square-to-string sq))))))
               '(:white :black))))))


(defgeneric print-board-fen (board &key stream)
  (:documentation "Выводит текущее положение в формате FEN.")
  (:method ((board board) &key (stream t) )
    (print-board board :stream stream)))


(defgeneric find-pieces (board piece color)
  (:documentation "Находит поля на которых стоят фигуры типа PIECE цвета COLOR. Если PIECE равно nil, то находит все фигуры указазнного цвета. Возвращает список полей, на которых сотоят фигуры.")
  (:method ((board board) piece-kind color)
    (loop :for sq :from 1 :to 64
       :when (and (not (null (aref (pieces board) sq)))
		  (or (not piece-kind)
		      (eql piece-kind (kind (aref (pieces board) sq))))
		  (eql color (color (aref (pieces board) sq))))
       :collecting sq)))


(defgeneric value-of (object &key)
  (:documentation "Вычисляет оценку заданного объекта."))


(defmethod value-of (p &key)
  "Метод, который обрабатывает типы, а не классы."
  (cond
    ((typep p 'piece-kind)
     (let ((pv '(:pawn 1 :knight 3 :bishop 3 :rook 5 :queen 9 :king 100)))
       (getf pv p 0)))
    (t (error "Unsupported type in value-of"))))

(defmethod value-of ((the-piece piece) &key)
  (let ((val (value-of (kind the-piece))))
    ;(* val (if (eql (color the-piece) :white) +1 -1))))
    val))

(defun piece-value (piece &key (color :white))
  "Оценка стоимости фигуры с точки зрения COLOR"
  (if piece
      (* (value-of piece)
         (if (eq (color piece) color) +1 -1))
      0))

(defun piece-of-color (piece color)
  (and piece
       (eq (color piece) color)))

;;
;; Функции для расстановки фигур
;;

(defgeneric clear (board)
  (:documentation "Убирает с доски все фигуры.")
  (:method ((board board))
    (progn
      (setf (slot-value board 'enpassant) nil)
      (setf (slot-value board 'turn) :white)
      (setf (slot-value board 'castlings) '(#@c1@ #@g1@ #@c8@ #@g8@))
      (fill (slot-value board 'pieces) nil)
      t)))


(defgeneric setup-piece (board sq piece &key)
  (:documentation "Ставит на поле SQ фигуру PIECE.")
  (:method ((board board) sq piece &key (replace t))
    "Если поле SQ занаято, то фигура на этом поле заменяется новой."
    (declare (ignorable replace))
    (setf (aref (slot-value board 'pieces) sq) piece)))


(defun piece-by-name (char)
  (cdr (assoc (char-upcase char)
	      '((#\K . :king) (#\Q . :queen) (#\R . :rook)
		(#\N . :knight) (#\B . :bishop) (#\P . :pawn)))))


(defmethod load-from-fen-string ((board board) fen-str)
  ;; Нотация FEN - строка, состоящая из:
  ;; - описание положения фигур (от 8-ой горизонтали к 1-ой, от a к h), заглавные
  ;;   буквы обозначают белые фигуры, строчные - черные. Разделитель строк - '/'.
  ;;   Цифры в строке обозначают количество пустых клеток;
  ;; - очередности хода (b или w);
  ;; - допустимых рокировках (KQkq);
  ;; - поле взятия на проходе в стандартной нотации, или '-';
  ;; - число полуходов с момента последнего хода пешки или взятия фигуры;
  ;; - число ходов, которые сделали черные (минимальное значение - 1).
  ;; Пример строки: rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2
  (flet ((piece-by-name (char)
	   (cdr (assoc (char-upcase char)
		       '((#\K . :king) (#\Q . :queen) (#\R . :rook)
			 (#\N . :knight) (#\B . :bishop) (#\P . :pawn)))))
	 (piece-color (char)
	   (if (upper-case-p char)
	       :white
	       :black)))
    (let ((tokens (cl-utilities:split-sequence #\Space fen-str))
	  tok (sq 1) empty-count char)
      (clear board)
      (setf tok (pop tokens)) ; положение фигур на доске
      (loop :for row :in (reverse (cl-utilities:split-sequence #\/ tok)) :do
	 (dotimes (i (length row))
	   (setf char (elt row i))
	   (setf empty-count (digit-char-p char))
	   (when (null empty-count)
	     (setf empty-count 1)
	     (setup-piece board sq
			  (make-instance 'piece :kind (piece-by-name char) :color (piece-color char))))
	   (incf sq empty-count)))
      (setf tok (pop tokens)) ; чей ход
      (setf (turn board) (if (char= (elt tok 0) #\w) :white :black))
      (setf tok (pop tokens)) ; рокировки
      (setf (castlings board)
	    (mapcar #'(lambda (c) (cdr (assoc c '((#\K . #@g1@) (#\Q . #@c1@)
						  (#\k . #@g8@) (#\q . #@c8@)))))
		    (coerce tok 'list)))
      (setf tok (pop tokens)) ; взятие на проходе
      (setf (enpassant board) (if (char= (elt tok 0) #\-) nil (string-to-square tok)))
      (setf tok (pop tokens)) ; число полуходов
      (setf tok (pop tokens)) ; число ходов
      t)))


(defgeneric new-game (board)
  (:documentation "Начальная позиция.")
  (:method ((board board))
    (load-from-fen-string board "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")))



;;; Передвижение фигуры

(defun move-piece (board from to)
  (setf (aref (pieces board) to) (aref (pieces board) from))
  (setf (aref (pieces board) from) nil))


(defgeneric make-move (board move &key)
  (:documentation "Делает ход на доске.")
  (:method ((the-board board) move &key (clone-board nil))
    "Если clone-board не равно nil, то BOARD не изменяется и ход возвращается копия BOARD со сделанным на ней ходом."
    (let* ((board (if clone-board (copy-board the-board) the-board))
	   (from (move-from move))
	   (to (move-to move))
	   (piece (whos-at board from))
	   (color (color piece)))
      ;; обработка рокировок; перставить ладью и запрет рокировок
      (let* ((e1e8 (if (eql color :white) #@e1@ #@e8@))
	     (c1c8 (- e1e8 2))
	     (g1g8 (+ e1e8 2))
	     (rook-sq-from (if (< 0 (- to from)) (+ to 1) (- to 2)))
	     (rook-sq-to (- to (signum (- to from)))))
	(cond
	  ((= from e1e8)
	   (setf (castlings board)
		 (remove-if #'(lambda (x) (member x '(c1c8 g1g8))) (castlings board)))
	   (when (castling-move-p piece color move)
	     (move-piece board rook-sq-from rook-sq-to))) ; переносим ладью
	  ((= from (- e1e8 4)) ; a1/8
	   (setf (castlings board) (remove c1c8 (castlings board))))
	  ((= from (+ 3 e1e8)) ; h1/8
	   (setf (castlings board) (remove g1g8 (castlings board))))))
      (setf (slot-value board 'turn) (opposite-color (slot-value board 'turn)))
      (move-piece board from to)
;      (when (not (null (move-transform move)))
;	(setf (kind piece) (move-transform move)))
      board)))


(defmethod initialize-instance :after ((the-board board) &key)
  (new-game the-board))


;;; макросы
(defmacro do-pieces ((board bind-vars &key (color nil)) &body body)
  "Выплняет BODY для всех значений SQ, которые соответствуе полям, на которых стоят фигуры цвета COLOR."
  (let (piece-bind (sq-bind (gensym)))
    (when (and (listp bind-vars) (= (length bind-vars) 2))
      (setf piece-bind (first bind-vars))
      (setf sq-bind (second bind-vars)))
    (when (symbolp bind-vars)
      (setf piece-bind bind-vars))
  `(do ((x-sq 1 (1+ x-sq)))
       ((> x-sq 64))
     (when (and (not (null (aref (pieces ,board) x-sq)))
		(or (null ,color) (eql ,color (color (aref (pieces ,board) x-sq)))))
       (let ((,piece-bind (aref (pieces ,board) x-sq))
	     (,sq-bind x-sq))
	 (declare (ignorable ,piece-bind) (ignorable ,sq-bind)) ;--- FIXME: generate nesessery bindings only
	 ,@body)))))

(defmacro with-move ((board from to) &body body)
  "Evaluates BODY in an implicit PROGN with move FROM-TO made on the
BOARD. Undo changes made by the move after BODY evaluation."
  (let ((piece-at-to (gensym)))
  `(let ((,piece-at-to (whos-at ,board ,to)))
     (move-piece ,board ,from ,to)
     (prog1
         (progn ,@body)
       (move-piece ,board ,to ,from)
       (setup-piece ,board ,to ,piece-at-to)))))
