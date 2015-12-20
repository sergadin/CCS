(defun square-to-number (str)
  "Преобразование из строки вида 'h3' в элемент типа square."
  (let ((col (elt (string-upcase str) 0))
	(row (elt str 1)))
    (handler-case
	(+ (position col "-ABCDEFGH")
	   (* 8 (position row "12345678")))
      (t () (error "~S is not a valid square notation." str)))))


(defmethod load-from-fen-string ((board board) fen-str)
  (let ((tokens (cl-utilities:split-sequence #\Space (string-upcase fen-str)))
	(color :white)
	(piece-name #'(lambda (tok)
			(if (= (length tok) 2)
			    :pawn
			    (car (assoc (elt tok 0)
				   '((#\K . :king) (#\Q . :queen) (#\R . :rook)
				     (#\N . :knight) (#\B . :bishop))))))))
    (clrhash (slot-value board 'pieces))
    (loop :for tok :in tokens :do
       (cond
	 ((string= tok "WHITE") (setf color :white))
	 ((string= tok "BLACK") (setf color :black))
	 (t (setf (gethash (square-to-number (subseq tok (- (length tok) 2)))
			   (slot-value board 'pieces))
		  (cons (funcall piece-name tok) color))))))
  t)





(defun valid-move-p-old (board from to)
  (declare (board board) (square from) (square to))
  "Проверка допустимости хода с поля FROM на поле TO."
  (multiple-value-bind (piece color) (whos-at board from)
    (when (or (null (whos-at board from)) ; нет фигуры
	      (not (eql color (turn board))) ; не тот цвет
	      (member color (multiple-value-list (whos-at board to))) ; на поле to наша фигура
	      (not (member to (moves piece from))) ; фигура так не ходит
	      (and (not (eql piece :knight))
		   (not (free-line-p board from to)))) ; занято поле на пути
      (return-from valid-move-p nil))
    t))
