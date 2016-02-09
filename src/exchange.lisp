;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; Размен на поле
;;;;
;;;; $Id: exchange.lisp 19 2013-10-12 20:50:46Z serg $
;;;;

(in-package :ccs)


(defun exchange-value (board sq color)
  "Вычисляет значение размена на поле SQ, при условии, что первый ход делают COLOR. Оценка вычисляется с точки зрения белых."
  (let ((val 0.0d0)
        (t-board (copy-board board))
        exchange-trace)
    (setf (turn t-board) color)
    (do* ((who color (opposite-color who))
          (direct-attackers (attackers t-board sq :color who)
                            (attackers t-board sq :color who))
          (p (whos-at t-board sq)
             (whos-at t-board sq))
          (sign +1 (* sign -1)))
         ((or (null direct-attackers) (null p)))
      (when (eql (color p) (opposite-color who))
        (push (incf val (* sign (value-of p))) exchange-trace))
      (make-move t-board
                 (create-move
                  (first (sort direct-attackers #'< :key #'(lambda (x) (value-of (whos-at board x)))))
                  sq)))
    ;; поиск оптимального отсечения
    (let ((answer
           (flet ((extr (list &optional (shift 0) &aux res)
                    (do ((l (nthcdr shift list) (cddr l)))
                        ((null l) (nreverse res))
                      (push (car l) res))))
             ((lambda (trace) (* (if (eql color :white) +1 -1) (or (first trace) 0))) ; оценка со стороны белых
              (fixed-point (let ((shift 0)) ; с какого индекса начинаются ходы оппонента
                             (lambda (trace) ; trace всегда четной длины
                               (when trace
                                 (let* ((func (if (= shift 0) #'max #'min))
                                        (mine-cuts (extr trace shift))
                                        ;; номер хода, который текущий игрок должен сделать последним
                                        (mine-last-move (or (position (apply func (car trace) mine-cuts)
                                                                      mine-cuts)
                                                            0)))
                                   (setf shift (logxor shift 1)) ; меняем очередность хода
                                   (subseq trace (* 2 mine-last-move))))))
                           ;; добавим к trace нейтральный ход (последний ход должен быть чужим)
                           (if (= 1 (mod (length exchange-trace) 2))
                               (append (cons (first exchange-trace) exchange-trace))
                               exchange-trace))))))
      (values answer t-board))))

(defun exchange-positive-p (value color &key strictly)
  (let ((cmp (if strictly #'< #'<=)))
    (if (eq color :white)
        (funcall cmp 0 value)
        (funcall cmp value 0))))

(defun better-exchange-p (value-new value-old color &key (strictly t))
  "Verify that VALUE-NEW is better for COLOR than VALUE-OLD."
  (let ((cmp (if strictly #'< #'<=)))
    (if (eq color :white)
        (funcall cmp value-old value-new)
        (funcall cmp value-new value-old))))
