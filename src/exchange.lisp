;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; Размен на поле
;;;;
;;;; $Id: exchange.lisp 19 2013-10-12 20:50:46Z serg $
;;;;

(in-package :ccs)


(defun exchange-value (board sq color)
  (let ((p (whos-at board sq)))
    (if (and p (eql color (color p)))
        (%exchange-value-aux board sq (opposite-color color))
        (%exchange-value-aux board sq color))))


(defun optimal-cut (trace color &optional (current 0.0d0))
  "COLOR делают первый ход в последовательности TRACE, взвешенной с точки зрения белых."
  (if (null trace)
      current
      (funcall (if (eql color :white) #'max #'min)
               current
               (optimal-cut (cdr trace) (opposite-color color) (car trace)))))



(defun %exchange-value-aux (board sq color)
  "Вычисляет значение размена на поле SQ, при условии, что первый ход делают COLOR. Оценка вычисляется с точки зрения белых."
  (let ((val 0.0d0)
        (t-board (copy-board board))
        exchange-trace)
    (setf (turn t-board) color)
    (do* ((who color
               (opposite-color who))
          (direct-attackers (attackers t-board sq :color who)
                            (attackers t-board sq :color who))
          (p (whos-at t-board sq)
             (whos-at t-board sq))
          (sign (if (eql who :white) +1 -1)
                (* sign -1)))
         ((or (null direct-attackers) (null p)))
      (when (eql (color p) (opposite-color who))
        (push (incf val (* sign (value-of p))) exchange-trace))

      (make-move t-board
                 (create-move
                  (first (sort direct-attackers #'< :key #'(lambda (x) (value-of (whos-at board x)))))
                  sq)))
    (values (optimal-cut (nreverse exchange-trace) color) t-board)))

(defun exchange-positive-p (value color &key strictly)
  (let ((cmp (if strictly #'< #'<=)))
    (if (eq color :white)
        (funcall cmp 0 value)
        (funcall cmp value 0))))

(defun better-exchange-p (value-new value-old color &key (strictly t))
  "Verify that exchange value VALUE-NEW is better than VALUE-OLD for COLOR."
  (let ((cmp (if strictly #'< #'<=)))
    (if (eq color :white)
        (funcall cmp value-old value-new)
        (funcall cmp value-new value-old))))
