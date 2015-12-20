;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; $Id: types.lisp 12 2013-03-30 16:10:21Z serg $
;;;;
;;;; Основные типы данных.
;;;;

(in-package :ccs)

(deftype color () '(member :white :black))
(deftype piece-kind () '(member :king :queen :rook :bishop :knight :pawn))
(deftype square ()
  "Поля задаются целыми числами из интервала [1,64]. Для преобразования строк вида 'a3' к элементам типа square может использоваться функция string-to-square или форма #@a3@."
  '(integer 1 64))

(defstruct (move
	     (:constructor create-move (from to &optional transform))
	     (:print-function print-move))
  "Структура для описания хода. BOA конструктор create-move. FROM и TO являются обязательными полями, TRANSFORM имеет смысл только для хода пешкой на 8-ю горизонталь."
  (from nil :type square)
  (to nil :type square)
  (transform nil :type (or null piece-kind)))


(defmethod make-load-form ((m move) &optional env)
  (declare (ignore env))
  (list 'create-move (move-from m) (move-to m) (move-transform m)))