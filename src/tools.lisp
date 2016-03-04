(in-package :ccs)

(defmacro push-to-end (item place)
  `(setf ,place (nconc ,place (list ,item))))



(defun string-to-square (str)
  "Преобразование из строки вида 'h3' в элемент типа square."
  (let ((p
	(position str '(:a1 :b1 :c1 :d1 :e1 :f1 :g1 :h1
			:a2 :b2 :c2 :d2 :e2 :f2 :g2 :h2
			:a3 :b3 :c3 :d3 :e3 :f3 :g3 :h3
			:a4 :b4 :c4 :d4 :e4 :f4 :g4 :h4
			:a5 :b5 :c5 :d5 :e5 :f5 :g5 :h5
			:a6 :b6 :c6 :d6 :e6 :f6 :g6 :h6
			:a7 :b7 :c7 :d7 :e7 :f7 :g7 :h7
			:a8 :b8 :c8 :d8 :e8 :f8 :g8 :h8)
		  :test #'string-equal)))
    (if p (+ 1 p) (error "~A не является корректным обозначением поля." str))))


(defun square-to-string (sq)
  (format nil "~C~A" (elt "habcdefgh" (mod sq 8)) (ceiling sq 8)))

(defun print-move (m stream depth)
  (declare (ignore depth))
  (format stream "#<~A-~A>" (square-to-string (move-from m)) (square-to-string (move-to m))))

(declaim (inline opposit-color))
(defun opposite-color (color)
  "Возвращает противоположенный цвет."
  (declare (color color))
  (if (eql color :white) :black :white))



(defun parse-move-string (s)
  "Разбирает строку в формате xboard. Например, e3e4, g1f3 или f7b8Q. Возвращает структуру move, если удалось разобрать строку, иначе nil."
  (if (<= 4 (length s) 5)
      (handler-case
	  (let* ((len (length s))
		 (from (string-to-square (subseq s 0 2)))
		 (to (string-to-square (subseq s 2 4)))
		 (piece (if (= len 5) (piece-by-name (elt s 4)) nil)))
	    (create-move from to piece))
	(t nil))
      nil))


(defun |#@-reader| (stream sub-char numarg)
  "Считывает либо обозначение поля (e2), либо хода (e2e4). Возвращает square или move."
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((curr (read-char stream) (read-char stream)))
        ((and (char= curr #\@)))
      (push curr chars))
    (let* ((s (coerce (nreverse chars) 'string)))
      (or (parse-move-string s) (string-to-square s)))))


(set-dispatch-macro-character
 #\# #\@ #'|#@-reader|)



#|
(:shadowing-import-from
 #+openmcl-native-threads #:ccl
 #+cmu #:pcl
 #+sbcl #:sb-pcl
 #+lispworks #:hcl
 #+allegro #:mop
 #+clisp #:clos
 #:class-slots #:slot-definition-name)
|#
(defun shallow-copy-object (original)
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'slot-definition-name (class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))


(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))


(defun fixed-point (f x &key (test #'equal))
  (do ((prev x current)
       (current (funcall f x) (funcall f current)))
      ((funcall test current prev) current)
    #| empty body |#))


(defmacro with-mixin ((obj mixin) &body body)
  (let ((mixin-obj (gensym)))
    `(let ((,mixin-obj (dynamic-mixins:ensure-mix ,obj ,mixin)))
       (prog1
           (progn ,@body)
         (dynamic-mixins:delete-from-mix ,mixin-obj ,mixin)))))
