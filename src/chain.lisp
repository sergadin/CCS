(in-package :ccs)


(defclass <piece> (piece)
  ((position :initarg :position)
   (square :type square
           :initarg :square)))


(defclass <position> (board)
  ((chains)))

(defun make-position (board)
  (let ((pos (make-instance '<position>)))
    (setf (pieces pos) (make-array 65 :initial-element nil)
          (turn pos) (turn board)
          (castlings pos) (castlings board)
          (enpassant pos) (enpassant board))
    (do-pieces (board (p sq))
       (setf (aref (pieces pos) sq)
             (make-instance '<piece>
                            :position pos
                            :kind (kind p)
                            :color (color p)
                            :square sq)))
    pos))

#+(or)(defun piece-at (board sq)
  "Возвращает объект класса `<piece>'"
  (multiple-value-bind (p c) (whos-at board sq)
    (make-instance '<piece> :position (make-position board) :kind (kind p) :color c)))



(deftype field-type () '(member :stop-field :internal-field))

(defclass <trajectory-field> ()
  ((pos :type square
        :initarg :pos
        :accessor field-square
        :initform (error "Undefined")
        :documentation "Координаты поля доски")
   (type :type field-type
         :initarg :type
         :accessor field-type
         :initform (error "Undefined type of trajectory-field")
         :documentation "Тип поля: поле остановки или поле, проходимое без остановки ключевой фигурой")
   (time-limit :type (or integer null)
               :initarg :time-limit
               :initform nil
               :documentation "")
   (subchains :type (vector t *)
              :accessor tf-subchains
              :initform (make-array 1 :adjustable t)))
  (:documentation "Поля траектории"))

(defun path-to-moves (path)
  (loop :for tail :on path
     :when (cdr tail) :collect (cons (first tail) (second tail))))

(defun make-trajectory-field (sq type)
  (make-instance '<trajectory-field> :pos sq :type type))

(defun make-trajectory (path)
  (let* ((moves (path-to-moves path))
         (segments (mapcar #'(lambda (move)
                               (squares-on-line (car move) (cdr move)))
                           moves))
         (len (- (reduce  #'+ segments :key #'length) (length segments) -1))
         (fields (make-array len :element-type '<trajectory-field>)))
    (loop :with i = 0
       :for tail :on (apply #'append segments)
       :for sq = (car tail)
       :and last = nil :then sq
       :and next = (cadr tail)
       :when (not (eql sq last))
       :do (setf (aref fields i)
             (make-trajectory-field sq
                                    (if (or (null next) (null last) (eql sq next))
                                        :stop-field :internal-field)))
           (incf i))
   fields))

(defclass <chain> ()
  ((level :type integer
          :initform 0
          :initarg :level
          :accessor chain-level
          :documentation "Порядок цепочки")
   (parent :type (or <chain> null)
           :initform nil
           :initarg :parent
           :documentation "Ссылка на цепочку меньшего порядка, породившую данную цепочку. В случае подцепи-0 - nil" )
   (trajectory :type (vector <trajectory-field> *)
               :initform (error "Undefined trajectory")
               :initarg :trajectory
               :accessor chain-trajectory
               :documentation "основная траектория цепи")
   (piece :type <piece>
          :initform (error "Undefined piece")
          :initarg :piece
          :accessor chain-piece
          :documentation "Ключевая фигура, реализующая подцепь")
   (position :initarg :position
             :accessor chain-position)))


(defun time-limit (chain index)
  (with-accessors ((traject chain-trajectory)
                   (position chain-position)
                   (piece chain-piece))
      chain
    (when (<= (array-dimension traject 0) index)
      (error "Incorrect index from chain ~d" index))
    (loop :for i :from 1 :to index
       :for field = (aref traject i)
       :for p = (whos-at position (field-square field))
       :summing
                 (if (eq (field-type field) :internal-field)
                    (if p 1 0)
                    ;;STOP-FIELD
                    (if (and p (eq (color p) (color piece)))
                        2
                        1)))))

(defun field-passible-p (field chain)
  (with-accessors ((pos chain-position))
      chain

(defun find-escape-chains (field position parent)
  (let* ((piece-at-field (whos-at position (field-square field)))
         (color (color piece-at-field))
         (paths
          (mapcar #'(lambda (sq) (list (field-square field) sq))
                  (delete-if #'(lambda (sq)
                                 (member sq
                                         (coerce (chain-trajectory parent) 'list)
                                         :key #'field-square))
                             (moves piece-at-field (field-square field) :color color)))))
    (mapcar #'(lambda (path) (make-chain path :position position :parent parent)) paths))) ; sort chains??


(defun find-support-chains (field position parent &key time-limit)
  (let* ((


(defun make-chain (path &key position parent)
  (let* ((sq (first path))
         (piece (whos-at position sq))
         (chain-color (color piece))
         (trajectory (make-trajectory path))
         (level (if parent (+ 1 (chain-level parent)) 0))
         (the-chain (make-instance '<chain>
                                   :piece piece
                                   :trajectory trajectory
                                   :level level
                                   :parent parent
                                   :position position)))
    (loop :with board = (copy-board position)
       :for index :from 1 :below (array-dimension trajectory 0)
       :for field = (aref trajectory index)
       :for time-limit = (time-limit the-chain index)
       :for piece-at-field = (whos-at position (field-square field))
       :do
       ;(move-piece board sq (field-square field))
       ;(print-board board)
       ;(print (list (field-square field) time-limit
       ;                 (exchange-value board (field-square field) :white)))
       (when (and piece-at-field (eq (field-type field) :internal-field))
         (when (eq (color piece-at-field) chain-color)
           (print (find-escape-chains field board the-chain))))
       (when ()
    the-chain))


(defun test-chain ()
  (let ((*board* (create-board))
        (initial-sq #@e6@) ; #@h5@)
        (target-sq #@h1@) ; #@h1@)
        (color :black))
    (load-from-fen-string *board* "8/8/4b1p1/2Bp3p/5P1P/1pK1Pk2/8/8 b - - 0 1")
    (print-board *board*)
    (let ((position (make-position *board*)))
      (do-pieces (*board* (p sq) :color color)
        (when (= sq initial-sq)
          (let* ((traject (first (last (find-paths (kind p) sq target-sq 4 :color color))))
                 (chain (make-chain traject :position position)))
            (print (mapcar #'square-to-string traject))))))))
