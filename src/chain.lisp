(in-package :ccs)

(defvar +default-chain-depath+ 3
  "Default value for maximum subchain level.")

(defun default-horizon (piece)
  (declare (ignore piece))
  3)

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


(defun stop-field-p (field)
  (eq (slot-value field 'type) :stop-field))

(defun make-trajectory-field (sq type)
  (make-instance '<trajectory-field> :pos sq :type type))

(defun make-trajectory (path)
  "Constructs a vector of `trajectory-field's corresponding to PATH."
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
                                            :stop-field
                                            :internal-field)))
           (incf i))
    fields))

(defclass <chain> ()
  ((level :type integer
          :initform 0
          :initarg :level
          :accessor chain-level
          :documentation "Порядок цепочки")
   (depth-analyzed :type integer
          :initform 1
          :accessor chain-depth-analyzed
          :documentation "How many subchain levels were analyzed for
          this chain so far. Value 1 means that no subchains were
          considered at all and only basic trajectory was
          constructed.")
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
   (escapers :initform nil
             :accessor chain-escapers
             :documentation "List of pieces that escapes from the chain's trajectory.")
   (position :initarg :position
             :accessor chain-position)))

(defun chain-color (chain)
  (color (chain-piece chain)))

(defun trajectory-time-limit (trajectory position &optional index)
  "Estimate number of moves required for a piece to reach INDEX's
field on the TRAJECTORY in POSITION. Opponent's moves are not
counted. If INDEX is not specified, then the result corresponds to
last field on TRAJECTORY."
  (when (and index (<= (array-dimension trajectory 0) index))
    (error "Incorrect index for trajectory-time-limit: ~D's field (zero-based) of trajectory with ~D fields requested."
           index (array-dimension trajectory 0)))
  (loop
     :with piece = (whos-at position (field-square (aref trajectory 0)))
     :for i :from 1 :to (or index (1- (array-dimension trajectory 0)))
     :for field = (aref trajectory i)
     :for p = (whos-at position (field-square field))
     :summing
     (if (eq (field-type field) :internal-field)
         (if p 1 0)
         ;;STOP-FIELD
         (if (and p (eq (color p) (color piece)))
             2
             1))))


(defun time-limit (chain index)
  (with-accessors ((traject chain-trajectory)
                   (position chain-position))
      chain
    (when (<= (array-dimension traject 0) index)
      (error "Incorrect index from chain ~d" index))
    (trajectory-time-limit traject position index)))


(defun subchain0-path (chain)
  "Extract underlying path, i.e. a sequecne of stop-fields' squares, from the CHAIN."
  (loop :for field :across (chain-trajectory chain)
     :when (stop-field-p field)
     :collect (field-square field)))



(defun estimate-chain-complexity (path position &key (precision 0))
  "Rough estimation of chain complexity, in moves, provided that chain
whould be constructed from specified PATH. Larger PRECISION values
presumably yields more accurate estimations."
  (declare (ignore precision))
  (let ((trajectory (make-trajectory path)))
    (trajectory-time-limit trajectory position)))



(defun find-escape-chains (field position parent)
  (log-message :trace "Searching for escape chain from ~A. Level=~D"
               (square-to-string (field-square field))
               (1+ (chain-level parent)))
  (let* ((piece-at-field (whos-at position (field-square field)))
         (color (color piece-at-field))
         (paths ; a list of two items paths
          (mapcar #'(lambda (sq) (list (field-square field) sq))
                  (delete-if #'(lambda (sq)
                                 (member sq
                                         (coerce (chain-trajectory parent) 'list)
                                         :key #'field-square))
                             (moves piece-at-field (field-square field) :color color)))))
    ;;--- TODO: find best candidate
    (mapcar #'(lambda (path)
                (format t "~A ==> ~A~%" (square-to-string (first path)) (square-to-string (second path)))
                (make-chain path position :parent parent))
            paths)))


(defun find-support-chains (field position parent &key time-limit (color (chain-color parent)))
  "Find subchains making FIELD passable by the PARENT chain piece."
  (log-message :trace "Searching for support chains on ~A" (square-to-string (field-square field)))
  ;; Attack the field by other pieces
  (let ((support-chains nil))
    (do-pieces (position (piece square) :color color)
      (let ((targets (pre-moves piece (field-square field)))
            (horizon (or time-limit (default-horizon piece)))
            (candidate-paths))
        (dolist (target-square targets)
          (dolist (path (find-paths (kind piece) square target-square horizon :color color))
            (push path candidate-paths)))
        (when candidate-paths
          (let ((ordered-candidates
                 (sort candidate-paths #'<
                       :key #'(lambda (path) (estimate-chain-complexity path position)))))
            (format t "Best chain found (~{~A ~})~%" (mapcar #'square-to-string (first ordered-candidates)))
            (push (make-chain (first ordered-candidates)
                              position
                              :parent parent)
                  support-chains)))))
    support-chains))


(defun make-chain (path position &key parent (max-level +default-chain-depath+))
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
    (when (null parent)
      (log-message :trace "Constructing chain for the path ~{~A ~}" (mapcar #'square-to-string path)))
    (loop :with board = (copy-board position)
       :for index :from 1 :below (array-dimension trajectory 0)
       :for field = (aref trajectory index)
       :and chain-piece-square = (field-square (aref trajectory 0)) :then (field-square field)
       :for square = (field-square field)
       :for time-limit = (time-limit the-chain index)
       :for piece-at-field = (whos-at position (field-square field))
       :while (< level max-level) ; do not create extremely nested subchains
       :do
       ;(move-piece board sq (field-square field))
       ;(print-board board)
       ;(print (list (field-square field) time-limit
       ;                 (exchange-value board (field-square field) :white)))
       (when piece-at-field ; the field is occupied by a piece
         (cond
           ((eq (color piece-at-field) chain-color)
            ;; remove our piece from the route
            (let* ((escapes (find-escape-chains field board the-chain))
                   (best-escape (first escapes)))
              ;; Make most reasonable escape move
              (move-piece position (field-square field) (first (subchain0-path best-escape)))))
           ((and (not (eq (color piece-at-field) chain-color))
                 (eq (field-type field) :internal-field))
            ;; opponent's piece in the middle: escape, protect or do nothing
            (exchange-value position square chain-color)
            nil)))

       (when (stop-field-p field)
         (let ((ev 0))
           (with-move (position chain-piece-square square)
             (setf ev (exchange-value position square (opposite-color chain-color))))
           (when (not (exchange-positive-p ev chain-color))
             ;; Can not simply move to that square due to negative exchange value.
             (log-message :trace "Exchange value on ~A ~F" (square-to-string square) ev)
             (find-support-chains field position the-chain))))
       )
    the-chain))


(defun test-chain ()
  (start-logging)
  (let ((*board* (create-board))
        (initial-sq #@e6@) ; #@h5@)
        (target-sq #@h1@) ; #@h1@)
        (color :black))
    (load-from-fen-string *board* "8/8/4b1p1/2Bp3p/5P1P/1pK1Pk2/8/8 b - - 0 1")
    (log-message :debug "~A" (print-board *board* :stream nil))
    (let ((position (make-position *board*)))
      (do-pieces (*board* (p sq) :color color)
        (when (= sq initial-sq)
          (let* ((traject (first (last (find-paths (kind p) sq target-sq 4 :color color))))
                 (chain (make-chain traject position)))
            (print (mapcar #'square-to-string traject))))))))
