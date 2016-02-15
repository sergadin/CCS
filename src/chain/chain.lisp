(in-package :ccs)

(defvar +default-chain-depath+ 3
  "Default value for maximum subchain level.")

(defparameter +infinite-piece-value+ 1000 "Used as an approximation of infinity.")


(defun default-horizon (piece)
  (declare (ignore piece))
  3)

(defclass <piece> (piece)
  ((position :initarg :position)
   (square :type square
           :initarg :square)
   (chains-db :initform (make-chains-database)
           :documentation
           "Tree-like database of all subchains this piece
           participates in. Indexed by subchain's path squares.")))

(defgeneric add-chain (piece chain)
  (:documentation "Register that PIECE paricipates in the CHAIN."))

(defmethod add-chain ((piece <piece>) (chain-or-subchain <chain>))
  (cdb-add (slot-value piece 'chains-db)
           (subchain0-path chain-or-subchain)
           (root-chain chain-or-subchain)))


(defclass <unlostable-piece> (<piece>)
  () ; no slots
  (:documentation "Mixin class used for local modification of value-of function."))

(defmethod value-of ((piece <unlostable-piece>) &key)
  (declare (ignore piece))
  +infinite-piece-value+)


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

(defun copy-position (position)
  (shallow-copy-object position))


#+(or)(defun piece-at (board sq)
  "Возвращает объект класса `<piece>'"
  (multiple-value-bind (p c) (whos-at board sq)
    (make-instance '<piece> :position (make-position board) :kind (kind p) :color c)))



(deftype field-type () '(member :stop-field :internal-field :extra-stop-feild))

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


(defun stop-field-p (field &key (strict t))
  (member (slot-value field 'type)
          (if strict '(:stop-field) '(:stop-field :extra-stop-field))))

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
           :accessor chain-parent
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
     (case (kind piece)
       (:pawn
        (if (pawn-take-move-p piece
                              (color piece)
                              (create-move (field-square (aref trajectory (- i 1)))
                                           (field-square field)))
            (cond
              ((null p) 2)
              ((eq (color p) (color piece)) 3) ;; or 2 ???
              (t 1))
            (cond
              ((null p) 1)
              ((eq (color p) (color piece)) 2)
              (t 3))))
       (t
        (if (eq (field-type field) :internal-field)
            (if p 1 0)
            ;;STOP-FIELD
            (if (and p (eq (color p) (color piece)))
                2
                1))))))


(defun root-chain (chain)
  (if (= (chain-level chain) 0)
      chain
      (root-chain (chain-parent chain))))


(defun time-limit (chain &optional index)
  (with-accessors ((traject chain-trajectory)
                   (position chain-position))
      chain
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
  "Field clearance."
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
                ; (format t "~A ==> ~A~%" (square-to-string (first path)) (square-to-string (second path)))
                (make-chain path position :parent parent))
            paths)))

(defun knapsack (objects min-gain-limit max-time-limit
                 &key (gain-key #'identity)
                   (time-key #'(lambda (x) (declare (ignore x)) 1)))
  "Жадный алгоритм решения задачи о рюкзаке"
  (loop :for obj :in (sort objects #'> :key gain-key)
     :when (or (null max-time-limit)
               (<= (+ (funcall time-key obj) current-time-spent) max-time-limit))
     :collect obj
     :summing (funcall time-key obj) :into current-time-spent
     :summing (funcall gain-key obj) :into achieved-gain
     :while (< achieved-gain min-gain-limit)))
     ;:for sum-gain = (gain-key obj)



(defun find-support-chains (field position parent &key time-limit (color (chain-color parent)))
  "Find subchains making FIELD passable by the PARENT chain piece."
  (log-message :trace "Searching for support chains on ~A" (square-to-string (field-square field)))
  (let ((support-chains nil)
        (old-exchange-value (exchange-value position (field-square field) (opposite-color color))))
    ;; Attack the field by other pieces
    (do-pieces (position (piece square) :color color) ;; ?.. what about pieces from parent's chain?
      (let ((targets (pre-moves piece (field-square field) :attack-only t))
            (horizon (or time-limit (default-horizon piece)))
            (candidate-paths)
            (new-exchange-value))
        ;; Find all empty squares where PIECE can participate in the exchange on FIELD.
        (dolist (target-square targets)
          (setf new-exchange-value
                (with-move (position square target-square)
                  (exchange-value position (field-square field) (opposite-color color))))
          (when (and (empty-square-p position target-square)
                     (better-exchange-p
                      new-exchange-value
                      old-exchange-value
                      color))
            (loop :for path = (find-paths (kind piece) square target-square horizon :color color)
               :do (push (cons (- old-exchange-value new-exchange-value) path)  candidate-paths))))
        (when candidate-paths
          (let ((ordered-candidates
                 (sort candidate-paths #'<
                       :key #'(lambda (g-path) (estimate-chain-complexity (cdr g-path) position)))))
            (format t "Best chain found (~{~A ~})~%" (mapcar #'square-to-string (first ordered-candidates)))
            ;;search best chain
            (loop :for (gain . candidate-path) :in ordered-candidates
               :for chain = (make-chain candidate-path
                                        position
                                        :parent parent)
               :when (not (and time-limit (> (time-limit chain) time-limit)))
               :do (push (list (time-limit chain) gain chain) support-chains)
               (return))))))
    ;;--- TODO: Make opponents moves impossible due to absolute or relative pin.

    ;best-support-chain ?..
    (knapsack support-chains old-exchange-value time-limit
              :gain-key #'second
              :time-key #'first)))
      ;;(mapcar #'(lambda (t-g-chain) (elt t-g-chain 2)) support-chains)))


(defun make-chain (path position &key parent (max-level +default-chain-depath+))
  (let* ((t-position (copy-position position))
         (sq (first path))
         (piece (whos-at t-position sq))
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
    (loop :with board = (copy-board t-position)
       :for index :from 1 :below (array-dimension trajectory 0)
       :for field = (aref trajectory index)
       :and chain-piece-square = (field-square (aref trajectory 0)) :then (field-square field)
       :for square = (field-square field)
       :for time-limit = (time-limit the-chain index)
       :for piece-at-field = (whos-at t-position (field-square field))
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
              (move-piece t-position (field-square field)
                          (first (subchain0-path best-escape)))
              ;;---------
              (setf piece-at-field nil)))
              ;;---------
           ((and (not (eq (color piece-at-field) chain-color))
                 (eq (field-type field) :internal-field))
            ;; opponent's piece in the middle: escape, protect or do nothing
            (setf (field-type field) :extra-stop-field)
            ;;(exchange-value t-position square chain-color)
            nil)))

       ;; now piece-at-field nil or belongs to opponent
       (when (stop-field-p field :strict nil)
         (let ((ev (piece-value piece-at-field :color chain-color))) ; exchange-value
                                        ;(log-message :debug "~A" (print-board t-position :stream nil))
           (with-move (t-position #|chain-piece-square|# sq square)
                                        ;(log-message :debug "bf ~A" (print-board t-position :stream nil))
             (incf ev (exchange-value t-position square (opposite-color chain-color))))
           (log-message :trace "Exchange value on ~A ~F" (square-to-string square) ev)
           (when (not (exchange-positive-p ev chain-color))
             ;; Can't move to that square due to negative exchange value. Find support chains.
             ;;(log-message :trace "Exchange value on ~A ~F" (square-to-string square) ev)
             (let ((support-chains
                    (find-support-chains field t-position the-chain)))
               (dolist (sc support-chains)
                 (add-chain (chain-piece sc) sc))))
           ;;(move-piece t-position chain-piece-square square)
           )))
    (add-chain piece the-chain)
    (print-chains-database (slot-value piece 'chains-db))
    (cdb-iterate (slot-value piece 'chains-db) (subchain0-path the-chain) #'(lambda (c) (print c)))
    the-chain))


(defun test-chain ()
  (start-logging)
  (let ((*board* (create-board))
        (initial-sq #@e6@) ; #@h5@)
        (target-sq #@e2@) ; #@h1@)
        (color :black))
    (load-from-fen-string *board* "8/8/4b1p1/2Bp3p/5P1P/1pK1Pk2/8/8 b - - 0 1")
    (log-message :debug "~A" (print-board *board* :stream nil))
    (let ((position (make-position *board*)))
      (do-pieces (*board* (p sq) :color color)
        (when (= sq initial-sq)
          (let* ((traject (first (last (find-paths (kind p) sq target-sq 4 :color color))))
                 (chain (make-chain traject position)))
            (print (mapcar #'square-to-string traject))))))))
