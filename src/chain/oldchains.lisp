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
  (:documentation "Register that PIECE participates in the CHAIN."))


(defclass <unlostable-piece> (<piece>)
  () ; no slots
  (:documentation "Mixin class used for local modification of value-of function.
Intended usage:
  (with-unlostable (piece)
    (forms))
Forms will be evaluated with redefined methods, e.g. value-of, for the PIECE.
"))

(defmacro with-unlostable ((piece) &body body)
  "Evaluate BODY with PIECE class changed to <unlostable-piece>."
  `(with-mixin (,piece '<unlostable-piece>)
     ,@body))

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

(defmethod print-object ((obj <position>) out)
  ;(print-unreadable-object (obj out :type t)
  ;  (format out "~s" (foo-name obj))))
  (format out "~%")
  (print-diagram obj :stream out))


(deftype field-type () '(member :stop-field :internal-field :extra-stop-feild))

(deftype chain-type () '(member :support :escape :main))


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
              :initform (make-array 0 :adjustable t
                                    :fill-pointer t)))
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
                               (let ((sol (squares-on-line (car move) (cdr move))))
                                 (if sol
                                     sol
                                     (list (car move) (cdr move)))))
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




(defclass <action> ()
  ((alternatives :type (or null <alternative-actions>)
                 :initform nil
                 :accessor alt-actions)))

(defclass <alternative-actions> ()
  ((actions :type list
            :initform nil
            :accessor aa-actions)
   (mainline :type (or null <action>)
             :initform nil
             :accessor aa-mainline
             :documentation "Member of ACTION that considered as the main alternative."))
  (:documentation "Representation of the available alternatives for some action."))

(defgeneric add-alternative (action alternative-action &key mainline)
  (:documentation "................"))


(defmethod add-alternative ((main <action>) (alt <action>) &key (mainline nil))
  (with-accessors ((maa alt-actions)) main
    (when (null maa)
      (setf maa (make-instance '<alternative-actions>)))
    (if (alt-actions alt)
        (loop :for act :in (aa-actions (alt-actions alt)) :do
           (when (not (find act (aa-actions maa)))
             (push act (aa-actions maa)))
           (setf (alt-actions act) maa))
        (progn
          (push alt (aa-actions maa))
          (setf (alt-actions alt) maa)))
    (when (or mainline (null (aa-mainline maa)))
      (setf (aa-mainline maa) alt))))


(defclass <chain> (<action>)
  ((level :type integer
          :initform 0
          :initarg :level
          :accessor chain-level
          :documentation "Порядок цепочки")
   (type :type chain-type
         :initarg :type
         :accessor chain-type
         :documentation "Chain type")
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

(defclass <bunch-of-chains> (<action>)
  ((chains :type list
           :initform nil
           :initarg :chains
           :accessor bunch-chains))
  (:documentation "A collection of chains that should be realized together."))

(defclass <cdb-node-data> ()
  ((chain :type <chain>
          :documentation "The chain.")
   (bunch :type (or <bunch-of-chains> null)
          :documentation "The bunch to which the chain belongs.")
   (selected :type (or t null)
             :documentation "T, if this subchain was selected for play in the CHAIN's root."))
  (:documentation "Data structure assigned to a node of the `piece''s chains database."))

(defun make-bunch (chain-or-chains)
  "Make a `bunch-of-chains' from the list of chains or a chain."
  (etypecase chain-or-chains
    (<chain> (make-bunch (list chain-or-chains)))
    (list (make-instance '<bunch-of-chains> :chains chain-or-chains))))
     ;(let ((bunch (make-instance '<bunch-of-chains> :chains chain-or-chains)))
     ;       (loop :for ch :in chain-or-chains :do ( (chain-bunch ))

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


(defmethod add-chain ((piece <piece>) (chain-or-subchain <chain>))
  ;; (log-message :trace "Adding the chain ~A of level ~D to the database" chain-or-subchain (chain-level chain-or-subchain))
  ;(let ((node
  (cdb-add (slot-value piece 'chains-db)
           (subchain0-path chain-or-subchain)
           chain-or-subchain))


(defun estimate-chain-complexity (path position &key (precision 0))
  "Rough estimation of chain complexity, in moves, provided that chain
whould be constructed from specified PATH. Larger PRECISION values
presumably yields more accurate estimations."
  (declare (ignore precision))
  (let ((trajectory (make-trajectory path)))
    (trajectory-time-limit trajectory position)))


(defun find-escape-chains (field position parent &key (register t))
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
                (format t "~A ==> ~A~%" (square-to-string (first path)) (square-to-string (second path)))
                (print (make-chain path position :parent parent :register register
                                   :type :escape)))
            paths)))



(defun knapsack (objects min-gain-limit max-time-limit
                 &key (gain-key #'identity)
                   (time-key #'(lambda (x) (declare (ignore x)) 1)))
  "Жадный алгоритм решения задачи о рюкзаке"
  (flet ((better-item (x y) ; X is better than Y: more gain in less time
           (let ((gx (funcall gain-key x))
                 (gy (funcall gain-key y)))
             (or (> gx gy)
                 (and (= gx gy)
                      (< (funcall time-key x) (funcall time-key y)))))))
    (loop :for obj :in (sort objects #'better-item)
       :when (or (null max-time-limit)
                 (<= (+ (funcall time-key obj) current-time-spent) max-time-limit))
       :collect obj
       :summing (funcall time-key obj) :into current-time-spent
       :summing (funcall gain-key obj) :into achieved-gain
       :while (< achieved-gain min-gain-limit))))

(defun piece-allowed-in-chain-p (piece path chain)
  (cond
    ((null chain) t)
    ((not (eq piece (chain-piece chain)))
      (piece-allowed-in-chain-p piece path (chain-parent chain)))
    (t
     (every #'(lambda (sq)  (let ((field (find sq
                                   (coerce (chain-trajectory chain) 'list)
                                   :key #'field-square)))
                              (and field (stop-field-p field :strict t))))
            path))))

;(defun path-along-parents-traject-p (
(defun find-support-chains (field position parent &key time-limit (color (chain-color parent))
                                                    (old-exchange-value 0)
                                                    (register t))
  "Find subchains making FIELD passable by the PARENT chain piece."
  (assert (not (null parent)))
  (log-message :trace "Searching for support chains on ~A" (square-to-string (field-square field)))
  (let ((support-chains nil))
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
                  (with-move (position (first (subchain0-path parent)) (field-square field))
                   ; (log-message :debug "~A" position)
                    (exchange-value position (field-square field) (opposite-color color)))))
         ; (log-message :debug "exchange value with ~A --- ~F -> ~F, ~F~%- - -- - - - - --  - -- - -- - - - - - - - - - -~%"
          ;             (square-to-string target-square)
           ;            old-exchange-value
            ;           new-exchange-value
             ;          (exchange-value position (field-square field) color))
          (when (and (empty-square-p position target-square)
                     (better-exchange-p new-exchange-value
                                        old-exchange-value
                                        color))
            (loop :for path :in (find-paths (kind piece) square target-square horizon :color color)
               :when (and (not (eq piece (chain-piece parent)))
                          (piece-allowed-in-chain-p piece path parent))
               :do
               (push (cons (- old-exchange-value new-exchange-value) path) candidate-paths))))

        (log-message :debug "Found ~D candidate ~A paths for ~A~A support on ~A (~F -> ~F)"
                     (length candidate-paths)
                     color
                     (piece-to-name (kind piece))
                     (square-to-string square)
                     (square-to-string (field-square field))
                     old-exchange-value
                     new-exchange-value)

        (when candidate-paths
          (let ((ordered-candidates
                 (sort candidate-paths #'<
                       :key #'(lambda (g-path) (estimate-chain-complexity (cdr g-path) position)))))
            ;; Find the best chain
            (loop :for (gain . candidate-path) :in ordered-candidates
               :for chain = (make-chain candidate-path
                                        position
                                        :parent parent
                                        :register nil
                                        :type :support)
               :when (or (not time-limit) (<= (time-limit chain) time-limit))
               :do (push (list (time-limit chain) gain chain) support-chains)
               (format t "~{~A~^-~}~%" (mapcar #'square-to-string candidate-path))
               (return))))))
    ;;--- TODO: Make opponents moves impossible due to absolute or relative pin.

    ;; best-support-chain ?..
    (log-message :debug "~A" support-chains)
    (loop
       :with best-bucket = (knapsack support-chains old-exchange-value time-limit
                                     :gain-key #'second
                                     :time-key #'first)
       :with chains = (mapcar #'(lambda (t-g-chain) (elt t-g-chain 2))
                              best-bucket)
       :initially
       (log-message :trace "Support found for ~A: ~{~A~^; ~}"
                    (square-to-string (field-square field))
                    (mapcar #'(lambda (chain)
                                (format nil "~A (~A)"
                                        (mapcar #'square-to-string (subchain0-path chain))
                                        (estimate-chain-complexity (subchain0-path chain) position)))
                            chains))

       :for chain :in chains :when register :do (add-chain (chain-piece chain) chain)

       :finally (return (make-bunch chains)))))


(defun find-threat-attackers (field position parent &key time-limit (color (chain-color parent))
                                                      (register t))
  (let (threat-chains)

))


(defmethod print-object ((obj <chain>) out)
  (print-unreadable-object (obj out :type t)
    (format out "[~A L~D ~A] ~{~A~^ -> ~}"
            (piece-to-name (kind (chain-piece obj)))
            (chain-level obj)
            (chain-type obj)
            (mapcar #'square-to-string (subchain0-path obj)))))


(defun make-chain (path position
                   &key parent
                     type
                     (max-level +default-chain-depath+)
                     (register t))
  (check-type type chain-type "not a valid value for type argument")
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
                                   :position position
                                   :type type)))
    (when (null parent)
      (log-message :trace "Constructing chain for the path ~{~A ~}" (mapcar #'square-to-string path)))
    (loop :with board = (copy-board t-position)
       :for index :from 1 :below (array-dimension trajectory 0)
       :for field = (aref trajectory index)
       ;:and chain-piece-square = (field-square (aref trajectory 0)) :then (field-square field)
       :for square = (field-square field)
       :for time-limit = (time-limit the-chain index)
       :for piece-at-field = (whos-at t-position (field-square field))
       :while (< level max-level) ; do not create extremely nested subchains
       :do

       (when piece-at-field ; the field is occupied by a piece
         (cond
           ((eq (color piece-at-field) chain-color)
            ;; remove our piece from the route
            (let* ((escapes (find-escape-chains field board the-chain :register nil))
                   (best-escape (first escapes)))
                                                                                      ;Add first escapes!!
              ;;add escape-chains to chains-list of piece-at-field
              (loop :for esc :in escapes :do
                 ;(format t "escape chain ~A ~% ~{~A~%~}" esc escapes)
                 (add-chain piece-at-field esc)
                 (vector-push-extend esc (tf-subchains field))
                 (add-alternative best-escape esc))))

           ((and (not (eq (color piece-at-field) chain-color))
                 (eq (field-type field) :internal-field))
            ;; opponent's piece in the middle: escape, protect or do nothing
            (setf (field-type field) :extra-stop-field)
            ;;(exchange-value t-position square chain-color)
            nil)))

       (when (stop-field-p field :strict nil)
         (let ((ev (if (piece-of-color piece-at-field chain-color)
                       0 ; pieces of the same color do not provide any gain to exchange value
                       (piece-value piece-at-field :color chain-color)))) ; exchange-value
           (with-move (t-position sq square)
             (incf ev (exchange-value t-position square (opposite-color chain-color))))
           ;(log-message :trace "Exchange value on ~A ~F" (square-to-string square) ev)
           (when (not (exchange-positive-p ev chain-color))
             ;; Can't move to that square due to negative exchange value. Find support chains.
             (let ((support-chains
                    (find-support-chains field position the-chain
                                         :old-exchange-value ev
                                         :register register)))
               (dolist (sc (bunch-chains support-chains))
                 (when (not (eq (chain-piece sc) piece))
                   (add-chain (chain-piece sc) sc)
                   (vector-push-extend sc (tf-subchains field))))))
           ;;(move-piece t-position chain-piece-square square)
           (let ((opposite-chains
                  (find-support-chains field position the-chain
                                       :color (opposite-color chain-color)
                                       :old-exchange-value ev
                                       :register register)))
             (dolist (sc (bunch-chains opposite-chains))
                 (when (not (eq (chain-piece sc) piece))
                   (add-chain (chain-piece sc) sc)
                   (vector-push-extend sc (tf-subchains field)))))
           )))
    (when register (add-chain piece the-chain))
    ;(format t "the-chain ~A~%" the-chain)
    ;(format t "parent ~A~%" parent)
    ;;(print-chains-database (slot-value piece 'chains-db))
    ;;(cdb-iterate (slot-value piece 'chains-db) (subchain0-path the-chain) #'(lambda (c) (print c)))
    the-chain))


(defun print-piece-chains (position square-name)
  (let* ((square (string-to-square square-name))
         (piece (whos-at position square)))
    (list
     "<pre>"
     (with-output-to-string (s)
       (print-chains-database (slot-value piece 'chains-db)
                              :stream s
                              :test #'(lambda (c)
                                       ; (format t "~A of level ~D~%" c (chain-level c))
                                        (<= 0 (chain-level c)))))
     "</pre>")))

(defun print-position-ajax (position square-name chains-level)
  `(hunchentoot:+http-ok+
    (:content-type "text/html")
    ("<h3>"
     ,square-name
     "</h3>"
     ,@(print-piece-chains position square-name))))


(defun print-position-in-hypertext (position fen)
  "Generate HTML pages required for chains browsing in the debugging frontwnd."
  #'(lambda (env)
     ; (print "0000000000000000000000000000000000000000000000000000000")
      (let* ((qs (getf env :query-string))
             (params (quri:url-decode-params (if qs qs ""))))
        (cond
          (params ; PARAMS are set: this is an ajax request for additional data
           (let ((square-name (cdr (assoc "square" params :test #'string-equal)))
                 (chains-level (cdr (assoc "chains-level" params :test #'string-equal))))
             (print-position-ajax position square-name chains-level)))
          (t ; Show default page with the diagram
           `(hunchentoot:+http-ok+
             (:content-type "text/html")
             ,(list
               "<head>"
               "<link rel=\"stylesheet\" href=\"/chessboard/css/chessboard-0.3.0.css\"/>"
               "</head>"
               "<script src=\"/chessboard/js/json3.min.js\"></script>"
               "<script src=\"/chessboard/js/jquery-1.10.1.min.js\"></script>"
               "<script src=\"/chessboard/js/chessboard-0.3.0.js\"></script>"
               ;; Position browsing script
               "<script src=\"/js/position.js\"></script>"
               ;; Initialize the board by FEN string provided
               (format nil
                       "<script>
                         $(document).ready(function() {
                           init_board();
                           board.position('~A');
                         });
                       </script>"
                       fen)
               "<div id=\"board\" style=\"width: 400px; padding-left: 20px;\"></div>"
               "Please, move a mouse over a piece to load its chains."
               "<div id=\"data\"></div>")))))))


(defun test-chain ()
  (start-logging)
  (start-clack)
  (let ((*board* (create-board))
        (fen "8/8/N6p/P7/8/7b/8/8 w - - 0 1")
        (initial-sq #@a5@)
        (target-sq #@a8@)
        (color :white))

   #+(or)(setf fen "8/8/N6p/P7/8/7b/8/8 w - - 0 1"
               initial-sq #@e6@
               target-sq #@f1@
               color :black)

   #+(or)(setf fen "8/8/4b1p1/2Bp3p/5P1P/1pK1Pk2/8/8 b - - 0 1"
               initial-sq #@e6@
               target-sq #@f1@
               color :white)


   #+(or)(setf fen "8/8/4b1p1/2Bp3p/5P1P/1pK1Pk2/8/8 b - - 0 1"
               initial-sq #@h5@
               target-sq #@h1@
               color :black)

    (load-from-fen-string *board* fen)
    (log-message :debug "~A" (print-board *board* :stream nil))
    (let ((position (make-position *board*)))
      (do-pieces (*board* (p sq) :color color)
        (when (= sq initial-sq)
          (print (first (last (find-paths (kind p) sq target-sq 4 :color color))))
          (let* ((traject (first (last (find-paths (kind p) sq target-sq 4 :color color))))
                 (chain (make-chain traject position :type :main)))
            (format t "*-*-*-*-*--********************************~%~A~%"
                    (identical-structure-p chain chain color))
            (print (mapcar #'square-to-string traject)))))
      (dfe-add-handler "/position/" (print-position-in-hypertext position fen))))
  (format t "~%Open http://localhost:8020/ in a browser.~%~
               ========================================="))
