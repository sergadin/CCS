(in-package :ccs)

(defun chain-color (chain)
  (color (chain-piece chain)))


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


(defun find-escape-chains (field position parent)
  "Field clearance."
  #+(or)(log-message :trace "Searching for escape chain from ~A. Level=~D"
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
                (print (make-chain path position :parent parent
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

#+(or)(defun find-support-chains (field position parent &key time-limit (color (chain-color parent))
                                                    (old-exchange-value 0))
  "Find subchains making FIELD passable by the PARENT chain piece."
  (assert (not (null parent)))
  ;;(log-message :trace "Searching for support chains on ~A" (square-to-string (field-square field)))
  ;; Attack the field by other pieces
  (do-pieces (position (piece square) :color color)
    ;; ?.. TODO: what about pieces from parent's chain?
    (let ((targets (pre-moves piece (field-square field) :attack-only t))
          (horizon (or time-limit (default-horizon piece)))
          (candidate-paths)
          (new-exchange-value))
      ;; Find all empty squares where PIECE can participate in the exchange on FIELD.
      (dolist (target-square targets)
        (setf new-exchange-value
              (with-move (position square target-square)
                (with-move (position (first (subchain0-path parent)) (field-square field))
                  ;; (log-message :debug "~A" position)
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
             (push (cons (- old-exchange-value new-exchange-value) path)
                   candidate-paths))))

     #+(or) (log-message :debug "Found ~D candidate ~A paths for ~A~A support on ~A (~F -> ~F)"
                   (length candidate-paths)
                   color
                   (piece-to-name (kind piece))
                   (square-to-string square)
                   (square-to-string (field-square field))
                   old-exchange-value
                   new-exchange-value)
      ;;--- TODO: Make opponents moves impossible due to absolute or relative pin.
      ;;--- TODO: прогнать атакующих, перекрыть траекторию атаки и проч.
      (when candidate-paths
          (let ((ordered-candidates
                 (sort candidate-paths #'<
                       :key #'(lambda (g-path)
                                (estimate-chain-complexity (cdr g-path) position)))))

            (loop :for (gain . candidate-path) :in ordered-candidates
               :and count :from 0 :to (ceiling (* +support-chains-trashold+
                                                  (length ordered-candidates)))
               :for chain = (make-chain candidate-path
                                        position
                                        :parent parent
                                        :type :support)
               :collect chain))))))


(defmethod print-object ((obj <chain>) out)
  (print-unreadable-object (obj out :type nil :identity nil)
    (format out " CHAIN [~A ~A L~D ~A] ~{~A~^ -> ~}"
            (chain-color obj)
            (piece-to-name (kind (chain-piece obj)))
            (chain-level obj)
            (chain-type obj)
            (mapcar #'square-to-string (subchain0-path obj)))))


(defun chain-feasibility (chain) ;; returns value from [0, 1]
 "Return value from [0, 1], where 1 - totally feasible chain, 0 - unfeasible."
 (return-from chain-feasibility 1.0d0)
 (* (/ 2 Pi)
    (atan  ;; TODO подобрать функцию
     (estimate-chain-complexity (subchain0-path chain) (chain-position chain)))))

(defun chain-target (chain) ;returns <target> obj for chain
  (with-accessors ((traject chain-trajectory)
                   (position chain-position))
      chain
    (let ((last-field (1- (array-dimension traject 0))))
      (whos-at position (field-square (aref traject last-field))))))


(defun chain-value (chain)
  (let ((target (chain-target chain)))       ;; returns value for white
    (if target (* -1 (piece-value target))
        (if (eql :white (chain-color chain))
            8.0d0
            -8.0d0)))) ;; TODO target-value ;;updated 11/11/2016

(defun chain-danger (chain)
  (* (chain-feasibility chain) (chain-value chain)))


(defun cdb-node-value (piece path)
  (let ((result 0.0d0))
    (cdb-iterate (piece-cdb piece) path
                 #'(lambda (chain)
                     (when (eql (chain-color chain) (color piece))
                       (incf result (chain-danger chain)))))
    result))

(defun add-trajectory-as-subchain (parent field piece path subchain-type)
  (let* ((position (chain-position parent))
         (trajectory (make-trajectory path))
         (level (if parent (+ 1 (chain-level parent)) 0))
         (the-chain (make-instance '<chain>
                                   :piece piece
                                   :trajectory trajectory
                                   :level level
                                   :parent parent
                                   :position position
                                   :type subchain-type)))
    (vector-push-extend the-chain (tf-subchains field))))


(defun make-chain (path position
                   &key
                     (parent nil)
                     (type :main)
                     (max-level +default-chain-depath+))
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
       :for square = (field-square field)
       :for piece-at-field = (whos-at t-position (field-square field))
       :while (< level max-level) ; do not create extremely nested subchains
       :do

       ;; TODO связанные фигуры.

       (when piece-at-field ; the field is occupied by a piece
         (cond
           ((eq (color piece-at-field) chain-color)
            ;; remove our piece from the route
            ;;add escape-chains to chains-list of piece-at-field
            (loop :for esc :in (find-escape-chains field board the-chain)
               :do (vector-push-extend esc (tf-subchains field))))

           ((and (not (eq (color piece-at-field) chain-color))
                 (eq (field-type field) :internal-field))
            ;; opponent's piece in the middle: escape, protect or do nothing
            (setf (field-type field) :extra-stop-field)
            nil)))

       (when (stop-field-p field :strict nil)
         (let ((ev (if (piece-of-color piece-at-field chain-color)
                       0 ; pieces of the same color do not provide any gain to exchange value
                       (piece-value piece-at-field :color chain-color)))) ; exchange-value
           (with-move (t-position sq square)
             (multiple-value-bind (val board-after-exchange pieces)
                 (exchange-value t-position square (opposite-color chain-color))
               (incf ev val)
               (loop :for (p . p-square) :in pieces
                  :do (add-trajectory-as-subchain
           ;            the-chain field p (list p-square square) :support))))
                       the-chain (print field) p nil :support))))
           ;;(log-message :trace "Exchange value on ~A ~F" (square-to-string square) ev)
           (when (not (exchange-positive-p ev chain-color))
             ;; Can't move to that square due to negative exchange value. Find support chains.
             (let ((support-chains
                    (find-support-chains field position the-chain
                                         :old-exchange-value ev)))
               (dolist (sc support-chains)
                 (when (not (eq (chain-piece sc) piece))
                   (vector-push-extend sc (tf-subchains field))))))
           ;; Find opposite-chains
           (dolist (sc (find-support-chains field position the-chain
                                            :color (opposite-color chain-color)
                                            :old-exchange-value ev))
             (when (not (eq (chain-piece sc) piece))
               (vector-push-extend sc (tf-subchains field)))))))
    (add-chain piece the-chain)
    the-chain))


(defun test-chain ()
  (start-logging)
  (start-clack)
  (let ((*board* (create-board))
        (fen "8/8/N6p/P7/8/7b/5b2/8 w - - 0 1")
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
                 (traject2 (first (last (find-paths :bishop #@h3@ #@a6@ 2 :color :black))))
                 (chain (make-chain traject position :type :main))
                 (chain2 (make-chain traject2 position :type :main)))
            (format t "*-*-*-*-*--********************************~%~A~%"
                    (identical-structure-p chain chain2 color))
            (print (mapcar #'square-to-string traject)))))
      (format t "~A~%" (length (position-chains position)))
      (dfe-add-handler "/position/" (print-position-in-hypertext position fen))))
  (format t "~%Open http://localhost:8020/ in a browser.~%~
               ========================================="))


(defun test-position (&optional (input-fen  "6k1/5ppp/3r4/8/8/3B4/8/3R2K1 w - - 0 1"))
  (start-logging)
  (start-clack)
  (let ((*board* (create-board))
        (fen input-fen))
    (load-from-fen-string *board* fen)
    (log-message :debug "~A" (print-board *board* :stream nil))
    (let ((position (make-position *board*)))
      (dfe-add-handler "/position/" (print-position-in-hypertext position fen))))
  (format t "~%Open http://localhost:8020/ in a browser.~%~
               ========================================="))
