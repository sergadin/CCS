(in-package :ccs)

(defclass <target> ()
  (); no slots
  (:documentation "Abstract base class representing chain's target."))

(defclass <piece> (piece <target>)
  ((position :initarg :position
             :accessor piece-position)
   (square :type square
           :accessor piece-square
           :initarg :square)
   (chains-db :initform (make-chains-database)
              :accessor piece-cdb
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
  ((chains :accessor position-chains :initform nil)))


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

(defun make-bunch (chain-or-chains)
  "Make a `bunch-of-chains' from a list of chains or a chain."
  (etypecase chain-or-chains
    (<chain> (make-bunch (list chain-or-chains)))
    (list (make-instance '<bunch-of-chains> :chains chain-or-chains))))
     ;(let ((bunch (make-instance '<bunch-of-chains> :chains chain-or-chains)))
     ;       (loop :for ch :in chain-or-chains :do ( (chain-bunch ))


(defclass <cdb-node-data> ()
  ((chain :type <chain>
          :documentation "The chain.")
   (bunch :type (or <bunch-of-chains> null)
          :documentation "The bunch to which the chain belongs.")
   (selected :type (or t null)
             :documentation "T, if this subchain was selected for play in the CHAIN's root."))
  (:documentation "Data structure assigned to a node of the `piece''s chains database."))
