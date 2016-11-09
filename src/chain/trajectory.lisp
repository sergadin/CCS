;;;;
;;;; Chain trajectory
;;;;

(in-package :ccs)

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
         (len (if moves
                  (- (reduce  #'+ segments :key #'length) (length segments) -1)
                  0))
         (fields (make-array len :element-type '(or null <trajectory-field>) :initial-element nil)))
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
