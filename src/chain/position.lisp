(in-package :ccs)

(defun position<-board (board)
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

(defun search-targets (piece)
  (let ((targets))
    (with-accessors ((pos piece-position)) piece
      (do-pieces (pos (target sq) :color (opposite-color (color piece)))
        (push target targets))
      targets)))

(defun make-piece-chains-to-target (piece target &key (horizon (default-horizon piece)))
  (let ((sq (piece-square piece))
        (target-sq (piece-square target))
        (position (piece-position piece)))
    (loop :for path :in (find-paths (kind piece) sq target-sq horizon :color (color piece))
       ;;:do (print "AAAAAAAAAAAAAAAAAAAAAAA")
       :when (<= (- (estimate-chain-complexity path position) horizon) 3)
       :collect (make-chain path position))))

(defun make-piece-chains (piece)
  (loop :for target :in (search-targets piece)
     ;:do (print (list target (make-piece-chains-to-target piece target)))
     :nconc (make-piece-chains-to-target piece target)))


(defun make-position (board)
  (let ((pos (position<-board board)))
    (with-accessors ((chains position-chains)) pos
      (do-pieces (pos (piece sq))
        (setf chains (nconc chains (make-piece-chains piece)))))
    pos))

(defun copy-position (position)
  (shallow-copy-object position))

(defmethod print-object ((obj <position>) out)
  ;(print-unreadable-object (obj out :type t)
  ;  (format out "~s" (foo-name obj))))
  (format out "~%")
  (print-diagram obj :stream out))
