(in-package :ccs)

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
