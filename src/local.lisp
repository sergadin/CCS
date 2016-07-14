(in-package :ccs)

(defun count-chains ()
  (start-logging)
  (let ((*board* (create-board))
        (initial-sq #@e6@) ; #@h5@)
        (target-sq #@e2@) ; #@h1@)
        (color :black)
        (count 0))
    (load-from-fen-string *board* "r3r1k1/p4pb1/2NB2np/3N1b1q/2PP1pnP/1Q2p1P1/P3P1B1/R2R2K1 b - h3 0 23")
    (log-message :debug "~A" (print-board *board* :stream nil))
    (dolist (color '(:black :white))
      (do-pieces (*board* (p sq) :color color)
        (do-pieces (*board* (targen-piece target-sq) :color (opposite-color color))
          (incf count (length (find-paths (kind p) sq target-sq 4 :color color)))))
      (log-message :debug "~A trajectories for ~A" count color)
      (setf count 0))))


(defun tdb-test ()
  (let ((tdb (cl-containers:make-container 'cl-containers:associative-container)))
    (setf (cl-containers:item-at tdb 1 2 0) 1)
    (setf (cl-containers:item-at tdb 1 2 3 ) 2)
    (cl-containers:item-at tdb 1 2 3 0)))

(defun tdb-performance ()
  (let ((tdb (treedb:make-alist-treedb))
        (cont-db (cl-containers:make-container 'cl-containers:associative-container)))
    (loop :for i :from 1 :to 100000
       :for k = (random 10000)
       :for level-1 = (mod k 67)
       :and level-2 = (mod (* k k) 29)
       :and level-3 = (mod (* (+ k 4) k) 17)
       :do
       (setf (treedb:node tdb level-1 level-2 level-3) 1)
       (setf (cl-containers:item-at cont-db level-1 level-2 level-3) 1))
    (time
     (loop :for i :from 1 :to 100000
        :for k = (random 10000)
        :for level-1 = (mod k 67)
        :and level-2 = (mod (* k k) 29)
        :and level-3 = (mod (* (+ k 4) k) 17)
        :do
        ;;(treedb:node tdb level-1 level-2 level-3)
        (cl-containers:item-at cont-db level-1 level-2 level-3)
        ))))

(defun containers-test ()
  (let ((db
         (cl-containers:make-container 'cl-containers:associative-container)))
    (setf (cl-containers:item-at db 1 2) "abc")
    (setf (cl-containers:item-at db 1 2 3) "zzz")
    (cl-containers:collect-keys db)
    (cl-containers:item-at db 1 2)))
