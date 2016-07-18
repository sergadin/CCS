(in-package :ccs)

(defun %find-support-chains (field position parent &key time-limit (color (chain-color parent))
                                                     (old-exchange-value 0))
  "Attack the FIELD by other pieces."
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
                  (exchange-value position (field-square field) (opposite-color color)))))
         #+(or)(log-message :debug "exchange value with ~A --- ~F -> ~F, ~F~%- - -- - - - - --  - -- - -- - - - - - - - - - -~%"
                            (square-to-string target-square)
                            old-exchange-value
                            new-exchange-value
                            (exchange-value position (field-square field) color))
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
     ;; Select best candidates, up to max-chains chains
     (loop
        :with ordered-candidates = (sort candidate-paths #'<
                                         :key #'(lambda (g-path)
                                                  (estimate-chain-complexity (cdr g-path)
                                                                             position)))
        :with max-chains = (ceiling (* +support-chains-trashold+ (length ordered-candidates)))
        :for (nil #|gain|# . candidate-path) :in ordered-candidates
        :and count :from 0 :to max-chains
        :collect (make-chain candidate-path
                             position
                             :parent parent
                             :type :support)))))



(defun %find-blockade-chains (field position parent &key time-limit (color (chain-color parent))
                                                    (old-exchange-value 0))
  (with-move (position (first (subchain0-path parent)) (field-square field))
    (let* ((sq (field-square field))
           (op-color (opposite-color color))
           (pieces-to-block (remove-if #'(lambda (p-sq) (eql (color (car p-sq)) color))
                                       (nth-value 2 (exchange-value position sq op-color)))))
      ;(when pieces-to-block (print pieces-to-block))
      (loop :for (p-to-block . p-square) :in pieces-to-block
         :do (print (squares-on-line p-square sq :include-frontier nil)))
      nil)))

(defun %find-pinning-chains (field position parent &key time-limit (color (chain-color parent))
                                                    (old-exchange-value 0))
  nil)


(defun find-support-chains (field position parent &key time-limit (color (chain-color parent))
                                                    (old-exchange-value 0))
  "Find subchains making FIELD passable by the PARENT chain piece."
  (assert (not (null parent)))
  ;;(log-message :trace "Searching for support chains on ~A" (square-to-string (field-square field)))
  (let ((exchange-support-chains
         (%find-support-chains field position parent :time-limit time-limit
                               :color color :old-exchange-value old-exchange-value))
        (blockade-chains
         (%find-blockade-chains field position parent :time-limit time-limit
                               :color color :old-exchange-value old-exchange-value)))
    exchange-support-chains))
