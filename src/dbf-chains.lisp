;;;; Function for displaying debugging information on positions and pieces' chains in a web browser

(in-package :ccs)

(defun print-piece-chains (position square-name)
  (let* ((square (string-to-square square-name))
         (piece (whos-at position square)))
    (list
     "<pre>"
     (with-output-to-string (s)
       (print-chains-database piece
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
  "Generate HTML pages required for chains browsing in the debugging frontend."
  #'(lambda (env)
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
