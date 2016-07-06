(in-package :ccs)


(defun trajectory-to-path (traject &key (test #'stop-field-p))
  (loop :for field :across traject
     :when (funcall test field)
     :collect (field-square field)))


(defun extract-paths (chain color)
  (check-type chain <chain>)
  (loop :for field :across (chain-trajectory chain)
     ;:do (format t "~%~A!!!!!!~%" (tf-subchains field))
     :nconc
     (loop :for sch :across (tf-subchains field)
        :nconc (extract-paths sch color))
     :into result
     :finally
     (return (append result (when (eq (chain-color chain) color)
                              (list (trajectory-to-path (chain-trajectory chain))))))))


(defun identical-structure-p (chain-1 chain-2 color)
  (let ((path-1 (extract-paths chain-1 color))
        (path-2 (extract-paths chain-2 color)))
    (format t "++++++++ ~A ~%++++++++ ~A~%" path-1 path-2)
    (and (every #'(lambda (p) (find p path-1 :test #'equal)) path-2)
         (every #'(lambda (p) (find p path-2 :test #'equal)) path-1))))
