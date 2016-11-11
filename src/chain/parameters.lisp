(in-package :ccs)

(defvar +default-chain-depath+ 3
  "Default value for maximum subchain level.")

(defvar +support-chains-trashold+ 0.1
  "Доля цепей из общего числа support chains, которые считаются
  пригодными для дальнейшего анализа")

(defparameter +infinite-piece-value+ 1000 "Used as an approximation of infinity.")


(defun default-horizon (piece)
  (declare (ignore piece))
  2)
