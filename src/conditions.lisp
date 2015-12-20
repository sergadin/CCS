;;;;
;;;; Possible conditions
;;;;

(define-condition ccs-error (error)
 ((text :initarg :text :reader text)
  (code :initform 0 :initarg :code :reader code)))
