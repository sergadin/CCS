(in-package :ccs)

(defclass <chains-database-node> ()
  ((data
    :initform (cl-containers:make-container 'cl-containers:bag-container)
    :accessor cdb-node-data
    :documentation "User information assigned to this node.")
   (children
    :initform (cl-containers:make-container 'cl-containers:simple-associative-container)
    :accessor cdb-node-children
    :documentation "Map from squares to nodes.")))

(defun make-cdb-node ()
  (make-instance '<chains-database-node>))

(defclass <chains-database> ()
  ((root :type <chains-database-node>
         :reader cdb-root
         :initform (make-cdb-node))
   (search-index
    :initform (cl-containers:make-container 'cl-containers:simple-associative-container :test #'equal)
    :documentation "Map from chain's path to corresponding database node."))
  (:documentation "Store chains in a tree-like structure."))


(defun make-chains-database ()
  (make-instance '<chains-database>))


(defgeneric cdb-add (db path node-data)
  (:documentation "Add new NODE-DATA, e.g. a chain, to the database DB at the node specified by PATH."))

(defgeneric cdb-iterate (db path function)
  (:documentation "Call the FUNCTION on each chain stored in the
  database DB at the node accessed by PATH."))

(defmethod cdb-add ((db <chains-database>) path node-data)
  (loop
     :for square :in path
     :for parent = (cdb-root db) :then node
     :for node = (cl-containers:item-at (cdb-node-children parent) square)
     :do
     (when (null node)
       (setf node (make-cdb-node))
       (setf (cl-containers:item-at (cdb-node-children parent) square)
             node))
     (cl-containers:insert-item (cdb-node-data node) node-data)
     :finally
     (setf (cl-containers:item-at (slot-value db 'search-index) path)
           node)))

(defmethod cdb-iterate ((db <chains-database>) path function)
  (let ((node
         (cl-containers:item-at (slot-value db 'search-index) path)))
    (when node
      (mapc function (cl-containers:collect-items (cdb-node-data node))))))

;  (cl-containers:iterate-container (cl-containers:make-iterator


(defun print-chains-database (piece &key (stream t) (test #'(lambda (data) (declare (ignore data)) t)))
  (format stream "~&--------------- chains-database -----------------~%")
  (loop
     :with db = (piece-cdb piece)
     :with search-index = (slot-value db 'search-index)
     :for path :in (cl-containers:collect-keys search-index)
     :for node = (cl-containers:item-at search-index path)
     :for chains-at-node = (cl-containers:collect-items (cdb-node-data node) :filter test)
     :when (< 0 (count-if test chains-at-node))
     :do (format stream "~A wight ~D ~%Parent-chains:~%            ~{~A~^~%            ~}~%"
                 (mapcar #'square-to-string path)
                 ;(length chains-at-node)
                 (cdb-node-value piece path)
                 (mapcar #'(lambda (chain)
                             (list (chain-type chain)
                                   (if (chain-parent chain)
                                       (chain-parent chain)
                                       chain)))
                         chains-at-node)))
  (format stream "~&--------------- --------------- -----------------~%")
  (values))

(defun cdb-iterate-nodes (db function)
  (loop
     :with search-index = (slot-value db 'search-index)
     :for path :in (cl-containers:collect-keys search-index)
     :for node = (cl-containers:item-at search-index path)
     :do (funcall function node))
  (values))

(defun cdb-get-keys (db)
  (cl-containers:collect-keys (slot-value db 'search-index)))
