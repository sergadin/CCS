(in-package :ccs)

(defclass <chains-db-node> ()
  ((data
    :initform (cl-containers:make-container 'cl-containers:bag-container)
    :accessor cdb-node-data
    :documentation "User information assigned to this node.")
   (children
    :initform (cl-containers:make-container 'cl-containers:simple-associative-container)
    :accessor cdb-node-children
    :documentation "Map from squares to nodes.")))

(defun make-cdb-node ()
  (make-instance '<chains-db-node>))


(defclass <chains-database> ()
  ((root :type <chains-db-node>
         :accessor cdb-root
         :initform (make-cdb-node))
   (search-index
    :initform (cl-containers:make-container 'cl-containers:simple-associative-container :test #'equal)
    :documentation "Map from chain's path to corresponding database node."))
  (:documentation "Store chains in a tree-like structure."))

(defun make-chains-database ()
  (make-instance '<chains-database>))


(defgeneric cdb-add (db path chain)
  (:documentation "Add new CHAIN to the database DB to the node specified by PATH."))

(defgeneric cdb-iterate (db path function)
  (:documentation "Call the FUNCTION on each chain stored in the
  database DB at the node accessed by PATH."))

(defmethod cdb-add ((db <chains-database>) path chain)
  (loop
     :for square :in path
     :for parent = (cdb-root db) :then node
     :for node = (cl-containers:item-at (cdb-node-children parent) square)
     :do
     (when (null node)
       (setf node (make-cdb-node))
       (setf (cl-containers:item-at (cdb-node-children parent) square)
             node))
     :finally
     (setf (cl-containers:item-at (slot-value db 'search-index) path)
           node)
     (cl-containers:insert-item (cdb-node-data node) chain)))

(defmethod cdb-iterate ((db <chains-database>) path function)
  (let ((node
         (cl-containers:item-at (slot-value db 'search-index) path)))
    (when node
      (mapc function (cl-containers:collect-elements (cdb-node-data node))))))

;  (cl-containers:iterate-container (cl-containers:make-iterator


(defun print-chains-database (db &key (stream t))
  (format stream "~&--------------- chains-database -----------------~%")
  (loop
     :with search-index = (slot-value db 'search-index)
     :for path :in (cl-containers:collect-keys search-index)
     :for node = (cl-containers:item-at search-index path)
     :do (format stream "~A wight ~D~%"
                 (mapcar #'square-to-string path)
                 (length (cl-containers:collect-elements (cdb-node-data node)))))
  (format stream "~&--------------- --------------- -----------------~%")
  db)
