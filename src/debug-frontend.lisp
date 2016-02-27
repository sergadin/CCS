(in-package :ccs)

(defvar *clack-handle* nil "Instance of the running clack Web-server.")

(defvar *dfe-registered-handlers* nil
  "A mapping from URIs to handling applications.")

(defun stop-clack ()
  (prog1
      (when *clack-handle*
        (clack:stop *clack-handle*))
    (setf *clack-handle* nil)))

(defun load-static-file (env)
  (let ((path (concatenate 'string "./html" (getf env :path-info))))
    (when (open path :direction :probe)
      (list hunchentoot:+http-ok+
            '(:content-type "text/plain")
            (pathname path)))))


(defun start-clack ()
  (stop-clack)
  (setf *dfe-registered-handlers*
        (cl-containers:make-container 'cl-containers:simple-associative-container :test #'equal))
  (setf *clack-handle*
        (clack:clackup
         (lambda (env)
           (let ((app (cl-containers:item-at *dfe-registered-handlers*
                                             (getf env :path-info)))
                 (static-file (load-static-file env)))
             (cond
               (app (funcall app env))
               (static-file static-file)
               (t (list hunchentoot:+http-not-found+
                        '(:content-type "text/html")
                        (list (format nil "Hello!~%~%No handler for ~A was found.<br/>~%~
                                           Choices are:~%<ul>~{<li><a href=\"~A\">~:*~A</a></li>~%~}</ul>~%"
                                      (getf env :path-info)
                                      (cl-containers:collect-keys *dfe-registered-handlers*))
                               ""))))))
         :port 8020
         :server :hunchentoot)))


(defun dfe-add-handler (path handler)
  (setf (cl-containers:item-at *dfe-registered-handlers* path)
        handler))
