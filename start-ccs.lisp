#!/usr/local/bin/sbcl --script
;;--noinform --noinform --noprint --no-sysinit --no-userinit --disable-debugger

;; Start swank

(require 'asdf)
(setf asdf:*central-registry*
   ;; Default directories, usually just the ``current directory''
  '(*default-pathname-defaults*
    ;; Additional places where ASDF can find
    ;; system definition files
    #p"/home/serg/work/ccs/trunk/"))


;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;(asdf:disable-output-translations)


(let ((*standard-output* (make-broadcast-stream)))
  ;(asdf:oos 'asdf:load-op 'ccs)
  (ql:quickload :ccs))

;(asdf:oos 'asdf:load-op 'ccs)
;(setf swank:*globally-redirect-io* t)


;; https://github.com/l0stman/Hunchentoot-daemon/blob/master/startup.lisp

(defparameter *swank-port* 4005)
(defparameter *swank-server* nil)

#+sbcl
(defun sigterm-handler (sig code scp)
  (declare (ignore sig code scp))
  ;; Shut down Swank and anyone else by terminating all threads
  (dolist (thread (sb-thread:list-all-threads))
    (unless (equal sb-thread:*current-thread* thread)
      (sb-thread:terminate-thread thread)))
  (sleep 1)
  (sb-ext:exit))

#|
(defun start-swank ()
  "Start a Swank server for SLIME."
  (let ((swank::*loopback-interface* "localhost"))
   (setf *swank-server*
    (swank:create-server :style :spawn
                         :port *swank-port*
                         :dont-close t)))
  (format *standard-output* "Swank server started on port ~S~%" *swank-port*))
|#

(defvar *ccs-log-file* "/tmp/ccs.log")
(progn
  #+windows (setf *ccs-log-file* nil)) ;; disable logging


(defun run-ccs ()
  (setf *random-state* (make-random-state t))
  ;; this initializes the global random state by
  ;; "some means" (e.g. current time.)
  ;;(ccs:read-xboard-commands))
  (ccs:main :interface-mode :xboard :log-file *ccs-log-file*))
     

#+sbcl
(sb-sys:enable-interrupt sb-unix:sigterm #'sigterm-handler)
;(let ((*standard-output* (make-broadcast-stream)))
;  (start-swank))
(run-ccs)
(quit)
