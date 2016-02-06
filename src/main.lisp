;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; $Id: main.lisp 16 2013-07-04 20:53:22Z serg $
;;;;

(in-package :ccs)

(defun start-logging (&key (log-file nil))
  (setf (log-manager)
        (make-instance 'log-manager :message-class 'formatted-message))
  (if log-file
      (progn
        (delete-file log-file)
        (start-messenger 'text-file-messenger
                         :filename log-file))
      (cl-log:start-messenger 'cl-log:text-stream-messenger
                              :stream *standard-output*
                              :category '(or :debug :info :trace))))


(defun main (&key (interface-mode :xboard) (log-file "/tmp/ccs.log"))
  ;; Инициализация системы журналирования
  (setf (log-manager)
        (make-instance 'log-manager :message-class 'formatted-message))
  (when log-file
    (delete-file log-file)
    (start-messenger 'text-file-messenger
                     :filename log-file))
  (log-message :trace "Hello")
  (case interface-mode
    (:xboard (read-xboard-commands))
    (t (error 'ccs-error :text "Неизвестный тип пользовательского интерфейса."))))



(defmethod format-message ((self formatted-message))
  (flet ((format-timestamp (timestamp)
           (let ((ut (timestamp-universal-time timestamp))
                 (fraction (timestamp-fraction timestamp)))
             (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
                 (decode-universal-time ut)
               (declare (ignore dow dst-p tz))
               (declare (ignore yr mon day))
               (concatenate 'string
                            ""
                            ;(format nil "~4,'0d-~2,'0d-~2,'0d " yr mon day)
                            (format nil "~2,'0d:~2,'0d:~2,'0d.~6,'0d" hr min sec fraction))))))
  (format nil "~a ~a ~?~&"
          (format-timestamp (message-timestamp self))
          (message-category self)
          (message-description self)
          (message-arguments self))))
