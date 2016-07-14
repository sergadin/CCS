;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)


#+or(let ((translation (concatenate 'string
                                (directory-namestring *load-pathname*)
                                "**/*.*")))
  (setf (logical-pathname-translations "ccs")
        `(("code;**;*.*.*" ,translation)
          ("**;*.*.*" "**/*.*"))))


(defpackage #:ccs-asd
  (:use :common-lisp :asdf))

(in-package :ccs-asd)

(asdf:defsystem "ccs"
  :name "CCS"
  :version "0.1"
  :author "serg"
  ;;:pathname #P"ccs:code;"
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "specials")
               (:file "types")
               (:file "conditions")
               (:file "tools")
               (:file "board")
               (:file "printing")
               (:file "moves")
               (:file "evaluate")
               (:file "exchange")
               (:file "debug-frontend")
               (:module "chain"
                        :serial t
                        :pathname "chain"
                        :components ((:file "parameters")
                                     (:file "classes")
                                     (:file "trajectory")
                                     (:file "chain")
                                     (:file "support")
                                     (:file "position")
                                     (:file "db")
                                     (:file "graph")))
               (:file "dbf-chains")
               (:file "candidates")
               (:file "play")
               (:file "xboard")
               (:file "main"))
  :depends-on ("cl-utilities" "cl-ppcre" "cl-log" "cl-containers" "dynamic-mixins"
                              "fare-memoization"
                              "hunchentoot" "clack" "quri"))


(asdf:defsystem :ccs-test
  :name "CCS"
  :version "0.1"
  :author "serg"
  :depends-on (:ccs :lift)
  :components ((:module "test"
                        :components ((:file "package")
                                     (:file "moves" :depends-on ("package"))
                                     (:file "exchange" :depends-on ("package"))
                                     ;;(:file "tactics" :depends-on ("package"))
                                     ))))
