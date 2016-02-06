;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:ccs-asd
  (:use :common-lisp :asdf))

(in-package :ccs-asd)

(asdf:defsystem :ccs
  :name "CCS"
  :version "0.1"
  :author "serg"
  :components ((:module "src"
			:components ((:file "package")
                                     (:file "specials" :depends-on ("package"))
                                     (:file "types" :depends-on ("package"))
                                     (:file "conditions")
                                     (:file "tools" :depends-on ("package" "types"))
                                     (:file "board" :depends-on ("package" "tools"))
                                     (:file "printing" :depends-on ("package" "board" "moves"))
                                     (:file "moves" :depends-on ("package" "board" "tools"))
                                     (:file "evaluate" :depends-on ("package" "board" "moves"))
                                     (:file "exchange" :depends-on ("package" "board" "moves"))
                                     (:file "chain" :depends-on ("exchange"))
                                     (:file "candidates" :depends-on ("package" "evaluate" "exchange"))
                                     (:file "play" :depends-on ("package" "board" "moves"))
                                     (:file "xboard" :depends-on("package" "board"))
                                     (:file "main" :depends-on("package" "xboard")))))
  :depends-on ("cl-utilities" "cl-ppcre" "cl-log" "cl-containers"))


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
