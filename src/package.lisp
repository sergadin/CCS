(in-package :cl-user)

(defpackage :ccs
  (:use :common-lisp :cl-utilities :cl-ppcre :cl-log)
  (:shadowing-import-from
   #+openmcl-native-threads #:ccl
   #+cmu #:pcl
   #+sbcl #:sb-pcl
   #+lispworks #:hcl
   #+allegro #:mop
   #+clisp #:clos
   #:class-slots #:slot-definition-name)
  (:export
   :main))
