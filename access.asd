;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :access.system)
    (defpackage :access.system (:use :common-lisp :asdf))))

(in-package :access.system)

(defsystem :access
  :description "A library providing functions that unify data-structure access for Common Lisp:
      access and (setf access)"
  :licence "BSD"
  :version "0.2"
  :serial t
  :components ((:file "access")
               (:file "arg-list-manipulation"))
  :depends-on (:iterate :closer-mop :alexandria :anaphora :cl-interpol))

(defsystem :access-test
  :description "Tests for the access library"
  :licence "BSD"
  :version "0.2"
  :serial t
  :components ((:module :test
                        :serial t
                        :components ((:file "access")
                                     (:file "arg-list-manipulation"))))
  :depends-on (:access :lisp-unit2))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :access))))
  (asdf:oos 'asdf:load-op :access-test)
  (let ((*package* (find-package :access-test)))
    (eval (read-from-string "(run-all-tests)"))))
