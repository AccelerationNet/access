;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :access.system)
    (defpackage :access.system (:use :common-lisp :asdf))))

(in-package :access.system)

(defsystem :access
  :description "A library providing functions that unify data-structure access for Common Lisp:
      access and (setf access)"
  :licence "BSD"
  :author "Acceleration.net, Russ Tyndall, Nathan Bird, Ryan Davis"
  :version "1.5.0"
  :serial t
  :components ((:file "access")
               (:file "arg-list-manipulation"))
  :depends-on (:iterate :closer-mop :alexandria :anaphora :cl-interpol)
  :in-order-to ((test-op (load-op :access-test))))

(defsystem :access-test
  :description "Tests for the access library"
  :licence "BSD"
  :version "1.5.0"
  :author "Acceleration.net, Russ Tyndall, Nathan Bird, Ryan Davis"
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
