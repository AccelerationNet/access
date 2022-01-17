;; -*- lisp -*-

(defsystem "access"
  :description "A library providing functions that unify data-structure access for Common Lisp: access and (setf access)"
  :licence "BSD"
  :author "Acceleration.net, Russ Tyndall, Nathan Bird, Ryan Davis"
  :version "1.6.0"
  :serial t
  :components ((:file "access")
               (:file "arg-list-manipulation"))
  :depends-on ("iterate" "closer-mop" "alexandria" "cl-ppcre")
  :in-order-to ((test-op (load-op "access/test")))
  :perform (test-op (op c) (symbol-call '#:access-test '#:run-all-tests)))

(defsystem "access/test"
  :description "Tests for the access library"
  :licence "BSD"
  :version "1.5.0"
  :author "Acceleration.net, Russ Tyndall, Nathan Bird, Ryan Davis"
  :serial t
  :components ((:module :test
                        :serial t
                        :components ((:file "access")
                                     (:file "arg-list-manipulation"))))
  :depends-on ("access" "lisp-unit2"))
