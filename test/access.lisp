(cl:defpackage :access-test
  (:use :cl :cl-user :iterate :access :lisp-unit)
  (:shadowing-import-from :alexandria #:ensure-list )
  (:shadowing-import-from :anaphora #:awhen #:aif #:it)
  (:export ))

;; for a specific test
(cl:defpackage :access-test-other
  (:use :cl :cl-user :iterate :access :lisp-unit)
  (:export ))

(in-package :access-test)

(enable-dot-syntax)

(defun run-all-tests ()
  (let ((lisp-unit:*print-errors* t)
        (lisp-unit:*print-failures* t)
        (lisp-unit:*print-summary* t))
    (run-tests :all)))


(defparameter +al+ `((:one . 1) ("two" . 2) ("three" . 3) (four . 4) (:5 . 5)))
(defparameter +pl+ (list :one 1 "two" 2 "three" 3 'four 4 :5 5))
(defparameter +ht+ (alexandria::plist-hash-table (list "one" 1 "two" 2 "three" 3 "four" 4 "5" 5)
						   :test 'equalp))

(defclass access-test ()
  ((one :accessor one :initarg :one :initform 1)
   (two :accessor two :initarg :two :initform 2)
   (three :accessor three :initarg :three :initform 3)
   (four :initarg :four :initform 4)
   (five :initarg :five :initform 5)
   (null-slot :initarg :null-slot :initform ())
   (pl :initarg :pl :initform (copy-list +pl+) :accessor pl)))

(defun make-obj () (make-instance 'access-test))

(define-test access-basic
  (let ((o (make-obj)))
    (assert-equal 5 (access +al+ 'length))
    (assert-equal 3 (access +al+ 'three))
    (assert-equal 3 (access +pl+ 'three))
    (assert-equal 3 (access o 'three))
    (assert-equal 3 (access +ht+ 'three))
    (assert-equal
        (list "5" "four" "three" "two" "one")
        (access +ht+ 'alexandria:hash-table-keys))
    (assert-equal 3 (accesses o 'pl 'three ))))

(define-test test-with-access
  (let ((o (make-obj)))
    (with-access (one two (my-three three))
        o
      (assert-equal 1 one)
      (assert-equal 2 two)
      (assert-equal 3 my-three)
      (setf my-three 33)
      (assert-equal 33 my-three)
      (assert-equal 33 (access o 'three))
      (setf my-three 3)
      )))

(define-test access-and-setting-alist
  (let ((al (copy-alist +al+)))
    (assert-equal 3 (access al 'three) "inited correctly")
    (setf (access al 'three) 333)
    (assert-equal 333 (access al 'three) "first set")
    (setf (access al 'three) 3)
    (assert-equal nil (access al 'sixteen)  "key missing")
    (setf (access al 'sixteen) 16)
    (assert-equal 16 (access al 'sixteen) "new key set"))
  )

(define-test access-and-setting-plist
  (let ((pl (copy-list +pl+)))
    (assert-equal 3 (access pl 'three))
    (setf (access pl 'three) 333)
    (assert-equal 333 (access pl 'three))
    (assert-equal nil (access pl 'sixteen))
    (setf (access pl 'sixteen) 16)
    (assert-equal 16 (access pl 'sixteen))))

(define-test access-and-setting-hashtable
  (let ((+ht+ (alexandria:copy-hash-table +ht+) ))
    (assert-equal 3 (access +ht+ 'three))
    (setf (access +ht+ 'three) 333)
    (assert-equal 333 (access +ht+ 'three))
    (assert-equal 333 (access +ht+ "three"))
    (setf (access +ht+ 'three) 3)
    (assert-equal nil (access +ht+ 'sixteen))
    (setf (access +ht+ 'sixteen) 16)
    (assert-equal 16 (access +ht+ 'sixteen))
    (assert-equal 16 (access +ht+ "sixteen"))
    (remhash "sixteen" +ht+)))

(define-test access-and-setting-object
  (let ((o (make-obj)))
    (assert-equal nil (access o 'null-slot))
    (setf (accesses o 'null-slot 'not-a-fn) 'any-more)
    (assert-equal 'any-more (accesses o 'null-slot 'not-a-fn))
    (assert-equal 1 (access o 'one))
    (assert-equal 4 (access o 'four))
    (setf (access o 'four) 444
          (access o 'one) 111)
    (assert-equal 111 (access o 'one))
    (assert-equal 444 (access o 'four))
    (setf (access o 'four) 4
          (access o 'one) 1)
    (assert-equal nil (access o 'nothing))
    (setf (access o 'nothing) 10000)
    (assert-equal nil (access o 'nothing))))

(define-test setting-object-attributes
  (let ((o (make-obj)))
    (assert-equal 1 (accesses o 'pl :one) o (pl o))
    (setf (accesses o 'pl :one) 111)
    (assert-equal 111 (accesses o 'pl :one)  o (pl o))
    (setf (accesses o 'pl :one) 1)
    (assert-equal 1 (accesses o 'pl :one)  o (pl o))
    (assert-equal nil (accesses o 'pl :twenty)  o (pl o))
    (setf (accesses o 'pl :twenty) 20)
    (assert-equal 20 (accesses o 'pl :twenty)  o (pl o))
    (setf (accesses o 'pl :twenty) nil)
    (assert-equal nil (accesses o 'pl :twenty)  o (pl o))
    ))


(define-test dot-basic
  (let ((o (make-obj)))
    (with-dot ()
      (assert-equal 5 +al+.length)
      (assert-equal 3 +al+.three)
      (assert-equal 3 +pl+.three)
      (assert-equal 3 o.three)
      (assert-equal 3 o.pl.three))))

(define-test dot-set
  (let ((o (make-obj)))
    (with-dot ()
      (assert-equal 5 +al+.length)
      (assert-equal 3 +al+.three)
      (setf +al+.three 333)
      (assert-equal 333 +al+.three)
      (setf +al+.three 3)
      (assert-equal 3 o.pl.three)
      (setf o.pl.three 333)
      (assert-equal 333 o.pl.three)
      (setf o.pl.three 3)
      )))

(define-test dot-basic-reader
  (let ((o (make-obj)))
    (assert-equal 5 #D+al+.length)
    (assert-equal 3 #D+al+.three)
    (assert-equal 3 #D+pl+.three)
    (assert-equal 3 #Do.three)
    (assert-equal 3 #Do.pl.three)
    #D(let ((l (list 1 2 3 4 5)))
        (assert-equal 5 l.length)
        (assert-equal 5 +al+.length)
        (assert-equal 3 +al+.three)
        (assert-equal 3 +pl+.three)
        (assert-equal 3 o.three)
        (assert-equal 3 o.pl.three))))

(define-test dot-iteration
  (with-dot ()
    (iter (for (k v . rest) on (list :pl1 +pl+ :pl2 +pl+) by #'cddr)
	  (when (first-iteration-p)
	    (assert-equal 10 rest.pl2.length)
	    (assert-equal 4 rest.pl2.four))
	  (assert-equal 4 v.four))))

(defclass multi-package-test-obj ()
  ((my-slot :accessor my-slot :initarg :my-slot :initform nil)
   (access-test-other::my-slot :accessor access-test-other::my-slot
                               :initarg :my-slot :initform nil))
  (:documentation "Do you hate sanity?"))

(define-test has-slot
  (let ((o (make-instance 'multi-package-test-obj)))
    (assert-eql 'my-slot (has-slot? o 'my-slot))
    ;; seems like this *could be* implementation dependent based on the ordering returned from
    ;; the mop... Lets hope for the sanest (eg first listed)
    (assert-eql 'my-slot (has-slot? o :my-slot))
    (let ( warned? )
      (handler-case (has-slot? o :my-slot)
        (access-warning (c) (declare (ignore c))
          (setf warned? t)))
      (assert-true warned? "We got a warning for multi-slot-matches"))
    (assert-eql 'access-test-other::my-slot (has-slot? o 'access-test-other::my-slot))))

(defclass accessed-object ()
  ((my-slot :initarg :my-slot :initform nil)
   (no-access :initarg :no-access :initform nil)
   (call-number :accessor call-number :initarg :call-number :initform 0)))

(defmethod my-slot ((o accessed-object))
  (incf (call-number o))
  (slot-value o 'my-slot))

(defmethod (setf my-slot) (new (o accessed-object))
  (incf (call-number o))
  (setf (slot-value o 'my-slot) new ))

(define-test ensure-called-when-you-can
  (let ((o (make-instance 'accessed-object)))
    (setf (access o :my-slot) :test)
    (assert-eql 1 (call-number o))
    (assert-eql :test (access o :my-slot))
    (assert-eql 2 (call-number o))

    ;; check that accessorless slots still work correctly
    (setf (access o :no-access) :test2)
    (assert-eql :test2 (access o :no-access) :slot-access-by-name-failed)

    ;; check that accessorless slots still work correctly
    (setf (access o 'no-access) :test3)
    (assert-eql :test3 (access o 'no-access) :slot-access-failed)
    ))

