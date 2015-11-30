(cl:defpackage :access
  (:use :cl :cl-user :iterate)
  (:shadowing-import-from :alexandria #:ensure-list )
  (:shadowing-import-from :anaphora #:awhen #:aif #:it)
  (:export
   #:access-warning
   ;; utils to make this work
   #:has-reader?
   #:has-writer?
   #:has-slot?
   #:get-slot-value
   #:ensure-slot-name
   #:class-of-object
   #:class-slot-by-name
   #:class-slots
   #:class-direct-slots
   #:class-direct-slot-names
   #:class-direct-slot-readers
   #:class-direct-slot-writers
   #:class-slot-names
   #:class-slot-definitions
   #:class-slot-readers
   #:class-slot-writers
   #:equalper
   #:plist-val
   #:rem-plist-val
   #:rem-plist-val!
   #:set-plist-val
   #:set-plist-val!
   #:call-if-applicable
   #:call-applicable-fns

   ;; main stuff
   #:access
   #:accesses
   #:do-access
   #:set-access
   #:set-accesses
   #:do-set-access
   #:access-copy
   #:mutate-access
   #:with-access
   #:with-all-slot-accessors
   #:with-access-values
   #:with-all-slot-access-values

   ;; dot syntax stuff
   #:with-dot
   #:enable-dot-syntax
   #:disable-dot-syntax
   #:split-dot-sym

   ;; arg-list-manip
   #:arg-list-key-value
   #:set-arg-list-key-value
   #:set-arg-list-key-value!
   #:rem-arg-list-key-value
   #:rem-arg-list-key-value!
   #:ensure-arg-list-key-value
   #:ensure-arg-list-key-value!
   ))

(in-package :access)

(define-condition access-condition (simple-condition)
  ((format-control :accessor format-control :initarg :format-control :initform nil)
   (format-args :accessor format-args :initarg :format-args :initform nil)
   (original-error :accessor original-error :initarg :original-error :initform nil))
  (:report (lambda (c s)
	     (apply #'format
	      s
	      (format-control c)
	      (format-args c)))))

(define-condition access-warning (warning access-condition) ())

(defun access-warn (message &rest args)
  (warn (make-condition 'access-warning
			:format-control message
			:format-args args)))

(defun equalper (x y)
  "compares symbols by equalp symbol-name"
  (flet ((cast (it)
	   (typecase it
	     (symbol (string it))
	     (t it))))
    (or (eql x y)
	(equalp (cast x) (cast y)))))

(defvar *default-test* #'access:equalper)
(defvar *default-key* #'identity)

(defun default-test () (or *default-test* #'access:equalper))
(defun default-key () (or *default-key* #'identity))

(defgeneric plist-val (id list &key test key)
  (:documentation "get a value out of a plist based on its key")
  (:method (id list &key (test (default-test)) (key (default-key)))
    (iter (for (k v) on list by #'cddr)
      (for found = (funcall (or test (default-test))
                            (funcall (or key (default-key)) k)
                            id))
      (when found
          (return-from plist-val (values v found))))))

(defgeneric rem-plist-val (id list &key test key)
  (:documentation
   "removes key & its value from plist returning
   (values plist (list-of-values-removed))")
  (:method (id list &key (test (default-test)) (key (default-key)))
    (iter
      (for (k v) on list by #'cddr)
      (cond ((funcall (or test (default-test))
                      (funcall (or key (default-key)) k)
                      id)
             (collect v into removed))
            (T (collect k into plist)
               (collect v into plist)))
      (finally (return (values plist removed))))))

(defmacro rem-plist-val! (id place &key (test '(default-test)) (key '(default-key)))
  `(setf ,place
    (rem-plist-val ,id ,place :test ,test :key ,key)))

(defgeneric set-plist-val (new id list &key test key)
  (:documentation "If a key exists in the plist, set its value, otherwise add
  this key to the dictionary")
  (:method (new id list &key (test (default-test)) (key (default-key)))
    (iter
      (with collected)
      (for (k v) on list by #'cddr)
      (collect k into res)
      (if (funcall (or test (default-test))
                   (funcall (or key (default-key)) k)
                   id)
          (progn (setf collected T)
                 (collect new into res))
          (collect v into res))
      (finally
       (unless collected
         (setf res (list* id new res)))
       (return res)))))

(defmacro set-plist-val! (new id place
                          &key (test '(default-test)) (key '(default-key)))
  `(setf ,place
    (set-plist-val ,new ,id ,place :test ,test :key ,key)))

(defun %slot-readers (slots)
  (iter (for slot in (ensure-list slots))
    (for reader-name =
         (or
          ;; ESD's might not have this slot apparently
          (and
           (typep slot 'closer-mop:direct-slot-definition)
           (first (closer-mop::slot-definition-readers slot)))
          ;; NB: We should probably check for a reader fn here before assuming
          ;; the slot name is a reader-fn, but I couldnt find a cross platform way
          ;; of doing this
          (closer-mop:slot-definition-name slot)))
    (collecting reader-name into names)
    ;; some valid slot names are not valid function names (see type)
    (collecting (ignore-errors
                 (symbol-function reader-name)) into readers)
    (finally (return (values readers names)))))

(defun %slot-writers (slots)
  (iter (for slot in (ensure-list slots))
	(for sn = (closer-mop::slot-definition-name slot))
	;; effective slots dont have readers or writers
	;; but direct slots do, no idea why, I asked and its in the spec
    (for wn = (or (and (typep slot 'closer-mop:direct-slot-definition)
                       (first (closer-mop::slot-definition-writers slot)))
                  `(setf ,sn)))
	(collecting wn into writer-names)
	(collecting sn into slot-names)
	;; some valid slot names are not valid function names (see type)
	(collecting (ignore-errors (fdefinition wn)) into writers)
	(finally (return (values writers writer-names slot-names)))))

(defun class-of-object ( o )
  "returns the class of the object/symbol (or itself if it is a class),
   if passed a list returns a list of these results"
  (typecase o
    (keyword nil)
    (symbol (find-class o))
    (standard-class o)
    (condition (class-of o))
    (standard-object (class-of o))))

(defun appended (fn lst)
  "Mapcan caused all sorts of trouble with its NCONCing"
  (iter (for o in lst)
    (appending (funcall fn o))))

(defgeneric class-slots (o)
  (:documentation "returns the slots for the class/obj or list of class/obj passed in")
  (:method (o)
    (typecase o
      (list (appended #'access:class-slots o))
      (t
       (awhen (class-of-object o)
         (closer-mop:ensure-finalized it)
         (closer-mop:class-slots it))))))

(defun class-slot-definitions (o)
  (class-slots o))

(defun class-direct-slot-readers ( o )
  "ensures o is a class (or list thereof) and returns all the direct slot reader functions)"
  (typecase o
    (list (appended #'access:class-direct-slot-readers o))
    (t
     (awhen (class-of-object o)
       (%slot-readers (closer-mop:class-direct-slots it ))))))

(defun class-slot-readers ( o )
  (typecase o
    (list (appended #'access:class-slot-readers o))
    (t
     (awhen (class-of-object o)
       (%slot-readers (closer-mop:class-slots it))))))

(defun class-direct-slot-writers (o)
  (typecase o
    (list (appended #'access:class-direct-slot-writers o))
    (t
     (awhen (class-of-object o)
       (%slot-writers (closer-mop:class-direct-slots it))))))

(defun class-slot-writers (o)
  (typecase o
    (list (appended #'access:class-slot-writers o))
    (T
     (awhen (class-of-object o)
       (%slot-writers (closer-mop:class-slots it))))))

(defun class-direct-slots (o)
  (typecase o
    (list (appended #'access:class-direct-slots o))
    (t
     (awhen (class-of-object o)
       (closer-mop:ensure-finalized it)
       (closer-mop:class-direct-slots it)))))

(defun class-direct-slot-names (o)
  (mapcar
   #'closer-mop:slot-definition-name
   (access:class-direct-slots o)))

(defun class-slot-names (o)
  (mapcar
   #'closer-mop:slot-definition-name
   (access:class-slots o)))

(defun class-slot-by-name (o k &key (test (default-test)) )
  (iter (for s in (access:class-slots o))
    (for name = (ensure-slot-name s))
    (when (funcall (or test (default-test))
                   k name)
      (return (values s name)))))

(defun ensure-slot-name (sn)
  (typecase sn
    (list (mapcar #'ensure-slot-name sn))
    ((or symbol string) sn)
    (closer-mop:slot-definition
     (closer-mop:slot-definition-name sn))))

(defun has-reader? (o reader-name
                    &aux (match (ensure-slot-name reader-name)))
  "For o, does a reader function exist for it"
  (when (and o reader-name (typep o 'standard-object))
    (multiple-value-bind (readers names) (class-slot-readers o)
      (iter (for reader in readers)
        (for name in names)
        (when (typecase reader-name
                (function (eql reader reader-name))
                ((or symbol keyword string closer-mop:slot-definition)
                 (equalper name match))
                ((or list number) nil) ;; compound-keys , array indexes
                (T (access-warn "Not sure how to ~S maps to a function" reader-name)))
          (return (values reader name)))))))

(defun has-writer? (o writer-name
                    &aux (match (ensure-slot-name writer-name)))
  "For o, does a writer function exist for it?"
  (when (and o writer-name (typep o 'standard-object))
    (multiple-value-bind (writers wns sns) (class-slot-writers o)
      (or
       (iter (for writer in writers)
         (for wn in wns)
         (for sn in sns)
         (when (typecase writer-name
                 (function (eql writer writer-name))
                 (string
                  (or (equalper sn match) ;; handle "slot-name" matches
                      ;; handle "(setf slot-name)" matches
                      (equalper (princ-to-string wn) match)))
                 ((or symbol keyword closer-mop:slot-definition)
                  (or (equalper sn match) ;; handle slot-name matches
                      (equalper wn match) ;; handle (setf slot-name) matches
                      ))
                 (list ;; exact list match
                  (equal wn writer-name))
                 (T (access-warn "Not sure how to ~S maps to a function" writer-name)))
           (return (values writer wn sn))))
       ;; setf-form ;; try again with just the slotname
       (when (listp writer-name)
         (has-writer? o (second writer-name)))))))

(defun get-slot-value (o sn)
  "like slot-value but without boundedness errors and works with slot definitions"
  (setf sn (ensure-slot-name sn))
  (and (slot-boundp o sn) (slot-value o sn)))

(defun has-slot? (o slot-name &key (lax? t))
  "Does o have a slot named slot-name

   if lax? we will ignore packages to find the slot we will always return a
   slot-name from the specified package if it exists, otherwise we return the
   slot-name we found if its in a different package"
  (unless (and o (typep o '(or standard-object condition)))
    (return-from has-slot? nil))
  (let ((match (ensure-slot-name slot-name))
        (slot-names (class-slot-names o))
        lax)
    (or
     (iter (for sn in slot-names)
       (cond
         ;; exact match - always return this first if we find it
         ((eql sn match)
          (return sn))
         ;; save the lax matches
         ((and lax? (equalper sn match))
          (push sn lax))))
     (cond
       ((< 1 (length lax))
        (access-warn "Multiple slots inexactly matched for ~a on ~a" slot-name o)
        (first (reverse lax)))
       (t (first lax))))))

(defun setf-if-applicable (new o fn)
  "If we find a setf function named (setf fn) that can operate on o then call
   that with value new "
  (handler-bind ((undefined-function
		  (lambda (c) (declare (ignore c))
		    (return-from setf-if-applicable nil))))
    (setf fn
	  (typecase fn
	    ((or keyword string symbol closer-mop:slot-definition)
             (has-writer? o fn))
	    (function fn)
            ((or number list) nil);; compound-keys and indexes
	    (T (access-warn "Not sure how to call a ~A" fn) ))))
  (etypecase fn
    (null nil)
    (standard-generic-function
     (when (compute-applicable-methods fn (list new o))
       (values (funcall fn new o) T)))
    (function (values (funcall fn new o) T))))

(defun call-if-applicable (o fn &key (warn-if-not-a-fn? t))
  "See if there is a method named fn specialized on o, or a function named fn
   and call it if so

   TODO: dont call macro functions/special forms, they are not applicable
   "
  (handler-bind ((undefined-function
                   (lambda (c) (declare (ignore c))
                     (return-from call-if-applicable nil))))
    (setf fn
	  (typecase fn
	    ((or keyword string closer-mop:slot-definition) (has-reader? o fn))
	    (symbol (symbol-function fn))
	    (function fn)
            ((or number list) nil);; compound-keys and indexes
	    (T (when warn-if-not-a-fn?
                 (access-warn "Not sure how to call a ~A" fn))))))

  (handler-case
      (etypecase fn
        (null nil)
        (standard-generic-function
         (when (compute-applicable-methods fn (list o))
           (values (funcall fn o) T)))
        (function (values (funcall fn o) T)))
    (unbound-slot (c) (declare (ignore c)))))

(defun call-applicable-fns (o &rest fns)
  "For an object and a list of fn/fn names, call-if-applicable repeatedly"
  (iter (for fn in fns)
	(setf o (call-if-applicable o fn)))
  o)

(defgeneric do-access  (o k &key test key type skip-call?)
  (:method ((o list) k &key (test (default-test)) (key (default-key))
                       type skip-call?)
    (declare (ignore skip-call?))
    (if (or (eql type :alist)
            (and (null type) (consp (first o))))
        ;;alist
        (let ((assoc (assoc k o :test (or test (default-test))
                                :key (or key (default-key)))))
          (values (cdr assoc) (and assoc t)))
        ;;plist
        (plist-val k o :test test :key key)))

  (:method ((o array) k &key type test key skip-call?)
    (declare (ignore type test key skip-call?))
    (when (< k (length o))
      (values (aref o k) t)))

  (:method ((o hash-table) k &key type test key skip-call?)
    (declare (ignore type test key skip-call?))
    (multiple-value-bind (res found) (gethash k o)
      (if found
          (values res found)
          (awhen (ignore-errors (string k))
            (gethash it o)))))
  
  (:method (o  k &key (test (default-test)) (key (default-key))
                 type skip-call?)
    ;; not specializing on standard-object here
    ;; allows this same code path to work with conditions (in sbcl)
    (let ((actual-slot-name (has-slot? o k)))
      (cond
        ;; same package as requested, must be no accessor so handle slots
        ((eql actual-slot-name k)
         (when (slot-boundp o k)
           (values (slot-value o k) t)))

        ;; lets recheck for an accessor in the correct package
        (actual-slot-name
         (access o actual-slot-name
                 :test (or test (default-test))
                 :key (or key (default-key))
                 :type type :skip-call? skip-call?))))))

(defun access (o k &key type (test (default-test)) (key (default-key))
                   skip-call?)
  "Access plists, alists, arrays, hashtables and clos objects
   all through the same interface

   skip-call, skips trying to call "
  (multiple-value-bind (res called)
      (unless skip-call?
        ;; lets suppress the warning if it is just being called through access
        (call-if-applicable o k :warn-if-not-a-fn? nil))
    (if called
        (values res t)
        (do-access o k :test test :key key :type type))))

(defun %initialize-null-container (o k type test)
  (or o
      (when type
        (cond
          ((or (member type '(:hash-table))
               (subtypep type 'hash-table))
           (make-hash-table :test (%to-hash-test test)))

          ((or (member type '(:array)) (subtypep type 'array))
           ;;make an array big enough to hold our key
           (make-array (apply #'+ 1 (ensure-list k))
                       :adjustable t
                       :initial-element nil))

          ((subtypep type 'standard-object)
           (make-instance type))))))

(defgeneric do-set-access (new o k &key type test key)
  
  ;; always return the new value as the first value
  ;; every primary method should return the modified object
  (:method :around (new o k &key type test key)
    (declare (ignore o k type test key))
    (values new (call-next-method)))
  
  (:method (new (o list) k &key type test key )
    (if (or (eql type :alist)
            (and (null type) (consp (first o))))
        ;;alist
        (aif (assoc k o :test test :key key)
             (progn (setf (cdr it) new)
                    o)
             (list* (cons k new) o))
        ;;plist
        (set-plist-val new k o :test test :key key)
        ))
  
  (:method (new (o array) k &key type test key)
    (declare (ignore type test key))
    #+clisp
    (setf (apply #'aref o (ensure-list k)) new)
    #-clisp
    (apply '(setf aref) new o (ensure-list k))
    o)

  (:method (new (o hash-table) k &key type test key)
    (declare (ignore type test key))
    (let ((skey (string k)))
      (multiple-value-bind (res found) (gethash k o)
        (declare (ignore res))
        (multiple-value-bind (sres sfound)
            (awhen skey (gethash it o))
          (declare (ignore sres))
          (cond
            (found (setf (gethash k o) new))
            (sfound (setf (gethash skey o) new))
            (T (setf (gethash k o) new))))))
    o)
  
  (:method (new (o standard-object) k &key type test key)
    (declare (ignore type test key))
    (let ((actual-slot-name (has-slot? o k)))
      (cond
        ;; same package so there must be no accessor
        ((eql actual-slot-name k)
         (setf (slot-value o k) new))
        ;; different package, but we have a slot, so lets look for its accessor
        (actual-slot-name
         (set-access new o actual-slot-name))
        ))
    o))

(defun set-access (new o k &key type (test #'equalper) (key #'identity))
  "set places in plists, alists, hashtables and clos objects all
   through the same interface"
  ;; make these easy to have the same defaults everywhere
  (unless test (setf test #'equalper))
  (unless key (setf key #'identity))
  (setf o (%initialize-null-container o k type test))
  (multiple-value-bind (res called) (setf-if-applicable new o k)
    (if called
        (values res o)
        (do-set-access new o k :type type :test test :key key))))

(defun %to-hash-test (test)
  (typecase test
    (null 'eql)
    (symbol test)
    ;; dequote double quoted tests
    ;; (its pretty easy to double quote with accesses)
    (list (eql 'quote (first test))
     (%to-hash-test (second test)))
    (function
     (cond
       ((eql test #'eql) 'eql)
       ((eql test #'equal) 'equal)
       ;; equalper does string based symbol comparing
       ;; we also handle this for hashtables separately
       ;; so I think this should be fine
       ((member test (list #'equalp #'equalper)) 'equalp)
       (t (error "Hashtable tests should be a symbol! See make-hashtable"))))))

(define-setf-expander access (place k
                              &key type test key
                              &aux (new-val (gensym "NEW-VAL"))
                              (place-store (gensym "PLACE"))
                              &environment env)
  "This should allow setting places through access"
  (declare (ignore env))
  (values ()   ;; not using temp vars
          ()   ;; not using temp vals
          `(,new-val)
          `(progn
            (multiple-value-bind (,new-val ,place-store)
                (set-access ,new-val ,place ,k :test ,test :type ,type :key ,key)
              (setf ,place ,place-store)
              ,new-val))
          `(access ,place ,k :test ,test :type ,type :key ,key )))

(defun accesses (o &rest keys)
  "keep accessing keys on resulting objects
   eg: (accesses o k1 k2) => (access (access o k1) k2)"
  (iter (for k in keys)
    (destructuring-bind (k &key type test key)
        (alexandria:ensure-list k)
      (setf o (access o k :test test :type type :key key ))))
  o)

(defun set-accesses (new o &rest keys)
  "keep accessing till you get to the end of keys , then store the result of
   setting that field back up the call tree

   returns the new value and the object that was stored there
   (so for a plist / alist you have a ref to the val and the full list)
  "
  (labels ((rec-set (o key more)
             (destructuring-bind (k &key type test key)
                 (alexandria:ensure-list key)
               ;(unless test (setf test #'equalper))
               ;(unless key (setf key #'identity))
               (cond
                 (more
                  (setf o (%initialize-null-container o k type test))
                  (multiple-value-bind (new new-place-val)
                      (rec-set (access o k :test test :type type :key key)
                               (first more) (rest more))
                    (setf (access o k :test test :type type :key key) new-place-val)
                    (values new o)))
                 (T (set-access new o k :test test :type type :key key))))))
    (rec-set o (first keys) (rest keys))))

(define-setf-expander accesses (place &rest keys
                                &aux (new-val (gensym "NEW-VAL"))
                                (place-store (gensym "PLACE"))
                                &environment env)
  (declare (ignore env))
  (values ()   ;; not using temp vars
          ()   ;; not using temp vals
          `(,new-val)
          `(multiple-value-bind (,new-val ,place-store)
            (set-accesses ,new-val ,place ,@keys)
            (setf ,place ,place-store)
            ,new-val)
          `(accesses ,place ,@keys)))

(defun mutate-access (o k fn)
  "Mutate the value stored in key k on object o, by passing it through fn"
  (awhen (access o k)
    (setf (access o k) (funcall fn it))))

(defun access-copy (from to keys)
  "Copy the values on 'from' to 'to' for all of the keys listed  "
  (iter (for k in keys)
	(for (k1 k2) = (if (listp k) k (list k k)))
	(setf (access to k2) (access from k1))))

(defmacro with-access ((&rest keys) val-form &body body)
  "Similar to with-accessors except using the access functions"
  (let* ((gval (gensym "val"))
	 (forms
	  (iter (for k in keys)
		(for (k-to k-from) = (if (listp k) k (list k k)))
		(collect `(,k-to (access ,gval ',k-from))))))
    `(let ((,gval ,val-form))
       (declare (ignorable ,gval))
       (symbol-macrolet (,@forms)
	 ,@body
	 ))))

(defun %create-accessor-symbol-list (class)
  "Gets the slots off a class an builds binding like  (local::symbol orig::symbol)
   where local is the current *package* and orig is the original package of the symbol

   used in with-all-slot-accessors"
  (let ((class (etypecase class
		 (symbol (find-class class))
		 (standard-class class))))
    (closer-mop:ensure-finalized class)
    (iter (for slot-name in (class-slot-names class))
      ;; collect bindings of local-symbol to class-slot-name
      (collect (list (intern (symbol-name slot-name))
                     slot-name)))))

(defun %remove-quote-&-or (class-name)
  "remove any quote / ors so that list type-specifications"
  (typecase class-name
    (list
     (case (first class-name)
       (quote (%remove-quote-&-or (second class-name)))
       (or (%remove-quote-&-or (rest class-name)))
       (t class-name)))
    (symbol class-name)))

(defmacro with-access-values ((&rest bindings) obj &body body)
  "A macro which binds local variables from accessed values on object
   according to bindings

   bindings: (local-symbol-and-access-key
              or (local-symbol access-key)
               ...)
   obj: the thing we are accessing data from
  "
  (flet ((key-for (it)
           (etypecase it
             (symbol `(quote ,it))
             ((or string keyword list) it))))
    (let* ((o (gensym "OBJ"))
           (expanded-bindings
             (iter (for b in (ensure-list bindings))
               (when (first-iteration-p)
                 (collect `(,o ,obj)))
               (typecase b
                 (null)
                 (list (collect `(,(first b) (access ,o ,(key-for (second b))))))
                 (symbol (collect `(,b (access ,o ,(key-for b)))))))))
      `(let* ,expanded-bindings
        ,@body))))

(defun %with-all-slot-helper (data class-name body
                                   &key (with-name 'with-access)
                                   (add-ignorables? nil)
                                   &aux (sdata data))
  "A macro which binds (like with-access) all slot names of a class to a local
   symbolmacro let storing and retrieving using access

   class-name: a symbol or a list of class-names (symbols)
     to make this easier to call we ignore quote and or
     eg: 't1=>t1, (or 't1 't2 ...)=> (t1 t2 ...)
  "
  (setf with-name (%remove-quote-&-or with-name))
  (labels ((typed-form (class-name)
             (let* ((symlist (%create-accessor-symbol-list class-name)))
               `(,with-name ,symlist ,sdata
                 ,@(when add-ignorables?
                     `((declare (ignorable ,@(mapcar #'first symlist)))))
                 ,@body))))
    (setf class-name (%remove-quote-&-or class-name))
    (typecase class-name
      (list
       (setf sdata (gensym "DATA"))
       `(let ((,sdata ,data))
         (etypecase ,sdata
           ,@(iter (for cn in class-name)
               (setf cn (%remove-quote-&-or cn))
               (collect (list cn (typed-form cn)))))))
      (symbol (typed-form class-name)))))

(defmacro with-all-slot-accessors ((data class-name) &body body)
  "A macro which binds (like with-access) all slot names of a class to a local
   symbolmacro let storing and retrieving using access

   class-name: a symbol or a list of class-names (symbols)
     to make this easier to call we ignore quote and or
     eg: 't1=>t1, (or 't1 't2 ...)=> (t1 t2 ...)
  "
  (%with-all-slot-helper data class-name body))

(defmacro with-all-slot-access-values ((obj class) &body body)
  "A macro which binds local variables for each slot value in class
   as by access"
  (%with-all-slot-helper obj class body
    :with-name 'access:with-access-values
    :add-ignorables? t))

;;;; DOT Syntax stuff

(defun split-dot-sym (sym)
  (iter (for piece in (cl-ppcre:split "\\." (string sym)))
    (collect (intern piece (or (symbol-package sym) *package*)))))

(defun translate-dot-sym (sym)
  (let* ((pieces (split-dot-sym sym))
	 (fns (iter (for sym in (rest pieces))
		    (collect `(quote ,sym)))))
    (if (eql 1 (length pieces))
	sym
	`(accesses ,(first pieces) ,@fns))))

(defun dot-translate-walker (form)
  (typecase form
    (cons (cons (dot-translate-walker (car form))
		(dot-translate-walker (cdr form))))
    (symbol (translate-dot-sym form))
    (atom form)))

(defun name-has-dot? (n)
  (cl-ppcre:all-matches "\\." (string n)))

(defun replace-dot-calls (forms)
  (dot-translate-walker forms))

(defmacro with-dot (() &body body)
  `(progn ,@(replace-dot-calls body)))

(defun dot-reader (-stream- char arg)
  "Reads a form and replaces dot calls"
  (declare (ignore arg char))
  (first (replace-dot-calls (list (read -stream-)))))

(defvar *dot-previous-readtables* nil
  "A stack which holds the previous readtables that have been pushed
here by ENABLE-DOT-SYNTAX.")

(defun %enable-dot-syntax ()
  "Internal function used to enable reader syntax and store current
readtable on stack."
  (push *readtable*
        *dot-previous-readtables*)
  (setq *readtable* (copy-readtable))
  (set-dispatch-macro-character #\# #\D #'dot-reader)
  (values))

(defun %disable-dot-syntax ()
  "Internal function used to restore previous readtable." 
  (if *dot-previous-readtables*
    (setq *readtable* (pop *dot-previous-readtables*))
    (setq *readtable* (copy-readtable nil)))
  (values))

(defmacro enable-dot-syntax ()
  "Enable reader syntax."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-dot-syntax)))

(defmacro disable-dot-syntax ()
  "Restore readtable which was active before last call to If there was no such call, the standard
readtable is used."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-dot-syntax)))

