(in-package :access)

(defgeneric arg-list-key-value (id arg-list &key test key)
  (:documentation
   "Given an &rest value that contains a (partial) lambda list with keys somewhere in it,
   find the specified value for a given key")
  (:method (id arg-list &key (test #'equalper) (key #'identity))
    (iter (for (k v . rest) on arg-list)
      (unless (keywordp k) (next-iteration))
      (when (funcall test (funcall key k) id)
        (return v)))))

(defgeneric set-arg-list-key-value (new id arg-list &key test key ensure?)
  (:documentation
   "Set the keyword parameter id to the value new
   if ensure? then only set if it doesnt exist (in which case new acts as a default)")
  (:method (new id arg-list &key (test #'equalper) (key #'identity) ensure?)
    (cond
      ((< (length arg-list) 2)
       (append arg-list (list id new)))
      (t (iter
           (with skip?)
           (with len-1 = (- (length arg-list) 1))
           (for i from 0)
           (for (k v . rest) on arg-list)
           (when skip? (setf skip? nil) (next-iteration))
           (cond
             ;; we didnt get a keyword, so not it
             ((not (keywordp k))
              (collect k into res)
              ;; if we are the last possible spot to check for
              ;; keywords make sure we collect the final v
              (when (and (null rest) (= i len-1))
                (collect v  into res)))
             ;; when we are the key to set
             ((funcall test (funcall key k) id)
              (collect k into res)
              (collect (if ensure? v new) into res)
              (appending rest into res)
              (finish))
             ;; got a keyword, but not the correct one
             (t (collect k into res)
                ;; dont collect v if it is not a valid part of the arg-list
                ;; eg: '(:A :B :C) shouldnt collect an extra nil
                (unless (= i len-1) (collect v into res))
                (setf skip? t)))
           (when (null rest)
             (setf skip? t)
             (collect id into res)
             (collect new into res))
           (finally (return res)))))))

(defgeneric ensure-arg-list-key-value (default id arg-list &key test key)
  (:documentation
   "Ensure that a specific keyword has a value (or default) in an appliable arg list")
  (:method (default id arg-list &key (test #'equalper) (key #'identity))
    (set-arg-list-key-value default id arg-list :ensure? t :test test :key key )))

(defgeneric rem-arg-list-key-value (id arg-list &key test key)
  (:documentation
   "Remove a specific keyword and value from the ")
  (:method  (id arg-list
             &key (test #'equalper) (key #'identity)
             &aux removed-val)
    (values
     (iter
       (with skip?)
       (with len-1 = (- (length arg-list) 1))
       (for i from 0)
       (for (k v . rest) on arg-list)
       (when skip? (setf skip? nil) (next-iteration))
       ;; when we are not the key to remove
       (cond
         ((not (keywordp k))
          (collect k))
         ((not (funcall test (funcall key k) id))
          (setf skip? t)
          (collect k)
          (unless (= i len-1) (collect v)))
         (T
          (setf skip? t)
          (setf removed-val v))))
     removed-val)))

(defmacro set-arg-list-key-value! (new ids place &key (test '#'equalper) (key '#'identity))
  `(progn
    (iter (for id in (ensure-list ,ids))
      (setf ,place (set-arg-list-key-value ,new id ,place :test ,test :key ,key)))
    ,place))

(defmacro ensure-arg-list-key-value! (default ids place &key (test '#'equalper) (key '#'identity))
  `(progn
    (iter (for id in (ensure-list ,ids))
      (setf ,place (ensure-arg-list-key-value ,default id ,place :test ,test :key ,key)))
    ,place))

(defmacro rem-arg-list-key-value! (ids place &key (test '#'equalper) (key '#'identity))
  `(progn
    (iter (for id in (ensure-list ,ids))
      (setf ,place (rem-arg-list-key-value id ,place :test ,test :key ,key)))
    ,place))
