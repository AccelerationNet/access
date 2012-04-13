(in-package :access)

(defmethod arg-list-key-value (id arg-list &key (test #'equalper) (key #'identity))
  "Given an &rest value that contains a (partial) lambda list with keys somewhere in it,
   find the specified value for a given key"
  (iter (for (k v . rest) on arg-list)
    (unless (keywordp k) (next-iteration))
    (when (funcall test (funcall key k) id)
      (return v))))

(defmethod set-arg-list-key-value (new id arg-list
                                 &key (test #'equalper) (key #'identity) ensure?)
  "Set the keyword parameter id to the value new
   if ensure? then only set if it doesnt exist (in which case new acts as a default)"
  (iter
    (with skip?)
    (with len-1 = (- (length arg-list) 1))
    (for i from 0)
    (for (k v . rest) on arg-list)
    (when skip? (setf skip? nil) (next-iteration))
    (cond
      ;; we didnt get a keyword, so not it
      ((not (keywordp k))
       (collect k)
       ;; if we are the last possible spot to check for
       ;; keywords make sure we collect the final v
       (when (and (null rest) (= i len-1))
         (collect v)))
      ;; when we are the key to set
      ((funcall test (funcall key k) id)
       (collect k)
       (collect (if ensure? v new))
       (appending rest)
       (finish))
      ;; got a keyword, but not the correct one
      (t (collect k)
         ;; dont collect v if it is not a valid part of the arg-list
         ;; eg: '(:A :B :C) shouldnt collect an extra nil
         (unless (= i len-1) (collect v))
         (setf skip? t)))
    (when (null rest)
      (setf skip? t)
      (collect id)
      (collect new))))

(defmethod ensure-arg-list-key-value (default id arg-list &key (test #'equalper) (key #'identity))
  "Ensure that a specific keyword has a value (or default) in an appliable arg list"
  (set-arg-list-key-value default id arg-list :ensure? t :test test :key key ))

(defmethod rem-arg-list-key-value (id arg-list
                                   &key (test #'equalper) (key #'identity)
                                   &aux removed-val)
  "Remove a specific keyword and value from the "
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
   removed-val))

(defmacro set-arg-list-key-value! (new id place &key (test '#'equalper) (key '#'identity))
  `(setf ,place (set-arg-list-key-value ,new ,id ,place :test ,test :key ,key)))

(defmacro ensure-arg-list-key-value! (default id place &key (test '#'equalper) (key '#'identity))
  `(setf ,place (ensure-arg-list-key-value ,default ,id ,place :test ,test :key ,key)))

(defmacro rem-arg-list-key-value! (id place &key (test '#'equalper) (key '#'identity))
  `(setf ,place (rem-arg-list-key-value ,id ,place :test ,test :key ,key)))
