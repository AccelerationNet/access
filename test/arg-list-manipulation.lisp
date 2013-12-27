(in-package :access-test)

(define-test arg-list-key-value-test (:tags '(arglist))
  (assert-equal
   #\b (arg-list-key-value
        :key2
        '(:key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   #\b (arg-list-key-value
        :key2
        '(1 2 3 :key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   #\b (arg-list-key-value
        :key2
        '(:akey :key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   #\b (arg-list-key-value
        :key2
        '(:akey :akey2 :key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   #\b (arg-list-key-value
        :key2
        '(:akey :akey2 :akey3 :key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   nil (arg-list-key-value
        :key2
        '(:akey :akey2 :akey3)))
  (assert-equal
   nil (arg-list-key-value
        :key2
        '(1 #\a "b"))))

(defun write-then-read (val key list)
  (let ((new-list (set-arg-list-key-value val key list)))
    (arg-list-key-value key new-list)))

(defun setal (new key list)
  (set-arg-list-key-value new key list))

(define-test set-arg-list-key-value-test (:tags '(arglist))
  (assert-equal
   #\d (write-then-read #\d :key2 '(:key1 #\a :key2 #\b :key3 #\c) ))
  (assert-equal
   '(:key1 #\a :key2 #\d :key3 #\c)
   (setal #\d :key2 '(:key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   #\d (write-then-read #\d :key2 '(:key1 #\a :key2 #\b :key3 #\c) ))
  (assert-equal
   #\d (write-then-read #\d :key2 '(1 2 3 :key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   #\d (write-then-read
        #\d :key2 '(:akey :key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   #\d (write-then-read
        #\d :key2 '(:akey :akey2 :key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   #\d (write-then-read
        #\d :key2 '(:akey :akey2 :akey3 :key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   :key3 (write-then-read :key3 :key2 '(:akey :akey2 :akey3)))
  (assert-equal
   #\d (write-then-read #\d :key2 '(1 #\a "b")))
  (assert-equal
   #\d (write-then-read #\d :key2 '(:a :b :c)))
  (assert-equal
   '(:a :b :c :key2 #\d)
   (setal #\d :key2 '(:a :b :c)))
  (assert-equal
   #\d (write-then-read #\d :key2 '(:a :b :c :d)))
  (assert-equal
   '(:a :b :c :d :key2 #\d)
   (setal #\d :key2 '(:a :b :c :d))))

(define-test rem-arg-list-key-value-test (:tags '(arglist))
  (assert-equal
   '(:key1 #\a :key3 #\c)
   (rem-arg-list-key-value :key2 '(:key1 #\a :key2 #\b :key3 #\c) ))
  (assert-equal
   '(1 2 3 :key1 #\a :key3 #\c)
   (rem-arg-list-key-value :key2 '(1 2 3 :key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   '(:akey :key1 #\a :key3 #\c)
   (rem-arg-list-key-value :key2 '(:akey :key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   '(:akey :akey2 :key1 #\a :key3 #\c)
   (rem-arg-list-key-value :key2 '(:akey :akey2 :key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   '(:akey :akey2 :akey3 :key1 #\a :key3 #\c)
   (rem-arg-list-key-value :key2 '(:akey :akey2 :akey3 :key1 #\a :key2 #\b :key3 #\c)))
  (assert-equal
   '(:akey :akey2 :akey3) (rem-arg-list-key-value :key2 '(:akey :akey2 :akey3)))
  (assert-equal
   '(1 #\a "b") (rem-arg-list-key-value :key2 '(1 #\a "b")))
  (assert-equal
   '(:a :b :c)
   (rem-arg-list-key-value :key2 '(:a :b :c)))
  (assert-equal
   '(:a :b :c :d) (rem-arg-list-key-value :key2 '(:a :b :c :d)))
  (assert-equal
   '(:a :b :c :d :key2 #\d)
   (setal #\d :key2 '(:a :b :c :d))))

(define-test set-arg-list-key-value-nulls (:tags '(arglist))
  (assert-equal
   #\d (write-then-read #\d :key2 '() ))
  (assert-equal
   #\d (write-then-read #\d :key2 '( 1 ) ))
  (assert-equal
   #\d (write-then-read #\d :key2 '( :foo ) ))
  (assert-equal
   '(:foo :key2 #\d)
   (set-arg-list-key-value #\d :key2 '( :foo ))))