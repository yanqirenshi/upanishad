(in-package :upanishad-test.prevalence)

(defun serialize-and-deserialize-xml (object)
  (with-input-from-string
      (in (with-output-to-string (out)
            (serialize-xml object out)))
    (deserialize-xml in)))

(defun serialize-and-deserialize-sexp (object)
  (with-input-from-string
      (in (with-output-to-string (out)
            (serialize-sexp object out)))
    (deserialize-sexp in)))

(defun circular-list (&rest elements)
  (let ((cycle (copy-list elements)))
    (nconc cycle cycle)))

(defclass foobar ()
  ((foo :accessor get-foo :initarg :foo)
   (bar :accessor get-bar :initarg :bar)))

(defparameter *foobar* (make-instance 'foobar :foo 100 :bar "Bar"))

(defstruct foobaz foo baz)

(defparameter *foobaz* (make-foobaz :foo 100 :baz "Baz"))

(defparameter *hashtable*
  (let ((hashtable (make-hash-table :test 'equal)))
    (map nil
         #'(lambda (feature) (setf (gethash (symbol-name feature) hashtable) feature))
         *features*)
    hashtable))

(defparameter *empty-hashtable* (make-hash-table))

;;;
;;; Test
;;;
(plan 5)

;;;
;;; 1. primitivesn
;;;
(subtest "1. primitivesn"
  (is (serialize-and-deserialize-xml nil)
      nil "test-primitive-1")

  (is (serialize-and-deserialize-sexp nil)
      nil "test-primitive-2")

  (is (serialize-and-deserialize-xml t)
      t :test 'eq "test-primitive-3")

  (is (serialize-and-deserialize-sexp t)
      t :test 'eq "test-primitive-4")

  (ok (= (serialize-and-deserialize-xml 100)
         100) "test-primitive-5")

  (is (serialize-and-deserialize-sexp 100)
      100 "test-primitive-6")

  (is (serialize-and-deserialize-xml (/ 3))
      (/ 3) "test-primitive-7")

  (is (serialize-and-deserialize-sexp (/ 3))
      (/ 3) "test-primitive-8")

  (is (serialize-and-deserialize-xml pi)
      pi "test-primitive-9")

  (is (serialize-and-deserialize-sexp pi)
      pi "test-primitive-10")

  (is (serialize-and-deserialize-xml (complex 1.5 2.5))
      (complex 1.5 2.5) "test-primitive-11")

  (is (serialize-and-deserialize-sexp (complex 1.5 2.5))
      (complex 1.5 2.5) "test-primitive-12")

  (is (serialize-and-deserialize-xml 'foo)
      'foo :test 'eq "test-primitive-13")

  (is (serialize-and-deserialize-sexp 'foo)
      'foo :test 'eq "test-primitive-14")

  (is (serialize-and-deserialize-xml :foo)
      :foo :test 'eq "test-primitive-15")

  (is (serialize-and-deserialize-sexp :foo)
      :foo :test 'eq "test-primitive-16")

  (is (serialize-and-deserialize-xml 'room)
      'room :test 'eq "test-primitive-17")

  (is (serialize-and-deserialize-sexp 'room)
      'room :test 'eq "test-primitive-18")

  (is (serialize-and-deserialize-xml '|Unprintable|)
      '|Unprintable| :test 'eq "test-primitive-19")

  (is (serialize-and-deserialize-sexp '|Unprintable|)
      '|Unprintable| :test 'eq "test-primitive-20")

  (let ((sym (gensym)))
    (is (princ-to-string (serialize-and-deserialize-sexp sym))
        (princ-to-string sym)
        "test-uninterned-symbol-sexp"))

  (let ((sym (gensym)))
    (is (princ-to-string (serialize-and-deserialize-xml sym))
        (princ-to-string sym)
        "test-uninterned-symbol-xml"))

  (is (serialize-and-deserialize-xml "Hello")
      "Hello"
      "test-primitive-21")

  (is (serialize-and-deserialize-sexp "Hello")
      "Hello" "test-primitive-22")

  (is (serialize-and-deserialize-xml "")
      "" "test-primitive-23")

  (is (serialize-and-deserialize-sexp "")
      "" "test-primitive-24")

  (is (serialize-and-deserialize-xml #\A)
      #\A "test-primitive-25")

  (is (serialize-and-deserialize-sexp #\A)
      #\A
      "test-primitive-26")

  (is (serialize-and-deserialize-xml #\<)
      #\< "test-primitive-27")

  (is (serialize-and-deserialize-sexp #\<)
      #\< "test-primitive-28")

  (is (serialize-and-deserialize-xml "Hello <foo> & </bar>!")
      "Hello <foo> & </bar>!" "test-primitive-29")

  (is (serialize-and-deserialize-sexp "Hello <foo> & </bar>!")
      "Hello <foo> & </bar>!" "test-primitive-30"))

;;;
;;; 2. simple sequences
;;;
(subtest "2. simple sequences"
  (ok (reduce #'(lambda (x &optional (y t)) (and x y))
              (map 'list
                   #'eql
                   (serialize-and-deserialize-xml (list 1 2 3))
                   (list 1 2 3)))
      "test-simple-sequences-1")

  (ok (reduce #'(lambda (x &optional (y t)) (and x y))
              (map 'list
                   #'eql
                   (serialize-and-deserialize-sexp (list 1 2 3))
                   (list 1 2 3)))
      "test-simple-sequences-2")

  (is (serialize-and-deserialize-xml (list 1 2 3))
      (list 1 2 3) "test-simple-sequences-3")

  (is (serialize-and-deserialize-sexp (list 1 2 3))
      (list 1 2 3) "test-simple-sequences-4")

  (is (serialize-and-deserialize-xml (cons 1 2))
      (cons 1 2) "test-simple-sequences-5")

  (is (serialize-and-deserialize-sexp (cons 1 2))
      (cons 1 2) "test-simple-sequences-6")

  (is (serialize-and-deserialize-xml '(1 2 3 4 5 6 7 8 9 . 0))
      '(1 2 3 4 5 6 7 8 9 . 0) "test-simple-sequences-7")

  (is (serialize-and-deserialize-sexp '(1 2 3 4 5 6 7 8 9 . 0))
      '(1 2 3 4 5 6 7 8 9 . 0) "test-simple-sequences-8")

  (is (serialize-and-deserialize-xml (cons 'hi 2))
      (cons 'hi 2) "test-simple-sequences-9")

  (is (serialize-and-deserialize-sexp (cons 'hi 2))
      (cons 'hi 2) "test-simple-sequences-10")

  (subtest "circular-list"

    (is (third (serialize-and-deserialize-sexp (circular-list 'a 'b)))
        'a "test-circular-list-1")

    (is (third (serialize-and-deserialize-xml (circular-list 'a 'b)))
        'a
        "test-circular-list-2")

    (is (serialize-and-deserialize-xml (cons 'hi 2))
        (cons 'hi 2)
        "test-circular-list-3")

    (is (serialize-and-deserialize-sexp (cons 'hi 2))
        (cons 'hi 2)
        "test-circular-list-4")

    (is (third (serialize-and-deserialize-sexp (circular-list 'a 'b)))
        'a
        "test-circular-list-5")

    (is (third (serialize-and-deserialize-xml (circular-list 'a 'b)))
        'a "test-circular-list-6")))

;;;
;;; 3. simple objects
;;;
(subtest "3. simple objects"

  (subtest "test-simple-objects-1"
    (let ((foobar (serialize-and-deserialize-xml *foobar*)))
      (ok (and (equal (get-foo foobar) (get-foo *foobar*))
               (equal (get-bar foobar) (get-bar *foobar*))
               (eq (class-of foobar) (class-of *foobar*))))))

  (subtest "test-simple-objects-2"
    (let ((foobar (serialize-and-deserialize-sexp *foobar*)))
      (ok (and (equal (get-foo foobar) (get-foo *foobar*))
               (equal (get-bar foobar) (get-bar *foobar*))
               (eq (class-of foobar) (class-of *foobar*)))))))

;;;
;;; 4. standard structs
;;;
(subtest "4. standard structs"

  (subtest "test-standard-structs-1"
    (let ((foobaz (serialize-and-deserialize-xml *foobaz*)))
      (ok (and (foobaz-p foobaz)
               (equal (foobaz-foo foobaz) (foobaz-foo *foobaz*))
               (equal (foobaz-baz foobaz) (foobaz-baz *foobaz*))))))

  (subtest "test-standard-structs-2"
    (let ((foobaz (serialize-and-deserialize-sexp *foobaz*)))
      (ok (and (foobaz-p foobaz)
               (equal (foobaz-foo foobaz) (foobaz-foo *foobaz*))
               (equal (foobaz-baz foobaz) (foobaz-baz *foobaz*)))))))

;;;
;;; 5. hash-tables
;;;
(subtest "5. hash-tables"

  (subtest "test-hash-tables-1"
    (let (h2)
      (setf h2 (serialize-and-deserialize-xml *hashtable*))
      (maphash #'(lambda (k v) (ok (equal v (gethash k h2)))) *hashtable*)
      (maphash #'(lambda (k v) (ok (equal v (gethash k *hashtable*)))) h2)))

  (subtest "test-hash-tables-2"
    (let (h2)
      (setf h2 (serialize-and-deserialize-sexp *hashtable*))
      (maphash #'(lambda (k v) (ok (equal v (gethash k h2)))) *hashtable*)
      (maphash #'(lambda (k v) (ok (equal v (gethash k *hashtable*)))) h2)))

  (subtest "test-empty-hash-tables-1"
    (let (h2)
      (setf h2 (serialize-and-deserialize-xml *empty-hashtable*))
      (maphash #'(lambda (k v) (ok (equal v (gethash k h2)))) *empty-hashtable*)
      (maphash #'(lambda (k v) (ok (equal v (gethash k *hashtable*)))) h2)))

  (subtest "test-empty-hash-tables-2"
    (let (h2)
      (setf h2 (serialize-and-deserialize-sexp *empty-hashtable*))
      (maphash #'(lambda (k v) (ok (equal v (gethash k h2)))) *empty-hashtable*)
      (maphash #'(lambda (k v) (ok (equal v (gethash k *hashtable*)))) h2))))

(finalize)
