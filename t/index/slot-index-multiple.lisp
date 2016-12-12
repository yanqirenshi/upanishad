(defpackage :upanishad-test.slot-index-multiple
  (:use :cl
        #:upanishad
        #:prove
        #:upanishad-test.test-utility
        #:upanishad.index)
  (:import-from :upanishad
                #:meme
                #:%id))
(in-package :upanishad-test.slot-index-multiple)

(defparameter *test-pool-directory* (test-pool-directory "upanishad-test.slot-index-multiple"))

(plan nil)

(subtest "::slot-index-multiple-contexts" (skip 1 "wait"))

(subtest "::ensure-%id->object" (skip 1 "wait"))

(subtest "::add-object-add-multi" (skip 1 "wait"))

(subtest ":add-object"
  (let ((meme1 (make-instance 'meme1 :%id 1 :slot1 1))
        (meme2 (make-instance 'meme1 :%id 2 :slot1 1))
        (meme3 (make-instance 'meme1 :%id 3 :slot1 2)))

    (let ((index (up.index:make-slot-index 'meme1 'slot1 :multiple)))

      (subtest "add meme1"
        (add-object index meme1)
        (is-%id->value (%id->value index)
                       '((1 . 1))
                       "can update %id->objects")
        (is-value->objects (value->objects index)
                           `((1 (,meme1)))
                           "can update value->objects"))

      (subtest "add meme2"
        (add-object index meme2)
        (is-%id->value (%id->value index)
                       '((1 . 1) (2 . 1))
                       "can update %id->objects")
        (is-value->objects (value->objects index)
                           `((1 (,meme1 ,meme2)))
                           "can update value->objects"))

      (subtest "add meme3"
        (add-object index meme3)
        (is-%id->value (%id->value index)
                       '((1 . 1) (2 . 1) (3 . 2))
                       "can update %id->objects")
        (is-value->objects (value->objects index)
                           `((1 (,meme1 ,meme2))
                             (2 (,meme3)))
                           "can update value->objects")))))

(subtest ":add-objects"
  (let ((meme1 (make-instance 'meme1 :%id 1 :slot1 1))
        (meme2 (make-instance 'meme1 :%id 2 :slot1 1))
        (meme3 (make-instance 'meme1 :%id 3 :slot1 2)))

    (let ((index (up.index:make-slot-index 'meme1 'slot1 :multiple)))
      (add-objects index (list meme1 meme2 meme3))
      (is-%id->value (%id->value index)
                     '((1 . 1) (2 . 1) (3 . 2))
                     "can update %id->objects")
      (is-value->objects (value->objects index)
                         `((1 (,meme1 ,meme2))
                           (2 (,meme3)))
                         "can update value->objects"))))

(subtest "::add-object-remove-multi" (skip 1 "wait"))

(subtest ":remove-object"
  (let ((meme1 (make-instance 'meme1 :%id 1 :slot1 1))
        (meme2 (make-instance 'meme1 :%id 2 :slot1 1))
        (meme3 (make-instance 'meme1 :%id 3 :slot1 2)))

    (let ((index (up.index:make-slot-index 'meme1 'slot1 :multiple)))
      (add-objects index (list meme1 meme2 meme3))
      (remove-object index meme2)

      (is-%id->value (%id->value index)
                     '((1 . 1) (3 . 2))
                     "can update %id->objects")
      (is-value->objects (value->objects index)
                         `((1 (,meme1))
                           (2 (,meme3)))
                         "can update value->objects"))))

(subtest ":change-object"
  (let ((meme1 (make-instance 'meme1 :%id 1 :slot1 1))
        (meme2 (make-instance 'meme1 :%id 2 :slot1 1))
        (meme3 (make-instance 'meme1 :%id 3 :slot1 2)))

    (let ((index (up.index:make-slot-index 'meme1 'slot1 :multiple)))
      (add-objects index (list meme1 meme2 meme3))

      (setf (slot1 meme2) 2)

      (change-object index meme2)

      (is-%id->value (%id->value index)
                     '((1 . 1) (2 . 2) (3 . 2))
                     "can update %id->objects")

      (is-value->objects (value->objects index)
                         `((1 (,meme1))
                           (2 (,meme2 ,meme3)))
                         "can update value->objects"))))

(finalize)
 
