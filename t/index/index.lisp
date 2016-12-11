(defpackage :upanishad-test.index
  (:use :cl
        #:upanishad
        #:prove
        #:upanishad-test.test-utility
        #:upanishad.index)
  (:import-from :upanishad
                #:meme
                #:%id))
(in-package :upanishad-test.index)

(defparameter *test-pool-directory* (test-pool-directory "upanishad-test.index"))

(plan nil)

(subtest ":INDEX"
  (labels ((is-index (got expected)
             (is (class-symbol got)
                 (getf expected :class-symbol)
                 "class-symbol")
             (is (slot-symbol got)
                 (getf expected :slot-symbol)
                 "slot-symbol")
             (let ((value->object (value->object got)))
               (subtest "value->object"
                 (if (null (getf expected :value->object))
                     (progn
                       (ok (hash-table-p value->object))
                       (is (hash-table-count value->object)
                           0) "is default value")
                     (is value->object
                         (getf expected :value->object)
                         "is init value"))))))
    (subtest "without init"
      (is-index (make-instance 'slot-index-unique)
                '(:class-symbol nil
                  :slot-symbol nil
                  :value->object nil)))

    (subtest "without init"
      (let ((class-symbol 'slot-index-unique)
            (slot-symbol 'value->object)
            (value->object (make-hash-table)))
        (is-index (make-instance 'slot-index-unique
                                 :class-symbol class-symbol
                                 :slot-symbol slot-symbol
                                 :value->object value->object)
                  `(:class-symbol ,class-symbol
                    :slot-symbol ,slot-symbol
                    :value->object ,value->object))))))

(finalize)
