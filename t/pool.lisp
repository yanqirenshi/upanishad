(defpackage :upanishad-test.pool
  (:use :cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility)
  (:nicknames :up-test.pool))
(in-package :upanishad-test.pool)

(defparameter *test-pool-directory* (test-pool-directory "pool"))

(plan nil)


(subtest "::poolp"
  (with-pool (pool  *test-pool-directory*)
    (ok (up::poolp pool))
    (ok (not (up::poolp 1)))
    (ok (not (up::poolp nil)))))

(subtest "::get-root-object"
  (let* ((key 'x)
         (key-str (symbol-name key))
         (value 'x))
    (with-pool (pool  *test-pool-directory*)
      (is (up::get-root-object pool key) nil)
      (ok (setf (up::get-root-object pool key) value))
      (is (up::get-root-object pool key) value)
      (is-error (up::get-root-object pool key-str)
                'simple-error
                "Bad parameter"))))

(subtest "::get-index-object"
  (let* ((key 'x)
         (key-str (symbol-name key))
         (value 'x))
    (with-pool (pool  *test-pool-directory*)
      (is (up::get-index-object pool key) nil)
      (ok (setf (up::get-index-object pool key) value))
      (is (up::get-index-object pool key) value)
      (is-error (up::get-index-object pool key-str)
                'simple-error
                "Bad parameter"))))


(finalize)
