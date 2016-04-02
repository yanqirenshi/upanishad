(defpackage :upanishad-test.master-slave
  (:use #:cl
        #:upanishad
        #:prove
        #:s-serialization
        #:upanishad-test.test-utility))
(in-package :upanishad-test.master-slave)

(defparameter *test-pool-directory* (test-pool-directory "master-slave"))

(plan 4)


(subtest ":start-master-client" (skip 1 "準備中"))
(subtest ":stop-master-client" (skip 1 "準備中"))
(subtest ":start-slave-server" (skip 1 "準備中"))
(subtest ":stop-slave-server" (skip 1 "準備中"))


(finalize)
