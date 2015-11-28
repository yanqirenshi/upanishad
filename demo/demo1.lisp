(in-package :upanishad-demo)

(defun prime-p (n)
  "Prime predicate copied from Java code"
  (cond ((< n 2) nil)
        ((= n 2) t)
        ((evenp n) nil)
        (t (let ((factor 3)
                 (square (ceiling (sqrt n))))
             (loop
               (unless (<= factor square)
                 (return-from prime-p t))
               (if (zerop (mod n factor))
                   (return-from prime-p nil)
                   (incf factor 2)))))))

(defclass numbers ()
  ((numbers-list :accessor get-numbers-list :initform nil))
  (:documentation "Object to hold our list of numbers"))

(defun tx-create-numbers-root (pool)
  "Transaction function to create a numbers instance as a root object"
  (setf (get-root-object pool :numbers) (make-instance 'numbers)))

(defun tx-add-number (pool number)
  "Transaction function to add a number to the numbers list"
  (let ((numbers (get-root-object pool :numbers)))
    (push number (get-numbers-list numbers))))

(defparameter *pool-location* (pathname "/tmp/demo1-pool/")
  "Filesystem location of the prevalence system")

(defun demo1 ()
  "Run the demo1 loop, computing primes and making the list of primes found persistent"
  (let ((pool (make-pool *pool-location*)))
    (unwind-protect
         (let* ((numbers (or (get-root-object pool :numbers)
                             (execute pool (make-transaction 'tx-create-numbers-root))))
                (numbers-list (get-numbers-list numbers))
                (candidate (if numbers-list (1+ (first numbers-list)) 0))
                (largest 0))
           (loop
             (when (> candidate (min most-positive-fixnum 16777215))
               (return))
             (when (prime-p candidate)
               (execute pool (make-transaction 'tx-add-number candidate))
               (setf largest candidate)
               (format t "Primes found: ~d. Largest: ~d~%" (length (get-numbers-list numbers)) largest))
             (incf candidate)))
      (close-open-streams pool))))

(defun benchmark1 ()
  (let (pool)
    (setf pool (make-pool *pool-location*))
    (totally-destroy pool)
    (execute pool (make-transaction 'tx-create-numbers-root))
    (time (dotimes (i 10000) (execute pool (make-transaction 'tx-add-number i))))
    (close-open-streams pool)
    (setf pool (time (make-pool *pool-location*)))
    (close-open-streams pool)))

(defun benchmark2 ()
  (let (pool)
    (setf pool (make-pool *pool-location*
                          :init-args '(:serializer serialize-sexp
                                       :deserializer deserialize-sexp
                                       :file-extension "sexp")))
    (totally-destroy pool)
    (execute pool (make-transaction 'tx-create-numbers-root))
    (time (dotimes (i 10000) (execute pool (make-transaction 'tx-add-number i))))
    (close-open-streams pool)
    (setf pool (time (make-pool *pool-location*)))
    (close-open-streams pool)))

