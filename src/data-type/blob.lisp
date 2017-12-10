(in-package :upanishad)


(defmethod print-object ((blob blob) stream)
  (print-unreadable-object
      (blob stream :type t :identity t)
    (with-slots (id name mime-type) blob
      (format stream "#~d \"~a\" ~a" id name mime-type))))


(defvar *blob-root* nil
  "The directory in which to store the blob files")


(defmethod get-file ((blob blob))
  (merge-pathnames (princ-to-string (%id blob)) *blob-root*))


(defmethod get-size :before ((blob blob))
  (with-slots (size) blob
    (when (eql size -1)
      (setf size (size-from-file blob)))))


(defun copy-stream (in out &optional (element-type '(unsigned-byte 8)))
  "Copy everything from in to out"
  (let* ((buffer-size 4096)
         (buffer (make-array buffer-size :element-type element-type)))
    (labels ((read-chunks ()
               (let ((size (read-sequence buffer in)))
                 (if (< size buffer-size)
                     (write-sequence buffer out :start 0 :end size)
                     (progn
                       (write-sequence buffer out)
                       (read-chunks))))))
      (read-chunks))))


(defmethod fill-from-stream ((blob blob) binary-input-stream)
  "Fill the blob's contents with the bytes from binary-input-stream"
  (with-open-file (out (get-file blob)
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (copy-stream binary-input-stream out)))


(defmethod copy-to-stream ((blob blob) binary-output-stream)
  "Copy the bytes from blob to binary-output-stream"
  (with-open-file (in (get-file blob)
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (copy-stream in binary-output-stream)))


(defmethod fill-from-file ((blob blob) pathname)
  "Fill the blob's contents with the bytes read from the binary file at pathname"
  (with-open-file (in pathname :direction :input :element-type '(unsigned-byte 8))
    (fill-from-stream blob in))
  (push (format nil "~a.~a"
                (or (pathname-name pathname) "")
                (or (pathname-type pathname) ""))
        (get-keywords blob)))


(defmethod destroy ((blob blob))
  (when (probe-file (get-file blob))
    (delete-file (get-file blob)))
  (push :destroyed (get-keywords blob)))


(defmethod size-from-file ((blob blob))
  (let ((path (get-file blob)))
    (if (probe-file path)
        (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
          (file-length in))
        -1)))


(defmethod set-size-from-file ((blob blob))
  (with-slots (size) blob
    (setf size (size-from-file blob))))
